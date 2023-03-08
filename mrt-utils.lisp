(in-package :sbgp)

(defun mrt-read (filename-in &key (start-record 1) (record-count nil) (mrt-msg-txt-out nil) (mrt-msg-bin-out nil) (bgp-msg-txt-out nil) (bgp-msg-bin-out nil) (sbgp-update-txt-out nil))
  (let ((mrt-msg-counter 0)
	(bgp-msg-counter 0)
	(bgp-open-counter 0)
	(bgp-update-counter 0)
	(bgp-notification-counter 0)
	(bgp-keepalive-counter 0)
	(bgp-route-refresh-counter 0)
	(unknown-message-counter 0)
	(stream-in (open filename-in :direction :input :element-type 'unsigned-byte))
	(mrt-msg-txt-stream-out (if mrt-msg-txt-out
				    (if (eq mrt-msg-txt-out t)
					t
					(open mrt-msg-txt-out :direction :output :if-exists :supersede))
				    nil))
	(mrt-msg-bin-stream-out (if mrt-msg-bin-out
				    (open mrt-msg-bin-out :direction :output :if-exists :supersede :element-type 'unsigned-byte)
				    nil))
	(bgp-msg-txt-stream-out (if bgp-msg-txt-out
				    (if (eq bgp-msg-txt-out t)
					t
					(open bgp-msg-txt-out :direction :output :if-exists :supersede))
				    nil))
	(bgp-msg-bin-stream-out (if bgp-msg-bin-out
				    (open bgp-msg-bin-out :direction :output :if-exists :supersede :element-type 'unsigned-byte)
				    nil))
	(sbgp-update-txt-stream-out (if sbgp-update-txt-out
					(if (eq sbgp-update-txt-out t)
					    t
					    (open sbgp-update-txt-out :direction :output :if-exists :supersede))
					nil)))
    (handler-case
	(progn 
	  (loop for i from 1
		until (and record-count (>= i (+ start-record record-count)))
		do
		   (let ((mrt-message (MRT-MESSAGE-io-read stream-in)))
		     (incf mrt-msg-counter)
		     (when (>= i start-record)
		       (when mrt-msg-txt-stream-out
			 (format mrt-msg-txt-stream-out "~&~S~%" mrt-message))
		       (when mrt-msg-bin-stream-out
			 (MRT-MESSAGE-io-write mrt-message mrt-msg-bin-stream-out))
		       (let* ((mrt-header (MRT-MESSAGE-get-header mrt-message))
			      (message-type (MRT-COMMON-HEADER-get-type mrt-header))
			      (message-subtype (MRT-COMMON-HEADER-get-subtype mrt-header)))
			 
			 (cond ((and (= 17 message-type)         ;; BGP4MP-AS4
				     (= 4 message-subtype))      ;; BGP4MP-MESSAGE-AS4
				(let* ((mrt-bgp4mp-message (MRT-MESSAGE-get-message mrt-message))
				       (bgp-message (MRT-BGP4MP-MESSAGE-AS4-get-bgp-message mrt-bgp4mp-message)))
				  (incf bgp-msg-counter)
				  (when bgp-msg-txt-stream-out
				    (format bgp-msg-txt-stream-out "~&~S~%" bgp-message))
				  (when bgp-msg-bin-stream-out
				    (BGP-MESSAGE-io-write t bgp-message bgp-msg-bin-stream-out))
				;;; bgp-message at this point has header, strip off header
				  (let ((bgp-message (BGP-MESSAGE-get-message bgp-message)))
				    (case (TL-get-name bgp-message)
				      (BGP-OPEN
				       (incf bgp-open-counter))
				      (BGP-UPDATE
				       (incf bgp-update-counter)
				       (let ((sbgp-update (BGP-UPDATE->SBGP-UPDATE bgp-message)))
					 (when sbgp-update-txt-stream-out
					   (format sbgp-update-txt-stream-out "~&~S~%" sbgp-update))))
				      (BGP-NOTIFICATION
				       (incf bgp-notification-counter))
				      (BGP-KEEPALIVE
				       (incf bgp-keepalive-counter))
				      (BGP-ROUTE-REFRESH
				       (incf bgp-route-refresh-counter))
				      (t
				       (incf unknown-message-counter))))))
			       (t (incf unknown-message-counter)))))))
	  (signal 'end-of-file))
	  
      (end-of-file ()
	(format t "~&End-of-file~%")
	(if stream-in (close stream-in))
	(if (and mrt-msg-txt-stream-out
		 (not (eq mrt-msg-txt-stream-out t)))
	    (close mrt-msg-txt-stream-out))
	(if mrt-msg-bin-stream-out
	    (close mrt-msg-bin-stream-out))
	(if (and bgp-msg-txt-stream-out
		 (not (eq bgp-msg-txt-stream-out t)))
	    (close bgp-msg-txt-stream-out))
	(if bgp-msg-bin-stream-out
	    (close bgp-msg-bin-stream-out))
	(if (and sbgp-update-txt-stream-out
		   (not (eq sbgp-update-txt-stream-out t)))
	      (close sbgp-update-txt-stream-out))
	
	(format t "~&~S mrt-messages~%~S bgp-messages~%~S bgp-open-counter~%~S bgp-update-counter~%~S bgp-notification-counter~%~S bgp-keepalive-counter~%~S bgp-route-refresh-counter~%~S unknown-message-counter~%" mrt-msg-counter bgp-msg-counter bgp-open-counter bgp-update-counter bgp-notification-counter bgp-keepalive-counter bgp-route-refresh-counter unknown-message-counter)
	))))


(defun mrt-updates-split-peers (output-files-prefix)
  (let ((input-file-names (directory (pathname "updates.*.*")))
	(peer-mrt-ht (make-hash-table :test 'equal))
	(peer-bgp-ht (make-hash-table :test 'equal)))
    
    (format t "~&input-file-name: ~S~%output-prefix: ~S~%" input-file-names output-files-prefix)

    (when input-file-names
      (dolist (input-file-name input-file-names)
	(handler-case
	    (with-open-file (stream-in input-file-name :direction :input :element-type 'unsigned-byte)
	      (format t "opened: ~&~S: ~S~%" input-file-name stream-in)
	      (loop 
		(let* ((mrt-message (MRT-MESSAGE-io-read stream-in))
		       (mrt-header (MRT-MESSAGE-get-header mrt-message))
		       (message-type (MRT-COMMON-HEADER-get-type mrt-header))
		       (message-subtype (MRT-COMMON-HEADER-get-subtype mrt-header))
		       (message (MRT-MESSAGE-get-message mrt-message)))
		  (when (and (= 17 message-type)                           ;; BGP4MP-AS4
			     (= 4 message-subtype))                        ;; BGP4MP-MESSAGE-AS4
		    (let ((peer-address (MRT-BGP4MP-MESSAGE-AS4-get-peer-ip-address message)))
		      (let ((output-stream (or (gethash peer-address peer-mrt-ht)
					       (let ((output-filename (concatenate 'string output-files-prefix "-mrt-"
										   (case (car peer-address)
										     (IPV4 (u32-pprint-ipv4 nil (cadr peer-address)))
										     (IPV6 (list-u32-pprint-ipv6 nil (cdr peer-address)))))))
						 (format t "~&adding output peer: ~S, output-filename: ~S~%" peer-address output-filename)
						 (setf (gethash peer-address peer-mrt-ht)
						       (open output-filename :direction :output :element-type 'unsigned-byte))))))
			(MRT-MESSAGE-io-write mrt-message output-stream))
		      (let ((output-stream (or (gethash peer-address peer-bgp-ht)
					       (let ((output-filename (concatenate 'string output-files-prefix "-bgp-"
										   (case (car peer-address)
										     (IPV4 (u32-pprint-ipv4 nil (cadr peer-address)))
										     (IPV6 (list-u32-pprint-ipv6 nil (cdr peer-address)))))))
						 (format t "~&adding output peer: ~S, output-filename: ~S~%" peer-address output-filename)
						 (setf (gethash peer-address peer-bgp-ht)
						       (open output-filename :direction :output :element-type 'unsigned-byte))))))
			(BGP-MESSAGE-io-write t (MRT-BGP4MP-MESSAGE-AS4-get-bgp-message message) output-stream))))))
	      (sleep 1))
	  (end-of-file () nil)))
      (maphash #'(lambda (key value)
		   (format t "~&peer ~S closing ~S~%" key value)
		   (close value))
	       peer-mrt-ht))))

(defun mrt-rib-import (filename-in rib-loc &key (record-count nil) (nlri-cache nil) (set-new-announcement-flag nil) (path-attrib-cache nil) (path-attrib-list-cache nil))
  (let ((*nlri-cache* nlri-cache)
	(*path-attrib-cache* path-attrib-cache)
	(*path-attrib-list-cache* path-attrib-list-cache)
	(mrt-message-counter 0)
	(rib-table-counter 0)
	(rib-entry-counter 0)
	(peer-array (make-array 32)))

    (handler-case 
	;; read mrt-peer-index-table
	(let* ((stream-in (open filename-in :direction :input :element-type 'unsigned-byte))
	       (mrt-message (MRT-MESSAGE-io-read stream-in)))
	  (incf mrt-message-counter)
	  (let* ((mrt-peer-index-table (tl-find-in-tree 'MRT-PEER-INDEX-TABLE mrt-message))
		 (mrt-peer-entries (MRT-PEER-INDEX-TABLE-get-peer-entries mrt-peer-index-table)))
	    ;; (format t "~&peer-entries:~%~S~%" mrt-peer-entries)
	    (dolist (mrt-peer-entry mrt-peer-entries)
	      
	      (let ((rib-peer (RIB-PEER-make (intern (format nil "MRT-PEER-~D" (MRT-PEER-INDEX-TABLE-ENTRY-get-index-number mrt-peer-entry)))
					     'EXTERNAL
					     (MRT-PEER-INDEX-TABLE-ENTRY-get-peer-bgp-id mrt-peer-entry)
					     (MRT-PEER-INDEX-TABLE-ENTRY-get-peer-ip-address mrt-peer-entry)
					     (MRT-PEER-INDEX-TABLE-ENTRY-get-peer-as mrt-peer-entry))))
		;; (format t "~&Adding peer: ~S~%" rib-peer)

	        (RIB-LOC-add-peer rib-loc rib-peer)
		(setf (svref peer-array (MRT-PEER-INDEX-TABLE-ENTRY-get-index-number mrt-peer-entry))
		      (RIB-LOC-get-rib-peer rib-loc
					    (RIB-PEER-get-thread-name rib-peer)))))

	    (loop for i from 0
		  until (and record-count (>= i record-count))
		  do (let ((mrt-message (MRT-MESSAGE-io-read stream-in)))
		       (incf mrt-message-counter)
		       (let* ((mrt-header (MRT-MESSAGE-get-header mrt-message))
			      (message-type (MRT-COMMON-HEADER-get-type mrt-header))
			      (message-subtype (MRT-COMMON-HEADER-get-subtype mrt-header)))
			 (when (= message-type 13)  ; TABLE_DUMP_V2
			   (case message-subtype
			     ((2 3 4 5)  ; RIB_IPV4_UNICAST / RIB_IPV4_MULTICAST / RIB_IPV6_UNICAST / RIB_IPV6_MULTICAST
			      (incf rib-table-counter)
			      (let ((mrt-rib (MRT-MESSAGE-get-message mrt-message)))
				;; (format t "~%MRT-RIB for processing:~%~S~%" mrt-rib)
				(let ((nlri (MRT-RIB-get-nlri mrt-rib)))
				  (dolist (mrt-rib-entry (MRT-RIB-get-rib-entries mrt-rib))
				    (incf rib-entry-counter)
				    (let ((peer-index (MRT-RIB-ENTRY-get-peer-index mrt-rib-entry))
					  (originated-time (MRT-RIB-ENTRY-get-originated-time mrt-rib-entry))
					  (bgp-attributes (MRT-RIB-ENTRY-get-bgp-attributes mrt-rib-entry)))

				      (let ((new-rib-entry
					      (RIB-ENTRY-make (NLRI-get-afisafi nlri)
							      (svref peer-array peer-index)
							      (RIB-ADJ-ENTRY-make nlri
										  bgp-attributes)
							      originated-time
							      (if set-new-announcement-flag
								  +RIB-ENTRY-flag-new-announcement+
								  0))))
					;; (format t "~%NEW RIB-ENTRY:~%~S~%"  new-rib-entry)
					(RIB-LOC-add-entry rib-loc new-rib-entry)
					))))))

			     (6          ; RIB_GENERIC
			      )
			     (t
			      nil))))))))
      (end-of-file () (format t "~&end-of-file!~%"))
      (sb-ext:file-does-not-exist () (format t "~&file-does-not-exist~%")))

    ;;(loop for i across peer-array do (print i))
    (format t "~&mrt-message-counter: ~S~%rib-table-counter: ~S~%rib-entry-counter: ~S~%"
	    mrt-message-counter rib-table-counter rib-entry-counter)))

  
  
  
  

  ;; -> populate rib-loc with rib-peers
