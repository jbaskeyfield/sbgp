(in-package :sbgp)

;;; ROUTER thread
;;; contains RIB-Loc
;;; control connection back to main thread
;;; child connection to PEER(s) and TCPSERVER

(define-T-THREAD-LOOP-FUNCTION ROUTER
  :let-env-vars
  ((tcpserver-thread nil)
   (peer-threads nil)

   (rib-loc nil)
   (rib-loc-flags 0)
   (*nlri-cache* nil)
   
   (router-config nil)
   (peer-configs nil))  ;; plist of <thread-name> PEER-CONFIG

  ;;; create TCPSERVER thread
  :thread-entry-block
  ((setf tcpserver-thread (THREAD-make 'TCPSERVER %this-thread-name #'TCPSERVER-thread-loop))
   (push tcpserver-thread %child-threads)
   (setf (getf %all-queues 'TCPSERVER)
	 (THREAD-get-control-queue tcpserver-thread)))
  
  :message-case-block
  ;; (case (MSG-get-command %message)
  ((ADD
    (case (MSG-get-arg1 %message)
      (PEER                                         ; example '(ADD PEER PEER1)
       (let* ((peer-name (MSG-get-arg2 %message))
	      (new-peer-thread (THREAD-make peer-name
					    %this-thread-name
					    #'PEER-thread-loop)))
	 (push new-peer-thread %child-threads)
	 (push new-peer-thread peer-threads)
	 (setf (getf %all-queues
		     (THREAD-get-thread-name-symbol new-peer-thread))
	       (THREAD-get-control-queue new-peer-thread))))))
   (GET
    (case (MSG-get-arg1 %message)
      (*NLRI-CACHE*    (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name '*NLRI-CACHE* *nlri-cache*)))
      (RIB-LOC         (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name rib-loc)))
      (ROUTER-CONFIG   (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name router-config)))
      (PEER-CONFIGS    (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name peer-configs)))))

   (SET
    (case (MSG-get-arg1 %message)
      (ROUTER-CONFIG                            ; example %message = '(SET ROUTER-CONFIG (ROUTER-CONFIG ROUTER1 ... 
       (let ((config-list (MSG-get-arg2 %message)))
	 (setf router-config config-list)
	 (dolist (peer peer-threads)
	   (THREAD-send-message peer %message))))

      (PEER-CONFIG
       (let* ((peer-config (MSG-get-arg2 %message))
	      (peer-name (PEER-CONFIG-get-peer-name peer-config)))
	 (setf (getf peer-configs peer-name) peer-config)
	 (QUEUE-send-message (getf %all-queues peer-name) %message)))
      
      (*NLRI-CACHE*
       (setf *nlri-cache* (MSG-get-arg2 %message))                               
       (dolist (peer peer-threads)
	 (THREAD-send-message peer %message))) 

      (RIB-LOC
       (setf rib-loc (MSG-get-arg2 %message)))))
   
   (ANNOUNCE-RIB-ADJ->RIB-LOC
    (let ((peer-id            (MSG-get-arg1 %message))
	  (rib-adj-entry-list (MSG-get-arg2 %message)))
      
      (setf rib-loc-flags (logior +RIB-ENTRY-flag-new-announcement+
				  rib-loc-flags))
      
      (dolist (rib-adj-entry rib-adj-entry-list)
	(RIB-LOC-add-entry RIB-Loc
			   (RIB-ENTRY-make (NLRI-get-afisafi (RIB-ADJ-ENTRY-get-nlri rib-adj-entry))
					   peer-id
					   rib-adj-entry
					   +RIB-ENTRY-flag-new-announcement+)))))
   (WITHDRAWL-RIB-ADJ->RIB-LOC
    (let ((peer-id            (MSG-get-arg1 %message))
	  (rib-adj-entry-list (MSG-get-arg2 %message)))
      
      (setf rib-loc-flags (logior +rib-entry-flag-new-withdrawl+
				  rib-loc-flags))

      (dolist (rib-adj-entry rib-adj-entry-list)
	(let ((rib-entry (RIB-LOC-find-rib-entry RIB-Loc
						 peer-id
						 rib-adj-entry)))
	  (if rib-entry
	      (RIB-ENTRY-set-new-withdrawl-flag rib-entry))))))
      
   (PRINT-ENV2
    (format t "~&~S PRINT-ENV2~%tcpserver-thread: ~S~%peer-threads: ~S~%RIB-Loc: ~S~%RIB-Loc-flagst: ~S~%router-config: ~S~%peer-configs: ~S~%"
	    %this-thread-name
	    tcpserver-thread
	    peer-threads
	    RIB-Loc
	    RIB-Loc-flags
	    router-config
	    peer-configs)))

  :loop-end-block
  (;; process updates/withdrwals to RIB-LOCs

   (unless (= 0 (logand rib-loc-flags +RIB-ENTRY-flag-new-announcement+))

     (format t "~&ENTERING LOOP-END-BLOCK process announcements~%")
     
     (let ((new-announcements (RIB-LOC-update-collect-entries rib-loc
							      :test-fn #'RIB-ENTRY-new-announcement-flag-setp
							      :update-fn #'RIB-ENTRY-clear-new-announcement-flag
							      :collect? t)))
        (loop for peer-thread in peer-threads
	      do (let ((rib-adj-entries
			 (map 'list #'RIB-ENTRY-get-rib-adj-entry
			      (remove-if #'(lambda (x) (eq (THREAD-get-thread-name-symbol peer-thread)
								    (RIB-ENTRY-get-peer-id x)))
						  new-announcements))))
		  (when rib-adj-entries
		    (THREAD-send-message peer-thread
					 (MSG-make 'ANNOUNCE-RIB-LOC->RIB-ADJ rib-adj-entries))))))
     
     ;; clear new-announcement-flag
     (setf rib-loc-flags (logandc1 +RIB-ENTRY-flag-new-announcement+
					        rib-loc-flags)))
   
   (unless (= 0 (logand rib-loc-flags +RIB-ENTRY-flag-new-withdrawl+))
     ;; process withdrawls
     (format t "~&ENTERING LOOP-END-BLOCK process withdrawls~%")
     (let ((new-withdrawls (RIB-LOC-delete-collect-entries rib-loc
							   :test-fn #'RIB-ENTRY-new-withdrawl-flag-setp
							   :collect? t)))
       (loop for peer-thread in peer-threads
	      do (let ((rib-adj-entries
			 (map 'list #'RIB-ENTRY-get-rib-adj-entry
			      (remove-if #'(lambda (x) (eq (THREAD-get-thread-name-symbol peer-thread)
								    (RIB-ENTRY-get-peer-id x)))
						  new-withdrawls))))
		  (when rib-adj-entries
		    (THREAD-send-message peer-thread
					 (MSG-make 'WITHDRWAL-RIB-LOC->RIB-ADJ rib-adj-entries))))))
     
     ; clear new-withdrawl-flag
     (setf rib-loc-flags (logandc1 +RIB-ENTRY-flag-new-withdrawl+
					        rib-loc-flags)))
   ))
