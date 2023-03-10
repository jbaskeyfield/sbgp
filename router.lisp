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
   (*nlri-cache* nil)
   
   (router-config nil)
   (peer-configs nil)   ;; plist of <thread-name> PEER-CONFIG
   (router-timers (ROUTER-TIMERS-make-default))
   (*debug-router-timers* *debug-router-timers*))

  ;;; create TCPSERVER thread
  :thread-entry-block
  ((setf tcpserver-thread (THREAD-make 'TCPSERVER %this-thread-name #'TCPSERVER-thread-loop))
   (push tcpserver-thread %child-threads)
   (setf (getf %all-queues 'TCPSERVER)
	 (THREAD-get-control-queue tcpserver-thread))
   
   (ROUTER-TIMERS-start-rib-loc-scan-Timer router-timers %this-thread-name))

   :loop-entry-block
   ;; check if any timers have been triggered. ROUTER-TIMERS-poll will enqueue events on %timers-queue
   ((ROUTER-TIMERS-poll ROUTER-timers %timers-queue %this-thread-name))
  
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
	       (THREAD-get-control-queue new-peer-thread))))
      (RIB-PEER     ;; message sent from peer thread to register itself in the rib-loc, creates entry in the rib-peer table
       (let ((peer-thread-name (MSG-get-arg2 %message))
	     (rib-peer (MSG-get-arg3 %message)))
	 (format t "~&SETTING RIB-PEER ~S : ~S~%" peer-thread-name rib-peer)
	 (RIB-LOC-add-peer rib-loc rib-peer)
	 (format t "~&RIB-LOC-get-peers: ~S~%" (RIB-LOC-get-peers rib-loc))))))
   
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
	  (rib-adj-entry-list (MSG-get-arg2 %message))
	  (originated-time (sb-posix:time)))

      ;; TODO rib-adj -> rib-loc filter/update function
      (RIB-LOC-set-new-announcements-flag rib-loc)
     
      (dolist (rib-adj-entry rib-adj-entry-list)
	(RIB-LOC-add-rib-adj-entry RIB-Loc rib-adj-entry peer-id originated-time)))) 
   
   (WITHDRAWL-RIB-ADJ->RIB-LOC
    (let ((peer-thread-id     (MSG-get-arg1 %message))
	  (rib-adj-entry-list (MSG-get-arg2 %message)))

      ;; TODO rib-adj -> rib-loc filter/update function
      (RIB-LOC-set-new-withdrawls-flag rib-loc)
      
      (dolist (rib-adj-entry rib-adj-entry-list)
	(let ((rib-entry (RIB-LOC-find-rib-entry RIB-Loc
						 peer-thread-id
						 rib-adj-entry)))
	  (if rib-entry
	      (RIB-ENTRY-set-new-withdrawl-flag rib-entry))))))

   
   (ROUTER-TIMERS-rib-loc-scan-Timer-Expires
    ;; process announcements
    (when (RIB-LOC-new-announcements-flag-set-p rib-loc)
      (format t "~&ENTERING LOOP-END-BLOCK process updates~%")
      ;; collect announcements with best-path flag set, to send to peer threads
      (let ((new-announcements (RIB-LOC-collect-if #'(lambda (rib-entry)
							(and (RIB-ENTRY-best-path-flag-setp rib-entry)
							     (RIB-ENTRY-new-announcement-flag-setp rib-entry)))
						   rib-loc)))
	(when new-announcements
	  ;; sort in path-attribute-list order
	  (let ((sorted-rib-entries (sort new-announcements
					  #'list-of-tl-greater-than-p
					  :key #'(lambda (x)
						   (RIB-ADJ-ENTRY-get-pa-list (RIB-ENTRY-get-rib-adj-entry x))))))
	    ;; send list to all peer threads
	    (loop for peer-thread in peer-threads
		  do (THREAD-send-message peer-thread
					  (MSG-make 'ANNOUNCE-RIB-LOC->RIB-ADJ sorted-rib-entries))))))

      ;; clear new-announcement flag on all rib entries
      (RIB-LOC-update-if #'RIB-ENTRY-new-announcement-flag-setp
			 #'RIB-ENTRY-clear-new-announcement-flag
			 rib-loc)
      ;; clear global new-announcement flag
      (RIB-LOC-clear-new-announcements-flag rib-loc))

    (when (RIB-LOC-new-withdrawls-flag-set-p rib-loc)
      ;; process withdrawls

      ;; collect withdrawn routes with best-path flag set, to send to peer threads
      (let ((new-withdrawls (RIB-LOC-collect-if #'(lambda (rib-entry)
						    (and (RIB-ENTRY-best-path-flag-setp rib-entry)
							 (RIB-ENTRY-new-withdrawl-flag-setp rib-entry)))
						rib-loc)))
	(when new-withdrawls
	  ;; sort in path-attribute-list order
	  (let ((sorted-rib-entries (sort new-withdrawls
					  #'list-of-tl-greater-than-p
					  :key #'(lambda (x)
						   (RIB-ADJ-ENTRY-get-pa-list (RIB-ENTRY-get-rib-adj-entry x))))))
	    ;; send list to all peer threads
	    (loop for peer-thread in peer-threads
		  do (THREAD-send-message peer-thread
					  (MSG-make 'WITHDRAWL-RIB-LOC->RIB-ADJ sorted-rib-entries))))))

      ;; delete all rib entries with new-withdrawl flag set
      (RIB-LOC-delete-if #'RIB-ENTRY-new-withdrawl-flag-setp
			 rib-loc)
      ;; clear global new-withdrawl flag
      (RIB-LOC-clear-new-withdrawls-flag rib-loc))

    ;; restart rib-loc-scan-timer
    (ROUTER-TIMERS-start-rib-loc-scan-Timer router-timers %this-thread-name))
    
   (PRINT-ENV2
    (format t "~&~S PRINT-ENV2~%tcpserver-thread: ~S~%peer-threads: ~S~%RIB-Loc: ~S~%router-config: ~S~%peer-configs: ~S~%"
	    %this-thread-name
	    tcpserver-thread
	    peer-threads
	    RIB-Loc
	    router-config
	    peer-configs)))

  :loop-end-block
  ())
