(in-package :sbgp)

(defun make-loop-idle-timer (&key (min-sleep-seconds 0.001) (max-sleep-seconds 0.2) (delay-increment 0.005))
  "Returns number of seconds slept"
  (let ((current-idle-delay max-sleep-seconds))
    #'(lambda (loop-message-count)
	;;(let ((time-now (get-internal-real-time)))

	  (cond ((> loop-message-count 0)                      
		 (setf current-idle-delay min-sleep-seconds)  
		 ;; (setf last-call-real-time time-now)
		 0)
		(t
		 (setf current-idle-delay (min max-sleep-seconds
					       (+ current-idle-delay
						  delay-increment)))
		 (sleep current-idle-delay)
		 current-idle-delay)))));)


(defun THREAD-make (thread-name-symbol parent-name-symbol thread-loop-function)
  "Creates worker thread. Returns: 
(list 'THREAD 
<A-end of duplex queue (B-end held by created thread)>
<thread name (symbol)>
<thread object>)"
  (format t "~&THREAD-make ~S ~S ~S~%" thread-name-symbol parent-name-symbol thread-loop-function)
  (force-output)
  
  (let* ((new-thread-name-symbol (if parent-name-symbol
				     (intern (concatenate 'string (symbol-name parent-name-symbol) "-" (symbol-name thread-name-symbol)) 'SBGP)
				     thread-name-symbol))
	 (queue-pair (QUEUE-PAIR-make))
	 (queue-a-end (QUEUE-PAIR-get-a-end queue-pair))
	 (queue-b-end (QUEUE-PAIR-get-b-end queue-pair)))
    (list  'THREAD
	   new-thread-name-symbol
	   queue-a-end
	   (sb-thread:make-thread thread-loop-function
				  :name (string new-thread-name-symbol)
				  :arguments (list new-thread-name-symbol queue-b-end)))))

(defmacro THREAD-get-thread-name-symbol (obj)   `(cadr ,obj))
(defmacro THREAD-get-control-queue (obj)        `(caddr ,obj))
(defmacro THREAD-get-sb-thread-object (obj)     `(cadddr ,obj))

(defmacro QUEUE-send-message (queue message)
  "Wrapper around QUEUE-send that optionally logs to output stream DEBUG-MESSAGES-STREAM.
NOTE: If QUEUE is full, function currently does does not wait for an empty slot. Just returns T or NIL depending on whether message was successfully enqueued in empty slot.
Returns T if message was sent successfully, or NIL if QUEUE is full"
  (let ((status (gensym "STATUS")))
    `(let ((,status (QUEUE-send ,queue ,message)))
       (when debug-messages-stream
	 (let ((*print-pretty* nil))
	   (if ,status
	       (format debug-messages-stream "~&~S~%" (list 'TX (QUEUE-get-name ,queue) (get-internal-real-time) ,message))
	       (format debug-messages-stream "~&~S~%" (list 'TX (QUEUE-get-name ,queue) (get-internal-real-time) 'TX-QUEUE-FULL))))
	 (if *t-thread-debug-messages-force-output* (force-output debug-messages-stream)))
       ,status)))

(defmacro QUEUE-receive-message (queue)
  "Wrapper around QUEUE-receive that optionally logs to output stream DEBUG-MESSAGES-STREAM.
Returns NIL if polled queue was empty, or returns message object from QUEUE and prints message to DEBUG-LOG-STREAM."
  (let ((message (gensym "MESSAGE")))
    `(let ((,message (QUEUE-receive ,queue)))
       (when (and ,message debug-messages-stream)
	 (let ((*print-pretty* nil))
	   (print (list 'RX (QUEUE-get-name ,queue) (get-internal-real-time) ,message) debug-messages-stream)))
       ,message)))

(defmacro THREAD-send-message (thread message)
  `(QUEUE-send-message (THREAD-get-control-queue ,thread) ,message))


(defmacro define-T-THREAD-LOOP-FUNCTION (name &key let-env-vars thread-entry-block quit-cleanup-block loop-entry-block message-case-block handler-case-block loop-end-block)
  (let ((function-name (intern (concatenate 'string (symbol-name name) (string "-THREAD-LOOP")))))
    `(defun ,function-name (%this-thread-name %control-queue)
       (let* ((%timers-queue (LOOPBACK-QUEUE-make))
	     ;; %all-queues is a p-list of <name> <queue> *. <name> is either a thread name or CONTROL|TIMERS 
	     (%all-queues          (list 'CONTROL %control-queue
					 'TIMERS %timers-queue))

	     ;; list of THREAD objects created by (THREAD-make name-symbol control-queue sb-thread:thread)
	     (%child-threads nil)             

	      (loop-idle-fn (make-loop-idle-timer))
	      (loop-messages-count 0)
	      ;;(%loop-idle-delay 1)             ;; time in seconds
	     (debug-messages-stream nil)      ;; debug text stream destination for text log of all received messages at top of main loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OBJECT ENVIRONMENT VARIABLES
	     ,@let-env-vars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     )
	 (when *t-thread-debug-messages-flag*
	   (let ((debug-filename (concatenate 'string "./debug/t-debug-" (symbol-name %this-thread-name) ".log")))
	     (setf debug-messages-stream (open debug-filename :direction :output :if-exists :append :if-does-not-exist :create))
	     (format debug-messages-stream "~&((LOGFILE-START ~S T-THREAD-MESSAGES ~S)" %this-thread-name (get-internal-real-time))
	     (force-output debug-messages-stream)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THREAD ENTRY BLOCK
	 ;; (format t "~&~S Entering thread-entry-block~%" %this-thread-name)
	 (block THREAD-ENTRY
	   ,@thread-entry-block)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 (handler-case
	     (let ((%message nil))
	       (loop named MAIN-THREAD-LOOP
		     do  ;; main loop with sleep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOOP ENTRY BLOCK
			 ;; (format t "~&~S Entering loop-entry-block~%" %this-thread-name)
		     ,@loop-entry-block
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		     ;; loop through all neighbor queues, read up to *rr-polling-bucket-size* from each queue before dropping out to main loop 
		       (loop for queue in (cdr %all-queues) by #'cddr
			     do (loop for i from 0 until (or (>= i *rr-polling-bucket-size*)              
							     (not (setf %message (QUEUE-receive-message queue))))
				      do (incf loop-messages-count)        ; counter-value passed to loop-idle-fn
					 (if (and (MSG-typep %message)     ; test message is properly formatted MSG object
						  (MSG-valid2-p %message))
					     (case (MSG-get-command %message)
					       (TO
						(let* ((destination (MSG-get-arg1 %message))
						       (destination-queue (getf %all-queues destination)))
						  (if destination-queue
						      (if (eq destination 'CONTROL)
							  (QUEUE-send-message destination-queue %message)            ; forward message unchanged
						          (QUEUE-send-message destination-queue (MSG-remove-arg1-arg2 %message)))))) ; strip off "TO <thread-id> header"
					       (PING
						(QUEUE-send-message %control-queue (MSG-make 'PONG %this-thread-name %message)))

					       (PONG
						(QUEUE-send-message %control-queue (MSG-make 'PONG %this-thread-name %message)))

					       (QUIT
						;;(format t "~&~S Entering quit-cleanup-block~%" %this-thread-name)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QUIT-CLEANUP BLOCK
						(block QUIT-CLEANUP
						  ,@quit-cleanup-block)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						(dolist (elem %child-threads)   
						  (QUEUE-send-message (THREAD-get-control-queue elem) (MSG-make 'QUIT)))

						(when debug-messages-stream
						  (format debug-messages-stream "~&(LOGFILE-END ~S T-THREAD-MESSAGES ~S))~%" %this-thread-name (get-internal-real-time))
						  (format t "~&~S Closing log file..." %this-thread-name)
						  (finish-output debug-messages-stream)
						  (close debug-messages-stream)
						  (setf debug-messages-stream nil)
						  (format t "OK.~%"))
						
						(return-from MAIN-THREAD-LOOP))

					       (PRINT-ENV1
						(format t "~&~S PRINT-ENV1~%%control-queue: ~S~%%all-queues ~S~%%child-threads: ~S~%%debug-messages-stream: ~S~%" %this-thread-name %control-queue %all-queues %child-threads debug-messages-stream))
					       
					       (GET-THREAD-ENV
						(QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL 'THREAD-ENV %this-thread-name %control-queue %all-queues %child-threads debug-messages-stream)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MESSAGE PROCESSING CASE
					       ,@message-case-block
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					       )
					     ;; else clause of (if (and (MSG-typep %message) (MSG-valid2-p %message)) ...
					     (QUEUE-send-message %control-queue (MSG-make 'MSG-ERROR %message)))
				      ) ; end (loop for i from 0 until (or (>= i *rr-polling-bucket-size*) 
			     ) ; end (loop for queue in (cdr %all-neighbor-queues) by #'cddr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOOP END BLOCK
		       (block LOOP-END-BLOCK
			 ,@loop-end-block)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		       (funcall loop-idle-fn loop-messages-count)
		       (setf loop-messages-count 0))
		       ;;(sleep %loop-idle-delay)) ;; end (loop named MAIN-THREAD-loop 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HANDLER CASE BLOCK
	       ) ; end (let ((%message nil))... 
	   ,@handler-case-block)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	 
	 ))))

#| FAILED attempt at making common thread creation function/macro, currently only router and peer threads do this so as long as both follow this 'template' of Thread-make get control queue, push and three setf's, should be ok. currently router has list of peer threads and peers have two variables for rx and tx threads 
(defmacro T-THREAD-spawn-child (thread-name-symbol thread-loop-fn thread-variable! queue-variable!)
  "Wrapper for THREAD-make that updates %child-threads, %all-queues. Also sets thread-variable! to newly created THREAD object, and sets queue-variable! to the control queue (DUPLEXQ object) of the new thread"
  
  `(let* ((new-threadGENSYM (THREAD-make ,thread-name-symbol %this-thread-name ,thread-loop-fn))
	  (new-duplexqGENSYM (THREAD-get-control-queue new-threadGENSYM)))
    (push new-threadGENSYM %child-threads)
    (setf ,thread-variable! new-threadGENSYM)
    (setf (getf %all-queues (THREAD-get-thread-name-symbol new-threadGENSYM)) new-duplexqGENSYM)
    (setf ,queue-variable! (THREAD-get-thread-name-symbol new-threadGENSYM))))
|#


