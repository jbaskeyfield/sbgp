(in-package :sbgp)

(define-T-THREAD-LOOP-FUNCTION NETIORX
  :let-env-vars
  ((net-stream-in nil)
   (4-octet-asn-flag t)
   (delay-open-running-flag nil)
   (debug-mrt-messages nil)
   (*nlri-cache* nil)
   (*path-attrib-cache* nil))

  :message-case-block
  ;; (case (MSG-get-command %message)
  ((PRINT-ENV2
    (format t "~&~S PRINT-ENV2~%net-stream-in: ~S~%4-octet-asn-flag: ~S~%delay-open-running-flag: ~S~%debug-mrt-messages: ~S~%"
	    %this-thread-name net-stream-in 4-octet-asn-flag delay-open-running-flag debug-mrt-messages))
   (SET
    (case (MSG-get-arg1 %message)
      (STREAM-IN
       (setf net-stream-in (MSG-get-arg2 %message))
       ;; NOTE: FSM state change currently triggered by Event-17 being sent by NETIOTX thread only, this message should be ignored. being sent for debugging only
       (QUEUE-send-message %control-queue (MSG-make 'Event-17-TcpConnectionConfirmed-NETIORX)))
       
      (4-OCTET-ASN-FLAG
       (setf 4-octet-asn-flag (MSG-get-arg2 %message)))
      (DELAY-OPEN-RUNNING-FLAG
       (setf delay-open-running-flag (MSG-get-arg2 %message)))
      (DEBUG-MRT-MESSAGES
       (setf debug-mrt-messages (MSG-get-arg2 %message)))
      (*NLRI-CACHE*
       (setf *nlri-cache* (MSG-get-arg2 %message)))
      (*PATH-ATTRIB-CACHE*
       (setf *path-attrib-cache* (MSG-get-arg2 %message)))
      ))
    (GET
    (case (cadr %message)
      (NET-STREAM-IN           (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'NET-STREAM-IN net-stream-in)))
      (4-OCTET-ASN-FLAG        (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name '4-OCTET-ASN-FLAG 4-octet-asn-flag)))
      (DELAY-OPEN-RUNNING-FLAG (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'DELAY-OPEN-RUNNING-FLAG delay-open-running-flag)))
      (DEBUG-MRT-MESSAGES      (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'DEBUG-MRT-MESSAGES debug-mrt-messages)))
      (*NLRI-CACHE*            (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name '*NLRI-CACHE* *nlri-cache*)))
      (*PATH-ATTRIB-CACHE*     (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name '*PATH-ATTRIB-CACHE* *path-attrib-cache*)))
      )))
  :loop-end-block
  (;;(format t "~&##NETIORX LOOP-END-BLOCK START~%")
   (when (and net-stream-in
	      (not (queue-tx-full-p %control-queue)))

     ;;(format t "~&##NETIORX LOOP-END-BLOCK READING BGP-MESSAGE...~%")
     (let* ((msg-header (MSG-HEADER-io-read net-stream-in)))                       ; read BGP message header

       (cond ((not (MSG-HEADER-valid2-p msg-header))                              ; validate message header
	      (QUEUE-send-message %control-queue (MSG-make 'Event-21-BGPHeaderErr msg-header))
	      (setf net-stream-in nil))
	     (t
	      (let* ((msg-length (- (MSG-HEADER-get-length msg-header)
				    19))
		     (msg-body (case (MSG-HEADER-get-type msg-header)
				 (1  (BGP-OPEN-io-read msg-length net-stream-in))
				 (2  (BGP-UPDATE-io-read 4-octet-asn-flag msg-length net-stream-in))
				 (3  (BGP-NOTIFICATION-io-read msg-length net-stream-in))
				 (4  (BGP-KEEPALIVE-make))
				 (5  (BGP-ROUTE-REFRESH-io-read msg-length net-stream-in))
				 (t  (QUEUE-send-message %control-queue (MSG-make 'Event-21-BGPHeaderErr msg-header))))))
		
		(when debug-mrt-messages (MRT-MESSAGE-io-write (list 'BGP-MESSAGE msg-header msg-body) debug-mrt-messages))

		(case (TL-get-name msg-body)
		  ;; TODO Event-23-OpenCollisionDump 
		  (BGP-OPEN
		   (cond ((not (BGP-OPEN-valid2-p msg-body))
			  (QUEUE-send-message %control-queue (MSG-make 'Event-22-BGPOpenMsgErr msg-body))
			  (setf net-stream-in nil))
			 (t
			  (if delay-open-running-flag
			      (QUEUE-send-message %control-queue (MSG-make 'Event-20-BGPOpen-with-DelayOpenTimer-running msg-body))
			      (QUEUE-send-message %control-queue (MSG-make 'Event-19-BGPOpen msg-body))))))

		  (BGP-UPDATE
		   (cond ((not (BGP-UPDATE-valid2-p msg-body))
			  (QUEUE-send-message %control-queue (MSG-make 'Event-28-UpdateMsgErr msg-body))
			  (setf net-stream-in nil))
			 (t
			  (let ((sbgp-update (BGP-UPDATE->SBGP-UPDATE msg-body)))
				 (QUEUE-send-message %control-queue (MSG-make 'Event-27-UpdateMsg sbgp-update))))))

		  (BGP-NOTIFICATION  (QUEUE-send-message %control-queue (MSG-make 'Event-25-NotifMsg msg-body)))
		  ;; TODO  Event-24-NotifMsgVerErr 

		  (BGP-KEEPALIVE     (QUEUE-send-message %control-queue (MSG-make 'Event-26-KeepAliveMsg msg-body)))

		  (BGP-ROUTE-REFRESH (QUEUE-send-message %control-queue (MSG-make 'Event-ROUTE-REFRESH msg-body))))))))))

  :handler-case-block
  ((end-of-file ()
		(setf net-stream-in nil)
		(QUEUE-send-message %control-queue (MSG-make 'Event-18-TcpConnectionFails)))))


(define-T-THREAD-LOOP-FUNCTION NETIOTX
  :let-env-vars
  ((net-stream-out nil)
   (4-octet-asn-flag nil)
   (debug-mrt-messages nil))

  :message-case-block
  ;; (case (MSG-get-command %message)
  ((PRINT-ENV2
    (format t "~&~S PRINT-ENV2~%net-stream-out: ~S~%4-octet-asn-flag: ~S~%"
	    %this-thread-name net-stream-out 4-octet-asn-flag))

   (SET
    (case (MSG-get-arg1 %message)
      (STREAM-OUT
       (setf net-stream-out (MSG-get-arg2 %message))
       (QUEUE-send-message %control-queue (MSG-make 'Event-17-TcpConnectionConfirmed)))
      (4-OCTET-ASN-FLAG
       (setf 4-octet-asn-flag (MSG-get-arg2 %message)))
      (DEBUG-MRT-MESSAGES
       (setf debug-mrt-messages (MSG-get-arg2 %message)))))

   (GET
    (case (MSG-get-arg1 %message)
      (NET-STREAM-OUT          (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'NET-STREAM-OUT net-stream-out)))
      (4-OCTET-ASN-FLAG        (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name '4-OCTET-ASN-FLAG 4-octet-asn-flag)))
      (DEBUG-MRT-MESSAGES      (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'DEBUG-MRT-MESSAGES debug-mrt-messages)))))
   
   (SEND
    (when net-stream-out
      (let ((bgp-message-out (MSG-get-arg1 %message)))
	(when debug-mrt-messages (MRT-MESSAGE-io-write bgp-message-out debug-mrt-messages))
	(BGP-MESSAGE-io-write 4-octet-asn-flag bgp-message-out net-stream-out))))
   
   (SEND-IMMEDIATE
    (when net-stream-out
      (let ((bgp-message-out (MSG-get-arg1 %message)))
	(BGP-MESSAGE-io-write 4-octet-asn-flag bgp-message-out net-stream-out)
	(force-output net-stream-out)))))
  )
