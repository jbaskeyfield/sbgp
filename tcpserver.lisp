(in-package :sbgp)
;;; TODO - add and remove sections - need to check that duplicate ip addresses are not being added or same address assigned to different peers
;;; TODO - sanity checking of input from command queue

(defun TCPSERVER-LISTENER-make (address address-vector-u8 tcp-port-number socket)
  (list 'TCPSERVER-LISTENER
	address                   ; address of bound interface. example '(IPV4 0) or '(IPV6 0 0 0 1)
	address-vector-u8         ; interface address in u8 vector format. example #(0 0 0 0) or #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
	tcp-port-number           ; example 179
	socket))                  ; socket returned by (make-instance 'sb-bsd-sockets:inet-socket and bind to above address & port

(defmacro TCPSERVER-LISTENER-get-address (obj)           "-> IPV4 | IPV6"            `(cadr ,obj))
(defmacro TCPSERVER-LISTENER-get-address-vector-u8 (obj) "-> vector u8"              `(caddr ,obj))
(defmacro TCPSERVER-LISTENER-get-tcp-port-number (obj)   "-> integer"                `(cadddr ,obj))
(defmacro TCPSERVER-LISTENER-get-socket (obj)            "-> (sb-bsd-sockets:socket" `(car (cddddr ,obj)))

(defun TCPSERVER-PEER-ADDRESS-make (peer-thread-name peer-address peer-address-vector-u8)
  "TL for keeping track of allowed connection ip addresses and mapping to peer threads"
  (list 'TCPSERVER-PEER-ADDRESS
	peer-thread-name                 ; name of peer/thread-process. example. 'ROUTER1-PEER1
	peer-address                     ; address of remote peer. example '(IPV4 #x0a0a0a01)
	peer-address-vector-u8))          ; address in u8 vector form. example #(10 10 10 1)

(defmacro TCPSERVER-PEER-ADDRESS-get-peer-thread-name (obj)       "-> symbol"       `(cadr ,obj))
(defmacro TCPSERVER-PEER-ADDRESS-get-peer-address (obj)           "-> IPV4 | IPV6"  `(caddr ,obj))
(defmacro TCPSERVER-PEER-ADDRESS-get-peer-address-vector-u8 (obj) "-> vector u8"    `(cadddr ,obj))

(defun TCPSERVER-ACTIVE-STREAM-make (peer-thread-name socket stream)
  "TL for keeping track of active connections/streams and mapping to peer threads"
  (list 'ACTIVE-STREAM
	peer-thread-name                 ; name of peer/thread-process. example. 'ROUTER1-PEER1
	socket                           ; result of socket-accept
	stream))                         ; result of socket-make-stream

(defmacro TCPSERVER-ACTIVE-STREAM-get-peer-thread-name (obj)       `(cadr ,obj))
(defmacro TCPSERVER-ACTIVE-STREAM-get-socket (obj)                 `(caddr ,obj))
(defmacro TCPSERVER-ACTIVE-STREAM-get-stream (obj)                 `(cadddr ,obj))


(define-T-THREAD-LOOP-FUNCTION TCPSERVER
  :let-env-vars
  ((listener-sockets nil)   ; list of TCPSERVER-LISTENER
    (peer-addresses nil)    ; list of TCPSERVER-PEER-ADDRESS
    (active-streams nil)    ; list of TCPSERVER-ACTIVE-STREAM
    (tcp-opt-backlog 5)
    (debug-tcp-events nil))

  :quit-cleanup-block
  ((dolist (elem listener-sockets)
     (sb-bsd-sockets:socket-close (TCPSERVER-LISTENER-get-socket elem))))

  :message-case-block
  ;; (case (MSG-get-command %message)
  ((PRINT-ENV2
    (format t "~&~S PRINT-ENV2~%listener-sockets: ~S~%peer-addresses: ~S~%active-streams: ~S~%tcp-opt-backlog: ~S~%"
	    %this-thread-name listener-sockets peer-addresses active-streams tcp-opt-backlog))
   
   (ADD
    (case (MSG-get-arg1 %message)
      (LISTENER      ;; example: '(ADD LISTENER (IPV4 #x7f000001) 8179)
       (let ((new-listener-address (MSG-get-arg2 %message))
	     (new-listener-port    (MSG-get-arg3 %message)))
	 (case (TL-get-name new-listener-address)
	   (IPV4
	    (let* ((address-vector-u8 (IPV4->vector-u8 new-listener-address))
		   (new-socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))

	      (setf (sb-bsd-sockets:non-blocking-mode new-socket) t
		    (sb-bsd-sockets:sockopt-reuse-address new-socket) t)
	      
	      (sb-bsd-sockets:socket-bind new-socket address-vector-u8 new-listener-port)
	      (sb-bsd-sockets:socket-listen new-socket tcp-opt-backlog)
	      (push (TCPSERVER-LISTENER-make new-listener-address
					     address-vector-u8
					     new-listener-port
					     new-socket)
		    listener-sockets)))
	   
	   (IPV6
	    (let* ((address-vector-u8 (IPV6->vector-u8 new-listener-address))
		   (new-socket (make-instance 'sb-bsd-sockets:inet6-socket :type :stream :protocol :tcp)))

	      (setf (sb-bsd-sockets:non-blocking-mode new-socket) t
		    (sb-bsd-sockets:sockopt-reuse-address new-socket) t)
	      
	      (sb-bsd-sockets:socket-bind new-socket address-vector-u8 new-listener-port)
	      (sb-bsd-sockets:socket-listen new-socket tcp-opt-backlog)
	      (push (TCPSERVER-LISTENER-make new-listener-address
					     address-vector-u8
					     new-listener-port
					     new-socket)
		    listener-sockets))))))
      
      (PEER-ADDRESS    ;; example '(ADD PEER-ADDRESS PEER1 (IPV4 #x0a010101))       
       (let ((peer-thread-name  (MSG-get-arg2 %message))
	     (address           (MSG-get-arg3 %message)))
	 (let ((address-vector-u8 (case (TL-get-name address)                    ; u8 vector (as returned by sb-bsd-sockets:socket-peername)
				    (IPV4 (IPV4->vector-u8 address))
				    (IPV6 (IPV6->vector-u8 address)))))
	   (push (TCPSERVER-PEER-ADDRESS-make peer-thread-name address address-vector-u8)
		 peer-addresses))))))
   
   (REMOVE
    (case (MSG-get-arg1 %message)
      (LISTENER    ;; example: '(REMOVE LISTENER (IPV4 #x7f000001) 8179)
       (let ((listener-address (MSG-get-arg2 %message))
	     (listener-port    (MSG-get-arg3 %message)))
	 (let ((matching-listener (find-if #'(lambda (elem)
					       (and (equalp listener-address (TCPSERVER-LISTENER-get-address elem))
						    (= listener-port (TCPSERVER-LISTENER-get-tcp-port-number elem))))
					   listener-sockets)))
	   (when matching-listener
	     (sb-bsd-sockets:socket-close (TCPSERVER-LISTENER-get-socket matching-listener))
	     (setf listener-sockets (remove matching-listener listener-sockets))))))
   
      
      (PEER-ADDRESS ;; example '(REMOVE PEER-ADDRESS (IPV4 #x0a010101))
       (let ((address (MSG-get-arg2 %message)))
	 (setf peer-addresses
	       (remove-if #'(lambda (elem)
			      (equalp address
				      (TCPSERVER-PEER-ADDRESS-get-peer-address elem)))
			  peer-addresses))))))

   (GET
    (case (MSG-get-arg1 %message)
      (LISTENER-SOCKETS (QUEUE-send-message %control-queue (MSG-make 'TO-CONTROL %this-thread-name 'LISTENER-SOCKETS listener-sockets)))
      (PEER-ADDRESSES   (QUEUE-send-message %control-queue (MSG-make 'TO-CONTROL %this-thread-name 'PEER-ADDRESSES peer-addresses)))
      (ACTIVE-STREAMS   (QUEUE-send-message %control-queue (MSG-make 'TO-CONTROL %this-thread-name 'ACTIVE-STREAMS active-streams)))
      (TCP-OPT-BACKLOG  (QUEUE-send-message %control-queue (MSG-make 'TO-CONTROL %this-thread-name 'TCP-OPT-BACKLOG tcp-opt-backlog)))
      (DEBUG-TCP-EVENTS (QUEUE-send-message %control-queue (MSG-make 'TO-CONTROL %this-thread-name 'DEBUG-TCP-EVENTS debug-tcp-events)))))
   
   (SET
    (case (MSG-get-arg1 %message)
      (LISTENER-SOCKETS (setf listener-sockets (MSG-get-arg2 %message)))
      (PEER-ADDRESSES   (setf peer-addresses   (MSG-get-arg2 %message)))
      (ACTIVE-STREAMS   (setf active-streams   (MSG-get-arg2 %message)))
      (TCP-OPT-BACKLOG  (setf tcp-opt-backlog  (MSG-get-arg2 %message)))
      (DEBUG-TCP-EVENTS (setf debug-tcp-events (MSG-get-arg2 %message))))))
  
  :loop-end-block
  ((dolist (elem listener-sockets)
     (when (not (queue-tx-full-p %control-queue))
       (let ((new-connection (sb-bsd-sockets:socket-accept (TCPSERVER-LISTENER-get-socket elem))))
	 (when new-connection
	   (when debug-tcp-events (format debug-tcp-events "~&~S DEBUG-TCP-EVENTS new-connection: ~S~%" %this-thread-name new-connection))
	   (let* ((peer-address-vector-u8 (sb-bsd-sockets:socket-peername new-connection))
		  (allowed-peer (find peer-address-vector-u8
				      peer-addresses
				      :key #'(lambda (x) (TCPSERVER-PEER-ADDRESS-get-peer-address-vector-u8 x))
				      :test #'equalp)))
	     (when debug-tcp-events (format debug-tcp-events "~&~S DEBUG-TCP-EVENTS allowed-peer: ~S~%" %this-thread-name allowed-peer))
	      (cond (allowed-peer
		     (let ((new-stream (sb-bsd-sockets:socket-make-stream new-connection
									  :input t :output t
									  ;;:timeout 2
									  :element-type 'unsigned-byte)) ;; :element-type 'unsigned-byte
			   (peer-thread-name (TCPSERVER-PEER-ADDRESS-get-peer-thread-name allowed-peer)))

		       (when debug-tcp-events (format debug-tcp-events "~&~S DEBUG-TCP-EVENTS new-stream: ~S~%" %this-thread-name new-stream))
		       ;; TODO-?? pass entire ACTIVE-STREAM list to netio worker threads so they can also look at status of connection object or take what information they want. need to decide who owns this object dont want it in two places at once
		       (push (TCPSERVER-ACTIVE-STREAM-make peer-thread-name new-connection new-stream)
			     active-streams)
		       (QUEUE-send-message %control-queue (MSG-make 'TO peer-thread-name 'TCPSERVER-NEW-STREAM new-stream))))
		    (t
		     (sb-bsd-sockets:socket-close new-connection))))))))))    ; new connection not in allowed list => close connection

