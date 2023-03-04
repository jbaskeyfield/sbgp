(in-package :sbgp)
;;; PEER thread
;;; contains BGP FSM logic, RIB-Adj-In, RIB-Adj-Out
;;; control connection back to ROUTER
;;; child connection to NET-IO thread

(defparameter +FSM-Events+
  `(
    ;; Events-26 and 27 moved to head of list to speed up member test on entry to FSM case block
    Event-27-UpdateMsg                                                            ; Mandatory   (NETIORX)
    Event-26-KeepAliveMsg                                                         ; Mandatory   (NETIORX)
    
    ;; ADMINISTRATIVE EVENTS
    Event-1-ManualStart                                                           ; Mandatory
    Event-2-ManualStop                                                            ; Mandatory
    Event-3-AutomaticStart                                                        ; Optional 
    Event-4-ManualStart-with-PassiveTcpEstablishment                              ; Optional
    Event-5-AutomaticStart-with-PassiveTcpEstablishment                           ; Optional
    Event-6-AutomaticStart-with-DampPeerOscillations                              ; Optional
    Event-7-AutomaticStart-with-DampPeerOscillations-and-PassiveTcpEstablishment  ; Optional
    Event-8-AutomaticStop                                                         ; Optional

    ;; TIMER-EVENTS
    Event-9-ConnectRetryTimer-Expires                                             ; Mandatory
    Event-10-HoldTimer-Expires                                                    ; Mandatory
    Event-11-KeepaliveTimer-Expires                                               ; Mandatory
    Event-12-DelayOpenTimer-Expires                                               ; Optional
    Event-13-IdleHoldTimer-Expires                                                ; Optional

    ;; TCP CONNECTION-BASED EVENTS
    Event-14-TcpConnection-Valid                                                  ; Optional
    Event-15-Tcp-CR-Invalid                                                       ; Optional
    Event-16-Tcp-CR-Acked                                                         ; Optional
    Event-17-TcpConnectionConfirmed                                               ; Mandatory   (NETIORX & NETIOTX) 
    Event-18-TcpConnectionFails                                                   ; Mandatory
    Event-19-BGPOpen                                                              ; Mandatory   (NETIORX)
    Event-20-BGPOpen-with-DelayOpenTimer-running                                  ; Optional
    Event-21-BGPHeaderErr                                                         ; Mandatory
    Event-22-BGPOpenMsgErr                                                        ; Mandatory
    Event-23-OpenCollisionDump                                                    ; Optional
    Event-24-NotifMsgVerErr                                                       ; Mandatory
    Event-25-NotifMsg                                                             ; Mandatory   (NETIORX)
    Event-28-UpdateMsgErr))                                                       ; Mandatory   TODO (NETIORX)

(define-T-THREAD-LOOP-FUNCTION PEER
  :let-env-vars
  ((netiorx-thread nil)
   (netiorx-queue nil)
   (netiotx-thread nil)
   (netiotx-queue nil)
   
   (FSM-state 'IDLE)                                  ; STATE   symbol = [ IDLE | CONNECT | ACTIVE | OPENSENT | OPENCONFIRM | ESTABLISHED ]
   (FSM-timers (FSM-TIMERS-make-default))             ; Timers
   (FSM-attrib (FSM-ATTRIB-make-default))             ; Configuration/Attributes
   (RIB-Adj-in nil)
   (RIB-Adj-out nil)

   (*nlri-cache* nil)                                 ; used by RIB-ADJ-ENTRY-make on receipt of Updates from netiorx
   ;;(path-attrib-cache nil)                            ; used by NETIO-RX thread but saved here incase we need it??
   (*path-attrib-list-cache* nil)                     ; used by RIB-ADJ-ENTRY-make on receipt of Updates from netiorx

   (router-config nil)                                ; ROUTER-CONFIG
   (peer-config nil)                                  ; PEER-CONFIG
   (peer-session-state (PEER-SESSION-STATE-make))     ; PEER-SESSION-STATE
   
   (debug-fsm-timers nil)
   (debug-fsm-state nil)
   (debug-fsm-events nil)
   )
  ;;; create NETIO RX & TX threads
  :thread-entry-block
  ((setf netiorx-thread (THREAD-make 'NETIORX %this-thread-name #'NETIORX-thread-loop))
   (push netiorx-thread %child-threads)
   (setf netiorx-queue (THREAD-get-control-queue netiorx-thread))
   (setf (getf %all-queues (THREAD-get-thread-name-symbol netiorx-thread)) netiorx-queue)
   
   (setf netiotx-thread (THREAD-make 'NETIOTX %this-thread-name #'NETIOTX-thread-loop))
   (push netiotx-thread %child-threads)
   (setf netiotx-queue (THREAD-get-control-queue netiotx-thread))
   (setf (getf %all-queues (THREAD-get-thread-name-symbol netiotx-thread)) netiotx-queue))
  
  :quit-cleanup-block
  (;; quit messages is sent to child threads by t-threads.lisp tempate after this block
   ;; TODO: need to add bgp-notification to send to netiotx
   )

  :loop-entry-block
  (;; check to see if any FSM timers have been triggered. FSM-TIMERS-poll will enqueue events on %timers-queue
   (FSM-TIMERS-poll FSM-timers %timers-queue %this-thread-name debug-fsm-timers)
   )
  
  :message-case-block
  ;; (case (MSG-get-command %message)
  ((PRINT-ENV2
    (format t "~&~S PRINT-ENV2~%netiorx-thread: ~S~%netiorx-queue: ~S~%netiotx-thread: ~S~%netiotx-queue: ~S~%FSM-state: ~S~%FSM-timers: ~S~%FSM-attrib: ~S~%RIB-Adj-in: ~S~%RIB-Adj-out: ~S~%router-config: ~S~%peer-config: ~S~%peer-session-state: ~S~%debug-fsm-timers: ~S~%debug-fsm-state: ~S~%debug-fsm-events: ~S~%"
	    %this-thread-name
	    netiorx-thread
	    netiorx-queue
	    netiotx-thread
	    netiotx-queue 
	    FSM-state
	    FSM-timers
	    FSM-attrib
	    RIB-Adj-in
	    RIB-Adj-out
	    router-config
	    peer-config
	    peer-session-state
	    debug-fsm-timers
	    debug-fsm-state
	    debug-fsm-events
	    ))
   
   (TCPSERVER-NEW-STREAM
    (let ((new-stream (MSG-get-arg1 %message)))
      (QUEUE-send-message netiorx-queue (MSG-make 'SET 'STREAM-IN new-stream))
      (QUEUE-send-message netiotx-queue (MSG-make 'SET 'STREAM-OUT new-stream))))

   (GET
    (case (MSG-get-arg1 %message)
      ;; object cache tables
      (*NLRI-CACHE*             (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name '*NLRI-CACHE* *nlri-cache*)))
      ;;(*PATH-ATTRIB-CACHE*      (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name '*PATH-ATTRIB-CACHE* path-attrib-cache)))
      (*PATH-ATTRIB-LIST-CACHE* (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name '*PATH-ATTRIB-LIST-CACHE* *path-attrib-list-cache*)))
      
      ;; config and state objects
      (ROUTER-CONFIG       (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'ROUTER-CONFIG router-config)))
      (PEER-CONFIG         (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'PEER-CONFIG peer-config)))
      (FSM-STATE           (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'FSM-STATE FSM-state)))
      (FSM-TIMERS          (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'FSM-TIMERS FSM-timers)))
      (FSM-ATTRIB          (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'FSM-ATTRIB FSM-attrib)))
      (RIB-ADJ-IN          (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'RIB-ADJ-IN RIB-Adj-in)))
      (RIB-ADJ-OUT         (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'RIB-ADJ-OUT RIB-Adj-out)))

      ;; debug streams
      (DEBUG-FSM-TIMERS    (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'DEBUG-FSM-TIMERS debug-fsm-timers)))   
      (DEBUG-FSM-STATE     (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'DEBUG-FSM-STATE debug-fsm-state)))
      (DEBUG-FSM-EVENTS    (QUEUE-send-message %control-queue (MSG-make 'TO 'CONTROL %this-thread-name 'DEBUG-FSM-EVENTS debug-fsm-events)))))
   
   (SET
    (case (MSG-get-arg1 %message)
      ;; object cache tables
      (*NLRI-CACHE*                                      ; *NLRI-CACHE*
       (setf *nlri-cache* (MSG-get-arg2 %message))       ; shared by all child threads of router. shadow variable locally,
       (QUEUE-send-message netiorx-queue %message))      ; and forward on to RX thread.
      ;;(*PATH-ATTRIB-CACHE*                               ; *PATH-ATTRIB-CACHE*
      ;; (setf path-attrib-cache (MSG-get-arg2 %message))  ; shadowed by RX thread only (but save copy incase we need to resend / restart RX thread) 
      ;; (QUEUE-send-message netiorx-queue %message))      ; forward on to RX thread.
      (*PATH-ATTRIB-LIST-CACHE*                          ; *PATH-ATTRIB-LIST-CACHE*
       (setf *path-attrib-list-cache* (MSG-get-arg2 %message))) ; shadowed locally (used by RIB-ADJ-ENTRY-make)
      
      ;; config and state objects
      (ROUTER-CONFIG            (setf router-config (MSG-get-arg2 %message)))
      (PEER-CONFIG              (setf peer-config (MSG-get-arg2 %message)))
      (FSM-STATE                (setf FSM-state (MSG-get-arg2 %message)))
      (FSM-TIMERS               (setf FSM-timers (MSG-get-arg2 %message)))
      (FSM-ATTRIB               (setf FSM-attrib (MSG-get-arg2 %message)))
      (RIB-ADJ-IN               (setf RIB-Adj-in (MSG-get-arg2 %message)))
      (RIB-ADJ-OUT              (setf RIB-Adj-out (MSG-get-arg2 %message)))

      ;; debug streams
      (DEBUG-FSM-TIMERS         (setf debug-fsm-timers (MSG-get-arg2 %message)))         
      (DEBUG-FSM-STATE          (setf debug-fsm-state (MSG-get-arg2 %message)))
      (DEBUG-FSM-EVENTS         (setf debug-fsm-events (MSG-get-arg2 %message)))
      ))

   ;; sent from router.lisp: (list 'ANNOUNCE-RIB-LOC->RIB-ADJ rib-adj-entries)
   (ANNOUNCE-RIB-LOC->RIB-ADJ
    (let ((rib-adj-entries (MSG-get-arg1 %message)))
      (dolist (rib-adj-entry rib-adj-entries)
	(RIB-ADJ-add-entry RIB-Adj-out rib-adj-entry)
	(QUEUE-send-message netiotx-queue (MSG-make 'SEND
						    (RIB-ADJ-ENTRY->BGP-UPDATE-MESSAGE (PEER-SESSION-STATE-get-4-octet-asn-flag peer-session-state)
										       t
										       rib-adj-entry))))))
   (WITHDRWAL-RIB-LOC->RIB-ADJ
    (let ((rib-adj-entries (MSG-get-arg1 %message)))
      (dolist (rib-adj-entry rib-adj-entries)
	(let ((removed-entry (RIB-ADJ-remove-entry RIB-Adj-out rib-adj-entry)))
	  (if removed-entry
	      (QUEUE-send-message netiotx-queue (MSG-make 'SEND
							  (RIB-ADJ-ENTRY->BGP-UPDATE-MESSAGE (PEER-SESSION-STATE-get-4-octet-asn-flag peer-session-state)
										       nil
										       rib-adj-entry))))))))
							  
	
   
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BGP FSM START
   (t
    (let ((event (MSG-get-command %message)))
      (cond ((member event +FSM-Events+)
	     (when debug-fsm-events (format debug-fsm-events "~&~S FSM-EVENTS State: ~S Event: ~S~%" %this-thread-name fsm-state event))
	     (case FSM-state
;;; IDLE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	       (IDLE
		;;  p52 In this state, BGP FSM refuses all incoming BGP connections for
		;;  this peer.  No resources are allocated to the peer.
		(cond ((and RIB-Adj-in RIB-Adj-out router-config peer-config
			    (ROUTER-CONFIG-get-router-id router-config)         
			    (ROUTER-CONFIG-get-local-asn router-config)         
			    (PEER-CONFIG-get-peer-asn peer-config)           
			    (PEER-CONFIG-get-peer-ip-address peer-config))
		       (case event

;;; IDLE  Event-1-ManualStart  Event-3-AutomaticStart
			 
			 ((Event-1-ManualStart Event-3-AutomaticStart)
			  
			  ;; initializes all BGP resources for the peer connection,
			  
			  ;; sets ConnectRetryCounter to zero,
			  (setf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib) 0)
			  ;; starts the ConnectRetryTimer with the initial value,
			  (FSM-TIMERS-start-ConnectRetryTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started ConnectRetryTimer~%" %this-thread-name))
			  ;; initiates a TCP connection to the other BGP peer,
			  
			  ;; listens for a connection that may be initiated by the remote BGP peer, and
			  (QUEUE-send-message %control-queue
					      (MSG-make 'TO 'TCPSERVER 'ADD 'PEER-ADDRESS %this-thread-name (PEER-CONFIG-get-peer-ip-address peer-config)))
			  ;; changes its state to CONNECT
			  (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> CONNECT~%" %this-thread-name FSM-state))
			  (setf FSM-state 'CONNECT))

;;; IDLE  Event-2-ManualStop  Event-8-AutomaticStop

			 ;; p52 The ManualStop event (Event 2) and AutomaticStop (Event 8) event are ignored in the Idle state.
			 ((Event-2-ManualStop  Event-8-AutomaticStop) nil)

;;; IDLE Event-4-ManualStart-with-PassiveTcpEstablishment  Event-5-AutomaticStart-with-PassiveTcpEstablishment

			 ;; p53 In response to a ManualStart-with-PassiveTcpEstablishment event (Event 4) or AutomaticStart-with-PassiveTcpEstablishment event (Event 5), the local system:
			 ((Event-4-ManualStart-with-PassiveTcpEstablishment Event-5-AutomaticStart-with-PassiveTcpEstablishment)

			  ;; initializes all BGP resources,

			  ;; sets the ConnectRetryCounter to zero,
			  (setf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib) 0)
			  ;; starts the ConnectRetryTimer with the initial value,
			  (FSM-TIMERS-start-ConnectRetryTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started ConnectRetryTimer~%" %this-thread-name))
			  ;; listens for a connection that may be initiated by the remote peer, and
			  (QUEUE-send-message %control-queue
					      (MSG-make 'TO 'TCPSERVER 'ADD 'PEER-ADDRESS %this-thread-name (PEER-CONFIG-get-peer-ip-address peer-config)))
			  ;; changes its state to ACTIVE
			  (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> ACTIVE~%" %this-thread-name FSM-state))
			  (setf FSM-state 'ACTIVE))
			 
			 (t
			  ;; p53 If the DampPeerOscillations attribute is set to TRUE, the
			  ;; following three additional events may occur within the Idle state:
			  (cond ((eql (FSM-ATTRIB-get-DampPeerOscillations FSM-attrib) t)
				 (case event
				   (Event-6-AutomaticStart-with-DampPeerOscillations
				    )
				   (Event-7-AutomaticStart-with-DampPeerOscillations-and-PassiveTcpEstablishment
				    )
				   (Event-13-IdleHoldTimer-Expires
				    )))
				(t
				 ;; Any other event (Events 9-12, 15-28) received in the Idle state does not cause change in the state of the local system.
				 nil)))))
		      (t
		       (format *standard-output* "~&~S NOTIFICATION Event ~S ignored. Peer configuration or RIB-ADJ not set~%" %this-thread-name event))) 
		) ;; END (CASE IDLE 

;;; CONNECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	       
	       (CONNECT
		;; p53 In this state, BGP FSM is waiting for the TCP connection to be completed.
		(case event
		  
;;; CONNECT  Event-1-ManualStart  Event-3-AutomaticStart  Event-4-ManualStart-with-PassiveTcpEstablishment  Event-5-AutomaticStart-with-PassiveTcpEstablishment  Event-6-AutomaticStart-with-DampPeerOscillations  Event-7-AutomaticStart-with-DampPeerOscillations-and-PassiveTcpEstablishment    

		  ;; The start events (Events 1, 3-7) are ignored in the Connect state
		  ((Event-1-ManualStart Event-3-AutomaticStart	Event-4-ManualStart-with-PassiveTcpEstablishment Event-5-AutomaticStart-with-PassiveTcpEstablishment Event-6-AutomaticStart-with-DampPeerOscillations Event-7-AutomaticStart-with-DampPeerOscillations-and-PassiveTcpEstablishment)
		   nil)

;;; CONNECT  Event-2-ManualStop	   

		  ;; p53 In response to a ManualStop event (Event 2), the local system:
		  (Event-2-ManualStop
		   ;; drops the TCP connection,

		   ;; releases all BGP resources,

		   ;; sets ConnectRetryCounter to zero,
		   (setf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib) 0)
		   ;; stops the ConnectRetryTimer and sets ConnectRetryTimer to zero, and
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

;;; CONNECT  Event-9-ConnectRetryTimer-Expires
		  
		  ;; p54 In response to the ConnectRetryTimer-Expires event (Event 9), the local system:
		  (Event-9-ConnectRetryTimer-Expires
		   ;; drops the TCP connection,

		   ;; restarts the ConnectRetryTimer,
		   (FSM-TIMERS-start-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started ConnectRetryTimer~%" %this-thread-name))
		   ;; stops the DelayOpenTimer and resets the timer to zero,
		   (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
		   (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))
		   ;; initiates a TCP connection to the other BGP peer,

		   ;; continues to listen for a connection that may be initiated by the remote BGP peer, and
		   ;; stays in the CONNECT state.
		   )

;;; CONNECT  Event-12-DelayOpenTimer-Expires
		  
		  ;; p54 If the DelayOpenTimer-Expires event (Event 12) occurs in the Connect state, the local system:	   
		  (Event-12-DelayOpenTimer-Expires

		   (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))

		   ;; sends an OPEN message to its peer,
		   (QUEUE-send-message netiotx-queue
				       (MSG-make 'SEND
					     (BGP-MESSAGE-make (BGP-OPEN-make (ROUTER-CONFIG-get-local-asn router-config)
									      (PEER-CONFIG-get-hold-time peer-config)
									      (ROUTER-CONFIG-get-router-id router-config)
									      (PEER-CONFIG-get-capability-set peer-config)))))
		   ;; sets the HoldTimer to a large value, and
		   (FSM-TIMERS-start-HoldTimer-LargeValue FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer-LargeValue" %this-thread-name))
		   ;; changes its state to OPENSENT
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> OPENSENT~%" %this-thread-name FSM-state))
		   (setf FSM-state 'OPENSENT))

;;; CONNECT  Event-14-TcpConnection-Valid
		  
		  ;; p54 If the BGP FSM receives a TcpConnection-Valid event (Event 14), the TCP connection is processed, and the connection remains in the Connect state.
		  (Event-14-TcpConnection-Valid nil)

;;; CONNECT  Event-15-Tcp-CR-Invalid
		  
		  ;; p54 If the BGP FSM receives a Tcp-CR-Invalid event (Event 15), the local system rejects the TCP connection, and the connection remains in the Connect state.
		  (Event-15-Tcp-CR-Invalid
		   ;; TODO ???
		   )

;;; CONNECT  Event-16-Tcp-CR-Acked  Event-17-TcpConnectionConfirmed
		  
		  ;; p54 If the TCP connection succeeds (Event 16 or Event 17), the local system checks the DelayOpen attribute prior to processing. 
		  ((Event-16-Tcp-CR-Acked Event-17-TcpConnectionConfirmed)
		   (cond ((FSM-ATTRIB-get-DelayOpen FSM-attrib)
			  ;; If the DelayOpen attribute is set to TRUE, the local system:
			  ;; stops the ConnectRetryTimer (if running) and sets the ConnectRetryTimer to zero,
			  (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started ConnectRetryTimer~%" %this-thread-name))
			  ;; sets the DelayOpenTimer to the initial value, and
			  (FSM-TIMERS-start-DelayOpenTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started DelayOpenTimer~%" %this-thread-name))
			  (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG t))
			  ;; stays in the CONNECT state.
			  )
			 (t
			  ;; If the DelayOpen attribute is set to FALSE, the local system:
			  ;; stops the ConnectRetryTimer (if running) and sets the ConnectRetryTimer to zero,
			  (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
			  ;; completes BGP initialization

			  ;; sends an OPEN message to its peer,
			  (QUEUE-send-message netiotx-queue
					      (MSG-make 'SEND
						        (BGP-MESSAGE-make (BGP-OPEN-make (ROUTER-CONFIG-get-local-asn router-config)
											 (PEER-CONFIG-get-hold-time peer-config)
											 (ROUTER-CONFIG-get-router-id router-config)
											 (PEER-CONFIG-get-capability-set peer-config)))))
			  
			  ;; sets the HoldTimer to a large value (a HoldTimer value of 4 minutes is suggested), and
			  (FSM-TIMERS-start-HoldTimer-LargeValue FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer-LargeValue" %this-thread-name))
			  ;; changes its state to OPENSENT
			  (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> OPENSENT~%" %this-thread-name FSM-state))
			  (setf FSM-state 'OPENSENT))))

;;; CONNECT  Event-18-TcpConnectionFails
		  
		  ;; p55 If the TCP connection fails (Event 18), the local system checks the DelayOpenTimer. 
		  (Event-18-TcpConnectionFails
		   (cond ((FSM-TIMERS-get-DelayOpenTimer FSM-timers)
			  ;; If the DelayOpenTimer is running, the local system:
			  ;; restarts the ConnectRetryTimer with the initial value,
			  (FSM-TIMERS-start-ConnectRetryTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started ConnectRetryTimer~%" %this-thread-name))
			  ;; stops the DelayOpenTimer and resets its value to zero,
			  (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
			  (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))
			  ;; continues to listen for a connection that may be initiated by the remote BGP peer, and

			  ;; changes its state to ACTIVE
			  (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> ACTIVE~%" %this-thread-name FSM-state))
			  (setf FSM-state 'ACTIVE))
			 (t
			  ;; If the DelayOpenTimer is not running, the local system:
			  ;; stops the ConnectRetryTimer to zero,
			  (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

			  ;; drops the TCP connection,

			  ;; releases all BGP resources, and

			  ;; changes its state to IDLE
			  (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
			  (setf FSM-state 'IDLE))))
		  
;;; CONNECT  Event-20-BGPOpen-with-DelayOpenTimer-running  
		  
		  ;; p55 If an OPEN message is received while the DelayOpenTimer is running (Event 20), the local system:
		  (Event-20-BGPOpen-with-DelayOpenTimer-running
		   ;;; BGPOpen message processing START ;;;
		   ;; Set session state and timers
		   ;; peer-session state: peer-advertised-hold-time, peer-router-id, peer-asn, peer-advertised-caps, capability-set, 4-octet-asn-flag
		   ;; FSM-timers: HoldTime, KeepAliveTime
		   (let ((bgp-open-msg (MSG-get-arg1 %message)))
		     
		     (setf (PEER-SESSION-STATE-get-peer-advertised-hold-time peer-session-state)
			   (BGP-OPEN-get-hold-time bgp-open-msg))

		     (setf (PEER-SESSION-STATE-get-peer-router-id peer-session-state) 
			   (BGP-OPEN-get-bgp-id bgp-open-msg))
		     
		     (setf (PEER-SESSION-STATE-get-peer-asn peer-session-state)
			   (BGP-OPEN-get-my-as bgp-open-msg))

		     (setf (PEER-SESSION-STATE-get-peer-advertised-caps peer-session-state)
			   (tl-tree-filter-collect1 +BGP-CAP-all-types+ bgp-open-msg))

		     (setf (PEER-SESSION-STATE-get-common-capability-set peer-session-state)
			   (intersection (PEER-SESSION-STATE-get-peer-advertised-caps peer-session-state)
					 (PEER-CONFIG-get-capability-set peer-config)
					 :test #'equal))
		     
		     (cond ((tl-find-in-tree 'BGP-CAP-4-OCTET-ASN (PEER-SESSION-STATE-get-common-capability-set peer-session-state))
			    (setf (PEER-SESSION-STATE-get-4-octet-asn-flag peer-session-state) t)
			    (QUEUE-send-message netiorx-queue (MSG-make 'SET '4-OCTET-ASN-FLAG t))
			    (QUEUE-send-message netiotx-queue (MSG-make 'SET '4-OCTET-ASN-FLAG t)))
			   (t
			    (setf (PEER-SESSION-STATE-get-4-octet-asn-flag peer-session-state) nil)
			    (QUEUE-send-message netiorx-queue (MSG-make 'SET '4-OCTET-ASN-FLAG nil))
			    (QUEUE-send-message netiotx-queue (MSG-make 'SET '4-OCTET-ASN-FLAG nil))))
		     
		     ;; sets a KeepaliveTimer (via the text below)
		     ;; sets the HoldTimer according to the negotiated value (see Section 4.2),
		     (setf (FSM-TIMERS-get-HoldTime FSM-timers) (* (min (PEER-CONFIG-get-hold-time peer-config)
									(PEER-SESSION-STATE-get-peer-advertised-hold-time peer-session-state))
								   internal-time-units-per-second))
		     (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Set negotiated HoldTime: ~D seconds~%" %this-thread-name (/ (FSM-TIMERS-get-HoldTime FSM-timers) internal-time-units-per-second)))
		     
		     (setf (FSM-TIMERS-get-KeepAliveTime FSM-timers) (floor (/ (FSM-TIMERS-get-HoldTime FSM-timers)
									       3)))
		     (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Set negotiated KeepaliveTime: ~D seconds~%" %this-thread-name (/ (FSM-TIMERS-get-KeepaliveTime FSM-timers) internal-time-units-per-second))))
		   ;;; BGPOpen message processing END ;;;

		   ;; stops the ConnectRetryTimer (if running) and sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   
		   ;; completes the BGP initialization,

		   ;; stops and clears the DelayOpenTimer (sets the value to zero),
		   (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
		   (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))

		   ;; sends an OPEN message,

		   ;; sends a KEEPALIVE message,
		   (QUEUE-send-message netiotx-queue (MSG-make 'SEND-IMMEDIATE (BGP-MESSAGE-make (BGP-KEEPALIVE-make))))
		   (FSM-TIMERS-start-KeepaliveTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started KeepaliveTimer~%" %this-thread-name))

		   ;; if the HoldTimer initial value is non-zero,
		   (cond ((not (= (FSM-TIMERS-get-HoldTimer FSM-timers) 0))
			  ;; starts the KeepaliveTimer with the initial value and
			  (FSM-TIMERS-start-KeepaliveTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started KeepaliveTimer~%" %this-thread-name))

			  ;; resets the HoldTimer to the negotiated value,
			  (FSM-TIMERS-start-HoldTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer~%" %this-thread-name)))
			 
			 ;; else, if the HoldTimer initial value is zero,
			 (t
			  ;; resets the KeepaliveTimer and
			  (FSM-TIMERS-start-KeepaliveTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started KeepaliveTimer~%" %this-thread-name))
			  
			  ;; resets the HoldTimer value to zero,
			  (FSM-TIMERS-stop-HoldTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped HoldTimer~%" %this-thread-name))
			  
			  ;; and changes its state to OPENCONFIRM
			  (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> OPENCONFIRM~%" %this-thread-name FSM-state))
			  (setf FSM-state 'OPENCONFIRM))
			 
			 ;; (If the value of the autonomous system field is the same as the
			 ;; local Autonomous System number, set the connection status to an
			 ;; internal connection; otherwise it will be "external".)

			 ))
		  
;;; CONNECT  Event-21-BGPHeaderErr  Event-22-BGPOpenMsgErr
		  
		  ;; p56 If BGP message header checking (Event 21) or OPEN message checking  detects an error (Event 22) (see Section 6.2), the local system:
		  ((Event-21-BGPHeaderErr
		    Event-22-BGPOpenMsgErr)
		   ;; (optionally) If the SendNOTIFICATIONwithoutOPEN attribute is
		   ;; set to TRUE, then the local system first sends a NOTIFICATION
		   ;; message with the appropriate error code, and then

		   ;; stops the ConnectRetryTimer (if running) and sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

;;; CONNECT  Event-24-NotifMsgVerErr
		  
		  ;; p56 If a NOTIFICATION message is received with a version error (Event 24), the local system checks the DelayOpenTimer.
		  (Event-24-NotifMsgVerErr
		   ;; If the DelayOpenTimer is running, the local system:
		   (cond ((FSM-TIMERS-DelayOpenTimer-is-running-p FSM-timers)
			  
			  ;; stops the ConnectRetryTimer (if running) and sets the ConnectRetryTimer to zero,
			  (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
			  
			  ;; stops and resets the DelayOpenTimer (sets to zero),
			  (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS StoppedDelayOpenTimer~%" %this-thread-name))
			  (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))
			  
			  ;; releases all BGP resources,

			  ;; drops the TCP connection, and

			  ;; changes its state to IDLE
			  (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
			  (setf FSM-state 'IDLE))
			 
			 (t 
			  ;; If the DelayOpenTimer is not running, the local system:
			  (when (not (FSM-TIMERS-DelayOpenTimer-is-running-p FSM-timers))

			    ;; stops the ConnectRetryTimer and sets the ConnectRetryTimer to zero,
			    (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
			    (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

			    ;; releases all BGP resources,

			    ;; drops the TCP connection,

			    ;; increments the ConnectRetryCounter by 1,
			    (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

			    ;; performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

			    ;; changes its state to IDLE
			    (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
			    (setf FSM-state 'IDLE)))))

;;; CONNECT  Any other event
		  
		  ;; p57 In response to any other events (Events 8, 10-11, 13, 19, 23, 25-28), the local system:
		  (t
		   ;; if the ConnectRetryTimer is running, stops and resets the ConnectRetryTimer (sets to zero),
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),
		   (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
		   (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))

		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))))
	       
;;; ACTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	       
	       (ACTIVE
		;; p58 In this state, BGP FSM is trying to acquire a peer by listening
		;; for, and accepting, a TCP connection.
		(case event

;;; ACTIVE  Events 1, 3-7		  

		  ;; p58 The start events (Events 1, 3-7) are ignored in the Active state.
		  ((Event-1-ManualStart
		    Event-3-AutomaticStart
		    Event-4-ManualStart-with-PassiveTcpEstablishment
		    Event-5-AutomaticStart-with-PassiveTcpEstablishment
		    Event-6-AutomaticStart-with-DampPeerOscillations
		    Event-7-AutomaticStart-with-DampPeerOscillations-and-PassiveTcpEstablishment)
		   nil)

;;; ACTIVE  Event-2-ManualStop
		  
		  ;; p58 In response to a ManualStop event (Event 2), the local system:
		  (Event-2-ManualStop
		   ;; If the DelayOpenTimer is running and the SendNOTIFICATIONwithoutOPEN session attribute is set, the local system sends a NOTIFICATION with a Cease,
		   ;; releases all BGP resources including stopping the DelayOpenTimer
		   (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
		   
		   (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))

		   ;; drops the TCP connection,

		   ;; sets ConnectRetryCounter to zero,
		   (setf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib) 0)

		   ;; stops the ConnectRetryTimer and sets the ConnectRetryTimer to zero, and
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
		   
		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))
		  
;;; ACTIVE  Event-9-ConnectRetryTimer-Expires
		  
		  ;; p58 In response to a ConnectRetryTimer-Expires event (Event 9), the local system:
		  (Event-9-ConnectRetryTimer-Expires
		   ;; restarts the ConnectRetryTimer (with initial value),
		   (FSM-TIMERS-start-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started ConnectRetryTimer~%" %this-thread-name))

		   ;; initiates a TCP connection to the other BGP peer,

		   ;; continues to listen for a TCP connection that may be initiated by a remote BGP peer, and

		   ;; changes its state to CONNECT
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> CONNECT~%" %this-thread-name FSM-state))
		   (setf FSM-state 'CONNECT))

		  ;; p58 If the local system receives a DelayOpenTimer-Expires event (Event 12), the local system:
		  (Event-12-DelayOpenTimer-Expires
		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; stops and clears the DelayOpenTimer (set to zero),
		   (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
		   (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))
		   
		   ;; completes the BGP initialization,

		   ;; sends the OPEN message to its remote peer,

		   ;; sets its hold timer to a large value (a HoldTimer value of 4 minutes is suggested for this state transition), and
		   (FSM-TIMERS-start-HoldTimer-LargeValue FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer-LargeValue" %this-thread-name))

		   ;; changes its state to OPENSENT
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> OPENSENT~%" %this-thread-name FSM-state))
		   (setf FSM-state 'OPENSENT))

;;; ACTIVE  Event-14-TcpConnection-Valid
		  
		  ;; p59 If the local system receives a TcpConnection-Valid event (Event 14), the local system processes the TCP connection flags and stays in the Active state.
		  (Event-14-TcpConnection-Valid
		   )
		  
;;; ACTIVE  Event-15-Tcp-CR-Invalid
		  
		  ;; p59 If the local system receives a Tcp-CR-Invalid event (Event 15), the local system rejects the TCP connection and stays in the Active State.
		  (Event-15-Tcp-CR-Invalid
		   )

;;; ACTIVE  Event-16-Tcp-CR-Acked  Event-17-TcpConnectionConfirmed
		  
		  ;; p59 In response to the success of a TCP connection (Event 16 or Event 17), the local system checks the DelayOpen optional attribute prior to processing.
		  ((Event-16-Tcp-CR-Acked Event-17-TcpConnectionConfirmed)
		   ;; If the DelayOpen attribute is set to TRUE, the local system:
		   (cond ((FSM-ATTRIB-get-DelayOpen FSM-attrib)
			  ;; stops the ConnectRetryTimer and sets the ConnectRetryTimer to zero,
			  (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
			  
			  ;; sets the DelayOpenTimer to the initial value (DelayOpenTime), and
			  (FSM-TIMERS-start-DelayOpenTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started DelayOpenTimer~%" %this-thread-name))
			  (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG t))

			  ;; stays in the ACTIVE state.
			  )
			 (t  ;; If the DelayOpen attribute is set to FALSE, the local system:
			  ;; sets the ConnectRetryTimer to zero,
			  (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
			  
			  ;; completes the BGP initialization,

			  ;; sends the OPEN message to its peer,

			  ;; sets its HoldTimer to a large value (a HoldTimer value of 4 minutes is suggested for this state transition), and
			  (FSM-TIMERS-start-HoldTimer-LargeValue FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer-LargeValue" %this-thread-name))

			  ;; changes its state to OPENSENT
			  (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> OPENSENT~%" %this-thread-name FSM-state))
			  (setf FSM-state 'OPENSENT))))

;;; ACTIVE  Event-18-TcpConnectionFails
		  
		  ;; p59 If the local system receives a TcpConnectionFails event (Event 18), the local system:
		  (Event-18-TcpConnectionFails
		   ;; restarts the ConnectRetryTimer (with the initial value),
		   (FSM-TIMERS-start-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started ConnectRetryTimer~%" %this-thread-name))

		   ;; stops and clears the DelayOpenTimer (sets the value to zero),
		   (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
		   (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))
		   
		   ;; releases all BGP resource,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; optionally performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

;;; ACTIVE  Event-20-BGPOpen-with-DelayOpenTimer-running
		  
		  ;; p60 If an OPEN message is received and the DelayOpenTimer is running (Event 20), the local system:
		  (Event-20-BGPOpen-with-DelayOpenTimer-running
		   ;;; BGPOpen message processing START ;;;
		   ;; Set session state and timers
		   ;; peer-session state: peer-advertised-hold-time, peer-router-id, peer-asn, peer-advertised-caps, capability-set, 4-octet-asn-flag
		   ;; FSM-timers: HoldTime, KeepAliveTime
		   (let ((bgp-open-msg (MSG-get-arg1 %message)))
		     
		     (setf (PEER-SESSION-STATE-get-peer-advertised-hold-time peer-session-state)
			   (BGP-OPEN-get-hold-time bgp-open-msg))

		     (setf (PEER-SESSION-STATE-get-peer-router-id peer-session-state) 
			   (BGP-OPEN-get-bgp-id bgp-open-msg))
		     
		     (setf (PEER-SESSION-STATE-get-peer-asn peer-session-state)
			   (BGP-OPEN-get-my-as bgp-open-msg))

		     (setf (PEER-SESSION-STATE-get-peer-advertised-caps peer-session-state)
			   (tl-tree-filter-collect1 +BGP-CAP-all-types+ bgp-open-msg))

		     (setf (PEER-SESSION-STATE-get-common-capability-set peer-session-state)
			   (intersection (PEER-SESSION-STATE-get-peer-advertised-caps peer-session-state)
					 (PEER-CONFIG-get-capability-set peer-config)
					 :test #'equal))
		     
		     (cond ((tl-find-in-tree 'BGP-CAP-4-OCTET-ASN (PEER-SESSION-STATE-get-common-capability-set peer-session-state))
			    (setf (PEER-SESSION-STATE-get-4-octet-asn-flag peer-session-state) t)
			    (QUEUE-send-message netiorx-queue (MSG-make 'SET '4-OCTET-ASN-FLAG t))
			    (QUEUE-send-message netiotx-queue (MSG-make 'SET '4-OCTET-ASN-FLAG t)))
			   (t
			    (setf (PEER-SESSION-STATE-get-4-octet-asn-flag peer-session-state) nil)
			    (QUEUE-send-message netiorx-queue (MSG-make 'SET '4-OCTET-ASN-FLAG nil))
			    (QUEUE-send-message netiotx-queue (MSG-make 'SET '4-OCTET-ASN-FLAG nil))))
		     
		     ;; sets a KeepaliveTimer (via the text below)
		     ;; sets the HoldTimer according to the negotiated value (see Section 4.2),
		     (setf (FSM-TIMERS-get-HoldTime FSM-timers) (* (min (PEER-CONFIG-get-hold-time peer-config)
									(PEER-SESSION-STATE-get-peer-advertised-hold-time peer-session-state))
								   internal-time-units-per-second))
		     (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Set negotiated HoldTime: ~D seconds~%" %this-thread-name (/ (FSM-TIMERS-get-HoldTime FSM-timers) internal-time-units-per-second)))
		     
		     (setf (FSM-TIMERS-get-KeepAliveTime FSM-timers) (floor (/ (FSM-TIMERS-get-HoldTime FSM-timers)
									       3)))
		     (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Set negotiated KeepaliveTime: ~D seconds~%" %this-thread-name (/ (FSM-TIMERS-get-KeepaliveTime FSM-timers) internal-time-units-per-second))))
		   ;;; BGPOpen message processing END ;;;
		   
		   ;; stops the ConnectRetryTimer (if running) and sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; stops and clears the DelayOpenTimer (sets to zero),
		   (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
		   (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))

		   ;; completes the BGP initialization,

		   ;; sends an OPEN message,

		   ;; sends a KEEPALIVE message,
		   (QUEUE-send-message netiotx-queue (MSG-make 'SEND-IMMEDIATE (BGP-MESSAGE-make (BGP-KEEPALIVE-make))))
		   (FSM-TIMERS-start-KeepaliveTimer FSM-timers)

		   ;; TODO: need to understand what is being negotiated here if negoatiated HoldTime is zero? Does the RFC mean HoldTime rather than HoldTimer???
		   ;; if the HoldTimer value is non-zero,
		   (cond ((not (= 0 (FSM-TIMERS-get-HoldTime FSM-timers)))
			  
			  ;; starts the KeepaliveTimer to initial value,
			  (FSM-TIMERS-start-KeepaliveTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started KeepaliveTimer~%" %this-thread-name))

			  ;; resets the HoldTimer to the negotiated value,
			  )
			 (t  ;; else if the HoldTimer is zero
			  ;; resets the KeepaliveTimer (set to zero),
;;;;; TODO OPEN MSG PROCESSING		     
			  (setf (FSM-TIMERS-get-KeepaliveTimer FSM-timers) 0)

			  ;; resets the HoldTimer to zero, and
			  (setf (FSM-TIMERS-get-HoldTimer FSM-timers) 0)

			  ;; changes its state to OPENCONFIRM
			  (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> OPENCONFIRM~%" %this-thread-name FSM-state))
			  (setf FSM-state 'OPENCONFIRM))))

		  ;; p60 If the value of the autonomous system field is the same as the local Autonomous System number, set the connection status to an internal connection; otherwise it will be external.

		  ;; p60-61 If BGP message header checking (Event 21) or OPEN message checking detects an error (Event 22) (see Section 6.2), the local system:

;;; ACTIVE  Event-21-BGPHeaderErr  Event-22-BGPOpenMsgErr
		  
		  ((Event-21-BGPHeaderErr Event-22-BGPOpenMsgErr)
		   ;; (optionally) sends a NOTIFICATION message with the appropriate error code if the SendNOTIFICATIONwithoutOPEN attribute is set to TRUE,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers )

		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

;;; ACTIVE  Event-24-NotifMsgVerErr
		  
		  ;; p61 If a NOTIFICATION message is received with a version error (Event 24), the local system checks the DelayOpenTimer.  
		  (Event-24-NotifMsgVerErr
		   ;; If the DelayOpenTimer is running, the local system:
		   (cond ((FSM-TIMERS-DelayOpenTimer-is-running-p FSM-timers)
			  
			  ;; stops the ConnectRetryTimer (if running) and sets the ConnectRetryTimer to zero,
			  (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

			  ;; stops and resets the DelayOpenTimer (sets to zero),
			  (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
			  (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))
			  
			  ;; releases all BGP resources,

			  ;; drops the TCP connection, and

			  ;; changes its state to Idle.
			  (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
			  (setf FSM-state 'IDLE))
			 (t  ;; If the DelayOpenTimer is not running, the local system:
			  ;; sets the ConnectRetryTimer to zero,
			  (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
			  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

			  ;; releases all BGP resources,

			  ;; drops the TCP connection,

			  ;; increments the ConnectRetryCounter by 1,
			  (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

			  ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

			  ;; changes its state to IDLE
			  (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
			  (setf FSM-state 'IDLE))))

;;; ACTIVE  Other Events 8, 10-11, 13, 19, 23, 25-28
		  
		  ;; p62 In response to any other event (Events 8, 10-11, 13, 19, 23, 25-28), the local system:
		  (t
		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
		   
		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by one,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE)))

		) ;; END (CONNECT

;;; OPENSENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	       
	       (OPENSENT
		;; p62  In this state, BGP FSM waits for an OPEN message from its peer.
		(case event
;;; OPENSENT Start Events 1, 3-7
		  ;; The start events (Events 1, 3-7) are ignored in the OpenSent state.
		  ((Event-1-ManualStart
		    Event-3-AutomaticStart
		    Event-4-ManualStart-with-PassiveTcpEstablishment
		    Event-5-AutomaticStart-with-PassiveTcpEstablishment
		    Event-6-AutomaticStart-with-DampPeerOscillations
		    Event-7-AutomaticStart-with-DampPeerOscillations-and-PassiveTcpEstablishment)
		   nil)

		  ;; p62 If a ManualStop event (Event 2) is issued in the OpenSent state, the local system:
		  (Event-2-ManualStop
		   ;; sends the NOTIFICATION with a Cease,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; sets the ConnectRetryCounter to zero, and
		   (setf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib) 0)

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

;;; OPENSENT  Event-8-AutomaticStop

		  ;; p62 If an AutomaticStop event (Event 8) is issued in the OpenSent state, the local system:
		  (Event-8-AutomaticStop
		   ;; sends the NOTIFICATION with a Cease,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; releases all the BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p63 If the HoldTimer-Expires (Event 10), the local system:
		  (Event-10-HoldTimer-Expires
		   ;; sends a NOTIFICATION message with the error code Hold Timer Expired,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
		   
		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p63 If a TcpConnection-Valid (Event 14), Tcp-CR-Acked (Event 16), or a TcpConnectionConfirmed event (Event 17) is received, a second TCP connection may be in progress.  This second TCP connection is tracked per Connection Collision processing (Section 6.8) until an OPEN message is received.
		  ((Event-14-TcpConnection-Valid
		    Event-16-Tcp-CR-Acked
		    Event-17-TcpConnectionConfirmed)

		   )

		  ;; p63 A TCP Connection Request for an Invalid port (Tcp-CR-Invalid (Event 15)) is ignored.
		  (Event-15-Tcp-CR-Invalid
		   'IGNORED)

		  ;; p63 If a TcpConnectionFails event (Event 18) is received, the local system:
		  (Event-18-TcpConnectionFails
		   ;; closes the BGP connection,

		   ;; restarts the ConnectRetryTimer,
		   (FSM-TIMERS-start-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started ConnectRetryTimer~%" %this-thread-name))
		   
		   ;; continues to listen for a connection that may be initiated by the remote BGP peer, and

		   ;; changes its state to ACTIVE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p64 When an OPEN message is received, all fields are checked for correctness.  If there are no errors in the OPEN message (Event 19), the local system:
		  (Event-19-BGPOpen
		   ;;; BGPOpen message processing START ;;;
		   ;; Set session state and timers
		   ;; peer-session state: peer-advertised-hold-time, peer-router-id, peer-asn, peer-advertised-caps, capability-set, 4-octet-asn-flag
		   ;; FSM-timers: HoldTime, KeepAliveTime
		   (let ((bgp-open-msg (MSG-get-arg1 %message)))
		     
		     (setf (PEER-SESSION-STATE-get-peer-advertised-hold-time peer-session-state)
			   (BGP-OPEN-get-hold-time bgp-open-msg))

		     (setf (PEER-SESSION-STATE-get-peer-router-id peer-session-state) 
			   (BGP-OPEN-get-bgp-id bgp-open-msg))
		     
		     (setf (PEER-SESSION-STATE-get-peer-asn peer-session-state)
			   (BGP-OPEN-get-my-as bgp-open-msg))

		     (setf (PEER-SESSION-STATE-get-peer-advertised-caps peer-session-state)
			   (tl-tree-filter-collect1 +BGP-CAP-all-types+ bgp-open-msg))

		     (setf (PEER-SESSION-STATE-get-common-capability-set peer-session-state)
			   (intersection (PEER-SESSION-STATE-get-peer-advertised-caps peer-session-state)
					 (PEER-CONFIG-get-capability-set peer-config)
					 :test #'equal))
		     
		     (cond ((tl-find-in-tree 'BGP-CAP-4-OCTET-ASN (PEER-SESSION-STATE-get-common-capability-set peer-session-state))
			    (setf (PEER-SESSION-STATE-get-4-octet-asn-flag peer-session-state) t)
			    (QUEUE-send-message netiorx-queue (MSG-make 'SET '4-OCTET-ASN-FLAG t))
			    (QUEUE-send-message netiotx-queue (MSG-make 'SET '4-OCTET-ASN-FLAG t)))
			   (t
			    (setf (PEER-SESSION-STATE-get-4-octet-asn-flag peer-session-state) nil)
			    (QUEUE-send-message netiorx-queue (MSG-make 'SET '4-OCTET-ASN-FLAG nil))
			    (QUEUE-send-message netiotx-queue (MSG-make 'SET '4-OCTET-ASN-FLAG nil))))
		     
		     ;; sets a KeepaliveTimer (via the text below)
		     ;; sets the HoldTimer according to the negotiated value (see Section 4.2),
		     (setf (FSM-TIMERS-get-HoldTime FSM-timers) (* (min (PEER-CONFIG-get-hold-time peer-config)
									(PEER-SESSION-STATE-get-peer-advertised-hold-time peer-session-state))
								   internal-time-units-per-second))
		     (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Set negotiated HoldTime: ~D seconds~%" %this-thread-name (/ (FSM-TIMERS-get-HoldTime FSM-timers) internal-time-units-per-second)))
		     
		     (setf (FSM-TIMERS-get-KeepAliveTime FSM-timers) (floor (/ (FSM-TIMERS-get-HoldTime FSM-timers)
									       3)))
		     (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Set negotiated KeepaliveTime: ~D seconds~%" %this-thread-name (/ (FSM-TIMERS-get-KeepaliveTime FSM-timers) internal-time-units-per-second))))
		   ;;; BGPOpen message processing END ;;;
		   
		   ;; resets the DelayOpenTimer to zero,
		   (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
		   (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))

		   ;; sets the BGP ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
		   
		   ;; sends a KEEPALIVE message, and
		   (QUEUE-send-message netiotx-queue (MSG-make 'SEND-IMMEDIATE (BGP-MESSAGE-make (BGP-KEEPALIVE-make))))
		   (FSM-TIMERS-start-KeepaliveTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started KeepaliveTimer~%" %this-thread-name))
		   
		   ;; changes its state to OPENCONFIRM
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> OPENCONFIRM~%" %this-thread-name FSM-state))
		   (setf FSM-state 'OPENCONFIRM)
		   ;; If the negotiated hold time value is zero, then the HoldTimer and KeepaliveTimer are not started.  If the value of the Autonomous System field is the same as the local Autonomous System number, then the connection is an "internal" connection; otherwise, it is an "external" connection.  (This will impact UPDATE processing as described below.)
		   )

		  ;; p64 If the BGP message header checking (Event 21) or OPEN message checking detects an error (Event 22)(see Section 6.2), the local system:
		  (Event-21-BGPHeaderErr
		   ;; sends a NOTIFICATION message with the appropriate error code,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
		   
		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))
		  #|
		  ;; p64-65 Collision detection mechanisms (Section 6.8) need to be applied when a valid BGP OPEN message is received (Event 19 or Event 20). Please refer to Section 6.8 for the details of the comparison.  A CollisionDetectDump event occurs when the BGP implementation determines, by means outside the scope of this document, that a connection collision has occurred.
		  ((Event-19-BGPOpen
		  Event-20-BGPOpen-with-DelayOpenTimer-running)
		  'NOT-IMPLEMENTED
		  )
		  |#
		  ;; p65 If a connection in the OpenSent state is determined to be the connection that must be closed, an OpenCollisionDump (Event 23) is signaled to the state machine.  If such an event is received in the OpenSent state, the local system:
		  (Event-23-OpenCollisionDump
		   ;; sends a NOTIFICATION with a Cease,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p65 If a NOTIFICATION message is received with a version error (Event 24), the local system:
		  (Event-24-NotifMsgVerErr
		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
		   
		   ;; releases all BGP resources,

		   ;; drops the TCP connection, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p65 In response to any other event (Events 9, 11-13, 20, 25-28), the local system:
		  (t
		   ;; sends the NOTIFICATION with the Error Code Finite State Machine Error,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE)))
		
		) ; END (OPENSENT

;;; OPENCONFIRM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	       
	       (OPENCONFIRM
		;; p66 In this state, BGP waits for a KEEPALIVE or NOTIFICATION message.
		(case event
		  ;; Any start event (Events 1, 3-7) is ignored in the OpenConfirm state.
		  ((Event-1-ManualStart
		    Event-3-AutomaticStart
		    Event-4-ManualStart-with-PassiveTcpEstablishment
		    Event-5-AutomaticStart-with-PassiveTcpEstablishment
		    Event-6-AutomaticStart-with-DampPeerOscillations
		    Event-7-AutomaticStart-with-DampPeerOscillations-and-PassiveTcpEstablishment)
		   'IGNORED)

		  ;; p66 In response to a ManualStop event (Event 2) initiated by the operator, the local system:
		  (Event-2-ManualStop
		   ;; sends the NOTIFICATION message with a Cease,

		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; sets the ConnectRetryCounter to zero,
		   (setf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib) 0)

		   ;; sets the ConnectRetryTimer to zero, and
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
		   
		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p66 In response to the AutomaticStop event initiated by the system (Event 8), the local system:
		  (Event-8-AutomaticStop
		   ;; sends the NOTIFICATION message with a Cease,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
		   
		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))


		  ;; p66-67 If the HoldTimer-Expires event (Event 10) occurs before a KEEPALIVE message is received, the local system:
		  (Event-10-HoldTimer-Expires
		   ;; sends the NOTIFICATION message with the Error Code Hold Timer Expired,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
		   
		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the  DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p67 If the local system receives a KeepaliveTimer-Expires event (Event 11), the local system:
		  (Event-11-KeepaliveTimer-Expires
		   ;; sends a KEEPALIVE message,
		   (QUEUE-send-message netiotx-queue (MSG-make 'SEND-IMMEDIATE (BGP-MESSAGE-make (BGP-KEEPALIVE-make))))

		   ;; restarts the KeepaliveTimer, and
		   (FSM-TIMERS-start-KeepaliveTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started KeepaliveTimer~%" %this-thread-name))

		   ;; remains in the OpenConfirmed state.
		   )

		  ;; p67 In the event of a TcpConnection-Valid event (Event 14), or the success of a TCP connection (Event 16 or Event 17) while in OpenConfirm, the local system needs to track the second connection.
		  ((Event-14-TcpConnection-Valid
		    Event-16-Tcp-CR-Acked
		    Event-17-TcpConnectionConfirmed)
		   'NOT-IMPLEMENTED)

		  ;; p67 If a TCP connection is attempted with an invalid port (Event 15), the local system will ignore the second connection attempt.
		  (Event-15-Tcp-CR-Invalid
		   )

		  ;; p67 If the local system receives a TcpConnectionFails event (Event 18) from the underlying TCP or a NOTIFICATION message (Event 25), the local system:
		  ((Event-18-TcpConnectionFails
		    Event-25-NotifMsg)
		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p68 If the local system receives a NOTIFICATION message with a version error (NotifMsgVerErr (Event 24)), the local system:
		  (Event-24-NotifMsgVerErr
		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; releases all BGP resources,

		   ;; drops the TCP connection, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; P68 If the local system receives a valid OPEN message (BGPOpen (Event 19)), the collision detect function is processed per Section 6.8. If this connection is to be dropped due to connection collision, the local system:
		  (Event-19-BGPOpen
		   ;; sends a NOTIFICATION with a Cease,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; releases all BGP resources,

		   ;; drops the TCP connection (send TCP FIN),

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p68 If an OPEN message is received, all fields are checked for correctness.  If the BGP message header checking (BGPHeaderErr (Event 21)) or OPEN message checking detects an error (see Section 6.2) (BGPOpenMsgErr (Event 22)), the local system:
		  (Event-22-BGPOpenMsgErr
		   ;; sends a NOTIFICATION message with the appropriate error code,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p69 If, during the processing of another OPEN message, the BGP implementation determines, by a means outside the scope of this document, that a connection collision has occurred and this connection is to be closed, the local system will issue an OpenCollisionDump event (Event 23).  When the local system receives an OpenCollisionDump event (Event 23), the local system:
		  (Event-23-OpenCollisionDump
		   ;; sends a NOTIFICATION with a Cease,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; releases all BGP resources

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p69 If the local system receives a KEEPALIVE message (KeepAliveMsg (Event 26)), the local system:
		  (Event-26-KeepAliveMsg
		   ;; restarts the HoldTimer and
		   (FSM-TIMERS-start-HoldTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer~%" %this-thread-name))

		   ;; changes its state to ESTABLISHED
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> ESTABLISHED~%" %this-thread-name FSM-state))
		   (setf FSM-state 'ESTABLISHED))

		  ;; p69 In response to any other event (Events 9, 12-13, 20, 27-28), the local system:
		  (t
		   ;; sends a NOTIFICATION with a code of Finite State Machine Error,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE)))
		
		) ; END (OPENCONFIRM

;;; ESTABLISHED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	       
	       (ESTABLISHED
		;; p70 In the Established state, the BGP FSM can exchange UPDATE,
		;; NOTIFICATION, and KEEPALIVE messages with its peer.
		(case event
		  ;; p70 Any Start event (Events 1, 3-7) is ignored in the Established state.
		  ((Event-1-ManualStart
		    Event-3-AutomaticStart
		    Event-4-ManualStart-with-PassiveTcpEstablishment
		    Event-5-AutomaticStart-with-PassiveTcpEstablishment
		    Event-6-AutomaticStart-with-DampPeerOscillations
		    Event-7-AutomaticStart-with-DampPeerOscillations-and-PassiveTcpEstablishment)
		   nil)

		  ;; p70 In response to a ManualStop event (initiated by an operator) (Event 2), the local system:
		  (Event-2-ManualStop
		   ;; sends the NOTIFICATION message with a Cease,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started ConnectRetryTimer~%" %this-thread-name))

		   ;; deletes all routes associated with this connection,

		   ;; releases BGP resources,

		   ;; drops the TCP connection,

		   ;; sets the ConnectRetryCounter to zero, and
		   (setf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib) 0)

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p70 In response to an AutomaticStop event (Event 8), the local system:
		  (Event-8-AutomaticStop
		   ;; sends a NOTIFICATION with a Cease,

		   ;; sets the ConnectRetryTimer to zero
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started ConnectRetryTimer~%" %this-thread-name))
		   
		   ;; deletes all routes associated with this connection,

		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))
		  ;; One reason for an AutomaticStop event is: A BGP receives an UPDATE messages with a number of prefixes for a given peer such that the total prefixes received exceeds the maximum number of prefixes configured.  The local system automatically disconnects the peer.

		  ;; p71 If the HoldTimer-Expires event occurs (Event 10), the local system:
		  (Event-10-HoldTimer-Expires
		   
		   ;; sends a NOTIFICATION message with the Error Code Hold Timer Expired,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
		   
		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p71 If the KeepaliveTimer-Expires event occurs (Event 11), the local system:
		  (Event-11-KeepaliveTimer-Expires
		   ;; sends a KEEPALIVE message, and
		   (QUEUE-send-message netiotx-queue (MSG-make 'SEND-IMMEDIATE (BGP-MESSAGE-make (BGP-KEEPALIVE-make))))
		   ;; restarts its KeepaliveTimer, unless the negotiated HoldTime value is zero.
		   (FSM-TIMERS-start-KeepaliveTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started KeepaliveTimer~%" %this-thread-name))
		   
		   ;; Each time the local system sends a KEEPALIVE or UPDATE message, it restarts its KeepaliveTimer, unless the negotiated HoldTime value is zero.
		   )

		  ;; p71 A TcpConnection-Valid (Event 14), received for a valid port, will cause the second connection to be tracked.
		  (Event-14-TcpConnection-Valid
		   )

		  ;; p71 An invalid TCP connection (Tcp-CR-Invalid event (Event 15)) will be ignored.
		  (Event-15-Tcp-CR-Invalid
		   nil)

		  ;; p71 In response to an indication that the TCP connection is successfully established (Event 16 or Event 17), the second connection SHALL be tracked until it sends an OPEN message.
		  ((Event-16-Tcp-CR-Acked Event-17-TcpConnectionConfirmed)
		   )
		  
		  ;; p72 If a valid OPEN message (BGPOpen (Event 19)) is received, and if the CollisionDetectEstablishedState optional attribute is TRUE, 
		  (Event-19-BGPOpen
		   (when (FSM-ATTRIB-get-CollisionDetectEstablishedState FSM-attrib)
		     ;;the OPEN message will be checked to see if it collides (Section 6.8) with any other connection.  If the BGP implementation determines that this connection needs to be terminated, it will process an OpenCollisionDump event (Event 23).  If this connection needs to be terminated, the local system:
		     ;; sends a NOTIFICATION with a Cease,

		     ;; sets the ConnectRetryTimer to zero,
		     (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		     (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
		     
		     ;; deletes all routes associated with this connection,

		     ;; releases all BGP resources,

		     ;; drops the TCP connection,

		     ;; increments the ConnectRetryCounter by 1,
		     (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		     ;; (optionally) performs peer oscillation damping if the DampPeerOscillations is set to TRUE, and

		     ;; changes its state to IDLE
		     (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		     (setf FSM-state 'IDLE)))

		  ;; p72 If the local system receives a NOTIFICATION message (Event 24 or Event 25) or a TcpConnectionFails (Event 18) from the underlying TCP, the local system:
		  ((Event-24-NotifMsgVerErr
		    Event-25-NotifMsg
		    Event-18-TcpConnectionFails)
		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))
		   
		   ;; deletes all routes associated with this connection,

		   ;; releases all the BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))

		  ;; p73 If the local system receives a KEEPALIVE message (Event 26), the local system:
		  (Event-26-KeepAliveMsg
		   ;; restarts its HoldTimer, if the negotiated HoldTime value is non-zero, and
		   (FSM-TIMERS-start-HoldTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer~%" %this-thread-name))

		   ;; remains in the Established state.
		   )

		  ;; p73 If the local system receives an UPDATE message (Event 27), the local system:
		  (Event-27-UpdateMsg
		   ;; processes the message,
		   (let ((sbgp-update (MSG-get-arg1 %message)))
		     ;; (format t "~%UPDATEMSG ~S~%" sbgp-update)
		     (when RIB-Adj-in

		       (let ((rib-adj-entry-list
			       (loop for nlri-withdrawl in (SBGP-UPDATE-get-nlri-withdrawl-list sbgp-update)
				     collect (RIB-ADJ-remove-nlri RIB-Adj-in nlri-withdrawl))))
			 (when rib-adj-entry-list
			   (QUEUE-send-message %control-queue (MSG-make 'WITHDRAWL-RIB-ADJ->RIB-LOC %this-thread-name rib-adj-entry-list))))

		       (let ((rib-adj-entry-list
			       (loop for nlri in (SBGP-UPDATE-get-nlri-list sbgp-update)
				     collect (RIB-ADJ-add-entry RIB-Adj-in
								(RIB-ADJ-entry-make nlri
										    (SBGP-UPDATE-get-path-attrib-list sbgp-update))))))
			 (when rib-adj-entry-list
			   (QUEUE-send-message %control-queue (MSG-make 'ANNOUNCE-RIB-ADJ->RIB-LOC %this-thread-name rib-adj-entry-list))))))

		   ;; restarts its HoldTimer, if the negotiated HoldTime value is non-zero, and
		   (FSM-TIMERS-start-HoldTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer~%" %this-thread-name))
		   
		   ;; remains in the Established state.
		   )

		  ;; p73 If the local system receives an UPDATE message, and the UPDATE message error handling procedure (see Section 6.3) detects an error (Event 28), the local system:
		  (Event-28-UpdateMsgErr
		   ;; sends a NOTIFICATION message with an Update error,

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; deletes all routes associated with this connection,

		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE))
		  
		  ;; p73 In response to any other event (Events 9, 12-13, 20-22), the local system:
		  (t
		   ;; sends a NOTIFICATION message with the Error Code Finite State Machine Error,

		   ;; deletes all routes associated with this connection,
		   

		   ;; sets the ConnectRetryTimer to zero,
		   (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

		   ;; releases all BGP resources,

		   ;; drops the TCP connection,

		   ;; increments the ConnectRetryCounter by 1,
		   (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

		   ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

		   ;; changes its state to IDLE
		   (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
		   (setf FSM-state 'IDLE)))
		
		) ; END (ESTABLISHED
	       (t  
		(error "Unexpected STATE value = ~S. Expected [IDLE | CONNECT | ACTIVE | OPENSENT | OPENCONFIRM | ESTABLISHED ]"
		       FSM-state))
	       ) ; END (case FSM-state
	     ) ; END (cond ((member event +FSM-events+)
	    (t ;; not (member event +FSM-events+)
	     (when debug-fsm-events (format debug-fsm-events "~&~S FSM-EVENTS State: ~S UNKNOWN EVENT: ~S~%" %this-thread-name fsm-state event))))
      ))
;;; BGP FSM END 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      )
    )
