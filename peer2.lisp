
;;; this is a rewite of the peer.lisp FSM case statements so
;;; 1. is ordered by event then by stete within each event, rather than direct transposition of rfc4271 - should result in less duplication of code
;;; 2. exclude optional events except 12, 13 & 20
#|
(case FSM-State
       (IDLE)
       (CONNECT)
       (ACTIVE)
       (OPENSENT)
       (OPENCONFIRM)
       (ESTABLISHED))
|#

(defun NEW-FSM-test (event FSM-state)
  (case event
    ;; ADMINISTRATIVE EVENTS
    (Event-1-ManualStart
     (case FSM-state
       (IDLE
	(cond ((and RIB-Adj-in RIB-Adj-out router-config peer-config
		    (ROUTER-CONFIG-get-router-id router-config)         
		    (ROUTER-CONFIG-get-local-asn router-config)         
		    (PEER-CONFIG-get-peer-asn peer-config)           
		    (PEER-CONFIG-get-peer-ip-address peer-config))
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

	       ;; changes its state to Connect.
	       (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> CONNECT~%" %this-thread-name FSM-state))
	       (setf FSM-state 'CONNECT)	       		  
	)))
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED) 'IGNORED)))
    
    (Event-2-ManualStop
     (case FSM-state
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED)
	;; (ACTIVE) stops the DelayOpenTimer
	(when (eq FSM-state 'ACTIVE)
	  (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
	  (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil)))

	;; (ACTIVE) If the DelayOpenTimer is running and the SendNOTIFICATIONwithoutOPEN session attribute is set, the local system sends a NOTIFICATION with a Cease,
	(when (and (eq FSM-state 'ACTIVE)
		   (FSM-TIMERS-DelayOpenTimer-is-running-p FSM-timers)
		   (FSM-ATTRIB-get-SendNOTIFICATIONwithoutOPEN FSM-attrib))
	  )
	
	;; (OPENSENT OPENCONFIRM ESTABLISHED) sends the NOTIFICATION message with a Cease,
	(when (or (eq FSM-state 'OPENSENT)
		  (eq FSM-state 'OPENCONFIRM)
		  (eq FSM-state 'ESTABLISHED))
	  )	  

	;; (ESTABLISHED) deletes all routes associated with this connection,
	(when (eq FSM-state 'ESTABLISHED)
	  )
	
        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; sets the ConnectRetryCounter to zero,
	(setf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib) 0)

        ;; sets the ConnectRetryTimer to zero, and
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))))
    
    ;; TIMER-EVENTS
    (Event-9-ConnectRetryTimer-Expires
     (case FSM-state
       (IDLE 'IGNORED)
       (CONNECT
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

        ;; stays in the Connect state.
	)
       (ACTIVE
	;; restarts the ConnectRetryTimer (with initial value),
	(FSM-TIMERS-start-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started ConnectRetryTimer~%" %this-thread-name))

        ;; initiates a TCP connection to the other BGP peer,

        ;; continues to listen for a TCP connection that may be initiated by a remote BGP peer, and

        ;; changes its state to Connect.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> CONNECT~%" %this-thread-name FSM-state))
	(setf FSM-state 'CONNECT))
       
       ((OPENSENT OPENCONFIRM ESTABLISHED)
	;; sends the NOTIFICATION with the Error Code Finite State Machine Error,

	;; (ESTABLISHED) deletes all routes associated with this connection,
	(when (eq FSM-state 'ESTABLISHED)
	  )
	
        ;; sets the ConnectRetryTimer to zero,
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))
	
	;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))))
    
    (Event-10-HoldTimer-Expires
     (case FSM-state
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED)
	;; (CONNECT ACTIVE) if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),
	(when (or (eq FSM-state 'CONNECT)
		  (eq FSM-state 'ACTIVE))
	  (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
	  (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil)))

	;; (OPENSENT OPENCONFIRM ESTABLISHED) sends a NOTIFICATION message with the Error Code Hold Timer Expired,
	(when (or (eq FSM-state 'OPENSENT)
		  (eq FSM-state 'OPENCONFIRM)
		  (eq FSM-state 'ESTABLISHED))
	  )

	;; (ESTABLISHED) deletes all routes associated with this connection,
	(when (eq FSM-state 'ESTABLISHED)
	  )

        ;; sets the ConnectRetryTimer to zero,
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))
	
        ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))))
    
    (Event-11-KeepaliveTimer-Expires
     (case FSM-state
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT)
	;; if the ConnectRetryTimer is running, stops and resets the ConnectRetryTimer (sets to zero),
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; (CONNECT ACTIVE) if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),
	(when (or (eq FSM-state 'CONNECT)
		  (eq FSM-state 'ACTIVE))
	  (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
	  (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil)))

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

        ;; performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))
       
       (OPENCONFIRM
	;; sends a KEEPALIVE message,
	(QUEUE-send-message netiotx-queue (MSG-make 'SEND-IMMEDIATE (BGP-MESSAGE-make (BGP-KEEPALIVE-make))))
	
        ;; restarts the KeepaliveTimer, and
	(FSM-TIMERS-start-KeepaliveTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started KeepaliveTimer~%" %this-thread-name))

        ;; remains in the OpenConfirmed state.
	)
       (ESTABLISHED
	;; sends a KEEPALIVE message, and
	(QUEUE-send-message netiotx-queue (MSG-make 'SEND-IMMEDIATE (BGP-MESSAGE-make (BGP-KEEPALIVE-make))))
	
        ;; restarts its KeepaliveTimer, unless the negotiated HoldTime value is zero.
	(unless (= (FSM-TIMERS-get-HoldTime FSM-timers) 0)
	  (FSM-TIMERS-start-KeepaliveTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started KeepaliveTimer~%" %this-thread-name)))
	)))
    
    (Event-12-DelayOpenTimer-Expires
     (case FSM-state
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE)
	;; (ACTIVE) sets the ConnectRetryTimer to zero,
	(when (eq FSM-state 'ACTIVE)
	  (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name)))

        ;; (ACTIVE) stops and clears the DelayOpenTimer (set to zero),
	(when (eq FSM-state 'ACTIVE)
	  (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
	  (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil)))

        ;; (ACTIVE) completes the BGP initialization,

        ;; sends the OPEN message to its remote peer,
	(QUEUE-send-message netiotx-queue
				       (MSG-make 'SEND
					     (BGP-MESSAGE-make (BGP-OPEN-make (ROUTER-CONFIG-get-local-asn router-config)
									      (PEER-CONFIG-get-hold-time peer-config)
									      (ROUTER-CONFIG-get-router-id router-config)
									      (PEER-CONFIG-get-capability-set peer-config)))))
		   
        ;; sets its hold timer to a large value, and
	(FSM-TIMERS-start-HoldTimer-LargeValue FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer-LargeValue" %this-thread-name))
	
        ;; changes its state to OpenSent.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> OPENSENT~%" %this-thread-name FSM-state))
	(setf FSM-state 'OPENSENT))
       
       ((OPENSENT OPENCONFIRM ESTABLISHED)
	;; sends the NOTIFICATION with the Error Code Finite State Machine Error,

	;; (ESTABLISHED) deletes all routes associated with this connection,
	(when (eq FSM-state 'ESTABLISHED)
	  )
	
        ;; sets the ConnectRetryTimer to zero,
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))
	
	;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.	
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))))
    
    (Event-13-IdleHoldTimer-Expires
     (case FSM-state
       (IDLE 'PREVENT-PEER-OSCILLATIONS) ;; TODO 
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED)
	;; (CONNECT ACTIVE) if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),
	(when (or (eq FSM-state 'CONNECT)
		  (eq FSM-state 'ACTIVE))
	  (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
	  (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil)))
	
	;; (OPENSENT OPENCONFIRM ESTABLISHED) sends the NOTIFICATION with the Error Code Finite State Machine Error,
	(when (or (eq FSM-state 'OPENSENT)
		  (eq FSM-state 'OPENCONFIRM)
		  (eq FSM-state 'ESTABLISHED))
	  )

	;; (ESTABLISHED) deletes all routes associated with this connection,
	(when (eq FSM-state 'ESTABLISHED)
	  )
	
        ;; sets the ConnectRetryTimer to zero,
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))
	
	;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))))

    ;; TCP CONNECTION-BASED EVENTS
    (Event-17-TcpConnectionConfirmed
     (case FSM-state
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE)
	;;If the DelayOpen attribute is set to TRUE, the local system:
	(cond ((FSM-ATTRIB-get-DelayOpen FSM-attrib)
	       
	       ;; stops the ConnectRetryTimer and sets the ConnectRetryTimer to zero,
	       (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	       (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

	       ;; sets the DelayOpenTimer to the initial value (DelayOpenTime), and
	       (FSM-TIMERS-start-DelayOpenTimer FSM-timers)
	       (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started DelayOpenTimer~%" %this-thread-name))
	       (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG t))
	       
	       ;; stays in the Connect or Active state.
	       )
	      (t
	       ;;If the DelayOpen attribute is set to FALSE, the local system:

	       ;; sets the ConnectRetryTimer to zero,
	       (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	       (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

	       ;; completes the BGP initialization,

	       ;; sends the OPEN message to its peer,
	       (QUEUE-send-message netiotx-queue
				   (MSG-make 'SEND
					     (BGP-MESSAGE-make (BGP-OPEN-make (ROUTER-CONFIG-get-local-asn router-config)
									      (PEER-CONFIG-get-hold-time peer-config)
									      (ROUTER-CONFIG-get-router-id router-config)
									      (PEER-CONFIG-get-capability-set peer-config)))))
	       
	       ;; sets its HoldTimer to a large value, and
	       (FSM-TIMERS-start-HoldTimer-LargeValue FSM-timers)
		   (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer-LargeValue" %this-thread-name))
		   
	       ;; changes its state to OpenSent.
	       	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> OPENSENT~%" %this-thread-name FSM-state))
		(setf FSM-state 'OPENSENT))))
       
       (OPENSENT 'CONNECTION-COLLISION-PROCESSING)
       (OPENCONFIRM 'CONNECTION-COLLISION-PROCESSING)
       (ESTABLISHED 'CONNECTION-COLLISION-PROCESSING)))
    
    (Event-18-TcpConnectionFails
     (case FSM-state
       (IDLE 'IGNORED)
       (CONNECT
	;; If the DelayOpenTimer is running, the local system:
	(cond ((FSM-TIMERS-DelayOpenTimer-is-running-p FSM-timers)

	       ;; restarts the ConnectRetryTimer with the initial value,
	       (FSM-TIMERS-start-ConnectRetryTimer FSM-timers)
	       (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started ConnectRetryTimer~%" %this-thread-name))

	       ;; stops the DelayOpenTimer and resets its value to zero,
	       (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
	       (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
	       (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))

	       ;; continues to listen for a connection that may be initiated by the remote BGP peer, and
	       ;; changes its state to Active.
	       (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> ACTIVE~%" %this-thread-name FSM-state))
	       (setf FSM-state 'ACTIVE))
	      (t
	       ;; If the DelayOpenTimer is not running, the local system:

	       ;; stops the ConnectRetryTimer to zero,
	       (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	       (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

	       ;; drops the TCP connection,

	       ;; releases all BGP resources, and

	       ;; changes its state to Idle.
	       (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	       (setf FSM-state 'IDLE))))
       
       (ACTIVE
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

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))
       
       (OPENSENT
	;; closes the BGP connection,

        ;; restarts the ConnectRetryTimer,
	(FSM-TIMERS-start-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started ConnectRetryTimer~%" %this-thread-name))

        ;; continues to listen for a connection tht may be initiated by the remote BGP peer, and

        ;; changes its state to Active.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> ACTIVE~%" %this-thread-name FSM-state))
	(setf FSM-state 'ACTIVE)
	)
       (OPENCONFIRM
	;; sets the ConnectRetryTimer to zero,
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

        ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))
       
       (ESTABLISHED
	;; sets the ConnectRetryTimer to zero,
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; deletes all routes associated with this connection,

        ;; releases all the BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))))
    
    (Event-19-BGPOpen
     (case FSM-state
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE)
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

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))
       
       (OPENSENT
	;; resets the DelayOpenTimer to zero,
	(FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
	(QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))

        ;; sets the BGP ConnectRetryTimer to zero,
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; sends a KEEPALIVE message, and
	(QUEUE-send-message netiotx-queue (MSG-make 'SEND-IMMEDIATE (BGP-MESSAGE-make (BGP-KEEPALIVE-make))))

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
        
	;; If the negotiated hold time value is zero, then the HoldTimer and KeepaliveTimer are not started.  If the value of the Autonomous System field is the same as the local Autonomous System number, then the connection is an "internal" connection; otherwise, it is an "external" connection.
	
	(when (not (= (FSM-TIMERS-get-HoldTime FSM-timers) 0))
	  (FSM-TIMERS-start-KeepaliveTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started KeepaliveTimer~%" %this-thread-name))
	  (FSM-TIMERS-start-HoldTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer~%" %this-thread-name)))
;;; TODO set connection state to internal or external
;;; BGPOpen message processing END ;;;
	
	;; changes its state to OpenConfirm.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> OPENCONFIRM~%" %this-thread-name FSM-state))
	(setf FSM-state 'OPENCONFIRM))
       
       (OPENCONFIRM
	;; If the local system receives a valid OPEN message (BGPOpen (Event 19)), the collision detect function is processed per Section 6.8. If this connection is to be dropped due to connection collision, the local system:

        ;; sends a NOTIFICATION with a Cease,

        ;; sets the ConnectRetryTimer to zero,
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; releases all BGP resources,

        ;; drops the TCP connection (send TCP FIN),

        ;; increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

        ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))
       
       (ESTABLISHED
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

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))))
    
    (Event-20-BGPOpen-with-DelayOpenTimer-running
     (case FSM-state
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE)
	;; stops the ConnectRetryTimer (if running) and sets the ConnectRetryTimer to zero,
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; completes the BGP initialization,

        ;; stops and clears the DelayOpenTimer (sets the value to zero),
	(FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
	(QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil))

        ;; sends an OPEN message,

        ;; sends a KEEPALIVE message,
	(QUEUE-send-message netiotx-queue (MSG-make 'SEND-IMMEDIATE (BGP-MESSAGE-make (BGP-KEEPALIVE-make))))
	
        ;; if the HoldTimer initial value is non-zero,

	;; starts the KeepaliveTimer with the initial value and
	(FSM-TIMERS-start-KeepaliveTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started KeepaliveTimer~%" %this-thread-name))

            ;; resets the HoldTimer to the negotiated value,

        ;; else, if the HoldTimer initial value is zero,

            ;; resets the KeepaliveTimer and

            ;; resets the HoldTimer value to zero,

        ;; and changes its state to OpenConfirm.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> OPENCONFIRM~%" %this-thread-name FSM-state))
	(setf FSM-state 'OPENCONFIRM)

        ;; If the value of the autonomous system field is the same as local Autonomous System number, set the connection status to an internal connection; otherwise it will be "external".
	)
       ((OPENSENT OPENCONFIRM ESTABLISHED)
	;; sends the NOTIFICATION with the Error Code Finite State Machine Error,

	;; (ESTABLISHED) deletes all routes associated with this connection,
	(when (eq FSM-state 'ESTABLISHED)
	  )

        ;; sets the ConnectRetryTimer to zero,
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))
	
	;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))))
    
    ((Event-21-BGPHeaderErr Event-22-BGPOpenMsgErr)
     (case FSM-state
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED)
	;; (CONNECT ACTIVE) (optionally) If the SendNOTIFICATIONwithoutOPEN attribute is set to TRUE, then the local system first sends a NOTIFICATION message with the appropriate error code, and then
	(when (of (eq FSM-state 'CONNECT)
		  (eq FSM-state 'ACTIVE))
	  )
	
	;; (OPENSENT OPENCONFIRM) sends a NOTIFICATION message with the appropriate error code,
	(when (or (eq FSM-state 'OPENSENT)
		  (eq FSM-state 'OPENCONFIRM))
	  )

	;; (ESTABLISHED)  sends a NOTIFICATION message with the Error Code Finite State Machine Error,
	;; (ESTABLISHED) deletes all routes associated with this connection,
	(when (eq FSM-state 'ESTABLISHED)
	  )

	;; sets the ConnectRetryTimer to zero,
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

        ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is TRUE, and

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))))

    (Event-24-NotifMsgVerErr
     (case FSM-state
       (IDLE 'IGNORED)
       
       ((CONNECT ACTIVE)
	(cond ((FSM-TIMERS-DelayOpenTimer-is-running-p FSM-timers)
	       ;; If the DelayOpenTimer is running, the local system:
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
	      (t
	       ;; If the DelayOpenTimer is not running, the local system:

	       ;; stops the ConnectRetryTimer and sets the ConnectRetryTimer to zero,
	       (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	       (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

	       ;; releases all BGP resources,

	       ;; drops the TCP connection,

	       ;; increments the ConnectRetryCounter by 1,
	       (incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

	       ;; performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

	       ;; changes its state to Idle.
	       (when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	       (setf FSM-state 'IDLE))))
       
       ((OPENSENT OPENCONFIRM ESTABLISHED)
	;; sets the ConnectRetryTimer to zero,
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

	;; (ESTABLISHED) deletes all routes associated with this connection,
	(when (eq FSM-state 'ESTABLISHED)
	  )

        ;; releases all BGP resources,

        ;; drops the TCP connection, and

	;; (ESTABLISHED) increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))))
    
    (Event-25-NotifMsg
     (case FSM-state
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED)
	;; (OPENSENT) sends the NOTIFICATION with the Error Code Finite State Machine Error,
	(when (eq FSM-state 'OPENSENT)
	  )
	
	;; if the ConnectRetryTimer is running, stops and resets the ConnectRetryTimer (sets to zero),
	(when (FSM-TIMERS-ConnectRetryTimer-is-running-p FSM-timers)
	  (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name)))

        ;; if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),
	(when (FSM-TIMERS-DelayOpenTimer-is-running-p FSM-timers)
	  (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
	  (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil)))

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

        ;; performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))))

    (Event-26-KeepAliveMsg
     (case FSM-state
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT)
	;; (OPENSENT) sends the NOTIFICATION with the Error Code Finite State Machine Error,
	(when (eq FSM-state 'OPENSENT)
	  )
	
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

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))
       
       (OPENCONFIRM
	;; restarts the HoldTimer and
	(FSM-TIMERS-start-HoldTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer~%" %this-thread-name))

        ;; changes its state to Established.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> ESTABLISHED~%" %this-thread-name FSM-state))
	(setf FSM-state 'ESTABLISHED))
	
       (ESTABLISHED
	;; restarts its HoldTimer, if the negotiated HoldTime value is non-zero, and
	(if (not (= (FSM-TIMERS-get-HoldTime FSM-timers) 0))
	    (FSM-TIMERS-start-HoldTimer FSM-timers)
	    (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer~%" %this-thread-name)))
	
        ;; remains in the Established state.
	)))
    
    (Event-27-UpdateMsg
     (case FSM-state
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM)
	;; (OPENSENT OPENCONFIRM) sends the NOTIFICATION with the Error Code Finite State Machine Error,
	(when (or (eq FSM-state 'OPENSENT)
		  (eq FSM-state 'OPENCONFIRM))
	  )
	
	;; if the ConnectRetryTimer is running, stops and resets the ConnectRetryTimer (sets to zero),
	(when (FSM-TIMERS-ConnectRetryTimer-is-running-p FSM-timers)
	  (FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name)))

        ;; if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),
	(when (FSM-TIMERS-DelayOpenTimer-is-running-p FSM-timers)
	  (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
	  (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil)))
	
        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

        ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))
       
       (ESTABLISHED
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
	(if (not (= (FSM-TIMERS-get-HoldTime FSM-timers) 0))
	    (FSM-TIMERS-start-HoldTimer FSM-timers)
	    (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Started HoldTimer~%" %this-thread-name)))
		   
        ;; remains in the Established state.
	)))
    
    (Event-28-UpdateMsgErr
     (case FSM-state
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED)
	;; if the ConnectRetryTimer is running, stops and resets the ConnectRetryTimer (sets to zero),
	(FSM-TIMERS-stop-ConnectRetryTimer FSM-timers)
	(when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" %this-thread-name))

        ;; (CONNECT  ACTIVE) if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),
	(when (or (eq FSM-state 'CONNECT)
		  (eq FSM-state 'ACTIVE))
	  (FSM-TIMERS-stop-DelayOpenTimer FSM-timers)
	  (when debug-fsm-timers (format debug-fsm-timers "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" %this-thread-name))
	  (QUEUE-send-message netiorx-queue (MSG-make 'SET 'DELAY-OPEN-RUNNING-FLAG nil)))

	;; (OPENSENT OPENCONFIRM ESTABLISHED) sends the NOTIFICATION with the Error Code Finite State Machine Error,
	(when (or (eq FSM-state 'OPENSENT)
		  (eq FSM-State 'OPENCONFIRM)
		  (eq FSM-State 'ESTABLISHED))
	  )

	;; (ESTABLISHED) deletes all routes associated with this connection,
	(when (eq FSM-State 'ESTABLISHED)
	  )

	;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	(incf (FSM-ATTRIB-get-ConnectRetryCounter FSM-attrib))

        ;; performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

        ;; changes its state to Idle.
	(when debug-fsm-state (format debug-fsm-state "~&~S FSM-STATE State change: ~S -> IDLE~%" %this-thread-name FSM-state))
	(setf FSM-state 'IDLE))))))
