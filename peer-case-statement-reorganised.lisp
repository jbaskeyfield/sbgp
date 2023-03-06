
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
     (case FSM-State
       (IDLE
	;; initializes all BGP resources for the peer connection,

        ;; sets ConnectRetryCounter to zero,

        ;; starts the ConnectRetryTimer with the initial value,

        ;; initiates a TCP connection to the other BGP peer,

        ;; listens for a connection that may be initiated by the remote BGP peer, and

        ;; changes its state to Connect.
	)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED) 'IGNORED)))
    
    (Event-2-ManualStop
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED)
	;; (ACTIVE) stops the DelayOpenTimer

	;; (ACTIVE) If the DelayOpenTimer is running and the SendNOTIFICATIONwithoutOPEN session attribute is set, the local system sends a NOTIFICATION with a Cease,
	;; (OPENSENT OPENCONFIRM ESTABLISHED) sends the NOTIFICATION message with a Cease,

	;; (ESTABLISHED) deletes all routes associated with this connection,
	
        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; sets the ConnectRetryCounter to zero,

        ;; sets the ConnectRetryTimer to zero, and

        ;; changes its state to Idle.
	)))
    
    ;; TIMER-EVENTS
    (Event-9-ConnectRetryTimer-Expires
     (case FSM-State
       (IDLE 'IGNORED)
       (CONNECT
	;; drops the TCP connection,

        ;; restarts the ConnectRetryTimer,

        ;; stops the DelayOpenTimer and resets the timer to zero,

        ;; initiates a TCP connection to the other BGP peer,

        ;; continues to listen for a connection that may be initiated by the remote BGP peer, and

        ;; stays in the Connect state.
	)
       (ACTIVE
	;; restarts the ConnectRetryTimer (with initial value),

        ;; initiates a TCP connection to the other BGP peer,

        ;; continues to listen for a TCP connection that may be initiated by a remote BGP peer, and

        ;; changes its state to Connect.
	)
       ((OPENSENT OPENCONFIRM ESTABLISHED)
	;; sends the NOTIFICATION with the Error Code Finite State Machine Error,

	;; (ESTABLISHED) deletes all routes associated with this connection,
	
        ;; sets the ConnectRetryTimer to zero,

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	
	;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	)))
    
    (Event-10-HoldTimer-Expires
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED)
	;; (CONNECT ACTIVE) if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),

	;; (OPENSENT OPENCONFIRM ESTABLISHED) sends a NOTIFICATION message with the Error Code Hold Timer Expired,

	;; (ESTABLISHED) deletes all routes associated with this connection,

        ;; sets the ConnectRetryTimer to zero,

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,

        ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	)))
    
    (Event-11-KeepaliveTimer-Expires
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT)
	;; if the ConnectRetryTimer is running, stops and resets the ConnectRetryTimer (sets to zero),

        ;; (CONNECT ACTIVE) if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,

        ;; performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

        ;; changes its state to Idle.

	)
       (OPENCONFIRM
	;; sends a KEEPALIVE message,

        ;; restarts the KeepaliveTimer, and

        ;; remains in the OpenConfirmed state.
	)
       (ESTABLISHED
	;; sends a KEEPALIVE message, and

        ;; restarts its KeepaliveTimer, unless the negotiated HoldTime value is zero.
	)))
    
    (Event-12-DelayOpenTimer-Expires
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE)
	;; (ACTIVE) sets the ConnectRetryTimer to zero,

        ;; (ACTIVE) stops and clears the DelayOpenTimer (set to zero),

        ;; (ACTIVE) completes the BGP initialization,

        ;; sends the OPEN message to its remote peer,

        ;; sets its hold timer to a large value, and

        ;; changes its state to OpenSent.
	)
       ((OPENSENT OPENCONFIRM ESTABLISHED)
	;; sends the NOTIFICATION with the Error Code Finite State Machine Error,

	;; (ESTABLISHED) deletes all routes associated with this connection,

        ;; sets the ConnectRetryTimer to zero,

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	
	;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	)
      ))
    
    (Event-13-IdleHoldTimer-Expires
     (case FSM-State
       (IDLE 'PREVENT-PEER-OSCILLATIONS) ;; TODO 
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED)
	;; (CONNECT ACTIVE) if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),
	
	;; (OPENSENT OPENCONFIRM ESTABLISHED) sends the NOTIFICATION with the Error Code Finite State Machine Error,

	;; (ESTABLISHED) deletes all routes associated with this connection,

        ;; sets the ConnectRetryTimer to zero,

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	
	;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	)
      ))

    ;; TCP CONNECTION-BASED EVENTS
    (Event-17-TcpConnectionConfirmed
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE)
	;;If the DelayOpen attribute is set to TRUE, the local system:

	;; stops the ConnectRetryTimer and sets the ConnectRetryTimer to zero,

	;; sets the DelayOpenTimer to the initial value (DelayOpenTime), and

	;; stays in the Connect or Active state.

        ;;If the DelayOpen attribute is set to FALSE, the local system:

	;; sets the ConnectRetryTimer to zero,

	;; completes the BGP initialization,

	;; sends the OPEN message to its peer,

	;; sets its HoldTimer to a large value, and

	;; changes its state to OpenSent.
	)
       (OPENSENT 'CONNECTION-COLLISION-PROCESSING)
       (OPENCONFIRM 'CONNECTION-COLLISION-PROCESSING)
       (ESTABLISHED 'CONNECTION-COLLISION-PROCESSING)))
    
    (Event-18-TcpConnectionFails
     (case FSM-State
       (IDLE 'IGNORED)
       (CONNECT
	;; If the DelayOpenTimer is running, the local system:

        ;; restarts the ConnectRetryTimer with the initial value,

        ;; stops the DelayOpenTimer and resets its value to zero,

        ;; continues to listen for a connection that may be initiated by the remote BGP peer, and

        ;; changes its state to Active.

        ;; If the DelayOpenTimer is not running, the local system:

        ;; stops the ConnectRetryTimer to zero,

        ;; drops the TCP connection,

        ;; releases all BGP resources, and

        ;; changes its state to Idle.
	)
       (ACTIVE
	;; restarts the ConnectRetryTimer (with the initial value),

        ;; stops and clears the DelayOpenTimer (sets the value to zero),

        ;; releases all BGP resource,

        ;; increments the ConnectRetryCounter by 1,

        ;; optionally performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	)
       (OPENSENT
	;; closes the BGP connection,

        ;; restarts the ConnectRetryTimer,

        ;; continues to listen for a connection tht may be initiated by the remote BGP peer, and

        ;; changes its state to Active.
	)
       (OPENCONFIRM
	;; sets the ConnectRetryTimer to zero,

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,

        ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	)
       (ESTABLISHED
	;; sets the ConnectRetryTimer to zero,

        ;; deletes all routes associated with this connection,

        ;; releases all the BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,

        ;; changes its state to Idle.
	)))
    
    (Event-19-BGPOpen
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE)
	;; if the ConnectRetryTimer is running, stops and resets the ConnectRetryTimer (sets to zero),

        ;; if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,

        ;; performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

        ;; changes its state to Idle.
	)
       (OPENSENT
	;; resets the DelayOpenTimer to zero,

        ;; sets the BGP ConnectRetryTimer to zero,

        ;; sends a KEEPALIVE message, and

        ;; sets a KeepaliveTimer (via the text below)

        ;; sets the HoldTimer according to the negotiated value (see Section 4.2),

        ;; changes its state to OpenConfirm.
	;; If the negotiated hold time value is zero, then the HoldTimer and KeepaliveTimer are not started.  If the value of the Autonomous System field is the same as the local Autonomous System number, then the connection is an "internal" connection; otherwise, it is an "external" connection.  (This will impact UPDATE processing as described below.)
	)
       (OPENCONFIRM
	;; If the local system receives a valid OPEN message (BGPOpen (Event 19)), the collision detect function is processed per Section 6.8. If this connection is to be dropped due to connection collision, the local system:

        ;; sends a NOTIFICATION with a Cease,

        ;; sets the ConnectRetryTimer to zero,

        ;; releases all BGP resources,

        ;; drops the TCP connection (send TCP FIN),

        ;; increments the ConnectRetryCounter by 1,

        ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	)
       (ESTABLISHED
	;; sends a NOTIFICATION message with the Error Code Finite State Machine Error,

        ;; deletes all routes associated with this connection,

        ;; sets the ConnectRetryTimer to zero,

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,

        ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	)))
    
    (Event-20-BGPOpen-with-DelayOpenTimer-running
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE)
	;; stops the ConnectRetryTimer (if running) and sets the ConnectRetryTimer to zero,

        ;; completes the BGP initialization,

        ;; stops and clears the DelayOpenTimer (sets the value to zero),

        ;; sends an OPEN message,

        ;; sends a KEEPALIVE message,

        ;; if the HoldTimer initial value is non-zero,

            ;; starts the KeepaliveTimer with the initial value and

            ;; resets the HoldTimer to the negotiated value,

        ;; else, if the HoldTimer initial value is zero,

            ;; resets the KeepaliveTimer and

            ;; resets the HoldTimer value to zero,

        ;; and changes its state to OpenConfirm.

        ;; If the value of the autonomous system field is the same as local Autonomous System number, set the connection status to an internal connection; otherwise it will be "external".
	)
       ((OPENSENT OPENCONFIRM ESTABLISHED)
	;; sends the NOTIFICATION with the Error Code Finite State Machine Error,

	;; (ESTABLISHED) deletes all routes associated with this connection,

        ;; sets the ConnectRetryTimer to zero,

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,
	
	;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	)))
    
    (Event-21-BGPHeaderErr
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED)
	;; (CONNECT ACTIVE) (optionally) If the SendNOTIFICATIONwithoutOPEN attribute is set to TRUE, then the local system first sends a NOTIFICATION message with the appropriate error code, and then
	;; (OPENSENT OPENCONFIRM) sends a NOTIFICATION message with the appropriate error code,

	;; (ESTABLISHED) sends a NOTIFICATION message with the Error Code Finite State Machine Error,

	;; (ESTABLISHED) deletes all routes associated with this connection,

        ;; sets the ConnectRetryTimer to zero,

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,

        ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to TRUE, and

        ;; changes its state to Idle.
	)))
    
    (Event-22-BGPOpenMsgErr
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED)
	;; (CONNECT ACTIVE) (optionally) If the SendNOTIFICATIONwithoutOPEN attribute is set to TRUE, then the local system first sends a NOTIFICATION message with the appropriate error code, and then
	
	;; (OPENSENT OPENCONFIRM) sends a NOTIFICATION message with the appropriate error code,

	;; (ESTABLISHED)  sends a NOTIFICATION message with the Error Code Finite State Machine Error,

	;; (ESTABLISHED) deletes all routes associated with this connection,

	;; sets the ConnectRetryTimer to zero,

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,

        ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is TRUE, and

        ;; changes its state to Idle.
	)))

    (Event-24-NotifMsgVerErr
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE)
	;; If the DelayOpenTimer is running, the local system:

        ;; stops the ConnectRetryTimer (if running) and sets the ConnectRetryTimer to zero,

        ;; stops and resets the DelayOpenTimer (sets to zero),

        ;; releases all BGP resources,

        ;; drops the TCP connection, and

        ;; changes its state to Idle.

        ;; If the DelayOpenTimer is not running, the local system:

        ;; stops the ConnectRetryTimer and sets the ConnectRetryTimer to zero,

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,

        ;; performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

        ;; changes its state to Idle.
	)
       ((OPENSENT OPENCONFIRM ESTABLISHED)
	;; sets the ConnectRetryTimer to zero,

	;; (ESTABLISHED) deletes all routes associated with this connection,

        ;; releases all BGP resources,

        ;; drops the TCP connection, and

	;; (ESTABLISHED) increments the ConnectRetryCounter by 1,

        ;; changes its state to Idle.
	)))
    
    (Event-25-NotifMsg
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED)
	;; (OPENSENT) sends the NOTIFICATION with the Error Code Finite State Machine Error,
	
	;; if the ConnectRetryTimer is running, stops and resets the ConnectRetryTimer (sets to zero),

        ;; if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,

        ;; performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

        ;; changes its state to Idle.
	)))

    (Event-26-KeepAliveMsg
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT)
	;; (OPENSENT) sends the NOTIFICATION with the Error Code Finite State Machine Error,
	
	;; if the ConnectRetryTimer is running, stops and resets the ConnectRetryTimer (sets to zero),

        ;; if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,

        ;; performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

        ;; changes its state to Idle.
	)
       (OPENCONFIRM
	;; restarts the HoldTimer and

        ;; changes its state to Established.
	)
       (ESTABLISHED
	;; restarts its HoldTimer, if the negotiated HoldTime value is non-zero, and

        ;; remains in the Established state.
	)))
    
    (Event-27-UpdateMsg
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM)
	;; (OPENSENT OPENCONFIRM) sends the NOTIFICATION with the Error Code Finite State Machine Error,
	
	;; if the ConnectRetryTimer is running, stops and resets the ConnectRetryTimer (sets to zero),

        ;; if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),

        ;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,

        ;; (optionally) performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

        ;; changes its state to Idle.
	)
       (ESTABLISHED
	;; processes the message,

        ;; restarts its HoldTimer, if the negotiated HoldTime value is non-zero, and

        ;; remains in the Established state.
	)))
    
    (Event-28-UpdateMsgErr
     (case FSM-State
       (IDLE 'IGNORED)
       ((CONNECT ACTIVE OPENSENT OPENCONFIRM ESTABLISHED)
	;; if the ConnectRetryTimer is running, stops and resets the ConnectRetryTimer (sets to zero),

        ;; (CONNECT  ACTIVE) if the DelayOpenTimer is running, stops and resets the DelayOpenTimer (sets to zero),

	;; (OPENSENT OPENCONFIRM ESTABLISHED) sends the NOTIFICATION with the Error Code Finite State Machine Error,

	;; (ESTABLISHED) deletes all routes associated with this connection,

	;; releases all BGP resources,

        ;; drops the TCP connection,

        ;; increments the ConnectRetryCounter by 1,

        ;; performs peer oscillation damping if the DampPeerOscillations attribute is set to True, and

        ;; changes its state to Idle.
	)))))
