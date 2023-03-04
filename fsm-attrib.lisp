(in-package :sbgp)
#|
;;; STATE AND SESSION ATTRIBUTES

STATE  "-> [ IDLE | CONNECT | ACTIVE | OPENSENT | OPENCONFIRM | ESTABLISHED ]"

;;; Mandatory Attributes                                                      
ConnectRetryCounter                "-> integer"                FSM-ATTRIB
ConnectRetryTime                   "-> Time (ticks)"           FSM-TIMERS  
ConnectRetryTimer                  "-> Clock value (ticks)"    FSM-TIMERS
HoldTime                           "-> Time (ticks)"           FSM-TIMERS
HoldTimer                          "-> Clock value (ticks)"    FSM-TIMERS
HoldTime-LargeValue                "-> 4 minutes in ticks"     FSM-TIMERS
KeepaliveTime                      "-> Time (ticks)"           FSM-TIMERS
KeepaliveTimer                     "-> Clock value"            FSM-TIMERS

;;; Optional Attributes                                                       

;; Group 1: Automatic Administrative Events (Start/Stop)                     
AllowAutomaticStart                "-> [ t | nil ]"            FSM-ATTRIB
AllowAutomaticStop                 "-> [ t | nil ]"            FSM-ATTRIB

;; Group 1: IdleHoldTime                                                     
DampPeerOscillations               "-> [ t | nil ]"            FSM-ATTRIB
IdleHoldTime                       "-> Time (ticks)"           FSM-TIMERS    
IdleHoldTimer                      "-> Clock value (ticks)"    FSM-TIMERS

;; Group 2: Unconfigured Peers                                               
AcceptConnectionsUnconfiguredPeers "-> [ t | nil ]"            FSM-ATTRIB

;; Group 3: TCP processing                                                   
PassiveTcpEstablishment            "-> [ t | nil ]"            FSM-ATTRIB  
TrackTcpState                      "-> [ t | nil ]"            FSM-ATTRIB

;; Group 4: BGP Message Processing                                           
;; DelayOpenTimer                                                            
DelayOpen                          "-> [ t | nil ]"            FSM-ATTRIB
DelayOpenTime                      "-> Time (ticks)"           FSM-TIMERS
DelayOpenTimer                     "-> Clock value (ticks)"    FSM-TIMERS
SendNOTIFICATIONwithoutOPEN        "-> [ t | nil ]"            FSM-ATTRIB
CollisionDetectEstablishedState    "-> [ t | nil ]"            FSM-ATTRIB
|#

(defun FSM-ATTRIB-make-default ()
  (list 'FSM-ATTRIB
	0				; ConnectRetryCounter                integer
	nil                             ; AllowAutomaticStart                [ t | nil ]
        nil                             ; AllowAutomaticStop                 [ t | nil ]
	nil                             ; DampPeerOscillations               [ t | nil ]
	nil                             ; AcceptConnectionsUnconfiguredPeers [ t | nil ]
	nil                             ; PassiveTcpEstablishment            [ t | nil ]
        nil                             ; TrackTcpState                      [ t | nil ]
	nil                             ; DelayOpen                          [ t | nil ]
	nil                             ; SendNOTIFICATIONwithoutOPEN        [ t | nil ]
        nil))                           ; CollisionDetectEstablishedState    [ t | nil ]

(defmacro FSM-ATTRIB-get-name (obj)                               "-> symbol"       `(nth 0 ,obj))
(defmacro FSM-ATTRIB-get-ConnectRetryCounter (obj)                "-> integer"      `(nth 1 ,obj))
(defmacro FSM-ATTRIB-get-AllowAutomaticStart (obj)                "-> [ t | nil ]"  `(nth 2 ,obj))  
(defmacro FSM-ATTRIB-get-AllowAutomaticStop (obj)                 "-> [ t | nil ]"  `(nth 3 ,obj))  
(defmacro FSM-ATTRIB-get-DampPeerOscillations  (obj)              "-> [ t | nil ]"  `(nth 4 ,obj)) 
(defmacro FSM-ATTRIB-get-AcceptConnectionsUnconfiguredPeers (obj) "-> [ t | nil ]"  `(nth 5 ,obj))
(defmacro FSM-ATTRIB-get-PassiveTcpEstablishment (obj)            "-> [ t | nil ]"  `(nth 6 ,obj)) 
(defmacro FSM-ATTRIB-get-TrackTcpState (obj)                      "-> [ t | nil ]"  `(nth 7 ,obj)) 
(defmacro FSM-ATTRIB-get-DelayOpen (obj)                          "-> [ t | nil ]"  `(nth 8 ,obj)) 
(defmacro FSM-ATTRIB-get-SendNOTIFICATIONwithoutOPEN (obj)        "-> [ t | nil ]"  `(nth 9 ,obj)) 
(defmacro FSM-ATTRIB-get-CollisionDetectEstablishedState (obj)    "-> [ t | nil ]"  `(nth 10 ,obj)) 

