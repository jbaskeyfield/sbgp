(in-package :sbgp)

;;; TIMERS
;; default values from RFC4271 p90 10.  BGP Timers

(defmacro FSM-TIMERS-get-name (obj)                  "-> symbol"               `(svref ,obj 0))
(defmacro FSM-TIMERS-get-ConnectRetryTimer (obj)     "-> Clock value (ticks)"  `(svref ,obj 1))
(defmacro FSM-TIMERS-get-ConnectRetryTime (obj)      "-> Time (ticks)"         `(svref ,obj 2))
(defmacro FSM-TIMERS-get-HoldTimer (obj)             "-> Clock value (ticks)"  `(svref ,obj 3))
(defmacro FSM-TIMERS-get-HoldTime (obj)              "-> Time (ticks)"         `(svref ,obj 4))                                                      
(defmacro FSM-TIMERS-get-KeepaliveTimer (obj)        "-> Clock value (ticks)"  `(svref ,obj 5))
(defmacro FSM-TIMERS-get-KeepaliveTime (obj)         "-> Time (ticks)"         `(svref ,obj 6))
(defmacro FSM-TIMERS-get-DelayOpenTimer (obj)        "-> Clock value (ticks)"  `(svref ,obj 7))
(defmacro FSM-TIMERS-get-DelayOpenTime (obj)         "-> Time (ticks)"         `(svref ,obj 8))
(defmacro FSM-TIMERS-get-IdleHoldTimer (obj)         "-> Clock value (ticks)"  `(svref ,obj 9))
(defmacro FSM-TIMERS-get-IdleHoldTime (obj)          "-> Time (ticks)"         `(svref ,obj 10))
(defmacro FSM-TIMERS-get-HoldTime-LargeValue (obj)   "-> Time (ticks)"         `(svref ,obj 11))
(defmacro FSM-TIMERS-get-LastPollingTime (obj)       "-> Clock value (ticks)"  `(svref ,obj 12))

(defun FSM-TIMERS-make-default ()
  (let ((fsm-timers (make-array 13 :initial-element 0)))
    (setf (FSM-TIMERS-get-name fsm-timers) 'FSM-TIMERS)
    (setf (FSM-TIMERS-get-ConnectRetryTime fsm-timers) (* 120 internal-time-units-per-second))
    (setf (FSM-TIMERS-get-HoldTime fsm-timers) (* 90 internal-time-units-per-second))
    (setf (FSM-TIMERS-get-KeepaliveTime fsm-timers) (* 30 internal-time-units-per-second)) 
    (setf (FSM-TIMERS-get-DelayOpenTime fsm-timers) (* 30 internal-time-units-per-second))
    (setf (FSM-TIMERS-get-IdleHoldTime fsm-timers) (* 90 internal-time-units-per-second))
    (setf (FSM-TIMERS-get-HoldTime-LargeValue fsm-timers) (* 240 internal-time-units-per-second))
    fsm-timers))  

(defun FSM-TIMERS-ConnectRetryTimer-is-running-p (obj)
  (not (= 0 (FSM-TIMERS-get-ConnectRetryTimer obj))))

(defun FSM-TIMERS-HoldTimer-is-running-p (obj)
  (not (= 0 (FSM-TIMERS-get-HoldTimer obj))))

(defun FSM-TIMERS-KeepaliveTimer-is-running-p (obj)
  (not (= 0 (FSM-TIMERS-get-KeepaliveTimer obj))))

(defun FSM-TIMERS-DelayOpenTimer-is-running-p (obj)
  (not (= 0 (FSM-TIMERS-get-DelayOpenTimer obj))))

(defun FSM-TIMERS-IdleHoldTimer-is-running-p (obj)
  (not (= 0 (FSM-TIMERS-get-IdleHoldTimer obj))))

(defun FSM-TIMERS-start-ConnectRetryTimer (obj thread-name)
  (unless (= (FSM-TIMERS-get-ConnectRetryTime obj) 0)
    (setf (FSM-TIMERS-get-ConnectRetryTimer obj)
	  (+ (FSM-TIMERS-get-ConnectRetryTime obj) (get-internal-real-time)))
     (when *debug-fsm-timers*
      (format *debug-fsm-timers* "~&~S FSM-TIMERS Started ConnectRetryTimer~%" thread-name))))

(defun FSM-TIMERS-stop-ConnectRetryTimer (obj thread-name)
  (setf (FSM-TIMERS-get-ConnectRetryTimer obj) 0)
  (when *debug-fsm-timers*
    (format *debug-fsm-timers* "~&~S FSM-TIMERS Stopped ConnectRetryTimer~%" thread-name)))

(defun FSM-TIMERS-start-HoldTimer (obj thread-name)
  (unless (= (FSM-TIMERS-get-HoldTime obj) 0)
    (setf (FSM-TIMERS-get-HoldTimer obj) (+ (FSM-TIMERS-get-HoldTime obj)
					    (get-internal-real-time)))
    (when *debug-fsm-timers*
      (format *debug-fsm-timers* "~&~S FSM-TIMERS Started HoldTimer~%" thread-name))))

(defun FSM-TIMERS-start-HoldTimer-LargeValue (obj thread-name)
  (unless (= (FSM-TIMERS-get-HoldTime-LargeValue obj) 0)
    (setf (FSM-TIMERS-get-HoldTimer obj)
	  (+ (FSM-TIMERS-get-HoldTime-LargeValue obj) (get-internal-real-time)))
    (when *debug-fsm-timers*
      (format *debug-fsm-timers* "~&~S FSM-TIMERS Started HoldTimer (LargeValue)~%" thread-name))))

(defun FSM-TIMERS-stop-HoldTimer (obj thread-name)
  (setf (FSM-TIMERS-get-HoldTimer obj) 0)
  (when *debug-fsm-timers*
      (format *debug-fsm-timers* "~&~S FSM-TIMERS Stopped HoldTimer~%" thread-name)))

;; Each time the local system sends a KEEPALIVE or UPDATE message, it restarts its KeepaliveTimer, unless the negotiated HoldTime value is zero.
(defun FSM-TIMERS-start-KeepaliveTimer (obj thread-name)
  (unless (or (= (FSM-TIMERS-get-HoldTime obj) 0)
	      (= (FSM-TIMERS-get-KeepaliveTime obj) 0))
    (setf (FSM-TIMERS-get-KeepaliveTimer obj)
	  (+ (FSM-TIMERS-get-KeepaliveTime obj) (get-internal-real-time)))
    (when *debug-fsm-timers*
      (format *debug-fsm-timers* "~&~S FSM-TIMERS Started KeepaliveTimer~%" thread-name))))

(defun FSM-TIMERS-stop-KeepaliveTimer (obj thread-name)
  (setf (FSM-TIMERS-get-KeepaliveTimer obj) 0)
  (when *debug-fsm-timers*
      (format *debug-fsm-timers* "~&~S FSM-TIMERS Stopped KeepaliveTimer~%" thread-name)))

(defun FSM-TIMERS-start-DelayOpenTimer (obj thread-name)
  (unless (= (FSM-TIMERS-get-DelayOpenTime obj) 0)
    (setf (FSM-TIMERS-get-DelayOpenTimer obj)
	  (+ (FSM-TIMERS-get-DelayOpenTime obj) (get-internal-real-time)))
    (when *debug-fsm-timers*
      (format *debug-fsm-timers* "~&~S FSM-TIMERS Started DelayOpenTimer~%" thread-name))))

(defun FSM-TIMERS-stop-DelayOpenTimer (obj thread-name)
  (setf (FSM-TIMERS-get-DelayOpenTimer obj) 0)
  (when *debug-fsm-timers*
      (format *debug-fsm-timers* "~&~S FSM-TIMERS Stopped DelayOpenTimer~%" thread-name)))

(defun FSM-TIMERS-start-IdleHoldTimer (obj thread-name)
  (unless (= (FSM-TIMERS-get-IdleHoldTime obj) 0)
    (setf (FSM-TIMERS-get-IdleHoldTimer obj)
	  (+ (FSM-TIMERS-get-IdleHoldTime obj) (get-internal-real-time)))
    (when *debug-fsm-timers*
      (format *debug-fsm-timers* "~&~S FSM-TIMERS Started IdleHoldTimer~%" thread-name))))

(defun FSM-TIMERS-stop-IdleHoldTimer (obj thread-name)
  (setf (FSM-TIMERS-get-IdleHoldTimer obj) 0)
  (when *debug-fsm-timers*
      (format *debug-fsm-timers* "~&~S FSM-TIMERS Stopped IdleHoldTimer~%" thread-name)))

(defun FSM-TIMERS-poll (fsm-timers timers-queue thread-name)
  (let ((time-now (get-internal-real-time)))
    
    (when (and (FSM-TIMERS-ConnectRetryTimer-is-running-p fsm-timers)
	       (< (FSM-TIMERS-get-ConnectRetryTimer fsm-timers) time-now))
      (when *debug-fsm-timers*
	(format *debug-fsm-timers* "~&~S FSM-TIMERS Event-9-ConnectRetryTimer-Expires~%" thread-name))
      (FSM-TIMERS-stop-ConnectRetryTimer fsm-timers thread-name)
      (QUEUE-send timers-queue (MSG-make 'Event-9-ConnectRetryTimer-Expires)))
    
    (when (and (FSM-TIMERS-HoldTimer-is-running-p fsm-timers)
	       (< (FSM-TIMERS-get-HoldTimer fsm-timers) time-now))
      (when *debug-fsm-timers*
	(format *debug-fsm-timers* "~&~S FSM-TIMERS Event-10-HoldTimer-Expires~%" thread-name))
      (FSM-TIMERS-stop-HoldTimer fsm-timers thread-name)
      (QUEUE-send timers-queue (MSG-make 'Event-10-HoldTimer-Expires)))
    
    (when (and (FSM-TIMERS-KeepaliveTimer-is-running-p fsm-timers)
	       (< (FSM-TIMERS-get-KeepaliveTimer fsm-timers) time-now))
      (when *debug-fsm-timers*
	(format *debug-fsm-timers* "~&~S FSM-TIMERS Event-11-KeepaliveTimer-Expires~%" thread-name))
      (FSM-TIMERS-stop-KeepaliveTimer fsm-timers thread-name)
      (QUEUE-send timers-queue (MSG-make 'Event-11-KeepaliveTimer-Expires)))
    
    (when (and (FSM-TIMERS-DelayOpenTimer-is-running-p fsm-timers)
	       (< (FSM-TIMERS-get-DelayOpenTimer fsm-timers) time-now))
      (when *debug-fsm-timers*
	(format *debug-fsm-timers* "~&~S FSM-TIMERS Event-12-DelayOpenTimer-Expires~%" thread-name))
      (FSM-TIMERS-stop-DelayOpenTimer fsm-timers thread-name)
      (QUEUE-send timers-queue (MSG-make 'Event-12-DelayOpenTimer-Expires)))
    
    (when (and (FSM-TIMERS-IdleHoldTimer-is-running-p fsm-timers)
	       (< (FSM-TIMERS-get-IdleHoldTimer fsm-timers) time-now))
      (when *debug-fsm-timers*
	(format *debug-fsm-timers* "~&~S FSM-TIMERS Event-13-IdleHoldTimer-Expires~%" thread-name))
      (FSM-TIMERS-stop-IdleHoldTimer fsm-timers thread-name)
      (QUEUE-send timers-queue (MSG-make 'Event-13-IdleHoldTimer-Expires)))))

