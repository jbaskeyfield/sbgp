(defun make-interval-timer (interval-seconds random-variance-percent)
  "Returns closure that functions as a simple timer that can be polled. 
Example: (make-interval-timer 10 20), creates a timer with interval of 10 seconds (plus/minus 1 second). 
Returned function can be called with one optional argument.
NIL or no argument => Polls the timer. Returns NIL if timer is stopped or has not expired. Returns non-NIL value (the new timer value after reset), if the timer has expired.
:start             => Starts the timer. When initially created the timer will be stopped (zero), but once running will restart every time it is polled and returns a non-NIL value.
:stop              => Sets timer to zero.
:status            => Returns T if timer is running, or NIL if stopped. Also, additional values of internal state 'interval-ticks, variance-ticks, timer, & skew-ticks' 
:debug-status      => Returns string suitable for writing to log file"
  
  (let* ((interval-ticks (ceiling (* interval-seconds
				     internal-time-units-per-second)))
	 (variance-ticks (ceiling (/ (* interval-ticks
					random-variance-percent)
				     100)))
	 (timer 0)
	 (skew-ticks 0))
    
     #'(lambda (&optional arg)
	(case arg
	  (:start
	   (setf skew-ticks (if (<= variance-ticks 0)
				0
				(ceiling (- (random variance-ticks)
					    (/ variance-ticks 2)))))
	   (setf timer (+ (get-internal-real-time)
			  interval-ticks
			  skew-ticks))
	   (values timer skew-ticks))
	  
	  (:stop
	   (setf skew-ticks 0)
	   (setf timer 0)
	   (values timer skew-ticks))
	  
	  (:status
	   (values (not (= timer 0)) interval-ticks variance-ticks timer skew-ticks))
	  
	  (:debug-status
	   (let ((time-now (get-internal-real-time)))
	     (format nil "~&~S, timer: ~S, time-now: ~S, delta: ~S s, interval: ~S s, variance: ~S s, skew: ~S s"
		     (if (= timer 0)
			 'STOPPED
			 'RUNNING)
		     timer
		     time-now
		     (if (= timer 0)
			 0
			 (float (/ (- timer time-now) internal-time-units-per-second)))
		     (float (/ interval-ticks internal-time-units-per-second))
		     (float (/ variance-ticks internal-time-units-per-second))
		     (float (/ skew-ticks internal-time-units-per-second)))))
	  (t
	   (let ((time-now (get-internal-real-time)))
	     (cond ((= timer 0)                  ; if timer is not running (zero), return nil
		    nil)
	           ((< time-now timer)           ; if timer has not expired, return nil
		    nil)                         
		   (t                            ; otherwise, reset timer with new increment and return non nil value (new timer value)
		    (setf skew-ticks  (if (<= variance-ticks 0)
					  0
					  (ceiling (- (random variance-ticks)
						      (/ variance-ticks 2)))))
		    (setf timer (+ time-now
				   interval-ticks
				   skew-ticks))
		    timer))))))))



(defun make-loop-idle-timer (&key (min-sleep-seconds 0.001) (max-sleep-seconds 0.2) (delay-multiplier 1.05))
  "Returns number of seconds slept"
  (let (;;(last-call-real-time (get-internal-real-time))
	(current-idle-delay min-sleep-seconds))
    #'(lambda (loop-message-count)
	;;(let ((time-now (get-internal-real-time)))

	  (cond ((> loop-message-count 0)                      
		 (setf current-idle-delay min-sleep-seconds)  
		 ;; (setf last-call-real-time time-now)
		 0)
		(t
		 ;; TODO: two pieces of information available for tuning updates to current-idle-delay:
		 ;; 1. number of messages processed in this loop pass 'loop-message-count' and,
		 ;; 2. amount of time spent processing messages since last call (- time-now last-call-real-time)
		 ;; Currently just reset idle delay to minimum if we have processed a message - queues serviced in round robin fashion
		 ;; so can assume there are more packets to process if we have just processed some. Every-time we call this function
		 ;; after not processing any packets, increase 'current-idle-delay' by multiplying by 'delay-multiplier'
		 ;; up until we get to 'max-sleep-seconds'
		 ;; Could it be this is good enough if select right min/max/multiplier values?
		 ;; if so, can get rid of 'last-call-real-time' and 'time-now' variables
		 (setf current-idle-delay (min max-sleep-seconds
					       (* current-idle-delay
						  delay-multiplier)))
		 ;; (setf last-call-real-time time-now)
		 (sleep current-idle-delay)
		 (format t ".")
		 current-idle-delay)))));)
