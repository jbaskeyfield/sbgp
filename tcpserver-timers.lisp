(in-package :sbgp)

(defmacro TCPSERVER-TIMERS-get-tcp-scan-Timer (obj) "-> Clock value (ticks)"  `(svref ,obj 1))
(defmacro TCPSERVER-TIMERS-get-tcp-scan-Time (obj)  "-> Time (ticks)"         `(svref ,obj 2))

(defun TCPSERVER-TIMERS-make-default ()
  (let ((tcpserver-timers (make-array 3 :initial-element 0)))
    (setf (TV-get-name tcpserver-timers) 'TCPSERVER-TIMERS)
    (setf (TCPSERVER-TIMERS-get-tcp-scan-Time tcpserver-timers) (* 10 internal-time-units-per-second))
    tcpserver-timers))

(defun TCPSERVER-TIMERS-tcp-scan-Timer-is-running-p (obj)
  (not (= 0 (TCPSERVER-TIMERS-get-tcp-scan-Timer obj))))

(defun TCPSERVER-TIMERS-start-tcp-scan-Timer (obj thread-name)
  (unless (= (TCPSERVER-TIMERS-get-tcp-scan-Time obj) 0)
    (setf (TCPSERVER-TIMERS-get-tcp-scan-Timer obj)
	  (+ (TCPSERVER-TIMERS-get-tcp-scan-Time obj) (get-internal-real-time)))
    (when *debug-tcpserver-timers*
      (format *debug-tcpserver-timers* "~&~S TCPSERVER-TIMERS Started tcp-scan-Timer~%" thread-name))))

(defun TCPSERVER-TIMERS-stop-tcp-scan-Timer (obj thread-name)
  (setf (TCPSERVER-TIMERS-get-tcp-scan-Timer obj) 0)
  (when *debug-tcpserver-timers*
      (format *debug-tcpserver-timers* "~&~S TCPSERVER-TIMERS Stopped tcp-scan-Timer~%" thread-name)))

(defun TCPSERVER-TIMERS-poll (tcpserver-timers timers-queue thread-name)
  (let ((time-now (get-internal-real-time)))
    (when (and (TCPSERVER-TIMERS-tcp-scan-Timer-is-running-p tcpserver-timers)
	       (< (TCPSERVER-TIMERS-get-tcp-scan-Timer tcpserver-timers) time-now))
      (when *debug-tcpserver-timers*
	(format *debug-tcpserver-timers* "~&~S TCPSERVER-TIMERS TCPSERVER-TIMERS-tcp-scan-Timer-Expires~%" thread-name))
      (TCPSERVER-TIMERS-stop-tcp-scan-Timer tcpserver-timers thread-name)
      (QUEUE-send timers-queue (MSG-make 'TCPSERVER-TIMERS-tcp-scan-Timer-Expires)))))

