(in-package :sbgp)

(defmacro PEER-TIMERS-get-rib-adj-scan-Timer (obj) "-> Clock value (ticks)"  `(svref ,obj 1))
(defmacro PEER-TIMERS-get-rib-adj-scan-Time (obj)  "-> Time (ticks)"         `(svref ,obj 2))

(defun PEER-TIMERS-make-default ()
  (let ((peer-timers (make-array 3 :initial-element 0)))
    (setf (TV-get-name peer-timers) 'PEER-TIMERS)
    (setf (PEER-TIMERS-get-rib-adj-scan-Time peer-timers) (* 10 internal-time-units-per-second))
    peer-timers))

(defun PEER-TIMERS-rib-adj-scan-Timer-is-running-p (obj)
  (not (= 0 (PEER-TIMERS-get-rib-adj-scan-Timer obj))))

(defun PEER-TIMERS-start-rib-adj-scan-Timer (obj thread-name)
  (unless (= (PEER-TIMERS-get-rib-adj-scan-Time obj) 0)
    (setf (PEER-TIMERS-get-rib-adj-scan-Timer obj)
	  (+ (PEER-TIMERS-get-rib-adj-scan-Time obj) (get-internal-real-time)))
    (when *debug-peer-timers*
      (format *debug-peer-timers* "~&~S PEER-TIMERS Started rib-adj-scan-Timer~%" thread-name))))

(defun PEER-TIMERS-stop-rib-adj-scan-Timer (obj thread-name)
  (setf (PEER-TIMERS-get-rib-adj-scan-Timer obj) 0)
  (when *debug-peer-timers*
      (format *debug-peer-timers* "~&~S PEER-TIMERS Stopped rib-adj-scan-Timer~%" thread-name)))

(defun PEER-TIMERS-poll (peer-timers timers-queue thread-name)
  (let ((time-now (get-internal-real-time)))
    (when (and (PEER-TIMERS-rib-adj-scan-Timer-is-running-p peer-timers)
	       (< (PEER-TIMERS-get-rib-adj-scan-Timer peer-timers) time-now))
      (when *debug-peer-timers*
	(format *debug-peer-timers* "~&~S PEER-TIMERS PEER-TIMERS-rib-adj-scan-Timer-Expires~%" thread-name))
      (PEER-TIMERS-stop-rib-adj-scan-Timer peer-timers thread-name)
      (QUEUE-send timers-queue (MSG-make 'PEER-TIMERS-rib-adj-scan-Timer-Expires)))))

