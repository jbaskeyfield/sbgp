(in-package :sbgp)

(defmacro ROUTER-TIMERS-get-rib-loc-scan-Timer (obj) "-> Clock value (ticks)"  `(svref ,obj 1))
(defmacro ROUTER-TIMERS-get-rib-loc-scan-Time (obj)  "-> Time (ticks)"         `(svref ,obj 2))

(defun ROUTER-TIMERS-make-default ()
  (let ((router-timers (make-array 3 :initial-element 0)))
    (setf (TV-get-name router-timers) 'ROUTER-TIMERS)
    (setf (ROUTER-TIMERS-get-rib-loc-scan-Time router-timers) (* 10 internal-time-units-per-second))
    router-timers))

(defun ROUTER-TIMERS-rib-loc-scan-Timer-is-running-p (obj)
  (not (= 0 (ROUTER-TIMERS-get-rib-loc-scan-Timer obj))))

(defun ROUTER-TIMERS-start-rib-loc-scan-Timer (obj thread-name)
  (unless (= (ROUTER-TIMERS-get-rib-loc-scan-Time obj) 0)
    (setf (ROUTER-TIMERS-get-rib-loc-scan-Timer obj)
	  (+ (ROUTER-TIMERS-get-rib-loc-scan-Time obj) (get-internal-real-time)))
    (when *debug-router-timers*
      (format *debug-router-timers* "~&~S ROUTER-TIMERS Started rib-loc-scan-Timer~%" thread-name))))

(defun ROUTER-TIMERS-stop-rib-loc-scan-Timer (obj thread-name)
  (setf (ROUTER-TIMERS-get-rib-loc-scan-Timer obj) 0)
  (when *debug-router-timers*
      (format *debug-router-timers* "~&~S ROUTER-TIMERS Stopped rib-loc-scan-Timer~%" thread-name)))

(defun ROUTER-TIMERS-poll (router-timers timers-queue thread-name)
  (let ((time-now (get-internal-real-time)))
    (when (and (ROUTER-TIMERS-rib-loc-scan-Timer-is-running-p router-timers)
	       (< (ROUTER-TIMERS-get-rib-loc-scan-Timer router-timers) time-now))
      (when *debug-router-timers*
	(format *debug-router-timers* "~&~S ROUTER-TIMERS ROUTER-TIMERS-rib-loc-scan-Timer-Expires~%" thread-name))
      (ROUTER-TIMERS-stop-rib-loc-scan-Timer router-timers thread-name)
      (QUEUE-send timers-queue (MSG-make 'ROUTER-TIMERS-rib-loc-scan-Timer-Expires)))))

