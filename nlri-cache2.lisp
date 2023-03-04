(in-package :sbgp)
;; NLRI-CACHE function that depend on NLRI-zhash
(defun NLRI-CACHE-write-to-file (nlri-cache)
  (let ((*print-pretty* nil)
	(*print-base* 10))
    (with-open-file (stream-out #p"nlri-cache-entries.txt" :direction :output :if-exists :supersede)
      (loop for slot across (NLRI-CACHE-get-cache-table nlri-cache)
	    do (loop for nlri in slot
		     do (format stream-out "~&~S~%" nlri))))))

(defun NLRI-CACHE-read-from-file (nlri-cache)
  (let ((stream-in (open #p"nlri-cache-entries.txt")))
    (handler-case
	(loop do (let ((new-nlri (read stream-in)))
		   (NLRI-CACHE-rw-lookup nlri-cache
				         new-nlri 
				         (NLRI-zhash 0 new-nlri))))
      (end-of-file ()
	(close stream-in)))))
