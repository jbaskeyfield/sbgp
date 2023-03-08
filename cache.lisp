(in-package :sbgp)

(defmacro CACHE-get-name (cache)                    `(svref ,cache 0))
(defmacro CACHE-get-num-bits (cache)                `(svref ,cache 1))
(defmacro CACHE-get-table-size (cache)              `(svref ,cache 2))
(defmacro CACHE-get-empty-slot-count (cache)        `(svref ,cache 3))
(defmacro CACHE-get-table-mask (cache)              `(svref ,cache 4))
(defmacro CACHE-get-lookup-counter (cache)          `(svref ,cache 5))
(defmacro CACHE-get-hit-counter (cache)             `(svref ,cache 6))
(defmacro CACHE-get-miss-counter (cache)            `(svref ,cache 7))
(defmacro CACHE-get-slot-collision-counter (cache)  `(svref ,cache 8))
(defmacro CACHE-get-key-collision-counter (cache)   `(svref ,cache 9))
(defmacro CACHE-get-caching-state (cache)           `(svref ,cache 10))
(defmacro CACHE-get-cache-table (cache)             `(svref ,cache 11))

(defun CACHE-make (table-size-num-bits)
  (let ((cache (make-array 12 :initial-element nil))
	(table-size (ash 1 table-size-num-bits)))
    (setf (CACHE-get-name cache)                    'CACHE)                 
    (setf (CACHE-get-num-bits cache)                table-size-num-bits)     
    (setf (CACHE-get-table-size cache)              table-size)     
    (setf (CACHE-get-empty-slot-count cache)        table-size)     
    (setf (CACHE-get-table-mask cache)              (1- table-size))     
    (setf (CACHE-get-lookup-counter cache)          0)     
    (setf (CACHE-get-hit-counter cache)             0)     
    (setf (CACHE-get-miss-counter cache)            0)     
    (setf (CACHE-get-slot-collision-counter cache)  0) 
    (setf (CACHE-get-key-collision-counter cache)   0)  
    (setf (CACHE-get-caching-state cache)           t)
    (setf (CACHE-get-cache-table cache)             (make-array table-size :initial-element nil))

    cache))

(defun CACHE-valid1-p (cache)
  (and (eq 'CACHE (svref cache 0))))


;;(deftype CACHE () '(and (vector * 12) (satisfies CACHE-valid1-p)))
;;(defun CACHE-typep (obj) (typep obj 'CACHE))

(defun CACHE-switch-on (cache)
  (setf (CACHE-get-caching-state cache) t))

(defun CACHE-switch-off (cache)
  (setf (CACHE-get-caching-state cache) nil))

(defun CACHE-clear (cache)
  (setf (CACHE-get-empty-slot-count cache)        (CACHE-get-table-size cache))
  (setf (CACHE-get-lookup-counter cache)          0)     
  (setf (CACHE-get-hit-counter cache)             0)     
  (setf (CACHE-get-miss-counter cache)            0)     
  (setf (CACHE-get-slot-collision-counter cache)  0) 
  (setf (CACHE-get-key-collision-counter cache)   0)
  (fill (CACHE-get-cache-table cache)             nil))

(defun CACHE-lookup (cache obj obj-hash)
  "Function is passed object OBJ to be cached. Returns an EQUAL copy of the object, if a copy is found in the cache table CACHE. Acts as identity function if the passed object is not found in the cache table.
Second value returned indicates the result of lookup:
:hit => copy found in table. EQUAL object returned.
:miss => object saved to empty cache table slot. EQ object returned.
:key-collision => object saved to occupied cache table (previous object in slot hashes to the same 56-bit value). EQ object returned.
:slot-collision => object saved to occupied cache table slot (previous object in slot hashed to a different 56-bit value). EQ object returned.
:cache-off => caching-state flag is nil. EQ object returned."

  (cond ((CACHE-get-caching-state cache)
	 (incf (CACHE-get-lookup-counter cache))                            ; increment lookup counter
	 (let* ((cache-table-index (logand obj-hash                         ; get index to hash table by masking. returns 'hash mod table-mask'
					    (CACHE-get-table-mask cache))) 
		(table-slot-value (svref (CACHE-get-cache-table cache)      ; table lookup to see if slot already occupied
					 cache-table-index)))
	   (cond ((null table-slot-value)
		  ;; cache table slot is empty => MISS
		  ;; increment cache-miss-counter, save (obj-hash . obj) in table slot,
		  ;; decrement empty-slot-count, and return values obj, :miss
		  (incf (CACHE-get-miss-counter cache))
		  (setf (svref (CACHE-get-cache-table cache)
			       cache-table-index)
			(cons obj-hash obj))
		  (decf (CACHE-get-empty-slot-count cache))
		  (values obj :miss))
		 (t
		  ;; cache table slot is already filled => HIT, slot-collision or key-collision
		  (let ((slot-hash (car table-slot-value))
			(slot-value (cdr table-slot-value)))
		    (cond ((= obj-hash slot-hash)
			   ;; HIT or SLOT-COLLISION
			   (cond ((equal obj slot-value)
				  ;; if 'obj' equal 'slot-value' this is a cache HIT:
				  ;; increment 'cache-hit-counter', and return values slot-value :hit
				  (incf (CACHE-get-hit-counter cache))
				  (values slot-value :hit))
				 (t
				  ;; otherwise this is a KEY-COLLISION: (different objects that hash to same 56-bit values)
				  ;; increment 'cache-miss-counter' & 'key-collision-counter',
				  ;; save/clobber (obj-hash . obj) into the slot, and return values obj :key-collision
;;	(format *slot-collisions* "SLOT-COLLISION OBJ: ~S~% SLOT-VALUE: ~S~%" obj slot-value)
				  (incf (CACHE-get-miss-counter cache))
				  (incf (CACHE-get-key-collision-counter cache))
				  (setf (svref (CACHE-get-cache-table cache) cache-table-index)
					(cons obj-hash obj))
				  (values obj :key-collision))))
			  (t
			   ;; otherwise this is a SLOT-COLLISION: (different objects with different 56-bit hash, but lower masked bits are the same)
			   ;; increment 'cache-misses' & 'slot-collisions',
			   ;; save/clobber (obj-hash. obj) in the slot, and return values 'obj', :slot-collision
			   (incf (CACHE-get-miss-counter cache))
			   (incf (CACHE-get-slot-collision-counter cache))
			   (setf (svref (CACHE-get-cache-table cache) cache-table-index)
				 (cons obj-hash obj))
			   (values obj :slot-collsion))))))))
	(t
	 (values obj :cache-off))))



(defun CACHE-pprint-2 (port cache &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (let* ((hits-percent (float (if (> (CACHE-get-lookup-counter cache) 0)
				  (* 100 (/ (CACHE-get-hit-counter cache)
					    (CACHE-get-lookup-counter cache)))
				  0)))
	 (misses-percent (float (if (> (CACHE-get-lookup-counter cache) 0)
				    (* 100 (/ (CACHE-get-miss-counter cache)
					      (CACHE-get-lookup-counter cache)))
				    0)))
	 (slot-collisions-percent (float (if (> (CACHE-get-lookup-counter cache) 0)
					     (* 100 (/ (CACHE-get-slot-collision-counter cache)
						       (CACHE-get-lookup-counter cache)))
					     0)))
	 (key-collisions-percent (float (if (> (CACHE-get-lookup-counter cache) 0)
					    (* 100 (/ (CACHE-get-key-collision-counter cache)
						      (CACHE-get-lookup-counter cache)))
					    0)))
	 
	 (table-fill-percent (float (if (> (CACHE-get-table-size cache) 0)
					(* 100 (/ (- (CACHE-get-table-size cache)
						     (CACHE-get-empty-slot-count cache))
						  (CACHE-get-table-size cache)))
					0))))
    (format port
	    "[CACHE~% ~
	     State: ~A~% ~
	     Table Size: ~A~% ~
	     Table fill percent: ~A %~% ~
	     Table Mask: #x~X~% ~
	     Lookup Count: ~A~% ~
	     Cache Hits: ~A (~A %)~% ~
	     Cache Misses: ~A (~A %)~% ~
	     Slot Collisions: ~A (~A %)~% ~
	     Key Collisions: ~A (~A %)~%"

	    (if (CACHE-get-caching-state cache) "On" "Off")
	    (CACHE-get-table-size cache) 
	    table-fill-percent
	    (CACHE-get-table-mask cache) 
	    (CACHE-get-lookup-counter cache)
	    (CACHE-get-hit-counter cache) hits-percent
	    (CACHE-get-miss-counter cache) misses-percent
	    (CACHE-get-slot-collision-counter cache) slot-collisions-percent
	    (CACHE-get-key-collision-counter cache) key-collisions-percent)))

;;
;;(set-pprint-dispatch 'CACHE #'CACHE-pprint-2  0 *sbgp-pprint-dispatch-table-2*)
;;(setf (get 'CACHE 'pprint-2) #'CACHE-pprint-2)
