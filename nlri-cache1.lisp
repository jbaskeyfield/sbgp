(in-package :sbgp)


;;; NLRI-CACHE
;; each slot contains list of NLRIs
;; writable by router thread only. peer threads have read-only access

(defmacro NLRI-CACHE-get-name (nlri-cache)             "-> symbol"   `(svref ,nlri-cache 0))
(defmacro NLRI-CACHE-get-num-bits (nlri-cache)         "-> integer"  `(svref ,nlri-cache 1))
(defmacro NLRI-CACHE-get-table-size (nlri-cache)       "-> integer"  `(svref ,nlri-cache 2))
(defmacro NLRI-CACHE-get-empty-slot-count (nlri-cache) "-> integer"  `(svref ,nlri-cache 3))
(defmacro NLRI-CACHE-get-entry-count (nlri-cache)      "-> integer"  `(svref ,nlri-cache 4))
(defmacro NLRI-CACHE-get-table-mask (nlri-cache)       "-> u56"      `(svref ,nlri-cache 5))
(defmacro NLRI-CACHE-get-cache-table (nlri-cache) "-> vector"   `(svref ,nlri-cache 6))

(defun NLRI-CACHE-make (table-size-num-bits)
  (let ((nlri-cache (make-array 7 :initial-element nil))
	(table-size (ash 1 table-size-num-bits)))

    (setf (NLRI-CACHE-get-name nlri-cache)                    'NLRI-CACHE)                 
    (setf (NLRI-CACHE-get-num-bits nlri-cache)                table-size-num-bits)     
    (setf (NLRI-CACHE-get-table-size nlri-cache)              table-size)     
    (setf (NLRI-CACHE-get-empty-slot-count nlri-cache)        table-size)
    (setf (NLRI-CACHE-get-entry-count nlri-cache)             0)
    (setf (NLRI-CACHE-get-table-mask nlri-cache)              (1- table-size))     
    (setf (NLRI-CACHE-get-cache-table nlri-cache)        (make-array table-size :initial-element nil))
    nlri-cache))

(defun NLRI-CACHE-valid1-p (nlri-cache)
  (and (= (length nlri-cache) 7)))

(deftype NLRI-CACHE () '(and (cons (member NLRI-CACHE)) (satisfies NLRI-CACHE-valid1-p)))

(defun NLRI-CACHE-rw-lookup (cache nlri nlri-hash)
  "Table owner's READ/WRITE function. To be called by ROUTER thread only.
Function is passed object NLRI to be cached. Returns an EQUAL copy of the object, if a copy is found in the cache table CACHE. Acts as identity function if the passed object is not found in the cache table.
Second value returned indicates the result of lookup:
:hit => copy found in table. EQUAL object returned.
:miss => added to table (either added to empty slot or appened to an existing slot list). EQ object returned."
  (let* ((cache-table-index (logand nlri-hash                             ; get index to hash table by masking. returns 'hash mod table-mask'
				    (NLRI-CACHE-get-table-mask cache)))
	 (table-slot-value (svref (NLRI-CACHE-get-cache-table cache)      ; table lookup to see if slot already occupied
				  cache-table-index)))
    (cond ((null table-slot-value)
	   ;; slot is empty. push a new RIB-ENTRY-TABLE onto table-slot-value,
	   ;; decrement empty-slot-count, and return values NLRI :miss
	   (sb-ext:compare-and-swap (svref (NLRI-CACHE-get-cache-table cache) cache-table-index)
				    nil
				    (list nlri))
	   (incf (NLRI-CACHE-get-entry-count cache))
	   (decf (NLRI-CACHE-get-empty-slot-count cache))
	   (values nlri :miss))
	  (t
	   ;; slot already occupied by list,
	   ;; find if there is a matching NLRI in the slot
	   (let ((matching-nlri (find nlri
				      table-slot-value
				      :test #'equal)))
	     (cond (matching-nlri
		    ;; found equal copy in table
		    (values matching-nlri :hit))
		   (t
		    ;; nlri needs to be appended to existing list
		    (let ((new-slot-value (cons nlri
						table-slot-value)))
		      (sb-ext:compare-and-swap (svref (NLRI-CACHE-get-cache-table cache) cache-table-index)
					       table-slot-value
					       new-slot-value)
		      (incf (NLRI-CACHE-get-entry-count cache))
		      (values nlri :miss)))))))))

(defun NLRI-CACHE-ro-lookup (cache nlri nlri-hash)
  "Read only version of NLRI-CACHE-rw-lookup. To be called by peer threads. Doesn't update any counters or update cache table state.
Returns EQUAL copy of the passed object if found in table. Acts as identity function if copy of passed object is not found.
Second value returned indicates result of lookup:
:hit => copy found in table. EQUAL object returned.
:miss => copy not found in table. EQ object returned."
  (let* ((cache-table-index (logand nlri-hash                             ; get index to hash table by masking. returns 'hash mod table-mask'
				    (NLRI-CACHE-get-table-mask cache)))
	 (table-slot-value (svref (NLRI-CACHE-get-cache-table cache)      ; table lookup to see if slot already occupied
				  cache-table-index)))
    (cond ((null table-slot-value)
	   ;; slot is empty, return values NLRI :miss
	   (values NLRI :miss))
	  (t
	   ;; slot already occupied by list,
	   ;; find if there is a matching NLRI in the slot
	   (let ((matching-nlri (find nlri
				      table-slot-value
				      :test #'equal)))
	     (cond (matching-nlri
		    ;; found equal copy in table
		    (values matching-nlri :hit))
		   (t
		    ;; copy not found
		    (values nlri :miss))))))))
