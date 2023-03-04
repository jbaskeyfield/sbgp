(in-package :sbgp)
#|
slot elements = list of RIB-ENTRY ('RIB-ENTRY flags peer-id . RIB-ADJ-ENTRY), sorted by RIB-ADJ-ENTRY-get-nlri
|#

(defun RIB-ENTRY-get-name (entry)          "-> symbol"                       (car entry))
(defun RIB-ENTRY-get-flags (entry)         "-> u56"                          (cadr entry))
(defmacro RIB-ENTRY-get-flags! (entry)     "-> u56"                          `(cadr ,entry))
(defun RIB-ENTRY-get-peer-id (entry)       "-> symbol (name of peer thread)" (caddr entry))
(defun RIB-ENTRY-get-rib-adj-entry (entry) "-> RIB-ADJ-ENTRY"                (cdddr entry))

(defun RIB-ENTRY-make (afisafi peer-id rib-adj-entry &rest flags)
  (cons 'RIB-ENTRY
	(cons (+ afisafi (reduce #'+ flags))
	      (cons peer-id
	            rib-adj-entry))))

(defconstant +RIB-ENTRY-flag-new-announcement+  #x01000000)
(defconstant +RIB-ENTRY-flag-new-withdrawl+     #x02000000)

(defun RIB-ENTRY-get-afisafi (rib-entry) (logand #xffffff (RIB-ENTRY-get-flags rib-entry)))

(defun RIB-ENTRY-new-announcement-flag-setp (rib-entry) (= +RIB-ENTRY-flag-new-announcement+
							   (logand +RIB-ENTRY-flag-new-announcement+
								   (RIB-ENTRY-get-flags rib-entry))))

(defun RIB-ENTRY-set-new-announcement-flag (rib-entry) (setf (RIB-ENTRY-get-flags! rib-entry)
							     (logior +RIB-ENTRY-flag-new-announcement+
								     (RIB-ENTRY-get-flags rib-entry))))

(defun RIB-ENTRY-clear-new-announcement-flag (rib-entry) (setf (RIB-ENTRY-get-flags! rib-entry)
							       (logandc1 +RIB-ENTRY-flag-new-announcement+
									 (RIB-ENTRY-get-flags rib-entry))))

(defun RIB-ENTRY-new-withdrawl-flag-setp (rib-entry) (= +RIB-ENTRY-flag-new-withdrawl+
							(logand +RIB-ENTRY-flag-new-withdrawl+
								(RIB-ENTRY-get-flags rib-entry))))

(defun RIB-ENTRY-set-new-withdrawl-flag (rib-entry) (setf (RIB-ENTRY-get-flags! rib-entry)
							  (logior +RIB-ENTRY-flag-new-withdrawl+
								  (RIB-ENTRY-get-flags rib-entry))))

(defun RIB-ENTRY-clear-new-withdrawl-flag (rib-entry) (setf (RIB-ENTRY-get-flags! rib-entry)
							    (logandc1 +RIB-ENTRY-flag-new-withdrawl+
								      (RIB-ENTRY-get-flags rib-entry))))

(defun RIB-ENTRY-TABLE-get-name (rib-entry-table)    "-> symbol"             (car rib-entry-table))
(defun RIB-ENTRY-TABLE-get-nlri (rib-entry-table)    "-> NLRI"               (cadr rib-entry-table))
(defun RIB-ENTRY-TABLE-get-entries (rib-entry-table) "-> list of RIB-ENTRY"  (cddr rib-entry-table))
(defmacro RIB-ENTRY-TABLE-get-entries! (rib-entry-table) "-> list of RIB-ENTRY"  `(cddr ,rib-entry-table))


(defun RIB-ENTRY-TABLE-make (nlri rib-entry)
  (list 'RIB-ENTRY-TABLE
	(if *nlri-cache*
	    (NLRI-CACHE-rw-lookup *nlri-cache*
				  nlri
				  (NLRI-zhash 0 nlri))
	    nlri)
	rib-entry))

(defun RIB-ENTRY-TABLE-add (rib-entry-table rib-entry)
  (setf (RIB-ENTRY-TABLE-get-entries! rib-entry-table)
	(cons rib-entry (RIB-ENTRY-TABLE-get-entries rib-entry-table))))

(defun RIB-ENTRY-TABLE-valid1-p (rib-entry-table)
  (>= (length rib-entry-table) 2))

(deftype RIB-ENTRY-TABLE () '(and (cons (member RIB-ENTRY-TABLE)) (satisfies RIB-ENTRY-TABLE-valid1-p)))

(defmacro RIB-LOC-get-name (rib-loc)             "-> symbol"   `(svref ,rib-loc 0))
(defmacro RIB-LOC-get-num-bits (rib-loc)         "-> integer"  `(svref ,rib-loc 1))
(defmacro RIB-LOC-get-table-size (rib-loc)       "-> integer"  `(svref ,rib-loc 2))
(defmacro RIB-LOC-get-empty-slot-count (rib-loc) "-> integer"  `(svref ,rib-loc 3))
(defmacro RIB-LOC-get-entry-count (rib-loc)      "-> integer"  `(svref ,rib-loc 4))
(defmacro RIB-LOC-get-table-mask (rib-loc)       "-> u56"      `(svref ,rib-loc 5))
(defmacro RIB-LOC-get-rib-loc-table (rib-loc)    "-> vector"   `(svref ,rib-loc 6))

(defun RIB-LOC-make (table-size-num-bits)
  (let ((rib-loc (make-array 7 :initial-element nil))
	(table-size (ash 1 table-size-num-bits)))

    (setf (RIB-LOC-get-name rib-loc)                    'RIB-LOC)                 
    (setf (RIB-LOC-get-num-bits rib-loc)                table-size-num-bits)     
    (setf (RIB-LOC-get-table-size rib-loc)              table-size)     
    (setf (RIB-LOC-get-empty-slot-count rib-loc)        table-size)
    (setf (RIB-LOC-get-entry-count rib-loc)             0)
    (setf (RIB-LOC-get-table-mask rib-loc)              (1- table-size))     
    (setf (RIB-LOC-get-rib-loc-table rib-loc)           (make-array table-size :initial-element nil))

    rib-loc))

(defun RIB-LOC-valid1-p (rib-loc)
  (and (= (length rib-loc) 7)))

(deftype RIB-LOC () '(and (cons (member RIB-LOC)) (satisfies RIB-LOC-valid1-p)))

(defun RIB-LOC-clear(rib-loc)
  (setf (RIB-LOC-get-empty-slot-count rib-loc)        (RIB-LOC-get-table-size rib-loc))
  (setf (RIB-LOC-get-entry-count rib-loc)              0)
  (fill (RIB-LOC-get-rib-loc-table rib-loc)           nil))

(defun RIB-LOC-find-rib-entry-table (rib-loc nlri)
  "Returns RIB-ENTRY-TABLE object if it exists in RIB-LOC matching passed NLRI. Otherwise, returns NIL."
  (let* ((table-index (logand (sxhash nlri)                       
			      (RIB-LOC-get-table-mask rib-loc)))
	 (table-slot-value (svref (RIB-LOC-get-rib-loc-table rib-loc) 
				  table-index)))
    (find nlri
	  table-slot-value
	  :key #'RIB-ENTRY-TABLE-get-nlri
	  :test #'equal)))

  
(defun RIB-LOC-add-entry (rib-loc rib-entry)
  "Adds passed RIB-ENTRY object to the hash table RIB-LOC. RIB-ENTRY is added to RIB-LOC if the object is not already present in the table.
Returns two values
1. RIB-ENTRY = the passed RIB-ENTRY
2. [ :added-new-entry | :duplicate-entry-found ]
  :added-entry         => entry successfully added to table (either added to empty slot or appened to an existing slot list)
  :duplicate-entry-found   => an EQUAL copy of the newly created RIB-ENTRY was found in the table (no changes made to RIB-LOC)"
  
  (let* ((rib-adj-entry (RIB-ENTRY-get-rib-adj-entry rib-entry))
	 (nlri (RIB-ADJ-ENTRY-get-nlri rib-adj-entry))
	 (table-index (logand (sxhash nlri)
			      (RIB-LOC-get-table-mask rib-loc)))           
	 (table-slot-value (svref (RIB-LOC-get-rib-loc-table rib-loc) 
				  table-index)))
        (cond ((null table-slot-value)
	       ;; slot is empty. push a new RIB-ENTRY-TABLE onto table-slot-value,
	       ;; decrement empty-slot-count, and return
	       (push (RIB-ENTRY-TABLE-make nlri rib-entry)
		     (svref (RIB-LOC-get-rib-loc-table rib-loc)
			    table-index))
	       (incf (RIB-LOC-get-entry-count rib-loc))
	       (decf (RIB-LOC-get-empty-slot-count rib-loc))
	      ;; (format t "~&NEW RIB-ENTRY-TABLE ADDED TO EMPTY SLOT~%")
	       (values rib-entry :added-entry))
	      (t
	       ;; slot already occupied by list
	       ;; 1. find if there is a RIB-ENTRY-TABLE for this nlri
	       (let ((matching-rib-entry-table (find nlri
					             table-slot-value
						     :key #'RIB-ENTRY-TABLE-get-nlri
						     :test #'equal)))
		 (cond (matching-rib-entry-table
		;;	(format t "~&NEW RIB-ENTRY ADDED TO EXISTING RIB-ENTRY-TABLE~%")
			(RIB-ENTRY-TABLE-add matching-rib-entry-table
					     rib-entry))
		       (t
		;;	(format t "~&NEW RIB-ENTRY-TABLE ADDED TO OCCUPIED SLOT~%")
			(push (RIB-ENTRY-TABLE-make nlri rib-entry)
			      (svref (RIB-LOC-get-rib-loc-table rib-loc)
				     table-index)))))))))

(defun RIB-LOC-update-collect-entries (rib-loc &key (test-fn #'identity) (update-fn nil) (collect t))
  "Iterates over entire RIB-LOC table and collects rib-entry objects for which TEST-FN returns true (also optionally updates each entry object)
Each rib-entry in the table '(funcall TEST-FN rib-entry) is applies. If returns true,
'(funcall UPDATE-FN rib-entry) if UPDATE-FN is non-nil, and the rib-entry collected in a returned list if COLLECT is true.
NOTE: the rib-adj-entry (nlri/pa-list created by each peer) within the rib-entry should be treated as immutable - only modifiable field is currently the flags field.
Examples:
(RIB-LOC-update-collect-entries rib-loc) => returns list of all table entries unmodified.
(RIB-LOC-update-collect-entries rib-loc :test-fn #'rib-entry-new-announcement-flag-setp :update-fn #'rib-entry-clear-new-announcement-flag) => returns all entries with announcement flag set and clears the flag on each entry"
  (let ((rtn-rib-entries nil))
    (loop for slot across (RIB-LOC-get-rib-loc-table rib-loc)
	  when slot
	    do (loop for rib-entry-table in slot
		     do (loop for rib-entry in (RIB-ENTRY-TABLE-get-entries rib-entry-table)
			      when (funcall test-fn rib-entry)
				do (progn
				     (if update-fn (funcall update-fn rib-entry))
				     (if collect (push rib-entry rtn-rib-entries))))))
    rtn-rib-entries))
