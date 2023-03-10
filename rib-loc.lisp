(in-package :sbgp)
#|
slot elements = list of RIB-ENTRY ('RIB-ENTRY flags peer-id . RIB-ADJ-ENTRY), sorted by RIB-ADJ-ENTRY-get-nlri
|#


(defun RIB-ENTRY-get-name (entry)             "-> symbol"        (car entry))
(defun RIB-ENTRY-get-flags (entry)            "-> u56"           (cadr entry))  ;; lower 16 bits coped from PEER-CONFIG-flags (ebgp,rr-client etc.)
(defmacro RIB-ENTRY-get-flags! (entry)        "-> u56"           `(cadr ,entry))
(defun RIB-ENTRY-get-peer-config (entry)      "-> PEER-CONFIG"   (caddr entry))
(defun RIB-ENTRY-get-originated-time (entry)  "-> u56"           (cadddr entry))
(defun RIB-ENTRY-get-rib-adj-entry (entry)    "-> RIB-ADJ-ENTRY" (cddddr entry))

(defun RIB-ENTRY-make (flags peer-config originated-time rib-adj-entry)
  (cons 'RIB-ENTRY
	(cons flags
	      (cons peer-config
		    (cons originated-time
			  rib-adj-entry)))))

(defconstant +RIB-ENTRY-flag-new-announcement+  #x010000)
(defconstant +RIB-ENTRY-flag-new-withdrawl+     #x020000)
(defconstant +RIB-ENTRY-flag-ebgp-learnt+       #x040000)
(defconstant +RIB-ENTRY-flag-best-path+         #x200000)

(defun RIB-ENTRY-get-afisafi (rib-entry) (logand #xffffff (RIB-ENTRY-get-flags rib-entry)))

(defun RIB-ENTRY-new-announcement-flag-setp (rib-entry)
  (= +RIB-ENTRY-flag-new-announcement+ (logand +RIB-ENTRY-flag-new-announcement+
					       (RIB-ENTRY-get-flags rib-entry))))
(defun RIB-ENTRY-set-new-announcement-flag (rib-entry)
  (setf (RIB-ENTRY-get-flags! rib-entry) (logior +RIB-ENTRY-flag-new-announcement+
						 (RIB-ENTRY-get-flags rib-entry))))
(defun RIB-ENTRY-clear-new-announcement-flag (rib-entry)
  (setf (RIB-ENTRY-get-flags! rib-entry) (logandc1 +RIB-ENTRY-flag-new-announcement+
						   (RIB-ENTRY-get-flags rib-entry))))

(defun RIB-ENTRY-new-withdrawl-flag-setp (rib-entry)
  (= +RIB-ENTRY-flag-new-withdrawl+ (logand +RIB-ENTRY-flag-new-withdrawl+
					    (RIB-ENTRY-get-flags rib-entry))))
(defun RIB-ENTRY-set-new-withdrawl-flag (rib-entry)
  (setf (RIB-ENTRY-get-flags! rib-entry) (logior +RIB-ENTRY-flag-new-withdrawl+
						 (RIB-ENTRY-get-flags rib-entry))))
(defun RIB-ENTRY-clear-new-withdrawl-flag (rib-entry)
  (setf (RIB-ENTRY-get-flags! rib-entry) (logandc1 +RIB-ENTRY-flag-new-withdrawl+
						   (RIB-ENTRY-get-flags rib-entry))))

(defun RIB-ENTRY-ebgp-learnt-flag-setp (rib-entry)
  (= +RIB-ENTRY-flag-ebgp-learnt+ (logand +RIB-ENTRY-flag-ebgp-learnt+
					  (RIB-ENTRY-get-flags rib-entry))))
(defun RIB-ENTRY-set-ebgp-learnt-flag (rib-entry)
  (setf (RIB-ENTRY-get-flags! rib-entry) (logior +RIB-ENTRY-flag-ebgp-learnt+
						 (RIB-ENTRY-get-flags rib-entry))))
(defun RIB-ENTRY-clear-ebgp-learnt-flag (rib-entry)
  (setf (RIB-ENTRY-get-flags! rib-entry) (logandc1 +RIB-ENTRY-flag-ebgp-learnt+
						   (RIB-ENTRY-get-flags rib-entry))))


(defun RIB-ENTRY-best-path-flag-setp (rib-entry)
  (= +RIB-ENTRY-flag-best-path+ (logand +RIB-ENTRY-flag-best-path+
					(RIB-ENTRY-get-flags rib-entry))))
(defun RIB-ENTRY-set-best-path-flag (rib-entry)
  (setf (RIB-ENTRY-get-flags! rib-entry) (logior +RIB-ENTRY-flag-best-path+
						 (RIB-ENTRY-get-flags rib-entry))))
(defun RIB-ENTRY-clear-best-path-flag (rib-entry)
  (setf (RIB-ENTRY-get-flags! rib-entry) (logandc1 +RIB-ENTRY-flag-best-path+
						   (RIB-ENTRY-get-flags rib-entry))))


(defun RIB-ENTRY-best-path (rib-entry1 rib-entry2)
  "Compares two RIB-ENTRY objects and returns 'best' path according to BGP Best Path Selection Algorithm"
 
    (let ((pa-list1 (RIB-ADJ-ENTRY-get-pa-list (RIB-ENTRY-get-rib-adj-entry rib-entry1)))
	  (pa-list2 (RIB-ADJ-ENTRY-get-pa-list (RIB-ENTRY-get-rib-adj-entry rib-entry2))))
      
      ;; Prefer highest LOCAL-PREF (paths with no LOCAL-PREF assigned 100)
      (let ((local-pref1 (find 'LOCAL-PREF pa-list1 :key #'car :test #'eq))
	    (local-pref2 (find 'LOCAL-PREF pa-list2 :key #'car :test #'eq)))

	(let ((value1 (if local-pref1
			  (LOCAL-PREF-get-value local-pref1)
			  100))
	      (value2 (if local-pref2
			  (LOCAL-PREF-get-value local-pref2)
			  100)))

	(if (> value1 value2) (return-from RIB-ENTRY-best-path rib-entry1))
	(if (> value2 value1) (return-from RIB-ENTRY-best-path rib-entry2)))
      
      ;; Prefer shortest AS-PATH
      (let ((as-path-length1 (AS-PATH-get-length (find 'AS-PATH pa-list1 :key #'car :test #'eq)))
	    (as-path-length2 (AS-PATH-get-length (find 'AS-PATH pa-list2 :key #'car :test #'eq))))

	(if (< as-path-length1 as-path-length2) (return-from RIB-ENTRY-best-path rib-entry1))
	(if (< as-path-length2 as-path-length1) (return-from RIB-ENTRY-best-path rib-entry2)))

      ;; Prefer lowest ORIGIN
      (let ((origin1 (ORIGIN-get-value (find 'ORIGIN pa-list1 :key #'car :test #'eq)))
	    (origin2 (ORIGIN-get-value (find 'ORIGIN pa-list2 :key #'car :test #'eq))))
	
	(if (< origin1 origin2) (return-from RIB-ENTRY-best-path rib-entry1))
	(if (< origin2 origin1) (return-from RIB-ENTRY-best-path rib-entry2))))

      ;; Prefer lowest MULTI-EXIT-DISC (paths with no MED assigned 0)
      (let ((med1 (find 'MULTI-EXIT-DISC pa-list1 :key #'car :test #'eq))
	    (med2 (find 'MULTI-EXIT-DISC pa-list2 :key #'car :test #'eq)))

	(let ((value1 (if med1
			  (MULTI-EXIT-DISC-get-value med1)
			  0))
	      (value2 (if med2
			  (MULTI-EXIT-DISC-get-value med2)
			  0)))
	  
	  (if (< value1 value2) (return-from RIB-ENTRY-best-path rib-entry1))
	  (if (< value2 value1) (return-from RIB-ENTRY-best-path rib-entry2))))
      
      ;; Prefer eBGP over iBGP learnt routes
      (let ((rib-entry1-is-ebgp (RIB-ENTRY-ebgp-learnt-flag-setp rib-entry1))
	    (rib-entry2-is-ebgp (RIB-ENTRY-ebgp-learnt-flag-setp rib-entry2)))
	
	(if (and rib-entry1-is-ebgp
		 (not rib-entry2-is-ebgp))
	    (return-from RIB-ENTRY-best-path rib-entry1))
	(if (and rib-entry2-is-ebgp
		 (not rib-entry1-is-ebgp))
	    (return-from RIB-ENTRY-best-path rib-entry2)))
      
      ;; Prefer lowest router-id (PEER-CONFIG-get-router-id peer)
      (let ((peer-config1 (RIB-ENTRY-get-peer-config rib-entry1))
	    (peer-config2 (RIB-ENTRY-get-peer-config rib-entry2)))
	
	(let ((router-id1 (PEER-CONFIG-get-peer-router-id peer-config1))
	      (router-id2 (PEER-CONFIG-get-peer-router-id peer-config2)))

	  (if (> (IPV4-get-value router-id1)
		 (IPV4-get-value router-id2))
	      (return-from RIB-ENTRY-best-path rib-entry1))
	  (if (> (IPV4-get-value router-id2)
		 (IPV4-get-value router-id1))
	      (return-from RIB-ENTRY-best-path rib-entry2)))
		 
	;; Prefer shortest cluster list length CLUSTER-LIST. Length is 0 for no list
	(let ((cluster-list1 (find 'CLUSTER-LIST pa-list1 :key #'car :test #'eq))
	      (cluster-list2 (find 'CLUSTER-LIST pa-list2 :key #'car :test #'eq)))
	  (let ((len1 (if cluster-list1
			  (length (CLUSTER-LIST-get-cluster-id-list cluster-list1))
			  0))
		(len2 (if cluster-list2
			  (length (CLUSTER-LIST-get-cluster-id-list cluster-list2))
			  0)))
	    
	    (if (< len1 len2) (return-from RIB-ENTRY-best-path rib-entry1))
	    (if (< len2 len1) (return-from RIB-ENTRY-best-path rib-entry2))))

	;; Prefer the peer with lowest IP address (PEER-CONFIG-get-ip-address peer)
	(let ((ip-addr1 (PEER-CONFIG-get-peer-ip-address peer-config1))
	      (ip-addr2 (PEER-CONFIG-get-peer-ip-address peer-config2)))

	  (if (TL-greater-than-p ip-addr1 ip-addr2)
	      rib-entry2
	      rib-entry1)))))

(defun RIB-ENTRY-TABLE-get-name (rib-entry-table)        "-> symbol"             (car rib-entry-table))
(defun RIB-ENTRY-TABLE-get-nlri (rib-entry-table)        "-> NLRI"               (cadr rib-entry-table))
(defun RIB-ENTRY-TABLE-get-entries (rib-entry-table)     "-> list of RIB-ENTRY"  (cddr rib-entry-table))
(defmacro RIB-ENTRY-TABLE-get-entries! (rib-entry-table) "-> list of RIB-ENTRY"  `(cddr ,rib-entry-table))


(defun RIB-ENTRY-TABLE-make (nlri rib-entry)
  (RIB-ENTRY-set-best-path-flag rib-entry)
  (list 'RIB-ENTRY-TABLE
	(if *nlri-cache*
	    (NLRI-CACHE-rw-lookup *nlri-cache*
				  nlri
				  (NLRI-zhash 0 nlri))
	    nlri)
	rib-entry))

(defun RIB-ENTRY-TABLE-mark-best-path (rib-entry-table)
  "Traverses entries in RIB-ENTRY-TABLE and sets flag +RIB-ENTRY-flag-best-path+ on the 'best path' as determined by 'RIB-ENTRY-best-path' function. Clears flag on all other entries in the table"
  (let ((best-rib-entry (reduce #'RIB-ENTRY-best-path
				(RIB-ENTRY-TABLE-get-entries rib-entry-table))))
    (when best-rib-entry
      (loop for rib-entry in (RIB-ENTRY-TABLE-get-entries rib-entry-table)
	    do (if (eq rib-entry best-rib-entry)
		   (RIB-ENTRY-set-best-path-flag rib-entry)
		   (RIB-ENTRY-clear-best-path-flag rib-entry))))
    best-rib-entry))

(defun RIB-ENTRY-TABLE-add-rib-entry (rib-entry-table rib-entry)
  "Adds RIB-ENTRY to RIB-ENTRY-TABLE.
If existing entry exists with matching rib-peer value, entry is replaced.
If existing entry is not found, entry is prepended to 'entries' list.
Returns two values: RIB-ENTRY [ :replaced-existing-entry | :added-new-entry ]"
  (let* ((rib-peer (RIB-ENTRY-get-peer-config rib-entry))
	 (rib-entries-list (RIB-ENTRY-TABLE-get-entries rib-entry-table))
	 (existing-entry (find rib-peer
			       rib-entries-list
			       :key #'RIB-ENTRY-get-peer-config
			       :test #'eq)))
    
    ;;(format t "~&RIB-PEER: ~S~%RIB-ENTRIES-LIST: ~S~%EXISTING-ENTRY: ~S"
    ;;	    rib-peer rib-entries-list existing-entry)
    (cond (existing-entry
	   ;; (format t "~%RIB-ENTRY-TABLE-add :replaced-existing-entry~%")
	   (setf (RIB-ENTRY-TABLE-get-entries! rib-entry-table)
		 (cons rib-entry
		       (remove existing-entry rib-entries-list)))
	   (RIB-ENTRY-TABLE-mark-best-path rib-entry-table)
	   (values rib-entry-table :replaced-existing-entry))
	  (t
	   ;; (format t "~%RIB-ENTRY-TABLE-add :added-new-entry~%")
	   (setf (RIB-ENTRY-TABLE-get-entries! rib-entry-table)
		 (cons rib-entry rib-entries-list))
	   (RIB-ENTRY-TABLE-mark-best-path rib-entry-table)
	   (values rib-entry-table :added-new-entry)))))

(defun RIB-ENTRY-TABLE-delete-entry (rib-entry-table rib-peer)
  (let ((existing-entry (find-if #'(lambda (x) (eq x rib-peer))
				 (RIB-ENTRY-TABLE-get-entries rib-entry-table))))
    (when existing-entry
      (setf (RIB-ENTRY-TABLE-get-entries! rib-entry-table)
	    (remove existing-entry (RIB-ENTRY-TABLE-get-entries rib-entry-table)))
      (if (RIB-ENTRY-best-path-flag-setp existing-entry)
	  (RIB-ENTRY-TABLE-mark-best-path (RIB-ENTRY-TABLE-get-entries rib-entry-table)))
      existing-entry)
    nil))

(defun RIB-ENTRY-TABLE-valid1-p (rib-entry-table)
  (>= (length rib-entry-table) 2))

(deftype RIB-ENTRY-TABLE () '(and (cons (member RIB-ENTRY-TABLE)) (satisfies RIB-ENTRY-TABLE-valid1-p)))

(defmacro RIB-LOC-get-name (rib-loc)             "-> symbol"    `(svref ,rib-loc 0))
(defmacro RIB-LOC-get-flags (rib-loc)            "-> u56"       `(svref ,rib-loc 1))
(defmacro RIB-LOC-get-num-bits (rib-loc)         "-> integer"   `(svref ,rib-loc 2))
(defmacro RIB-LOC-get-table-size (rib-loc)       "-> integer"   `(svref ,rib-loc 3))
(defmacro RIB-LOC-get-empty-slot-count (rib-loc) "-> integer"   `(svref ,rib-loc 4))
(defmacro RIB-LOC-get-entry-count (rib-loc)      "-> integer"   `(svref ,rib-loc 5))
(defmacro RIB-LOC-get-table-mask (rib-loc)       "-> u56"       `(svref ,rib-loc 6))
(defmacro RIB-LOC-get-table (rib-loc)            "-> vector"    `(svref ,rib-loc 7))
(defmacro RIB-LOC-get-peer-configs (rib-loc)     "-> assoc list [thread-name . PEER-CONFIG]" `(svref ,rib-loc 8))

(defconstant +RIB-LOC-flag-new-announcements+    #x1)
(defconstant +RIB-LOC-flag-new-withdrawls+       #x2)

(defun RIB-LOC-new-announcements-flag-set-p (rib-loc)
  (not (= 0 (logand (RIB-LOC-get-flags rib-loc)
		    +RIB-LOC-flag-new-announcements+))))

(defun RIB-LOC-set-new-announcements-flag (rib-loc)
  (setf (RIB-LOC-get-flags rib-loc)
	(logior +RIB-LOC-flag-new-announcements+
		(RIB-LOC-get-flags rib-loc))))

(defun RIB-LOC-clear-new-announcements-flag (rib-loc)
  (setf (RIB-LOC-get-flags rib-loc)
	(logandc1 +RIB-LOC-flag-new-announcements+
		  (RIB-LOC-get-flags rib-loc))))

(defun RIB-LOC-new-withdrawls-flag-set-p (rib-loc)
  (not (= 0 (logand (RIB-LOC-get-flags rib-loc)
		    +RIB-LOC-flag-new-withdrawls+))))

(defun RIB-LOC-set-new-withdrawls-flag (rib-loc)
  (setf (RIB-LOC-get-flags rib-loc)
	(logior +RIB-LOC-flag-new-withdrawls+
		(RIB-LOC-get-flags rib-loc))))

(defun RIB-LOC-clear-new-withdrawls-flag (rib-loc)
  (setf (RIB-LOC-get-flags rib-loc)
	(logandc1 +RIB-LOC-flag-new-withdrawls+
		  (RIB-LOC-get-flags rib-loc))))

(defun RIB-LOC-make (table-size-num-bits)
  (let ((rib-loc (make-array 9 :initial-element nil))
	(table-size (ash 1 table-size-num-bits)))

    (setf (RIB-LOC-get-name rib-loc)                    'RIB-LOC)
    (setf (RIB-LOC-get-flags rib-loc)                   0)
    (setf (RIB-LOC-get-num-bits rib-loc)                table-size-num-bits)     
    (setf (RIB-LOC-get-table-size rib-loc)              table-size)     
    (setf (RIB-LOC-get-empty-slot-count rib-loc)        table-size)
    (setf (RIB-LOC-get-entry-count rib-loc)             0)
    (setf (RIB-LOC-get-table-mask rib-loc)              (1- table-size))     
    (setf (RIB-LOC-get-table rib-loc)                   (make-array table-size :initial-element nil))
    (setf (RIB-LOC-get-peer-configs rib-loc)            nil)

    rib-loc))

(defun RIB-LOC-valid1-p (rib-loc)
  (and (= (length rib-loc) 8)))

(deftype RIB-LOC () '(and (cons (member RIB-LOC)) (satisfies RIB-LOC-valid1-p)))

(defun RIB-LOC-clear (rib-loc)
  (setf (RIB-LOC-get-empty-slot-count rib-loc)        (RIB-LOC-get-table-size rib-loc))
  (setf (RIB-LOC-get-entry-count rib-loc)             0)
  (fill (RIB-LOC-get-table rib-loc)                   nil))

;;; Add, remove & find PEER-CONFIG entries in RIB-LOC's peer-configs table
(defun RIB-LOC-add-peer-config (rib-loc peer-config)
  "Adds PEER-CONFIG to RIB-LOC's list of peer-configs. 
If 'EQUAL copy of PEER-CONFIG already exists in the table, returns the copy from the peer-configs table.
Otherwise, PEER-CONFIGS is added to the table, and the passed PEER-CONFIG is returned"
  (let* ((thread-name (PEER-CONFIG-get-thread-name peer-config))
	 (current-config-entry (cdr (assoc thread-name (RIB-LOC-get-peer-configs rib-loc)))))
    (if current-config-entry
	;; only replace existing entry if configuration has changed (to allow multiple reloading of mrt rib files into rib-loc - ensures rib-peer objects is 'EQ between file loads)
	(cond  ((not (equal current-config-entry peer-config))
		;;(format t "~%REPLACING EXISTING RIB-PEER ENTRY~%")
		(setf (cdr (assoc thread-name (RIB-LOC-get-peer-configs rib-loc)))
		      peer-config))
	       (t
		;;(format t "~%NOT REPLACING EXISTING RIB-PEER ENTRY~%")
		current-config-entry))
	(push (cons thread-name peer-config)
	      (RIB-LOC-get-peer-configs rib-loc)))))

(defun RIB-LOC-get-peer-config (rib-loc thread-name)
  "Returns RIB-PEER object with name THREAD-NAME"
  (cdr (assoc thread-name (RIB-LOC-get-peer-configs rib-loc))))

(defun RIB-LOC-find-rib-entry-table (rib-loc nlri)
  "Returns RIB-ENTRY-TABLE object if it exists in RIB-LOC matching passed NLRI. Otherwise, returns NIL."
  (let* ((table-index (logand (NLRI-zhash 0 nlri)                       
			      (RIB-LOC-get-table-mask rib-loc)))
	 (table-slot-value (svref (RIB-LOC-get-table rib-loc) 
				  table-index)))
    (find nlri
	  table-slot-value
	  :key #'RIB-ENTRY-TABLE-get-nlri
	  :test #'equal)))

(defun RIB-LOC-find-rib-entry (rib-loc thread-name rib-adj-entry)
  "Returns RIB-ENTRY object from RIB-LOC table if it matches PEER-ID and RIB-ADJ-ENTRY"
  (let ((rib-entry-table (RIB-LOC-find-rib-entry-table RIB-Loc         ; find if RIB-ENTRY-TABLE exists for this NLRI
						       (RIB-ADJ-ENTRY-get-nlri rib-adj-entry)))
	(rib-peer (RIB-LOC-get-peer-config rib-loc thread-name)))
    (if (and rib-entry-table rib-peer)
	(find rib-peer
	      (RIB-ENTRY-TABLE-get-entries rib-entry-table)
	      :key #'RIB-ENTRY-get-peer-config
	      :test #'eq)
	nil)))

(defun RIB-LOC-add-rib-adj-entry (rib-loc peer-config originated-time rib-adj-entry)
  "Adds passed RIB-ADJ-ENTRY object to the hash table RIB-LOC.
Rib-entry object (created from  is added to RIB-LOC if the object is not already present in the table (no RIB-ENTRY exists with same NLRI).
If RIB-ENTRY already exists for this NLRI/PEER-THREAD-NAME, the entry is replaced.
Returns two values: RIB-ENTRY [ :replaced-existing-entry | :added-new-entry | :added-new-table ]"
  
  (let* ((nlri (RIB-ADJ-ENTRY-get-nlri rib-adj-entry))

	 (new-rib-entry (RIB-ENTRY-make (+ +RIB-ENTRY-flag-new-announcement+
					   (PEER-CONFIG-get-flags peer-config))   ; flags
			                peer-config                               ; rib-peer
			                originated-time                           ; originated-time
			                rib-adj-entry))                           ; rib-adj-entry

	 (table-index (logand (NLRI-zhash 0 nlri)
			      (RIB-LOC-get-table-mask rib-loc)))           

	 (table-slot-value (svref (RIB-LOC-get-table rib-loc) 
				  table-index)))
    (cond ((null table-slot-value)
	   ;; slot is empty. push a new RIB-ENTRY-TABLE onto table-slot-value,
	   ;; decrement empty-slot-count, and return
	   (push (RIB-ENTRY-TABLE-make nlri new-rib-entry)
		 (svref (RIB-LOC-get-table rib-loc) table-index))
	   (incf (RIB-LOC-get-entry-count rib-loc))
	   (decf (RIB-LOC-get-empty-slot-count rib-loc))
	   ;; (format t "~&NEW RIB-ENTRY-TABLE ADDED TO EMPTY SLOT~%")
	   (values new-rib-entry :added-new-table))
	  (t
	   ;; slot already occupied by list
	   ;; 1. find if there is a RIB-ENTRY-TABLE for this nlri
	   (let ((matching-rib-entry-table (find nlri
						 table-slot-value
						 :key #'RIB-ENTRY-TABLE-get-nlri
						 :test #'equal)))
	     (cond (matching-rib-entry-table
		    ;;(format t "~&NEW RIB-ENTRY ADDED TO EXISTING RIB-ENTRY-TABLE~%")
		    (RIB-ENTRY-TABLE-add-rib-entry matching-rib-entry-table
						   new-rib-entry))
		   (t
		    ;;(format t "~&NEW RIB-ENTRY-TABLE ADDED TO OCCUPIED SLOT~%")
		    (push (RIB-ENTRY-TABLE-make nlri new-rib-entry)
			  (svref (RIB-LOC-get-table rib-loc) table-index))
		    (values new-rib-entry :added-new-table))))))))

(defun RIB-LOC-delete-entry (rib-loc nlri thread-name)
  (let* ((table-index (logand (NLRI-zhash 0 nlri)
			     (RIB-LOC-get-table-mask rib-loc)))           
	 (table-slot-value (svref (RIB-LOC-get-table rib-loc) 
				 table-index)))
    (when table-slot-value
      (let ((rib-entry-table (find nlri
				   table-slot-value
				   :key #'RIB-ENTRY-TABLE-get-nlri
				   :test #'equal)))
	(when rib-entry-table
	  (let ((rib-peer (RIB-LOC-get-peer-config rib-loc thread-name)))
	     (RIB-ENTRY-TABLE-delete-entry rib-entry-table rib-peer)))))))		


(defun RIB-LOC-collect-if (predicate rib-loc)
  "Collect and return list of RIB-ENTRY from RIB-LOC table for which PREDICATE returns true."
  (let ((rtn-rib-entries nil))
    (loop for slot across (RIB-LOC-get-table rib-loc)
	  when slot
	    do (loop for rib-entry-table in slot
		     do (loop for rib-entry in (RIB-ENTRY-TABLE-get-entries rib-entry-table)
			      do (if (funcall predicate rib-entry)
				     (push rib-entry rtn-rib-entries)))))
    rtn-rib-entries))

(defun RIB-LOC-update-if (predicate update-fn rib-loc)
  "Iterates over all rib-entries in RIB-LOC. Applies UPDATE-FN to entry if PREDICATE returns true.
Returns integer - number of rib entries updated."
  (let ((update-count 0))
    (loop for slot across (RIB-LOC-get-table rib-loc)
	  when slot
	    do (loop for rib-entry-table in slot
		     do (loop for rib-entry in (RIB-ENTRY-TABLE-get-entries rib-entry-table)
			      when (funcall predicate rib-entry)
				do (progn
				     (incf update-count)
				     (funcall update-fn rib-entry)))))
    update-count))

(defun RIB-LOC-delete-if (predicate rib-loc)
  "Iterates over entire RIB-LOC table and deletes each rib-entry object for which PREDICATE returns true.
Returns integer - number of rib entries deleted."
  ;; first pass across table collects entries
  (let ((delete-count 0))
    (loop for slot across (RIB-LOC-get-table rib-loc)
	  do (loop for rib-entry-table in slot
		   do (multiple-value-bind (updated-rib-entry-table remove-count)
			  (remove-if! predicate (RIB-ENTRY-TABLE-get-entries rib-entry-table))
			(when (> remove-count 0)
			  (setf (RIB-ENTRY-TABLE-get-entries! rib-entry-table) updated-rib-entry-table)
			  (decf (RIB-LOC-get-entry-count rib-loc) remove-count)
			  (incf delete-count remove-count)))))
    delete-count))

			       


		   
  
