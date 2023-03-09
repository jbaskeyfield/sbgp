(in-package :sbgp)
#|
slot elements = list of RIB-ENTRY ('RIB-ENTRY flags peer-id . RIB-ADJ-ENTRY), sorted by RIB-ADJ-ENTRY-get-nlri
|#

(defun RIB-PEER-get-thread-name (peer)       "-> symbol (name of peer thread)" (cadr peer))
(defun RIB-PEER-get-internal-external (peer) "-> symbol [internal|external]"   (caddr peer))
(defun RIB-PEER-get-router-id (peer)         "-> IPV4"                         (cadddr peer))
(defun RIB-PEER-get-ip-address (peer)        "-> IPV4 | IPV6"                  (car (cddddr peer)))
(defun RIB-PEER-get-peer-asn (peer)          "-> u32"                          (cadr (cddddr peer)))

(defun RIB-PEER-make (thread-name internal-external router-id ip-address peer-asn)
  "Link to RIB-PEER object is included in each RIB-ENTRY to indicate source and provide info for best-path selection (iBGP/eBGP, router id, ip address).
Arguments: thread-name [symbol - name of peer thread], internal-external [symbol - 'INTERNAL|'EXTERNAL], router-id [IPV4], ip-address [IPV4|IPV6]."
  (list 'RIB-PEER
	thread-name
	internal-external
	router-id
	ip-address
	peer-asn))
	
;;(defun RIB-ENTRY-get-name (entry)            "-> symbol"                  (car entry))
(defun RIB-ENTRY-get-flags (entry)           "-> u56"                     (car entry))
(defmacro RIB-ENTRY-get-flags! (entry)       "-> u56"                     `(car ,entry))
(defun RIB-ENTRY-get-rib-peer (entry)        "-> RIB-PEER"                (cadr entry))
(defun RIB-ENTRY-get-originated-time (entry) "-> u56"                     (caddr entry))
(defun RIB-ENTRY-get-rib-adj-entry (entry)   "-> RIB-ADJ-ENTRY"           (cdddr entry))

(defun RIB-ENTRY-make (afisafi rib-peer rib-adj-entry originated-time flags)
;;  (cons 'RIB-ENTRY
	(cons (+ afisafi flags)
	      (cons rib-peer
		    (cons originated-time
		          rib-adj-entry))));;)

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

(defun RIB-ENTRY-TABLE-get-name (rib-entry-table)        "-> symbol"             (car rib-entry-table))
(defun RIB-ENTRY-TABLE-get-nlri (rib-entry-table)        "-> NLRI"               (cadr rib-entry-table))
(defun RIB-ENTRY-TABLE-get-entries (rib-entry-table)     "-> list of RIB-ENTRY"  (cddr rib-entry-table))
(defmacro RIB-ENTRY-TABLE-get-entries! (rib-entry-table) "-> list of RIB-ENTRY"  `(cddr ,rib-entry-table))


(defun RIB-ENTRY-TABLE-make (nlri rib-entry)
  (list 'RIB-ENTRY-TABLE
	(if *nlri-cache*
	    (NLRI-CACHE-rw-lookup *nlri-cache*
				  nlri
				  (NLRI-zhash 0 nlri))
	    nlri)
	rib-entry))

;;(defun RIB-ENTRY-TABLE-add (rib-entry-table rib-entry)
;;  (setf (RIB-ENTRY-TABLE-get-entries! rib-entry-table)
;;	(cons rib-entry (RIB-ENTRY-TABLE-get-entries rib-entry-table))))

(defun RIB-ENTRY-TABLE-add (rib-entry-table rib-entry)
  "Adds RIB-ENTRY to RIB-ENTRY-TABLE.
If existing entry exists with matching rib-peer value, entry is replaced.
If existing entry is not found, entry is prepended to 'entries' list.
Returns two values: RIB-ENTRY [ :replaced-existing-entry | :added-new-entry ]"
  (let* ((rib-peer (RIB-ENTRY-get-rib-peer rib-entry))
	 (rib-entries-list (RIB-ENTRY-TABLE-get-entries rib-entry-table))
	 (existing-entry (find rib-peer
			       rib-entries-list
			       :key #'RIB-ENTRY-get-rib-peer
			       :test #'eq)))
    
    ;;(format t "~&RIB-PEER: ~S~%RIB-ENTRIES-LIST: ~S~%EXISTING-ENTRY: ~S"
   ;;	    rib-peer rib-entries-list existing-entry)
    (cond (existing-entry
	   ;; (format t "~%RIB-ENTRY-TABLE-add :replaced-existing-entry~%")
	   (values (setf (RIB-ENTRY-TABLE-get-entries! rib-entry-table)
			 (cons rib-entry
			       (remove-if #'(lambda (x) (eq (RIB-ENTRY-get-rib-peer x)
							    rib-peer))
					  rib-entries-list)))
		   :replaced-existing-entry))
	  (t
	   ;; (format t "~%RIB-ENTRY-TABLE-add :added-new-entry~%")
	   (values (setf (RIB-ENTRY-TABLE-get-entries! rib-entry-table)
			 (cons rib-entry rib-entries-list))
		   :added-new-entry)))))
							  
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
(defmacro RIB-LOC-get-rib-loc-table (rib-loc)    "-> vector"    `(svref ,rib-loc 7))
(defmacro RIB-LOC-get-peers (rib-loc)            "-> assoc list [thread-name . RIB-PEER]" `(svref ,rib-loc 8))

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
    (setf (RIB-LOC-get-rib-loc-table rib-loc)           (make-array table-size :initial-element nil))
    (setf (RIB-LOC-get-peers rib-loc)                   nil)

    rib-loc))

(defun RIB-LOC-valid1-p (rib-loc)
  (and (= (length rib-loc) 8)))

(deftype RIB-LOC () '(and (cons (member RIB-LOC)) (satisfies RIB-LOC-valid1-p)))

(defun RIB-LOC-clear (rib-loc)
  (setf (RIB-LOC-get-empty-slot-count rib-loc)        (RIB-LOC-get-table-size rib-loc))
  (setf (RIB-LOC-get-entry-count rib-loc)              0)
  (fill (RIB-LOC-get-rib-loc-table rib-loc)           nil))

;;; Add, remove & find RIB-PEER entries in RIB-LOC's peer table
(defun RIB-LOC-add-peer (rib-loc rib-peer)
  "Adds RIB-PEER to RIB-LOC's list of peers. 
If 'EQUAL copy of RIB-PEER already exists in the table, returns the copy from the RIB-LOC table.
Otherwise, RIB-PEER is added to the table, and the passed RIB-PEER is returned"
  (let* ((peer-thread-name (RIB-PEER-get-thread-name rib-peer))
	 (current-peer-entry (cdr (assoc peer-thread-name (RIB-LOC-get-peers rib-loc)))))
    (if current-peer-entry
	;; only replace existing entry if configuration has changed (to allow multiple reloading of mrt rib files into rib-loc - ensures rib-peer objects 'EQ between file loads)
	(cond  ((not (equal current-peer-entry rib-peer))
		;;(format t "~%REPLACING EXISTING RIB-PEER ENTRY~%")
		(setf (cdr (assoc peer-thread-name (RIB-LOC-get-peers rib-loc)))
		      rib-peer))
	       (t
		;;(format t "~%NOT REPLACING EXISTING RIB-PEER ENTRY~%")
		current-peer-entry))
	(push (cons peer-thread-name rib-peer)
	      (RIB-LOC-get-peers rib-loc)))))

(defun RIB-LOC-get-rib-peer (rib-loc peer-thread-name)
  "Returns RIB-PEER object with name PEER-THREAD-NAME"
  (cdr (assoc peer-thread-name (RIB-LOC-get-peers rib-loc))))

(defun RIB-LOC-find-rib-entry-table (rib-loc nlri)
  "Returns RIB-ENTRY-TABLE object if it exists in RIB-LOC matching passed NLRI. Otherwise, returns NIL."
  (let* ((table-index (logand (NLRI-zhash 0 nlri)                       
			      (RIB-LOC-get-table-mask rib-loc)))
	 (table-slot-value (svref (RIB-LOC-get-rib-loc-table rib-loc) 
				  table-index)))
    (find nlri
	  table-slot-value
	  :key #'RIB-ENTRY-TABLE-get-nlri
	  :test #'equal)))

(defun RIB-LOC-find-rib-entry (rib-loc peer-thread-id rib-adj-entry)
  "Returns RIB-ENTRY object from RIB-LOC table if it matches PEER-ID and RIB-ADJ-ENTRY"
  (let ((rib-entry-table (RIB-LOC-find-rib-entry-table RIB-Loc         ; find if RIB-ENTRY-TABLE exists for this NLRI
						       (RIB-ADJ-ENTRY-get-nlri rib-adj-entry)))
	(rib-peer (RIB-LOC-get-rib-peer rib-loc peer-thread-id)))
    (if (and rib-entry-table rib-peer)
	(find rib-peer
	      (RIB-ENTRY-TABLE-get-entries rib-entry-table)
	      :key #'RIB-ENTRY-get-rib-peer
	      :test #'eq)
	nil)))

(defun RIB-LOC-add-entry (rib-loc rib-entry)
  "Adds passed RIB-ENTRY object to the hash table RIB-LOC. RIB-ENTRY is added to RIB-LOC if the object is not already present in the table (no RIB-ENTRY exists with same NLRI).
If RIB-ENTRY already exists for this NLRI/PEER-ID, the entry is replaced.
Returns two values: RIB-ENTRY [ :replaced-existing-entry | :added-new-entry | :added-new-table ]"
  
  (let* ((rib-adj-entry (RIB-ENTRY-get-rib-adj-entry rib-entry))
	 (nlri (RIB-ADJ-ENTRY-get-nlri rib-adj-entry))
	 (table-index (logand (NLRI-zhash 0 nlri)
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
	   (values rib-entry :added-new-table))
	  (t
	   ;; slot already occupied by list
	   ;; 1. find if there is a RIB-ENTRY-TABLE for this nlri
	   (let ((matching-rib-entry-table (find nlri
						 table-slot-value
						 :key #'RIB-ENTRY-TABLE-get-nlri
						 :test #'equal)))
	     (cond (matching-rib-entry-table
		    ;;(format t "~&NEW RIB-ENTRY ADDED TO EXISTING RIB-ENTRY-TABLE~%")
		    (RIB-ENTRY-TABLE-add matching-rib-entry-table
					 rib-entry))
		   (t
		    ;;(format t "~&NEW RIB-ENTRY-TABLE ADDED TO OCCUPIED SLOT~%")
		    (values (push (RIB-ENTRY-TABLE-make nlri rib-entry)
				  (svref (RIB-LOC-get-rib-loc-table rib-loc)
					 table-index))
			    :added-new-table))))))))

(defun RIB-LOC-update-collect-entries (rib-loc &key (test-fn #'identity) (update-fn nil) (collect? t))
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
				     (if collect? (push rib-entry rtn-rib-entries))))))
    rtn-rib-entries))

(defun RIB-LOC-delete-collect-entries (rib-loc &key (test-fn #'RIB-ENTRY-new-withdrawl-flag-setp) (collect? t))
  "Iterates over entire RIB-LOC table and deletes each rib-entry object for which TEST-FN returns true. Also collects/returns deleted entries if COLLECT? is true"
  ;; first pass across table collects entries
  (let ((rtn-entries nil))
    (when collect?
     (loop for slot across (RIB-LOC-get-rib-loc-table rib-loc)
	  do (loop for rib-entry-table in slot
		    do (loop for rib-entry in (RIB-ENTRY-TABLE-get-entries rib-entry-table)
			         when (funcall test-fn rib-entry)
				   do (push rib-entry rtn-entries)))))
    (loop for slot across (RIB-LOC-get-rib-loc-table rib-loc)
	  do (loop for rib-entry-table in slot
		   do (multiple-value-bind (updated-rib-entry-table remove-count)
			  (remove-if! test-fn (RIB-ENTRY-TABLE-get-entries rib-entry-table))
			(when (> remove-count 0)
			  (setf (RIB-ENTRY-TABLE-get-entries! rib-entry-table)
				updated-rib-entry-table)
			  (decf (RIB-LOC-get-entry-count rib-loc)
				remove-count)))))
    rtn-entries))


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

      (let ((rib-peer1 (RIB-ENTRY-get-rib-peer rib-entry1))
	    (rib-peer2 (RIB-ENTRY-get-rib-peer rib-entry2)))
	
	;; Prefer EXTERNAL (EBGP) over INTERNAL (EBGP) (RIB-PEER-get-internal-external peer)
	(let ((int1 (RIB-PEER-get-internal-external rib-peer1))
	      (int2 (RIB-PEER-get-internal-external rib-peer2)))

	  (if (and (eq int1 'EXTERNAL)
		   (eq int2 'INTERNAL))
	      (return-from RIB-ENTRY-best-path rib-entry1))
	  (if (and (eq int2 'EXTERNAL)
		   (eq int1 'INTERNAL))
	      (return-from RIB-ENTRY-best-path rib-entry2)))
	      
	;; Prefer lowest router-id (RIB-PEER-get-router-id peer)
	(let ((router-id1 (RIB-PEER-get-router-id rib-peer1))
	      (router-id2 (RIB-PEER-get-router-id rib-peer2)))

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

	;; Prefer the peer with lowest IP address (RIB-PEER-get-ip-address peer)
	(let ((ip-addr1 (RIB-PEER-get-ip-address rib-peer1))
	      (ip-addr2 (RIB-PEER-get-ip-address rib-peer2)))

	  (if (TL-greater-than-p ip-addr1 ip-addr2)
	      rib-entry2
	      rib-entry1)))))
  
