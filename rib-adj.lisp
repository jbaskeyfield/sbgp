(in-package :sbgp)
#|
slot elements = list of RIB-ADJ-ENTRY ((nlri pa-list)* ... )

each rib-adj entry contains a single nlri (nlri object with additional hash value as cadr) and pa-list-h (list of path-attrib objects with hash value as cadr)
|#

(defun RIB-ADJ-ENTRY-get-nlri (entry)          "-> NLRI"                  (car entry))
(defun RIB-ADJ-ENTRY-get-pa-list (entry)       "-> list of PATH-ATTRIB"   (cadr entry))

(defun RIB-ADJ-ENTRY-make (nlri pa-list)  ; TODO can add code here to cache object and subobjects
  ;; object is untagged list (list nlri pa-list)
  (list (if *nlri-cache*
            (NLRI-CACHE-ro-lookup *nlri-cache*
				  nlri
				  (NLRI-zhash 0 nlri))
	    nlri)
	(if *path-attrib-list-cache*
	    (CACHE-lookup *path-attrib-list-cache*
			  pa-list
			  (PATH-ATTRIB-zhash-list pa-list))
	    pa-list)))

(defmacro RIB-ADJ-get-name (rib-adj)                    `(svref ,rib-adj 0))
(defmacro RIB-ADJ-get-num-bits (rib-adj)                `(svref ,rib-adj 1))
(defmacro RIB-ADJ-get-table-size (rib-adj)              `(svref ,rib-adj 2))
(defmacro RIB-ADJ-get-empty-slot-count (rib-adj)        `(svref ,rib-adj 3))
(defmacro RIB-ADJ-get-update-count (rib-adj)            `(svref ,rib-adj 4))
(defmacro RIB-ADJ-get-entries-count (rib-adj)           `(svref ,rib-adj 5))
(defmacro RIB-ADJ-get-table-mask (rib-adj)              `(svref ,rib-adj 6))
(defmacro RIB-ADJ-get-rib-adj-table (rib-adj)           `(svref ,rib-adj 7))

(defun RIB-ADJ-make (table-size-num-bits)
  (let ((rib-adj (make-array 8 :initial-element nil))
	(table-size (ash 1 table-size-num-bits)))

    (setf (RIB-ADJ-get-name rib-adj)                    'RIB-ADJ)                 
    (setf (RIB-ADJ-get-num-bits rib-adj)                table-size-num-bits)     
    (setf (RIB-ADJ-get-table-size rib-adj)              table-size)     
    (setf (RIB-ADJ-get-empty-slot-count rib-adj)        table-size)
    (setf (RIB-ADJ-get-update-count rib-adj)            0)
    (setf (RIB-ADJ-get-entries-count rib-adj)           0)
    (setf (RIB-ADJ-get-table-mask rib-adj)              (1- table-size))     
    (setf (RIB-ADJ-get-rib-adj-table rib-adj)           (make-array table-size :initial-element nil))

    rib-adj))

(defun RIB-ADJ-valid1-p (rib-adj)
  (and (= (length rib-adj) 8)))

(deftype RIB-ADJ () '(and (cons (member RIB-ADJ)) (satisfies RIB-ADJ-valid1-p)))

(defun RIB-ADJ-clear(rib-adj)
  (setf (RIB-ADJ-get-empty-slot-count rib-adj)        (RIB-ADJ-get-table-size rib-adj))
  (setf (RIB-ADJ-get-update-count rib-adj)            0)
  (setf (RIB-ADJ-get-entries-count rib-adj)           0)     
  (fill (RIB-ADJ-get-rib-adj-table rib-adj)           nil))


(defun RIB-ADJ-add-entry (rib-adj rib-adj-entry)
  "Adds passed RIB-ADJ-ENTRY object (untagged list of (nlri pa-list)) to the hash table RIB-ADJ. This new RIB entry is added to RIB-ADJ if nlri is not already present or if nlri exists but has a different pa-list.
Returns three values 
1. RIB-ADJ-ENTRY = the passed argument RIB-ADJ-ENTRY
2. [ :added-new-entry | :duplicate-entry-found | :replaced-existing-entry ]   
  :added-new-entry         => entry successfully added to table (either added to empty slot or appened to an existing slot list)
  :duplicate-entry-found   => an EQUAL copy of the passed NLRI/PA-LIST was found in the table (no changes made to RIB-ADJ)
  :replaced-existing-entry => entry found with same NLRI in table but different PA-LIST
3. [ nil (if :added-new-entry or :duplicate-entry-found) |
   previous-value (value of RIB-ADJ-ENTRY object if :replaced-existing-entry) ]"

  (incf (RIB-ADJ-get-update-count rib-adj))
  
  (let* ((nlri (RIB-ADJ-ENTRY-get-nlri rib-adj-entry))
	 (pa-list (RIB-ADJ-ENTRY-get-pa-list rib-adj-entry))
	 (table-index (logand (NLRI-zhash 0 nlri)                           ; get index to hash table by masking with hash 
			      (RIB-ADJ-get-table-mask rib-adj)))           
	 (table-slot-value (svref (RIB-ADJ-get-rib-adj-table rib-adj)       ; table lookup to see if slot already occupied
				  table-index)))

    (cond ((null table-slot-value)
	   ;; slot is empty. push RIB-ADJ-ENTRY onto table slot value,
	   ;; decrement empty-slot-count, and return
	   (push rib-adj-entry
		 (svref (RIB-ADJ-get-rib-adj-table rib-adj)
			table-index))
	   (incf (RIB-ADJ-get-entries-count rib-adj))
	   (decf (RIB-ADJ-get-empty-slot-count rib-adj))
	   (values rib-adj-entry :added-new-entry nil))
	  (t
	   ;; slot already occupied by list
	   ;; if nlri already exists in list, replace the RIB-ADJ-ENTRY
	   ;; if nlri doesnt exist, push RIB-ADJ-ENTRY to head of list

	   ;; 1. search for RIB-ADJ-ENTRY with matching nlri-hash
	   (let ((matching-nlri-record (find nlri
					     table-slot-value
					     :key #'RIB-ADJ-ENTRY-get-nlri
					     :test #'equal)))
	     ;; (format t "~&RECORD-MATCHING-HASH:~S~%" record-matching-hash)
	     
	     (cond (matching-nlri-record
		    ;; 2. if NLRI found in list,
		    (cond ((equal (RIB-ADJ-ENTRY-get-pa-list matching-nlri-record)
				  pa-list)
			   ;; and entry is EQUAL for nlri and pa-list, do not update table
			   (values rib-adj-entry :duplicate-entry nil))
			  (t 
			   ;; and entry is NOT EQUAL for pa-list, replace entry in list
			   (setf (svref (RIB-ADJ-get-rib-adj-table rib-adj) table-index)
				 (cons rib-adj-entry
				       (remove matching-nlri-record
					       table-slot-value)))
			   (values rib-adj-entry :replaced-existing-entry matching-nlri-record))))
		   (t
		    ;; 3. otherwise, nlri not found in table-slot-value. RIB-ADJ-ENTRY needs to be pushed to slot list as a new record
		    (push rib-adj-entry
			  (svref (RIB-ADJ-get-rib-adj-table rib-adj) table-index))
		    (incf (RIB-ADJ-get-entries-count rib-adj))
		    ;;  (format t "~&PUSHING TO OCCUPIED SLOT~%")
		    (values rib-adj-entry :added-new-entry nil))))))))

(defun RIB-ADJ-remove-entry (rib-adj rib-adj-entry)
  "Returns RIB-ADJ-ENTRY if entry was found and removed from table RIB-ADJ. Returns NIL if not found."
  (incf (RIB-ADJ-get-update-count rib-adj))
  (let* ((nlri (RIB-ADJ-ENTRY-get-nlri rib-adj-entry))
	 (table-index (logand (NLRI-zhash 0 nlri)                                           ; calculate hash table index from zhash
			      (RIB-ADJ-get-table-mask rib-adj)))           
	 (table-slot-value (svref (RIB-ADJ-get-rib-adj-table rib-adj)                       ; ; get list of rib-adj-entry from slot 
				  table-index)))
    (cond (table-slot-value
	   (let ((matching-rib-adj-entry (find rib-adj-entry
					       table-slot-value
					       :test #'equal)))
	     (cond (matching-rib-adj-entry                                                  ; if found
		    (cond ((= (length table-slot-value) 1)                                  ;  if is sole entry in the hash table slot
			   (setf (svref (RIB-ADJ-get-rib-adj-table rib-adj) table-index)   
				 nil)                                                       ;   set has table slot to nil
			   (decf (RIB-ADJ-get-entries-count rib-adj))                       ;   decrement entries count
			   (incf (RIB-ADJ-get-empty-slot-count rib-adj))                    ;   increment empty slot count
			   matching-rib-adj-entry)                                          ;   return deleted rib-adj-entry
			  (t                                                                ;  otherwise, 
			   (setf (svref (RIB-ADJ-get-rib-adj-table rib-adj) table-index)   
				 (remove matching-rib-adj-entry
					 table-slot-value))                                 ;   remove entry from list
			   (decf (RIB-ADJ-get-entries-count rib-adj))                       ;   decrement entry count
			   matching-rib-adj-entry)))                                       ;   return deleted rib-adj-entry
		   
		   (t                                                                       ; otherwise, entry not found
		    nil)))))))                                                              ;  return nil.
	  

(defun RIB-ADJ-remove-nlri (rib-adj nlri)
  "Removes rib-adj-entry from table RIB-ADJ that matches the passed NLRI. 
Returns RIB-ADJ-ENTRY removed from the table if found. Returns nil if not found."

  (incf (RIB-ADJ-get-update-count rib-adj))
  
  (let*  ((table-index (logand (NLRI-zhash 0 nlri)                                          ; calculate hash table index from zhash
			       (RIB-ADJ-get-table-mask rib-adj)))          
	  (table-slot-value (svref (RIB-ADJ-get-rib-adj-table rib-adj)                      ; get list of rib-adj-entry from slot 
				   table-index)))

    (cond (table-slot-value                           
	   (let ((matching-rib-adj-entry (find nlri                                         ; search for nlri in list of rib-adj-entries
					       table-slot-value                             ; (equality test ignores car so NLRI equal to NLRI-WITHDRAWL)
					       :key #'RIB-ADJ-ENTRY-get-nlri
					       :test #'NLRI/NLRI-WITHDRAWL-equal))) 
	     (cond (matching-rib-adj-entry                                                  ; if found
		    (cond  ((= (length table-slot-value) 1)                                 ;  if is sole entry in the hash table slot
			    (setf (svref (RIB-ADJ-get-rib-adj-table rib-adj) table-index)   
				  nil)                                                      ;   set has table slot to nil
			    (decf (RIB-ADJ-get-entries-count rib-adj))                      ;   decrement entries count
			    (incf (RIB-ADJ-get-empty-slot-count rib-adj))                   ;   increment empty slot count
			    matching-rib-adj-entry)                                         ;   return deleted rib-adj-entry
			   (t                                                               ;  otherwise, 
			    (setf (svref (RIB-ADJ-get-rib-adj-table rib-adj) table-index)   
				  (remove matching-rib-adj-entry
				          table-slot-value))                                ;   remove entry from list
			    (decf (RIB-ADJ-get-entries-count rib-adj))                      ;   decrement entry count
			    matching-rib-adj-entry)))                                       ;   return deleted rib-adj-entry
		   (t                                                                       ; otherwise, entry not found
		    nil)))))))                                                              ;  return nil.

(defun RIB-ADJ-collect-entries (rib-adj &key (test-fn #'identity))
  "Iterates over entire RIB-ADJ table and collects rib-adj-entry objects for which TEST-FN returns true"
  (let ((rtn-rib-adj-entries nil))
    (loop for slot across (RIB-ADJ-get-rib-adj-table rib-adj)
	  when slot
	    do (loop for rib-adj-entry in slot
		     when (funcall test-fn rib-adj-entry)
		       do (push rib-adj-entry rtn-rib-adj-entries)))
    rtn-rib-adj-entries))

;; TODO. collecting and sending of announce/withdrawls needs to be changed from per entry to processing batch. currently this function has to put single nlri in list
(defun RIB-ADJ-ENTRY->BGP-UPDATE-MESSAGE (4-octet-asn-flag is-announcement rib-adj-entry)  
  (BGP-MESSAGE-make-new (BGP-UPDATE-make-new 4-octet-asn-flag
					 (if is-announcement
					     nil
					     (list (RIB-ADJ-ENTRY-get-nlri rib-adj-entry)))
					 (RIB-ADJ-ENTRY-get-pa-list rib-adj-entry)
					 (if is-announcement
					     (list (RIB-ADJ-ENTRY-get-nlri rib-adj-entry))
					     nil))))
  
