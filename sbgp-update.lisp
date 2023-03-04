;;; internal format for update messages

(in-package :sbgp)

(defun SBGP-UPDATE-make (nlri-withrawl-list nlri-list path-attrib-list)
  (list 'SBGP-UPDATE
	nlri-withrawl-list     ; list of NLRI-WITHDRAWL
	nlri-list              ; list of NLRI
	path-attrib-list))     ; list of PATH-ATTRIB

(defun SBGP-UPDATE-get-name (obj)                "-> symbol"                    (car obj))
(defun SBGP-UPDATE-get-nlri-withdrawl-list (obj) "-> list of NLRI-WITHDRAWL"    (cadr obj))
(defun SBGP-UPDATE-get-nlri-list (obj)           "-> list of NLRI"              (caddr obj))
(defun SBGP-UPDATE-get-path-attrib-list (obj)    "-> list of PATH-ATTRB"        (cadddr obj))

(defun SBGP-UPDATE-valid1-p (obj)
  (= (length obj) 4))

(deftype SBGP-UPDATE () '(and (cons (member SBGP-UPDATE)) (satisfies SBGP-UPDATE-valid1-p)))

(defun BGP-UPDATE->SBGP-UPDATE (bgp-update)
  "Walks tree BGP-UPDATE and returns lists of NLRI-WITHDRAWL, NLRI, NEXT-HOP, and other PATH-ATTRIB (sorted in attribute type order)"
  (let ((nlri-withdrawl-list nil)
	(nlri-list nil)
	(path-attrib-list nil))
    (labels ((tree-walk (node)
	       (if (consp (car node))
		   (tree-walk (car node)))
	       (let ((value (car node)))
		 (cond ((eq value 'NLRI-WITHDRAWL )
			(push node
			      nlri-withdrawl-list))
		       ((eq value 'NLRI)
			(push node
			      nlri-list))
		       ((member value '(NEXT-HOP ORIGIN AS-PATH MULTI-EXIT-DISC LOCAL-PREF ATOMIC-AGGREGATE AGGREGATOR COMMUNITY LARGE-COMMUNITY PATH-ATTRIB-UNKNOWN))
			(push node path-attrib-list))
		       (t
			(if (consp (cdr node))
			    (tree-walk (cdr node))))))))
      (tree-walk bgp-update)

      (SBGP-UPDATE-make nlri-withdrawl-list       ; list of NLRI-WITHDRAWL
			nlri-list                 ; list of NLRI
			(sort path-attrib-list    ; list of other PATH-ATTRIB
			      #'<
			      :key #'PATH-ATTRIB-get-type-subfield)))))


;; (defun BGP-UPDATE-make-new (4-octet-asn-flag mp-extensions-flag withdrawn-routes path-attributes nlri-list)
#|
(defun SBGP-UPDATE->BGP-UPDATE (4-octet-asn-flag mp-extensions-flag sbgp-update)
  (cond (mp-extensions-flag
	 (BGP-UPDATE-make-new 4-octet-asn-flag
			      nil
			      (sort (append (if (SBGP-UPDATE-get-nlri-withdrawl-list sbgp-update)
						(cons (MP-UNREACH-NLRI-make-new (SBGP-UPDATE-get-nlri-withdrawl-list sbgp-update)) nil))
					    (if (SBGP-UPDATE-get-nlri-list sbgp-update)
						(cons (MP-REACH-NLRI-make-new (SBGP-UPDATE-get-next-hop-list sbgp-update)
									      (SBGP-UPDATE-get-nlri-list sbgp-update)) nil))
					    (SBGP-UPDATE-get-path-attrib-list sbgp-update))
				    #'<
				    :key #'PATH-ATTRIB-get-type-subfield)
			      nil))
	(t
	 (BGP-UPDATE-make-new 4-octet-asn-flag
			      (SBGP-UPDATE-get-nlri-withdrawl-list sbgp-update)
			      (sort (append (SBGP-UPDATE-get-next-hop-list sbgp-update)
				            (SBGP-UPDATE-get-path-attrib-list sbgp-update))
				    #'<
				    :key #'PATH-ATTRIB-get-type-subfield)
			      (SBGP-UPDATE-get-nlri-list sbgp-update)))))
  
|#
