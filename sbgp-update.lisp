;;; internal format for update messages
;;; functions used by NETIORX/TX threads for thranslating BGP-UPDATE messages into internal SBGP format
;;; in RX thread BGP-UPDATE->SBGP-UPDATE takes single bgp-update message and if MP-REACH/UNREACH records, extracts NLRI records
;;; in TX thread SBGP-UPDATE->BGP-UPDATE returns list of one or more RFC4271 formatted bgp-update messages
;;; if other address family, returns list with NLRIs embedded in MP-REACH records.
;;; Note: SBGP-UPDATES received from peer thread may have more NLRIs than can be contained in single 4k update message as will be collected by table scan, whereas one SBGP update is sent to peer thread for every update received from network.

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

(defparameter +SBGP-path-attributes+ (list 'ORIGIN 'AS-PATH 'NEXT-HOP 'MULTI-EXIT-DISC 'LOCAL-PREF 'ATOMIC-AGGREGATE 'AGGREGATOR 'COMMUNITY 'LARGE-COMMUNITY 'ORIGINATOR-ID 'CLUSTER-LIST 'PATH-ATTRIB-UNKNOWN))

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
		       ((member value +SBGP-path-attributes+)
			(push node path-attrib-list))
		       (t
			(if (consp (cdr node))
			    (tree-walk (cdr node))))))))
      (tree-walk bgp-update)

      (SBGP-UPDATE-make nlri-withdrawl-list       ; list of NLRI-WITHDRAWL
			nlri-list                 ; list of NLRI
			(sort path-attrib-list    ; list of other PATH-ATTRIB
			      #'PATH-ATTRIB-type-subfield-less-than-p)))))

(defun PA-LIST->SBGP-PA-LIST (path-attrib-list-in)
  "Walks tree of PATH-ATTRIB-LIST and returns lists of path attributes in +SBGP-path-attributes+ (excludes MP-REACH/UNREACH)"
  (let ((path-attrib-list-out nil))
    (labels ((tree-walk (node)
	       (if (consp (car node))
		   (tree-walk (car node)))
	       (let ((value (car node)))
		 (cond ((member value +SBGP-path-attributes+)
			(push node path-attrib-list-out))
		       (t
			(if (consp (cdr node))
			    (tree-walk (cdr node))))))))
      (tree-walk path-attrib-list-in)

      (sort path-attrib-list-out    ; list of other PATH-ATTRIB
	    #'<
	    :key #'PATH-ATTRIB-get-type-subfield))))

#|
(defun SBGP-UPDATE->BGP-UPDATE (4-octet-asn-flag sbgp-update)
  "Returns list of BGP-UPDATE messages"

  |#
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
