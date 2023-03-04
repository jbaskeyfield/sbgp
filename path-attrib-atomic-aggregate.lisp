;;; ATOMIC_AGGREGATE [RFC4271]
#|
RFC 4271
f) ATOMIC_AGGREGATE (Type Code 6)

ATOMIC_AGGREGATE is a well-known discretionary attribute of
length 0.

Usage of this attribute is defined in 5.1.6.


5.1.6.  ATOMIC_AGGREGATE

ATOMIC_AGGREGATE is a well-known discretionary attribute.

When a BGP speaker aggregates several routes for the purpose of
advertisement to a particular peer, the AS_PATH of the aggregated
route normally includes an AS_SET formed from the set of ASes from
which the aggregate was formed.  In many cases, the network
administrator can determine if the aggregate can safely be advertised
without the AS_SET, and without forming route loops.

If an aggregate excludes at least some of the AS numbers present in
the AS_PATH of the routes that are aggregated as a result of dropping
the AS_SET, the aggregated route, when advertised to the peer, SHOULD
include the ATOMIC_AGGREGATE attribute.

A BGP speaker that receives a route with the ATOMIC_AGGREGATE
attribute SHOULD NOT remove the attribute when propagating the route
to other speakers.

A BGP speaker that receives a route with the ATOMIC_AGGREGATE
attribute MUST NOT make any NLRI of that route more specific (as
defined in 9.1.4) when advertising this route to other BGP speakers.

A BGP speaker that receives a route with the ATOMIC_AGGREGATE
attribute needs to be aware of the fact that the actual path to
destinations, as specified in the NLRI of the route, while having the
loop-free property, may not be the path specified in the AS_PATH
attribute of the route.
|#

(in-package :sbgp)

(defun ATOMIC-AGGREGATE-zhash (octet-offset obj)
  (zhash-tagged-list '(2 2) octet-offset obj))

(defun ATOMIC-AGGREGATE-make (attribute-type attribute-length)
  (let ((obj (list 'ATOMIC-AGGREGATE
		   attribute-type
		   attribute-length)))
    (if *path-attrib-cache*
	(CACHE-lookup *path-attrib-cache*
		      obj
		      (ATOMIC-AGGREGATE-zhash 0 obj))
	obj)))

(defun ATOMIC-AGGREGATE-make-new ()
  (ATOMIC-AGGREGATE-make #x4006 ; Optional-bit(#x8000) not set; Transitive-bit(#x4000) set; Partial-bit(#x2000) not set; Extended-Length(#x1000) not set; Type 6
			 0))

(defun ATOMIC-AGGREGATE-valid1-p (obj)
    "Test list elements are of correct type"
  (and (consp obj)
       (= (length obj) 4)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))))

(deftype ATOMIC-AGGREGATE () '(and (cons (member ATOMIC-AGGREGATE)) (satisfies ATOMIC-AGGREGATE-valid1-p)))

(defun ATOMIC-AGGREGATE-typep (obj) (typep obj 'ATOMIC-AGGREGATE))

(defun ATOMIC-AGGREGATE-valid2-p (obj)
   "Test list values are within allowed range"
  (and (eq 'ATOMIC-AGGREGATE (TL-get-name obj))
       (= #x4006 (logandc1 #x1000 (PATH-ATTRIB-get-attribute-type-field obj)))   ; type field masked with extended length bit
       (= 0 (PATH-ATTRIB-get-attribute-length obj))))

(defun ATOMIC-AGGREGATE-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (if (PATH-ATTRIB-get-flag-extended-len-bit obj)                      ; attribute-type & attribute-length [ 3 or 4 bytes ]
      4
      3))

(defun ATOMIC-AGGREGATE-io-read (attribute-type attribute-length)
  (ATOMIC-AGGREGATE-make attribute-type attribute-length))

(defun ATOMIC-AGGREGATE-io-write (obj stream-out)
  (destructuring-bind (attribute-type attribute-length)
      (cdr obj)
    (io-write-uNbe u16 attribute-type stream-out)
    (if (uN-bit-set-p u16 attribute-type 3)           ; extended length bit set
	(io-write-uNbe u16 attribute-length stream-out)     
	(io-write-uNbe u8 attribute-length stream-out))))


(defun ATOMIC-AGGREGATE-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X #x~V,'0X)"
	  (TL-get-name obj)                                      ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (if (PATH-ATTRIB-get-flag-extended-len-bit obj) 4 2) 
	  (PATH-ATTRIB-get-attribute-length obj)))               ; u8|u16

(set-pprint-dispatch '(cons (member ATOMIC-AGGREGATE)) #'ATOMIC-AGGREGATE-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun ATOMIC-AGGREGATE-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~4,'0X ~D]~)"
	  (TL-get-name obj)                                      ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (PATH-ATTRIB-get-attribute-length obj)))               ; u8|u16

(set-pprint-dispatch '(cons (member ATOMIC-AGGREGATE)) #'ATOMIC-AGGREGATE-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'ATOMIC-AGGREGATE 'pprint-1) #'ATOMIC-AGGREGATE-pprint-1)
(setf (get 'ATOMIC-AGGREGATE 'pprint-2) #'ATOMIC-AGGREGATE-pprint-2)
(setf (get 'ATOMIC-AGGREGATE 'zhash) #'ATOMIC-AGGREGATE-zhash)
