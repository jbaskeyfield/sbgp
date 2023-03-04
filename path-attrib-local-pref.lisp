;; LOCAL_PREF   ; [RFC4271]
#|
e) LOCAL_PREF (Type Code 5):

LOCAL_PREF is a well-known attribute that is a four-octet
unsigned integer.  A BGP speaker uses it to inform its other
internal peers of the advertising speaker's degree of
preference for an advertised route.

Usage of this attribute is defined in 5.1.5.

5.1.5.  LOCAL_PREF

LOCAL_PREF is a well-known attribute that SHALL be included in all
UPDATE messages that a given BGP speaker sends to other internal
peers.  A BGP speaker SHALL calculate the degree of preference for
each external route based on the locally-configured policy, and
include the degree of preference when advertising a route to its
internal peers.  The higher degree of preference MUST be preferred.
A BGP speaker uses the degree of preference learned via LOCAL_PREF in
its Decision Process (see Section 9.1.1).

A BGP speaker MUST NOT include this attribute in UPDATE messages it
sends to external peers, except in the case of BGP Confederations
[RFC3065].  If it is contained in an UPDATE message that is received
from an external peer, then this attribute MUST be ignored by the
receiving speaker, except in the case of BGP Confederations
[RFC3065].
|#

(in-package :sbgp)

(defun LOCAL-PREF-get-value (obj) "-> u32" (cadddr obj))

(defun LOCAL-PREF-zhash (octet-offset obj)
  (zhash-tagged-list '(2 2 4) octet-offset obj))

(defun LOCAL-PREF-make (attribute-type attribute-length value)
  (let ((obj (list 'LOCAL-PREF
		   attribute-type
		   attribute-length
		   value)))
    (if *path-attrib-cache*
	(CACHE-lookup *path-attrib-cache*
		      obj
		      (LOCAL-PREF-zhash 0 obj))
	obj)))

(defun LOCAL-PREF-make-new (value-u32)
  (LOCAL-PREF-make #x4005 ; Optional-bit(#x8000) not set; Transitive-bit(#x4000) set; Partial-bit(#x2000) not set; Extended-Length(#x1000) not set; Type 5
		   4
		   value-u32))

(defun LOCAL-PREF-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (= (length obj) 4)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))
       (u32-p (LOCAL-PREF-get-value obj))))

(deftype LOCAL-PREF () '(and (cons (member LOCAL-PREF)) (satisfies LOCAL-PREF-valid1-p)))

(defun LOCAL-PREF-typep (obj) (typep obj 'LOCAL-PREF))

(defun LOCAL-PREF-valid2-p (obj)
   "Test list values are within allowed range"
  (and (eq 'LOCAL-PREF (TL-get-name obj))
       (= #x4005 (logandc1 #x1000 (PATH-ATTRIB-get-attribute-type-field obj)))   ; type field masked with extended length bit
       (= 4 (PATH-ATTRIB-get-attribute-length obj))))

(defun LOCAL-PREF-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (+ (if (PATH-ATTRIB-get-flag-extended-len-bit obj)                      ; attribute-type & attribute-length [ 3 or 4 bytes ]
	 4
	 3)
     4))
  
(defun LOCAL-PREF-io-read (attribute-type attribute-length stream-in)
  (LOCAL-PREF-make attribute-type
		   attribute-length
		   (io-read-uNbe u32 stream-in)))

(defun LOCAL-PREF-io-write (obj stream-out)
  (destructuring-bind (attribute-type attribute-length value)
      (cdr obj)
    (io-write-uNbe u16 attribute-type stream-out)
    (if (uN-bit-set-p u16 attribute-type 3)           ; extended length bit set
	(io-write-uNbe u16 attribute-length stream-out)     
	(io-write-uNbe u8 attribute-length stream-out))
    (io-write-uNbe u32 value stream-out)))

(defun LOCAL-PREF-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X #x~V,'0X #x~4,'0X)"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (if (PATH-ATTRIB-get-flag-extended-len-bit obj) 4 2) 
	  (PATH-ATTRIB-get-attribute-length obj)                 ; u8|u16
	  (LOCAL-PREF-get-value obj)))                           ; u32

(set-pprint-dispatch '(cons (member LOCAL-PREF)) #'LOCAL-PREF-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun LOCAL-PREF-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~4,'0X ~D ~D]~)"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (PATH-ATTRIB-get-attribute-length obj)                 ; u8|u16
	  (LOCAL-PREF-get-value obj)))                           ; u32

(set-pprint-dispatch '(cons (member LOCAL-PREF)) #'LOCAL-PREF-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'LOCAL-PREF 'pprint-1) #'LOCAL-PREF-pprint-1)
(setf (get 'LOCAL-PREF 'pprint-2) #'LOCAL-PREF-pprint-2)
(setf (get 'LOCAL-PREF 'zhash) #'LOCAL-PREF-zhash)
