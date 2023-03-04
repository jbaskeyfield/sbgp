;;; MULTI_EXIT_DISC inherits OBJECT PATH-ATTRIB
;;; within the path attribute, is a 32 bit unsigned integer

#|
5.1.4.  MULTI_EXIT_DISC

   The MULTI_EXIT_DISC is an optional non-transitive attribute that is
   intended to be used on external (inter-AS) links to discriminate
   among multiple exit or entry points to the same neighboring AS.  The
   value of the MULTI_EXIT_DISC attribute is a four-octet unsigned
   number, called a metric.  All other factors being equal, the exit
   point with the lower metric SHOULD be preferred.  If received over
   EBGP, the MULTI_EXIT_DISC attribute MAY be propagated over IBGP to
   other BGP speakers within the same AS (see also 9.1.2.2).  The
   MULTI_EXIT_DISC attribute received from a neighboring AS MUST NOT be
   propagated to other neighboring ASes.

   A BGP speaker MUST implement a mechanism (based on local
   configuration) that allows the MULTI_EXIT_DISC attribute to be
   removed from a route.  If a BGP speaker is configured to remove the

   MULTI_EXIT_DISC attribute from a route, then this removal MUST be
   done prior to determining the degree of preference of the route and
   prior to performing route selection (Decision Process phases 1 and
   2).

   An implementation MAY also (based on local configuration) alter the
   value of the MULTI_EXIT_DISC attribute received over EBGP.  If a BGP
   speaker is configured to alter the value of the MULTI_EXIT_DISC
   attribute received over EBGP, then altering the value MUST be done
   prior to determining the degree of preference of the route and prior
   to performing route selection (Decision Process phases 1 and 2).  See
   Section 9.1.2.2 for necessary restrictions on this.

|#
(in-package :sbgp)

(defun MULTI-EXIT-DISC-get-value (obj) "-> u32" (cadddr obj))

(defun MULTI-EXIT-DISC-zhash (octet-offset obj)
  (zhash-tagged-list '(2 2 4) octet-offset obj))

(defun MULTI-EXIT-DISC-make (attribute-type attribute-length value)
  (let ((obj (list 'MULTI-EXIT-DISC
		   attribute-type
		   attribute-length
		   value)))
    (if *path-attrib-cache*
	(CACHE-lookup *path-attrib-cache*
		      obj
		      (MULTI-EXIT-DISC-zhash 0 obj))
	obj)))

(defun MULTI-EXIT-DISC-make-new (value-u32)
  (MULTI-EXIT-DISC-make #x8004  ; Optional-bit(#x8000) set; Transitive-bit(#x4000) not set; Partial-bit(#x2000) not set; Extended-Length(#x1000) not set; Type 4
			4
			value-u32))

(defun MULTI-EXIT-DISC-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (= (length obj) 4)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))
       (u32-p (MULTI-EXIT-DISC-get-value obj))))

(deftype MULTI-EXIT-DISC () '(and (cons (member MULTI-EXIT-DISC)) (satisfies MULTI-EXIT-DISC-valid1-p)))

(defun MULTI-EXIT-DISC (obj) (typep obj 'MULTI-EXIT-DISC))

(defun MULTI-EXIT-DISC-valid2-p (obj)
   "Test list values are within allowed range"
  (and (eq 'MULTI-EXIT-DISC (TL-get-name obj))
       (= #x8004 (logandc1 #x1000 (PATH-ATTRIB-get-attribute-type-field obj)))   ; type field masked with extended length bit
       (= 4 (PATH-ATTRIB-get-attribute-length obj))))

(defun MULTI-EXIT-DISC-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (+ (if (PATH-ATTRIB-get-flag-extended-len-bit obj)                      ; attribute-type & attribute-length [ 3 or 4 bytes ]
	 4
	 3)
     4))

(defun MULTI-EXIT-DISC-io-read (attribute-type attribute-length stream-in)
  (MULTI-EXIT-DISC-make attribute-type
			attribute-length
			(io-read-uNbe u32 stream-in)))

(defun MULTI-EXIT-DISC-io-write (obj stream-out)
  (destructuring-bind (attribute-type attribute-length value)
      (cdr obj)
    (io-write-uNbe u16 attribute-type stream-out)
    (if (uN-bit-set-p u16 attribute-type 3)                     ; extended-length bit set
	(io-write-uNbe u16 attribute-length stream-out)
	(io-write-uNbe u8 attribute-length stream-out))
    (io-write-uNbe u32 value stream-out)))


  
(defun MULTI-EXIT-DISC-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X #x~V,'0X #x~4,'0X)"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (if (PATH-ATTRIB-get-flag-extended-len-bit obj) 4 2) 
	  (PATH-ATTRIB-get-attribute-length obj)                 ; u8|u16
	  (MULTI-EXIT-DISC-get-value obj)))                      ; u32

(set-pprint-dispatch 'MULTI-EXIT-DISC #'MULTI-EXIT-DISC-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun MULTI-EXIT-DISC-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~4,'0X ~D ~D]~)"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (PATH-ATTRIB-get-attribute-length obj)                 ; u8|u16
	  (MULTI-EXIT-DISC-get-value obj)))                      ; u32

(set-pprint-dispatch 'MULTI-EXIT-DISC #'MULTI-EXIT-DISC-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'MULTI-EXIT-DISC 'pprint-1) #'MULTI-EXIT-DISC-pprint-1)
(setf (get 'MULTI-EXIT-DISC 'pprint-2) #'MULTI-EXIT-DISC-pprint-2)
(setf (get 'MULTI-EXIT-DISC 'zhash) #'MULTI-EXIT-DISC-zhash)
