;;; AGGREGATOR object, within the path attribute is a list of two elements (<AS-number> <IP-Address>)

#|
RFC 4271
5.1.7.  AGGREGATOR

   AGGREGATOR is an optional transitive attribute, which MAY be included
   in updates that are formed by aggregation (see Section 9.2.2.2).  A
   BGP speaker that performs route aggregation MAY add the AGGREGATOR
   attribute, which SHALL contain its own AS number and IP address.  The
   IP address SHOULD be the same as the BGP Identifier of the speaker.

|#

(in-package :sbgp)


(defun AGGREGATOR-get-as-number (obj)   "-> [ u16 | u32 ]"  (cadddr obj))
(defun AGGREGATOR-get-ip-address (obj)  "-> IPV4"           (car (cddddr obj)))

(defun AGGREGATOR-zhash (octet-offset obj)
  (logxor (zhash-integer u16 octet-offset (PATH-ATTRIB-get-attribute-type-field obj))
	  (zhash-integer u16 (+ 2 octet-offset) (PATH-ATTRIB-get-attribute-length obj))
	  (zhash-integer u32 (+ 4 octet-offset) (AGGREGATOR-get-as-number obj))
	  (IPV4-zhash (+ 8 octet-offset) (AGGREGATOR-get-ip-address obj))))

(defun AGGREGATOR-make (attribute-type attribute-length asn ipv4-address)
  (let ((obj (list 'AGGREGATOR
		   attribute-type
		   attribute-length 
		   asn
		   ipv4-address)))
    (if *path-attrib-cache*
	(CACHE-lookup *path-attrib-cache*
		      obj
		      (AGGREGATOR-zhash 0 obj))
	obj)))

(defun AGGREGATOR-make-new (asn ipv4-address)
  (AGGREGATOR-make #xC007 ; Optional-bit(#x8000) set; Transitive-bit(#x4000) set; Partial-bit(#x2000) not set; Extended-Length(#x1000) not set; Type 7
		   8      ; length = asn u32 + ipv4-address u32
		   asn
		   ipv4-address))
	  
(defun AGGREGATOR-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (= (length obj) 4)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))
       (u32-p (AGGREGATOR-get-as-number obj))
       (typep (AGGREGATOR-get-ip-address obj) 'IPV4)))

(deftype AGGREGATOR () '(and (cons (member AGGREGATOR)) (satisfies AGGREGATOR-valid1-p)))

(defun AGGREGATOR-typep (obj) (typep obj 'AGGREGATOR))

(defun AGGREGATOR-valid2-p (obj)
  "Test list values are within allowed range"
  (and (eq 'AGGREGATOR (TL-get-name obj))
       (= #xC007 (logandc1 #x1000 (PATH-ATTRIB-get-attribute-type-field obj)))   ; type field masked with extended length bit
       (= 8 (PATH-ATTRIB-get-attribute-length obj))))

(defun AGGREGATOR-get-io-rw-octets (4-octet-asn-flag obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (+ (if (PATH-ATTRIB-get-flag-extended-len-bit obj)      ; attribute-type & attribute-length [ 3 or 4 bytes ]
	 4
	 3)
     (if 4-octet-asn-flag
	 4
	 2)
     4))

(defun AGGREGATOR-io-read (4-octet-asn-flag attribute-type attribute-length stream-in)
  (AGGREGATOR-make attribute-type
		   attribute-length
		   (if 4-octet-asn-flag
		       (io-read-uNbe u32 stream-in)
		       (io-read-uNbe u16 stream-in))
		   (IPV4-io-read stream-in)))

(defun AGGREGATOR-io-write (4-octet-asn-flag obj stream-out)
  (destructuring-bind (attribute-type attribute-length asn ipv4-address)
      (cdr obj)
    (io-write-uNbe u16 attribute-type stream-out)
    (if (uN-bit-set-p u16 attribute-type 3)           ; extended length bit set
	(io-write-uNbe u16 attribute-length stream-out)     
	(io-write-uNbe u8 attribute-length stream-out))

    (if 4-octet-asn-flag
	(io-write-uNbe u32 asn stream-out)
	(io-write-uNbe u16 asn stream-out))
    
    (IPV4-io-write ipv4-address stream-out)))

(defun AGGREGATOR-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X #x~V,'0X #x~8,'0X ~/sbgp:ipv4-pprint-1/)"
	  (TL-get-name obj)                                      ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (if (PATH-ATTRIB-get-flag-extended-len-bit obj) 4 2) 
	  (PATH-ATTRIB-get-attribute-length obj)                 ; [ u8 | u16 ]
	  (AGGREGATOR-get-as-number obj)                         ; [ u16 | u32 ]
	  (AGGREGATOR-get-ip-address obj)))                      ; IPV4

(set-pprint-dispatch '(cons (member AGGREGATOR)) #'AGGREGATOR-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun AGGREGATOR-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~4,'0X ~D [~/sbgp:u32-pprint-asn/] ~/sbgp:ipv4-pprint-2/]~)"
	  (TL-get-name obj)                                      ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (PATH-ATTRIB-get-attribute-length obj)                 ; [ u8 | u16 ]
	  (AGGREGATOR-get-as-number obj)                         ; [ u16 | u32 ]
	  (AGGREGATOR-get-ip-address obj)))                      ; IPV4

(set-pprint-dispatch '(cons (member AGGREGATOR)) #'AGGREGATOR-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'AGGREGATOR 'pprint-1) #'AGGREGATOR-pprint-1)
(setf (get 'AGGREGATOR 'pprint-2) #'AGGREGATOR-pprint-2)
(setf (get 'AGGREGATOR 'zhash) #'AGGREGATOR-zhash)
