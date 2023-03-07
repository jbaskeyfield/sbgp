;;; general NEXT-HOP object tagged with AFI/SAFI for use with both RFC4271 next-hop fields and within MP_REACH path attributes 

(in-package :sbgp)

(defun NEXT-HOP-get-afisafi (obj) "-> AFISAFI"                 (cadddr obj))
(defun NEXT-HOP-get-address (obj) "-> [ IPV4 | IPV6 | BYTES ]" (car (cddddr obj)))

(defun NEXT-HOP-zhash (octet-offset obj)
  (logxor (zhash-integer u16 octet-offset (PATH-ATTRIB-get-attribute-type-field obj))
	  (zhash-integer u16 (+ 2 octet-offset) (PATH-ATTRIB-get-attribute-length obj))
	  (zhash-integer u24 (+ 4 octet-offset) (NEXT-HOP-get-afisafi obj))
	  (case (AFISAFI-get-afi (NEXT-HOP-get-afisafi obj))
	    (+AFI-ipv4+ (IPV4-zhash (+ 7 octet-offset) (NEXT-HOP-get-address obj)))
	    (+AFI-ipv6+ (IPV6-zhash (+ 7 octet-offset) (NEXT-HOP-get-address obj)))
	    (t (BYTES-zhash (+ 7 octet-offset) (NEXT-HOP-get-address obj))))))

(defun NEXT-HOP-make (attribute-type attribute-length afisafi address)
  (let ((obj (list 'NEXT-HOP
		   attribute-type
		   attribute-length
		   afisafi
		   address)))
    (if *path-attrib-cache*
	(CACHE-lookup *path-attrib-cache*
		      obj
		      (NEXT-HOP-zhash 0 obj))
	obj)))

(defun NEXT-HOP-make-new (address)
  (case (TL-get-name address)
    (IPV4 (NEXT-HOP-make #x4003 4 +AFISAFI-ipv4-unicast+ address))
    (IPV6 (NEXT-HOP-make #x4003 16 +AFISAFI-ipv6-unicast+ address))))

(defun NEXT-HOP-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (= (length obj) 5)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))
       (u24-p (NEXT-HOP-get-afisafi obj))
       (or (typep (NEXT-HOP-get-address obj) 'IPV4)
	   (typep (NEXT-HOP-get-address obj) 'IPV6)
	   (typep (NEXT-HOP-get-address obj) 'BYTES))))

(deftype NEXT-HOP () '(and (cons (member NEXT-HOP)) (satisfies NEXT-HOP-valid1-p)))

(defun NEXT-HOP-typep (obj) (typep obj 'NEXT-HOP))

(defun NEXT-HOP-valid2-p (obj)
  "Test list values are within allowed range"
  (and (eq 'NEXT-HOP (TL-get-name obj))
       (= #x4003 (logandc1 #x1000 (PATH-ATTRIB-get-attribute-type-field obj)))   ; type field masked with extended length bit
       ))

(defun NEXT-HOP-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (+ (if (PATH-ATTRIB-get-flag-extended-len-bit obj)                      ; attribute-type & attribute-length [ 3 or 4 bytes ]
	 4
	 3)
     (case (TL-get-name (NEXT-HOP-get-address obj))
       (IPV4  4)
       (IPV6  16)
       (BYTES (BYTES-get-io-rw-octets (NEXT-HOP-get-address obj)))
       (t 0))))

(defun NEXT-HOP-io-read (attribute-type attribute-length afisafi stream-in)
  (NEXT-HOP-make attribute-type
		 attribute-length
		 afisafi
		 (cond ((and (= (AFISAFI-get-afi afisafi) 1)
			     (= attribute-length 4))
			(IPV4-io-read stream-in))
		       ((and (= (AFISAFI-get-afi afisafi) 2)
			     (= attribute-length 16))
			(IPV6-io-read stream-in))
		       (t
			(BYTES-io-read attribute-length stream-in)))))

(defun NEXT-HOP-io-write (obj stream-out)
  (let ((attribute-type (PATH-ATTRIB-get-attribute-type-field obj))
        (attribute-length (PATH-ATTRIB-get-attribute-length obj))
	(address (NEXT-HOP-get-address obj)))

    (io-write-uNbe u16 attribute-type stream-out)
    (if (uN-bit-set-p u16 attribute-type 3)                     ; extended-length bit set
	(io-write-uNbe u16 attribute-length stream-out)
	(io-write-uNbe u8 attribute-length stream-out))

    (case (TL-get-name address)
       (IPV4 (IPV4-io-write address stream-out))
       (IPV6 (IPV6-io-write address stream-out))
       (BYTES (BYTES-io-write address stream-out)))))


(defun NEXT-HOP-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X #x~V,'0X #x~6,'0X ~W)"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (if (PATH-ATTRIB-get-flag-extended-len-bit obj) 4 2) 
	  (PATH-ATTRIB-get-attribute-length obj)                 ; u8|u16
	  (NEXT-HOP-get-afisafi obj)                             ; AFISAFI
	  (NEXT-HOP-get-address obj)))                           ; [ IPV4 | IPV6 | BYTES ] 

(set-pprint-dispatch '(cons (member NEXT-HOP)) #'NEXT-HOP-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun NEXT-HOP-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~4,'0X ~D #x~6,'0X~) ~W]"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (PATH-ATTRIB-get-attribute-length obj)                 ; u8|u16
	  (NEXT-HOP-get-afisafi obj)                             ; AFISAFI
	  (NEXT-HOP-get-address obj)))                           ; [ IPV4 | IPV6 | BYTES ] 

(set-pprint-dispatch '(cons (member NEXT-HOP)) #'NEXT-HOP-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'NEXT-HOP 'pprint-1) #'NEXT-HOP-pprint-1)
(setf (get 'NEXT-HOP 'pprint-2) #'NEXT-HOP-pprint-2)
(setf (get 'NEXT-HOP 'zhash) #'NEXT-HOP-zhash)

