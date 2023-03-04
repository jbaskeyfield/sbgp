(in-package :sbgp)

;;; PATH-ATTRIB-UNKNOWN

(defun PATH-ATTRIB-UNKNOWN-get-byte-blob (obj) "-> BYTES" (cadddr obj))

(defun PATH-ATTRIB-UNKNOWN-zhash (octet-offset obj)
  (logxor (zhash-integer u16 octet-offset (PATH-ATTRIB-get-attribute-type-field obj))
	  (zhash-integer u16 (+ 2 octet-offset) (PATH-ATTRIB-get-attribute-length obj))
	  (BYTES-zhash (+ 4 octet-offset) (PATH-ATTRIB-UNKNOWN-get-byte-blob obj))))

(defun PATH-ATTRIB-UNKNOWN-make (attribute-type attribute-length byte-blob)
  (let ((obj (list 'PATH-ATTRIB-UNKNOWN
		   attribute-type
		   attribute-length
		   byte-blob)))
    (if *path-attrib-cache*
	(CACHE-lookup *path-attrib-cache*
		      obj
		      (PATH-ATTRIB-UNKNOWN-zhash 0 obj))
	obj)))

(defun PATH-ATTRIB-UNKNOWN-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (= (length obj) 4)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))
       (BYTES-typep (PATH-ATTRIB-UNKNOWN-get-byte-blob obj))))

(deftype PATH-ATTRIB-UNKNOWN () '(and (cons (member PATH-ATTRIB-UNKNOWN)) (satisfies PATH-ATTRIB-UNKNOWN-valid1-p)))
(defun PATH-ATTRIB-UNKNOWN-typep (obj) (typep obj 'PATH-ATTRIB-UNKNOWN))

(defun PATH-ATTRIB-UNKNOWN-valid2-p (obj)
  "Test list values are within allowed range"
  (and (eq 'PATH-ATTRIB-UNKNOWN (TL-get-name obj))
       (= (PATH-ATTRIB-get-attribute-length obj)
	  (BYTES-get-length-octets (PATH-ATTRIB-UNKNOWN-get-byte-blob obj)))))

(defun PATH-ATTRIB-UNKNOWN-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (+ (if (PATH-ATTRIB-get-flag-extended-len-bit obj)                      ; attribute-type & attribute-length [ 3 or 4 bytes ]
	 4
	 3)
     (BYTES-get-io-rw-octets (PATH-ATTRIB-UNKNOWN-get-byte-blob obj))))   ; byte-blob [ variable ]

(defun PATH-ATTRIB-UNKNOWN-io-read (attribute-type attribute-length stream-in)
  (PATH-ATTRIB-UNKNOWN-make attribute-type
			    attribute-length
			    (BYTES-io-read attribute-length stream-in)))

(defun PATH-ATTRIB-UNKNOWN-io-write (obj stream-out)
  (destructuring-bind (type length byte-blob)
      (cdr obj)    
    (io-write-uNbe u16 type stream-out)
    (if (uN-bit-set-p u16 type 3)           ; extended length bit set
	(io-write-uNbe u16 length stream-out)     
	(io-write-uNbe u8 length stream-out))

    (BYTES-io-write byte-blob stream-out)))


(defun PATH-ATTRIB-UNKNOWN-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X #x~V,'0X ~/sbgp:bytes-pprint-1/)"
	  (TL-get-name obj)                            ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)            ; u16
	  (if (PATH-ATTRIB-get-flag-extended-len-bit obj) 4 2)
	  (PATH-ATTRIB-get-attribute-length obj)                ; u8|u16
	  (PATH-ATTRIB-UNKNOWN-get-byte-blob obj)))             ; BYTES

(set-pprint-dispatch '(cons (member PATH-ATTRIB-UNKNOWN)) #'PATH-ATTRIB-UNKNOWN-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun PATH-ATTRIB-UNKNOWN-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~4,'0X ~D~) ~/sbgp:bytes-pprint-2/]"
	  (TL-get-name obj)                            ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)            ; u16
	  (PATH-ATTRIB-get-attribute-length obj)                ; u8|u16
	  (PATH-ATTRIB-UNKNOWN-get-byte-blob obj)))             ; BYTES

(set-pprint-dispatch '(cons (member PATH-ATTRIB-UNKNOWN)) #'PATH-ATTRIB-UNKNOWN-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'PATH-ATTRIB-UNKNOWN 'pprint-1) #'PATH-ATTRIB-UNKNOWN-pprint-1)
(setf (get 'PATH-ATTRIB-UNKNOWN 'pprint-2) #'PATH-ATTRIB-UNKNOWN-pprint-2)
(setf (get 'PATH-ATTRIB-UNKNOWN 'zhash) #'PATH-ATTRIB-UNKNOWN-zhash)
