;;; rfc6793
;;; The AS4_PATH attribute has the same semantics and the same encoding as the AS_PATH attribute, except that it is "optional transitive", and it carries four-octet AS numbers.

;;; AS4-PATH - 'inherits' functions from PATH-ATTRIB, TAGGED-LIST
(in-package :sbgp)

(defun AS4-PATH-get-type (obj)   "-> u8"                 (cadddr obj))
(defun AS4-PATH-get-length (obj) "-> u8"                 (car (cddddr obj)))
(defun AS4-PATH-get-value (obj)  "-> list of u32"        (cdr (cddddr obj)))

;; uses same -zhash function as AS-PATH

(defun AS4-PATH-make (attribute-type attribute-length as-path-type as-path-length as-path-list)
  (let ((obj (cons 'AS4-PATH
		   (cons attribute-type
			 (cons attribute-length
			       (cons as-path-type
				     (cons as-path-length
					   as-path-list)))))))
    (if *path-attrib-cache*
	(CACHE-lookup *path-attrib-cache*
		      obj
		      (AS-PATH-zhash 0 obj))
	obj)))

(defun AS4-PATH-make-new (asn-u32 &optional (repeat-count 1))
  "Returns new AS4-PATH object. AS4-PATH is considered malformed if zero length. So, unlike AS-PATH, asn-u32 is not optional"
  (AS-PATH-make #xC011  ; Optional-bit(#x8000) set; Transitive-bit(#x4000) set; Partial-bit(#x2000) not set; Extended-Length(#x1000) not set; Type 17 (#x11)
		(+ 2 (* 4 repeat-count))                    ; attribute-length
		2                                           ; 2 = AS_SEQ
		repeat-count                                ; length
		(make-list repeat-count
			   :initial-element asn-u32)))

(defun AS4-PATH-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (>= (length obj) 6)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))
       (u8-p (AS4-PATH-get-type obj))
       (u8-p (AS4-PATH-get-length obj))
       (listp (AS4-PATH-get-value obj))
       (every #'u32-p (AS4-PATH-get-value obj))))

(deftype AS4-PATH () '(and (cons (member AS4-PATH)) (satisfies AS4-PATH-valid1-p)))

(defun AS4-PATH-typep (obj) (typep obj 'AS4-PATH))

(defun AS4-PATH-valid2-p (obj)
  "Test list values are within allowed range"
  (and (eq 'AS4-PATH (TL-get-name obj))
       (= #xC011 (logandc1 #x1000 (PATH-ATTRIB-get-attribute-type-field obj)))   ; type field masked with extended length bit
       (member (AS4-PATH-get-type obj)
	       '(AS-SET AS-SEQ AS-CONFED-SEQUENCE AS-CONFED-SET))
       (let ((len (length (AS4-PATH-get-value obj))))                  
	 (and (= (PATH-ATTRIB-get-attribute-length obj) (+ 2 (* len 4)))         ; attribute-length field matches length of path list + 2 bytes
	      (<= (AS-PATH-get-length obj) len)))))                              ; possible AS-SET after AS-SEQ so path length field can be smaller 
       
(defun AS4-PATH-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream"
  (+ (if (PATH-ATTRIB-get-flag-extended-len-bit obj)     ; attribute-type & attribute-length [ 3 or 4 bytes ]
	 4
	 3)
     (PATH-ATTRIB-get-attribute-length obj)))


;; NOTE: reading of as-path value uses attribute-length field rather than as-path-length as possible trailing AS_SET in path
(defun AS4-PATH-io-read (attribute-type attribute-length stream-in)
  (AS4-PATH-make attribute-type
		 attribute-length
		 (io-read-uNbe u8 stream-in)                                      ; as-path-type
		 (io-read-uNbe u8 stream-in)                                      ; as-path-length
		 (io-read-uNbe-octets u32 (- attribute-length 2) stream-in)))     ; as-path-value


(defun AS4-PATH-io-write (obj stream-out)
  (destructuring-bind (attribute-type attribute-length as-path-type as-path-length . as-path-list)
      (cdr obj)
    (io-write-uNbe u16 attribute-type stream-out)
    (if (uN-bit-set-p u16 attribute-type 3)                     ; extended-length bit set
	(io-write-uNbe u16 attribute-length stream-out)
	(io-write-uNbe u8 attribute-length stream-out))

    (io-write-uNbe u8 as-path-type stream-out)
    (io-write-uNbe u8 as-path-length stream-out)
    (io-write-uNbe-octets u32 (- attribute-length 2) as-path-list stream-out)))


;; AS4-PATH uses same pprint-1 and -2 functions as AS-PATH

(set-pprint-dispatch '(cons (member AS4-PATH)) #'AS-PATH-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(set-pprint-dispatch '(cons (member AS4-PATH)) #'AS-PATH-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'AS4-PATH 'pprint-1) #'AS-PATH-pprint-1)
(setf (get 'AS4-PATH 'pprint-2) #'AS-PATH-pprint-2)
(setf (get 'AS4-PATH 'zhash) #'AS-PATH-zhash)


#|
NOTE: creation of AS4-PATH and replacement of >65535 with AS_TRANS (23456) is planned to be done when rib-loc (router) sends updates to rib-adj (peer), so all the AS-PATH serialisation routines have to worry about is selecting correct written octet size and modifying attribute length field on read/write. (all attribute-length fields on all AS-PATH records should assume 4 octet ASN)

RFC4893
4.2.2.  Generating Updates

   When communicating with an OLD BGP speaker, a NEW speaker MUST send
   the AS path information in the AS_PATH attribute encoded with 2-octet
   AS numbers.  The NEW speaker MUST also send the AS path information
   in the AS4_PATH attribute (encoded with 4-octet AS numbers), except
   for the case where the entire AS path information is composed of 2-
   octet AS numbers only.  In this case, the NEW speaker SHOULD NOT send
   the AS4_PATH attribute.

   In the AS_PATH attribute encoded with 2-octet AS numbers, non-
   mappable 4-octet AS numbers are represented by the well-known 2-octet
   AS number, AS_TRANS.  This will preserve the path length property of
   the AS path information and also help in updating the AS path
   information received on a NEW BGP speaker from an OLD speaker, as
   explained in the next section.
|#







