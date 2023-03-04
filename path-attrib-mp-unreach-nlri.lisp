;;; MP_UNREACH_NLRI object

#|
RFC 4760
4.  Multiprotocol Unreachable NLRI - MP_UNREACH_NLRI (Type Code 15):

   This is an optional non-transitive attribute that can be used for the
   purpose of withdrawing multiple unfeasible routes from service.

   The attribute is encoded as shown below:

        +---------------------------------------------------------+
        | Address Family Identifier (2 octets)                    |
        +---------------------------------------------------------+
        | Subsequent Address Family Identifier (1 octet)          |
        +---------------------------------------------------------+
        | Withdrawn Routes (variable)                             |
        +---------------------------------------------------------+

   The use and the meaning of these fields are as follows:

      Address Family Identifier (AFI):

         This field in combination with the Subsequent Address Family
         Identifier field identifies the set of Network Layer protocols
         to which the address carried in the Next Hop field must belong,
         the way in which the address of the next hop is encoded, and
         the semantics of the Network Layer Reachability Information
         that follows.  If the Next Hop is allowed to be from more than
         one Network Layer protocol, the encoding of the Next Hop MUST
         provide a way to determine its Network Layer protocol.

         Presently defined values for the Address Family Identifier
         field are specified in the IANA's Address Family Numbers
         registry [IANA-AF].

      Subsequent Address Family Identifier (SAFI):

         This field in combination with the Address Family Identifier
         field identifies the set of Network Layer protocols to which
         the address carried in the Next Hop must belong, the way in
         which the address of the next hop is encoded, and the semantics
         of the Network Layer Reachability Information that follows.  If
         the Next Hop is allowed to be from more than one Network Layer
         protocol, the encoding of the Next Hop MUST provide a way to
         determine its Network Layer protocol.

      Withdrawn Routes Network Layer Reachability Information:

         A variable-length field that lists NLRI for the routes that are
         being withdrawn from service.  The semantics of NLRI is
         identified by a combination of <AFI, SAFI> carried in the
         attribute.

         When the Subsequent Address Family Identifier field is set to
         one of the values defined in this document, each NLRI is
         encoded as specified in the "NLRI encoding" section of this
         document.

   An UPDATE message that contains the MP_UNREACH_NLRI is not required
   to carry any other path attributes.
|#

(in-package :sbgp)

(defun MP-UNREACH-NLRI-make (attribute-type attribute-length afisafi nlri-withdrawl-list)
  (list 'MP-UNREACH-NLRI
	attribute-type
	attribute-length 
	afisafi
        nlri-withdrawl-list))

(defun MP-UNREACH-NLRI-make-new (nlri-withdrawl-list)
  "Constructs MP-REACH-NLRI path attribute object from passed NLRI-WITHDRAWL-LIST.
Returns two values: MP-UNREACH-NLRI object and number of bytes to read/write to network."
  (let ((afisafi (NLRI-get-afisafi (car nlri-withdrawl-list)))
        (nlri-withdrawl-length (loop for nlri-withdrawl in nlri-withdrawl-list
			   sum (NLRI-get-io-rw-octets nlri-withdrawl))))
    (values (MP-UNREACH-NLRI-make  #x900f  ; Optional-bit(#x8000) set; Transitive-bit(#x4000) not set; Partial-bit(#x2000) not set; Extended-Length(#x1000) set; Type 15 (#x0f)
				   (+ 3                 ; afisafi u24
				      nlri-withdrawl-length)
				   afisafi
				   nlri-withdrawl-list)
	    (+ 7                     ; attribute-type u16 + attribute-length (extended) u16 + afisafi u24
	       nlri-withdrawl-length))))
    
  
(defun MP-UNREACH-NLRI-get-afisafi (obj)   "-> AFISAFI"                (cadddr obj))
(defun MP-UNREACH-NLRI-get-nlri-withdrawl-list (obj) "-> list of NLRI-WITHDRAWL" (car (cddddr obj)))

(defun MP-UNREACH-NLRI-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (>= (length obj) 5)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))
       (u24-p (MP-UNREACH-NLRI-get-afisafi obj))        
       (listp (MP-UNREACH-NLRI-get-nlri-withdrawl-list obj))
       (every #'NLRI-WITHDRAWL-typep (MP-UNREACH-NLRI-get-nlri-withdrawl-list obj))))

(deftype MP-UNREACH-NLRI () '(and (cons (member MP-UNREACH-NLRI)) (satisfies MP-UNREACH-NLRI-valid1-p)))

(defun MP-UNREACH-NLRI-typep (obj) (typep obj 'MP-UNREACH-NLRI))

(defun MP-UNREACH-NLRI-valid2-p (obj)
   "Test list values are within allowed range"
  (and (eq 'MP-UNREACH-NLRI (TL-get-name obj))
       (= #x800f (logandc1 #x1000 (PATH-ATTRIB-get-attribute-type-field obj)))))   ; type field masked with extended length bit

(defun MP-UNREACH-NLRI-io-read (attribute-type attribute-length stream-in)
  (let* ((afisafi (AFISAFI-io-read stream-in))
	 (nlri-withdrawl-list (NLRI-WITHDRAWL-io-read-list (- attribute-length 3)
						 afisafi
						 stream-in)))
    (MP-UNREACH-NLRI-make attribute-type
			  attribute-length
			  afisafi
			  nlri-withdrawl-list)))

(defun MP-UNREACH-NLRI-io-write (obj stream-out)
  (destructuring-bind (attribute-type attribute-length afisafi nlri-withdrawl-list)
      (cdr obj)
    (io-write-uNbe u16 attribute-type stream-out)           
    (if (uN-bit-set-p u16 attribute-type 3)           ; extended length bit set
	(io-write-uNbe u16 attribute-length stream-out)     
	(io-write-uNbe u8 attribute-length stream-out))

    (AFISAFI-io-write afisafi stream-out)
    (dolist (elem nlri-withdrawl-list)
      (NLRI-io-write elem stream-out))))


(defun MP-UNREACH-NLRI-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X #x~V,'0X #x~6,'0X (~{~W~^ ~}))"
	  (TL-get-name obj)                                        ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)               ; u16
	  (if (PATH-ATTRIB-get-flag-extended-len-bit obj) 4 2) 
	  (PATH-ATTRIB-get-attribute-length obj)                   ; [ u8 | u16 ]
	  (MP-UNREACH-NLRI-get-afisafi obj)                        ; AFISAFI
	  (MP-UNREACH-NLRI-get-nlri-withdrawl-list obj)))                    ; list of NLRI

(set-pprint-dispatch '(cons (member MP-UNREACH-NLRI)) #'MP-UNREACH-NLRI-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun MP-UNREACH-NLRI-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~4,'0X ~D #x~6,'0X [~{~W~}]]~)"
	  (TL-get-name obj)                                      ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (PATH-ATTRIB-get-attribute-length obj)                 ; [ u8 | u16 ]
	  (MP-UNREACH-NLRI-get-afisafi obj)                      ; AFISAFI
	  (MP-UNREACH-NLRI-get-nlri-withdrawl-list obj)))                  ; list of NLRI

(set-pprint-dispatch '(cons (member MP-UNREACH-NLRI)) #'MP-UNREACH-NLRI-pprint-2 0 *sbgp-pprint-dispatch-table-2*)
		
