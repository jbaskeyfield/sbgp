;;; MP_REACH_NLRI object

#|
RFC 4760
3.  Multiprotocol Reachable NLRI - MP_REACH_NLRI (Type Code 14):

This is an optional non-transitive attribute that can be used for the
following purposes:

(a) to advertise a feasible route to a peer

(b) to permit a router to advertise the Network Layer address of the
router that should be used as the next hop to the destinations
listed in the Network Layer Reachability Information field of the
MP_NLRI attribute.

The attribute is encoded as shown below:

+---------------------------------------------------------+
| Address Family Identifier (2 octets)                    |
+---------------------------------------------------------+
| Subsequent Address Family Identifier (1 octet)          |
+---------------------------------------------------------+
| Length of Next Hop Network Address (1 octet)            |
+---------------------------------------------------------+
| Network Address of Next Hop (variable)                  |
+---------------------------------------------------------+
| Reserved (1 octet)                                      |
+---------------------------------------------------------+
| Network Layer Reachability Information (variable)       |
+---------------------------------------------------------+

The use and meaning of these fields are as follows:

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

Length of Next Hop Network Address:

A 1-octet field whose value expresses the length of the
"Network Address of Next Hop" field, measured in octets.

Network Address of Next Hop:

A variable-length field that contains the Network Address of
the next router on the path to the destination system.  The
Network Layer protocol associated with the Network Address of
the Next Hop is identified by a combination of <AFI, SAFI>
carried in the attribute.

Reserved:

A 1 octet field that MUST be set to 0, and SHOULD be ignored
upon receipt.

Network Layer Reachability Information (NLRI):

A variable length field that lists NLRI for the feasible routes
that are being advertised in this attribute.  The semantics of
NLRI is identified by a combination of <AFI, SAFI> carried in
the attribute.

When the Subsequent Address Family Identifier field is set to
one of the values defined in this document, each NLRI is
encoded as specified in the "NLRI encoding" section of this
document.

The next hop information carried in the MP_REACH_NLRI path attribute
defines the Network Layer address of the router that SHOULD be used
as the next hop to the destinations listed in the MP_NLRI attribute
in the UPDATE message.


Bates, et al.               Standards Track                     [Page 4]

Multiprotocol Extensions for BGP-4       January 2007


The rules for the next hop information are the same as the rules for
the information carried in the NEXT_HOP BGP attribute (see Section
5.1.3 of [BGP-4]).

An UPDATE message that carries the MP_REACH_NLRI MUST also carry the
ORIGIN and the AS_PATH attributes (both in EBGP and in IBGP
exchanges).  Moreover, in IBGP exchanges such a message MUST also
carry the LOCAL_PREF attribute.

An UPDATE message that carries no NLRI, other than the one encoded in
the MP_REACH_NLRI attribute, SHOULD NOT carry the NEXT_HOP attribute.
If such a message contains the NEXT_HOP attribute, the BGP speaker
that receives the message SHOULD ignore this attribute.

An UPDATE message SHOULD NOT include the same address prefix (of the
same <AFI, SAFI>) in more than one of the following fields: WITHDRAWN
ROUTES field, Network Reachability Information fields, MP_REACH_NLRI
field, and MP_UNREACH_NLRI field.  The processing of an UPDATE
message in this form is undefined.
|#

(in-package :sbgp)

(defun MP-REACH-NLRI-make (attribute-type
			   attribute-length
			   afisafi
			   next-hop-length
			   next-hop-list
			   nlri-list)
  (list 'MP-REACH-NLRI
	attribute-type
	attribute-length 
	afisafi
	next-hop-length
	next-hop-list
	nlri-list))

(defun MP-REACH-NLRI-make-new (next-hop-list nlri-list)
  "Constructs MP-REACH-NLRI path attribute object from passed NEXT-HOP-LIST and NLRI-LIST.
Returns two values: MP-REACH-NLRI object and number of bytes to read/write to network."
  (let* ((afisafi (NLRI-get-afisafi (car nlri-list)))
	 (next-hop-length (* (length next-hop-list)
			     (if (= (AFISAFI-get-afi afisafi) +AFI-ipv4+)
				 4
				 16)))
	 (next-hop-list (loop for next-hop in next-hop-list
			      collect (NEXT-HOP-get-address next-hop)))
	 (nlri-length (loop for nlri in nlri-list
			    sum (NLRI-get-io-rw-octets nlri))))

    (values (MP-REACH-NLRI-make #x900e  ; Optional-bit(#x8000) set; Transitive-bit(#x4000) not set; Partial-bit(#x2000) not set; Extended-Length(#x1000) set; Type 14 (#x0e)
				(+ 5                 ; afisafi u24 + next-hop-length u8 + reserved octet u8
				   next-hop-length   ; length of IPV4|IPV6*
				   nlri-length)      ; length of NLRI*
				afisafi
				next-hop-length
				next-hop-list
				nlri-list)
	    (+ 9                  ; attribute-type u16 + attribute-length (extended) u16 + afisafi u24 + next-hop-length u8 + reserved octet u8
	       next-hop-length    ; length of IPV4|IPV6*
	       nlri-length))))    ; length of NLRI*

(defun MP-REACH-NLRI-get-afisafi (obj)         "-> AFISAFI"                         (cadddr obj))
(defun MP-REACH-NLRI-get-next-hop-length (obj) "-> u8"                              (car (cddddr obj)))
(defun MP-REACH-NLRI-get-next-hop-list (obj)   "-> list of NEXT-HOP" (cadr (cddddr obj)))
(defun MP-REACH-NLRI-get-nlri-list (obj)       "-> list of NLRI"                    (caddr (cddddr obj)))

(defun MP-REACH-NLRI-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (>= (length obj) 5)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))
       (u24-p (MP-REACH-NLRI-get-afisafi obj))        
       (u8-p (MP-REACH-NLRI-get-next-hop-length obj))
       (listp (MP-REACH-NLRI-get-next-hop-list obj))
       (every #'NEXT-HOP-typep (MP-REACH-NLRI-get-next-hop-list obj))
       (listp (MP-REACH-NLRI-get-nlri-list obj))
       (every #'NLRI-typep (MP-REACH-NLRI-get-nlri-list obj))))

(deftype MP-REACH-NLRI () '(and (cons (member MP-REACH-NLRI)) (satisfies MP-REACH-NLRI-valid1-p)))

(defun MP-REACH-NLRI-typep (obj) (typep obj 'MP-REACH-NLRI))

(defun MP-REACH-NLRI-valid2-p (obj)
   "Test list values are within allowed range"
  (and (eq 'MP-REACH-NLRI (TL-get-name obj))
       (= #x800e (logandc1 #x1000 (PATH-ATTRIB-get-attribute-type-field obj)))))   ; type field masked with extended length bit

       
(defun MP-REACH-NLRI-io-read (attribute-type attribute-length stream-in)
  (let* ((afisafi (AFISAFI-io-read stream-in))
	 (next-hop-length (io-read-uNbe u8 stream-in))
	 (next-hop-list (case (AFISAFI-get-afi afisafi)
			  (1  (loop repeat (/ next-hop-length 4)
				    collect (NEXT-HOP-io-read #x4003 4 afisafi stream-in)))  ;; (NEXT-HOP-io-read attribute-type 4 afisafi stream-in))) ;; TODO? fixed attribute type 0x4003 to be same as standard next-hop path attribute?
			  (2  (loop repeat (/ next-hop-length 16)
				    collect (NEXT-HOP-io-read #x4003 16 afisafi stream-in))) ;;(NEXT-HOP-io-read attribute-type 16 afisafi stream-in)))
						   
			  (t  (BYTES-io-read next-hop-length stream-in)))))                  ;; TODO? does this need to be raised as an error
    (io-read-uNbe u8 stream-in)  ; reserved octet
    (let ((nlri-list (NLRI-io-read-list (- attribute-length 5 next-hop-length)
					afisafi
					stream-in)))
      (MP-REACH-NLRI-make attribute-type
			  attribute-length
			  afisafi
			  next-hop-length
			  next-hop-list
			  nlri-list))))

(defun MP-REACH-NLRI-io-write (obj stream-out)
    (destructuring-bind (attribute-type attribute-length afisafi next-hop-length next-hop-list nlri-list)
	(cdr obj)
      
      (io-write-uNbe u16 attribute-type stream-out)           
      (if (uN-bit-set-p u16 attribute-type 3)           ; extended length bit set
	  (io-write-uNbe u16 attribute-length stream-out)     
	  (io-write-uNbe u8 attribute-length stream-out))

      (AFISAFI-io-write afisafi stream-out)
      (io-write-uNbe u8 next-hop-length stream-out)
      (dolist (elem next-hop-list)
	(case (AFISAFI-get-afi afisafi)
	  (1 (IPV4-io-write (NEXT-HOP-get-address elem) stream-out))
	  (2 (IPV6-io-write (NEXT-HOP-get-address elem) stream-out))
	  (t (BYTES-io-write elem stream-out))))

      (io-write-uNbe u8 0 stream-out)   ; reserved octet

      (dolist (elem nlri-list)
	(NLRI-io-write elem stream-out))))


(defun MP-REACH-NLRI-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X #x~V,'0X #x~6,'0X #x~2,0X (~{~W~^ ~}) (~{~W~^ ~}))"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (if (PATH-ATTRIB-get-flag-extended-len-bit obj) 4 2) 
	  (PATH-ATTRIB-get-attribute-length obj)                 ; [ u8 | u16 ]
	  (MP-REACH-NLRI-get-afisafi obj)                        ; AFISAFI
	  (MP-REACH-NLRI-get-next-hop-length obj)                ; u8
	  (MP-REACH-NLRI-get-next-hop-list obj)                  ; list of [ IPV4 | IPV6 | BYTES ]
	  (MP-REACH-NLRI-get-nlri-list obj)))                    ; list of NLRI

(set-pprint-dispatch '(cons (member MP-REACH-NLRI)) #'MP-REACH-NLRI-pprint-1 0 *sbgp-pprint-dispatch-table-1*)


(defun MP-REACH-NLRI-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~4,'0X ~D #x~6,'0X ~D [~{~W~}] [~{~W~}]]~)"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (PATH-ATTRIB-get-attribute-length obj)                 ; [ u8 | u16 ]
	  (MP-REACH-NLRI-get-afisafi obj)                        ; AFISAFI
	  (MP-REACH-NLRI-get-next-hop-length obj)                ; u8
	  (MP-REACH-NLRI-get-next-hop-list obj)                  ; list of [ IPV4 | IPV6 | BYTES ]
	  (MP-REACH-NLRI-get-nlri-list obj)))                    ; list of NLRI

(set-pprint-dispatch '(cons (member MP-REACH-NLRI)) #'MP-REACH-NLRI-pprint-2 0 *sbgp-pprint-dispatch-table-2*)
		
