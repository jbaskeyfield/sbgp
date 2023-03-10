;;; LARGE_COMMUNITY object,within the path attribute, is a list of one or more
;;; lists of three 32 bit unsigned integers

#|
RFC 8092
3.  BGP Large Communities Attribute

This document defines the BGP Large Communities attribute as an
optional transitive path attribute of variable length.  All routes
with the BGP Large Communities attribute belong to the communities
specified in the attribute.

Each BGP Large Community value is encoded as a 12-octet quantity, as
follows:

0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                      Global Administrator                     |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                       Local Data Part 1                       |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                       Local Data Part 2                       |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Global Administrator:  A four-octet namespace identifier.

Local Data Part 1:  A four-octet operator-defined value.

Local Data Part 2:  A four-octet operator-defined value.

The Global Administrator field is intended to allow different ASes to
define BGP Large Communities without collision.  This field SHOULD be
an ASN, in which case the Local Data Parts are to be interpreted as
defined by the owner of the ASN.  The use of Reserved ASNs (0
[RFC7607], 65535 and 4294967295 [RFC7300]) is NOT RECOMMENDED.

There is no significance to the order in which twelve-octet Large
Community Attribute values are encoded in a Large Communities
attribute, A BGP speaker can transmit them in any order.

Duplicate BGP Large Community values MUST NOT be transmitted.  A
receiving speaker MUST silently remove redundant BGP Large Community
values from a BGP Large Community attribute.
|#

(in-package :sbgp)

(defun LARGE-COMMUNITY-get-list (obj) "-> list u32" (cdddr obj))

(defun LARGE-COMMUNITY-zhash (octet-offset obj)
  (logxor (zhash-integer u16 octet-offset (PATH-ATTRIB-get-attribute-type-field obj))
	  (zhash-integer u16 (+ 2 octet-offset) (PATH-ATTRIB-get-attribute-length obj))
	  (zhash-list u32 (+ 4 octet-offset) (LARGE-COMMUNITY-get-list obj))))

(defun LARGE-COMMUNITY-make (attribute-type attribute-length list-u32)
  (let ((obj (cons 'LARGE-COMMUNITY
		   (cons attribute-type
			 (cons attribute-length
			       list-u32)))))
    (if *path-attrib-cache*
	(CACHE-lookup *path-attrib-cache*
		      obj
		      (LARGE-COMMUNITY-zhash 0 obj))
	obj)))

(defun LARGE-COMMUNITY-valid1-p (obj)
   "Test list elements are of correct type"
  (and (consp obj)
       (>= (length obj) 3)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))
       (listp (LARGE-COMMUNITY-get-list obj))
       (every #'u32-p (LARGE-COMMUNITY-get-list obj))))

(deftype LARGE-COMMUNITY () '(and (cons (member LARGE-COMMUNITY)) (satisfies LARGE-COMMUNITY-valid1-p)))

(defun LARGE-COMMUNITY-typep (obj) (typep obj 'LARGE-COMMUNITY))

(defun LARGE-COMMUNITY-valid2-p (obj)
   "Test list values are within allowed range"
  (and (eq 'LARGE-COMMUNITY (TL-get-name obj))
       (= #xC020 (logandc1 #x1000 (PATH-ATTRIB-get-attribute-type-field obj)))   ; type field masked with extended length bit
       (= (PATH-ATTRIB-get-attribute-length obj)
	  (* 4 (length (LARGE-COMMUNITY-get-list obj))))
       (= 0 (mod (length (LARGE-COMMUNITY-get-list obj))
		 3))))

(defun LARGE-COMMUNITY-io-read (attribute-type attribute-length stream-in)
  (LARGE-COMMUNITY-make attribute-type
			attribute-length
			(io-read-uNbe-octets u32 attribute-length stream-in)))

(defun LARGE-COMMUNITY-io-write (obj stream-out)
  (destructuring-bind (attribute-type attribute-length . list-u32)
      (cdr obj)
    (io-write-uNbe u16 attribute-type stream-out)
    (if (uN-bit-set-p u16 attribute-type 3)           ; extended length bit set
	(io-write-uNbe u16 attribute-length stream-out)     
	(io-write-uNbe u8 attribute-length stream-out))
    (io-write-uNbe-octets u32 attribute-length list-u32 stream-out)))


(defun LARGE-COMMUNITY-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X #x~V,'0X ~{#x~8,'0X~^ ~})"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (if (PATH-ATTRIB-get-flag-extended-len-bit obj) 4 2) 
	  (PATH-ATTRIB-get-attribute-length obj)                 ; [ u8 | u16 ]
	  (LARGE-COMMUNITY-get-list obj)))

(set-pprint-dispatch '(cons (member LARGE-COMMUNITY)) #'LARGE-COMMUNITY-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun LARGE-COMMUNITY-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~4,'0X ~D [~{#x~8,'0X~^ ~}]~)"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (PATH-ATTRIB-get-attribute-length obj)                 ; [ u8 | u16 ]
	  (LARGE-COMMUNITY-get-list obj)))

(set-pprint-dispatch '(cons (member LARGE-COMMUNITY)) #'LARGE-COMMUNITY-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'LARGE-COMMUNITY 'pprint-1) #'LARGE-COMMUNITY-pprint-1)
(setf (get 'LARGE-COMMUNITY 'pprint-2) #'LARGE-COMMUNITY-pprint-2)
(setf (get 'LARGE-COMMUNITY 'zhash) #'LARGE-COMMUNITY-zhash)
