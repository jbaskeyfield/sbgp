;;; ORIGINATOR-ID and CLUSTER-LIST
#| rfc4456
ORIGINATOR_ID

   ORIGINATOR_ID is a new optional, non-transitive BGP attribute of Type
   code 9.  This attribute is 4 bytes long and it will be created by an
   RR in reflecting a route.  This attribute will carry the BGP
   Identifier of the originator of the route in the local AS.  A BGP
   speaker SHOULD NOT create an ORIGINATOR_ID attribute if one already
   exists.  A router that recognizes the ORIGINATOR_ID attribute SHOULD
   ignore a route received with its BGP Identifier as the ORIGINATOR_ID.


   CLUSTER_LIST

   CLUSTER_LIST is a new, optional, non-transitive BGP attribute of Type
   code 10.  It is a sequence of CLUSTER_ID values representing the
   reflection path that the route has passed.

   When an RR reflects a route, it MUST prepend the local CLUSTER_ID to
   the CLUSTER_LIST.  If the CLUSTER_LIST is empty, it MUST create a new
   one.  Using this attribute an RR can identify if the routing
   information has looped back to the same cluster due to
   misconfiguration.  If the local CLUSTER_ID is found in the
   CLUSTER_LIST, the advertisement received SHOULD be ignored.

|#
(in-package :sbgp)

(defun ORIGINATOR-ID-get-router-id (obj) "-> IPV4"       (cadddr obj))

(defun ORIGINATOR-ID-zhash (octet-offset obj)
  (logxor (zhash-integer u16 octet-offset (PATH-ATTRIB-get-attribute-type-field obj))
	  (zhash-integer u16 (+ 2 octet-offset) (PATH-ATTRIB-get-attribute-length obj))
	  (IPV4-zhash (+ 4 octet-offset) (ORIGINATOR-ID-get-router-id obj))))

(defun ORIGINATOR-ID-make (attribute-type attribute-length router-id)
  (let ((obj (list 'ORIGINATOR-ID
		   attribute-type
		   attribute-length
		   router-id)))
    (if *path-attrib-cache*
	(CACHE-lookup *path-attrib-cache*
		      obj
		      (ORIGINATOR-ID-zhash 0 obj))
	obj)))

(defun ORIGINATOR-ID-make-new (router-id-ipv4)
  (ORIGINATOR-ID-make #x8009 ; Optional-bit(#x8000) set; Transitive-bit(#x4000) not set; Partial-bit(#x2000) not set; Extended-Length(#x1000) not set; Type 9
		      4
		      router-id-ipv4))

(defun ORIGINATOR-ID-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (= (length obj) 4)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))
       (typep (ORIGINATOR-ID-get-router-id obj) 'IPV4)))

(deftype ORIGINATOR-ID () '(and (cons (member ORIGINATOR-ID)) (satisfies ORIGINATOR-ID-valid1-p)))

(defun ORIGINATOR-ID-typep (obj) (typep obj 'ORIGINATOR-ID))

(defun ORIGINATOR-ID-valid2-p (obj)
  "Test list values are within allowed range"
  (and (eq 'ORIGINATOR-ID (TL-get-name obj))
       (= #x8009 (logandc1 #x1000 (PATH-ATTRIB-get-attribute-type-field obj)))   ; type field masked with extended length bit
       (= 4 (PATH-ATTRIB-get-attribute-length obj))))

(defun ORIGINATOR-ID-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (+ (if (PATH-ATTRIB-get-flag-extended-len-bit obj)                      ; attribute-type & attribute-length [ 3 or 4 bytes ]
	 4
	 3)
     4))

(defun ORIGINATOR-ID-io-read (attribute-type attribute-length stream-in)
  (ORIGINATOR-ID-make attribute-type
		      attribute-length
		      (IPV4-io-read stream-in)))

(defun ORIGINATOR-ID-io-write (obj stream-out)
  (let ((attribute-type (PATH-ATTRIB-get-attribute-type-field obj))
        (attribute-length (PATH-ATTRIB-get-attribute-length obj))
	(router-id (ORIGINATOR-ID-get-router-id obj)))

    (io-write-uNbe u16 attribute-type stream-out)
    (if (uN-bit-set-p u16 attribute-type 3)                     ; extended-length bit set
	(io-write-uNbe u16 attribute-length stream-out)
	(io-write-uNbe u8 attribute-length stream-out))
    (IPV4-io-write router-id stream-out)))

(defun ORIGINATOR-ID-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X #x~V,'0X ~W)"
	  (TL-get-name obj)                                      ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (if (PATH-ATTRIB-get-flag-extended-len-bit obj) 4 2) 
	  (PATH-ATTRIB-get-attribute-length obj)                 ; u8|u16
	  (ORIGINATOR-ID-get-router-id obj)))                    ; IPV4

(set-pprint-dispatch '(cons (member ORIGINATOR-ID)) #'ORIGINATOR-ID-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun ORIGINATOR-ID-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~4,'0X ~D~) ~W]"
	  (TL-get-name obj)                                      ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (PATH-ATTRIB-get-attribute-length obj)                 ; u8|u16
	  (ORIGINATOR-ID-get-router-id obj)))                    ; IPV4

(set-pprint-dispatch '(cons (member ORIGINATOR-ID)) #'ORIGINATOR-ID-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'ORIGINATOR-ID 'pprint-1) #'ORIGINATOR-ID-pprint-1)
(setf (get 'ORIGINATOR-ID 'pprint-2) #'ORIGINATOR-ID-pprint-2)
(setf (get 'ORIGINATOR-ID 'zhash) #'ORIGINATOR-ID-zhash)


(defun CLUSTER-LIST-get-cluster-id-list (obj) "-> list of IPV4" (cdddr obj))

(defun CLUSTER-LIST-zhash (octet-offset obj)
  (logxor (zhash-integer u16 octet-offset (PATH-ATTRIB-get-attribute-type-field obj))
	  (zhash-integer u16 (+ 2 octet-offset) (PATH-ATTRIB-get-attribute-length obj))
	  (do ((lst (CLUSTER-LIST-get-cluster-id-list obj) (cdr lst))
	       (offset (+ 4 octet-offset) (+ 4 offset))
	       (hash 0 (logxor hash
			       (IPV4-zhash offset (car lst)))))
	      ((null lst) hash))))

(defun CLUSTER-LIST-make (attribute-type attribute-length cluster-id-list)
  "Returns tagged list ('CLUSTER-LIST attribute-type attribute-length . cluster-id-list)
Arguments: attribute-type [u16], attribute-length [u8|u16], cluster-id-list [list of IPV4]"
   (let ((obj (cons 'CLUSTER-LIST
		    (cons attribute-type
		          (cons attribute-length
		                cluster-id-list)))))
    (if *path-attrib-cache*
	(CACHE-lookup *path-attrib-cache*
		      obj
		      (CLUSTER-LIST-zhash 0 obj))
	obj)))

(defun CLUSTER-LIST-make-new (cluster-id-ipv4)
  "Creates new CLUSTER-LIST attribute with single element cluster-id-list"
  (CLUSTER-LIST-make  #x800A ; Optional-bit(#x8000) set; Transitive-bit(#x4000) not set; Partial-bit(#x2000) not set; Extended-Length(#x1000) not set; Type 10
		      4
		      (list cluster-id-ipv4)))

(defun CLUSTER-LIST-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (= (length obj) 4)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))
       (every #'IPV4-typep (CLUSTER-LIST-get-cluster-id-list obj))))

(deftype CLUSTER-LIST () '(and (cons (member CLUSTER-LIST)) (satisfies CLUSTER-LIST-valid1-p)))

(defun CLUSTER-LIST-typep (obj) (typep obj 'CLUSTER-LIST))

(defun CLUSTER-LIST-valid2-p (obj)
  "Test list values are within allowed range"
  (and (eq 'ORIGINATOR-ID (TL-get-name obj))
       (= #x800A (logandc1 #x1000 (PATH-ATTRIB-get-attribute-type-field obj)))   ; type field masked with extended length bit
       (= (PATH-ATTRIB-get-attribute-length obj)
	  (* 4 (length (CLUSTER-LIST-get-cluster-id-list obj))))))

(defun CLUSTER-LIST-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (+ (if (PATH-ATTRIB-get-flag-extended-len-bit obj)                      ; attribute-type & attribute-length [ 3 or 4 bytes ]
	 4
	 3)
     (PATH-ATTRIB-get-attribute-length obj)))

(defun CLUSTER-LIST-io-read (attribute-type attribute-length stream-in)
  (CLUSTER-LIST-make attribute-type
		     attribute-length
		     (loop repeat (/ attribute-length 4)
			   collect (IPV4-io-read stream-in))))

(defun CLUSTER-LIST-io-write (obj stream-out)
  (let ((attribute-type (PATH-ATTRIB-get-attribute-type-field obj))
        (attribute-length (PATH-ATTRIB-get-attribute-length obj))
	(cluster-id-list (CLUSTER-LIST-get-cluster-id-list obj)))

    (io-write-uNbe u16 attribute-type stream-out)
    (if (uN-bit-set-p u16 attribute-type 3)                     ; extended-length bit set
	(io-write-uNbe u16 attribute-length stream-out)
	(io-write-uNbe u8 attribute-length stream-out))
    (dolist (cluster-id cluster-id-list)
      (IPV4-io-write cluster-id stream-out))))

(defun CLUSTER-LIST-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X #x~V,'0X ~{~W~^ ~})"
	  (TL-get-name obj)                                      ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (if (PATH-ATTRIB-get-flag-extended-len-bit obj) 4 2) 
	  (PATH-ATTRIB-get-attribute-length obj)                 ; u8|u16
	  (CLUSTER-LIST-get-cluster-id-list obj)))               ; list of IPV4

(set-pprint-dispatch '(cons (member CLUSTER-LIST)) #'CLUSTER-LIST-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun CLUSTER-LIST-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~4,'0X ~D~) ~{~W~^ ~}]"
	  (TL-get-name obj)                                      ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (PATH-ATTRIB-get-attribute-length obj)                 ; u8|u16
	  (CLUSTER-LIST-get-cluster-id-list obj)))               ; list of IPV4

(set-pprint-dispatch '(cons (member CLUSTER-LIST)) #'CLUSTER-LIST-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'CLUSTER-LIST 'pprint-1) #'CLUSTER-LIST-pprint-1)
(setf (get 'CLUSTER-LIST 'pprint-2) #'CLUSTER-LIST-pprint-2)
(setf (get 'CLUSTER-LIST 'zhash) #'CLUSTER-LIST-zhash)

(defun CLUSTER-LIST-prepend-local-cluster-id (obj cluster-id-ipv4)
   "Returns CLUSTER-LIST object that is copy of OBJ with CLUSTER-ID prepended to the 'cluster-id-list'.
Arguments: obj [CLUSTER-LIST], cluster-id [IPV4]"
  (CLUSTER-LIST-make (PATH-ATTRIB-get-attribute-type-field obj)
		     (+ 4 (PATH-ATTRIB-get-attribute-length obj))
		     (cons cluster-id-ipv4
			   (CLUSTER-LIST-get-cluster-id-list))))
