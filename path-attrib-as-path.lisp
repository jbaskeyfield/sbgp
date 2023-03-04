#|
RFC 4271
b) AS_PATH (Type Code 2):

            AS_PATH is a well-known mandatory attribute that is composed
            of a sequence of AS path segments.  Each AS path segment is
            represented by a triple <path segment type, path segment
            length, path segment value>.

            The path segment type is a 1-octet length field with the
            following values defined:

               Value      Segment Type

               1         AS_SET: unordered set of ASes a route in the
                            UPDATE message has traversed

               2         AS_SEQUENCE: ordered set of ASes a route in
                            the UPDATE message has traversed

            The path segment length is a 1-octet length field,
            containing the number of ASes (not the number of octets) in
            the path segment value field.

            The path segment value field contains one or more AS
            numbers, each encoded as a 2-octet length field.

            Usage of this attribute is defined in 5.1.2.

5.1.2.  AS_PATH

   AS_PATH is a well-known mandatory attribute.  This attribute
   identifies the autonomous systems through which routing information
   carried in this UPDATE message has passed.  The components of this
   list can be AS_SETs or AS_SEQUENCEs.

   When a BGP speaker propagates a route it learned from another BGP
   speaker's UPDATE message, it modifies the route's AS_PATH attribute
   based on the location of the BGP speaker to which the route will be
   sent:

      a) When a given BGP speaker advertises the route to an internal
         peer, the advertising speaker SHALL NOT modify the AS_PATH
         attribute associated with the route.

      b) When a given BGP speaker advertises the route to an external
         peer, the advertising speaker updates the AS_PATH attribute as
         follows:

         1) if the first path segment of the AS_PATH is of type
            AS_SEQUENCE, the local system prepends its own AS number as
            the last element of the sequence (put it in the leftmost
            position with respect to the position of octets in the
            protocol message).  If the act of prepending will cause an
            overflow in the AS_PATH segment (i.e., more than 255 ASes),
            it SHOULD prepend a new segment of type AS_SEQUENCE and
            prepend its own AS number to this new segment.

         2) if the first path segment of the AS_PATH is of type AS_SET,
            the local system prepends a new path segment of type
            AS_SEQUENCE to the AS_PATH, including its own AS number in
            that segment.

         3) if the AS_PATH is empty, the local system creates a path
            segment of type AS_SEQUENCE, places its own AS into that
            segment, and places that segment into the AS_PATH.

   When a BGP speaker originates a route then:

      a) the originating speaker includes its own AS number in a path
         segment, of type AS_SEQUENCE, in the AS_PATH attribute of all
         UPDATE messages sent to an external peer.  In this case, the AS
         number of the originating speaker's autonomous system will be
         the only entry the path segment, and this path segment will be
         the only segment in the AS_PATH attribute.

      b) the originating speaker includes an empty AS_PATH attribute in
         all UPDATE messages sent to internal peers.  (An empty AS_PATH
         attribute is one whose length field contains the value zero).

   Whenever the modification of the AS_PATH attribute calls for
   including or prepending the AS number of the local system, the local
   system MAY include/prepend more than one instance of its own AS
   number in the AS_PATH attribute.  This is controlled via local
   configuration.
|#

;;; AS-PATH - 'inherits' functions from PATH-ATTRIB, TAGGED-LIST
(in-package :sbgp)

(defun AS-PATH-get-type (obj)   "-> u8"                 (cadddr obj))
(defun AS-PATH-get-length (obj) "-> u8"                 (car (cddddr obj)))
(defun AS-PATH-get-value (obj)  "-> list [ u16 | u32 ]" (cdr (cddddr obj)))

(defun AS-PATH-zhash (octet-offset obj)
  (zhash-tagged-list '(2 2 1 1 4) octet-offset obj))

(defun AS-PATH-make (attribute-type attribute-length as-path-type as-path-length as-path-list)
  (let ((obj (cons 'AS-PATH
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

(defun AS-PATH-make-new (&optional asn-u32 (repeat-count 1))
  "Returns new AS-PATH object. If no ASN-U32 given, returns AS-PATH representing empty path (attribute-length = 0)"
  (if asn-u32
      (AS-PATH-make #x4002                                      ; attribute-type
		    (+ 2 (* 4 repeat-count))                    ; attribute-length
		    2                                           ; 2 = AS_SEQ
		    repeat-count                                ; length
		    (make-list repeat-count
			       :initial-element asn-u32))
      (AS-PATH-make #x4002                                      ; attribute-type
		    0                                           ; attribute-length
		    2                                           ; 2 = AS_SEQ
		    0                                           ; length
		    nil)))

(defun AS-PATH-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (>= (length obj) 5)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))
       (u8-p (AS-PATH-get-type obj))
       (u8-p (AS-PATH-get-length obj))
       (listp (AS-PATH-get-value obj))
       (every #'u32-p (AS-PATH-get-value obj))))

(deftype AS-PATH () '(and (cons (member AS-PATH)) (satisfies AS-PATH-valid1-p)))

(defun AS-PATH-typep (obj) (typep obj 'AS-PATH))

(defun AS-PATH-valid2-p (obj)
  "Test list values are within allowed range"
  (and (eq 'AS-PATH (TL-get-name obj))
       (= #x4002 (logandc1 #x1000 (PATH-ATTRIB-get-attribute-type-field obj)))   ; type field masked with extended length bit
       (or (= 1 (AS-PATH-get-type obj))                                          ; AS-SET = 1
	   (= 2 (AS-PATH-get-type obj)))                                         ; or AS-SEQ = 2
       (let ((len (length (AS-PATH-get-value obj))))                  
	 (and (= (PATH-ATTRIB-get-attribute-length obj) (+ 2 (* len 4)))         ; attribute-length field matches length of path list + 2 bytes
	      (<= (AS-PATH-get-length obj) len)))))                              ; possible AS-SET after AS-SEQ so path length field can be smaller 
       
(defun AS-PATH-get-io-rw-octets (4-octet-asn-flag obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream.
If 4-OCTET-ASN-FLAG is T => list of 4 byte AS numbers (otherwise 2 bytes)"
  (+ (if (PATH-ATTRIB-get-flag-extended-len-bit obj)     ; attribute-type & attribute-length [ 3 or 4 bytes ]
	 4
	 3)
     (if 4-octet-asn-flag
	 (PATH-ATTRIB-get-attribute-length obj)
	 (1+ (/ (PATH-ATTRIB-get-attribute-length obj) 2)))))

;; NOTE: reading of as-path value uses attribute-length field rather than as-path-length as possible trailing AS_SET in path
(defun AS-PATH-io-read-4-octet-asn (attribute-type attribute-length stream-in)
  (cond ((> attribute-length 0)
	 (AS-PATH-make attribute-type
		       attribute-length
		       (io-read-uNbe u8 stream-in)                                      ; as-path-type
		       (io-read-uNbe u8 stream-in)                                      ; as-path-length
		       (io-read-uNbe-octets u32 (- attribute-length 2) stream-in)))     ; as-path-value
	(t  
	 (AS-PATH-make attribute-type 0 2 0 nil))))                               ; empty AS-PATH

;; NOTE: reading of as-path value uses attribute-length field rather than as-path-length as possible trailing AS_SET in path
(defun AS-PATH-io-read-2-octet-asn (attribute-type attribute-length stream-in)
  (cond ((> attribute-length 0)
	 (AS-PATH-make attribute-type
		       (- (* 2 attribute-length) 2)                                ; convert length to 4 octet asn on read and write
		       (io-read-uNbe u8 stream-in)                                      ; as-path-type
		       (io-read-uNbe u8 stream-in)                                      ; as-path-length
		       (io-read-uNbe-octets u16 (- attribute-length 2) stream-in)))     ; as-path-value
	(t  
	 (AS-PATH-make attribute-type 0 2 0 nil))))                                ; empty AS-PATH

(defun AS-PATH-io-read (4-octet-asn-flag attribute-type attribute-length stream-in)
  (if 4-octet-asn-flag
      (AS-PATH-io-read-4-octet-asn attribute-type attribute-length stream-in)
      (AS-PATH-io-read-2-octet-asn attribute-type attribute-length stream-in)))

(defun AS-PATH-io-write-4-octet-asn (obj stream-out)
  (destructuring-bind (attribute-type attribute-length as-path-type as-path-length . as-path-list)
      (cdr obj)
    (io-write-uNbe u16 attribute-type stream-out)
    (if (uN-bit-set-p u16 attribute-type 3)                     ; extended-length bit set
	(io-write-uNbe u16 attribute-length stream-out)
	(io-write-uNbe u8 attribute-length stream-out))

    (when (> attribute-length 0)
      (io-write-uNbe u8 as-path-type stream-out)
      (io-write-uNbe u8 as-path-length stream-out)
      (io-write-uNbe-octets u32 (- attribute-length 2) as-path-list stream-out))))
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

(defun AS-PATH-io-write-2-octet-asn (obj stream-out)
  (destructuring-bind (attribute-type attribute-length as-path-type as-path-length . as-path-list)
      (cdr obj)
    (io-write-uNbe u16 attribute-type stream-out)
    (if (uN-bit-set-p u16 attribute-type 3)                     ; extended-length bit set
	(io-write-uNbe u16 (1+ (/ attribute-length 2)) stream-out)
	(io-write-uNbe u8 (1+ (/ attribute-length 2)) stream-out))
    
    (when (> attribute-length 0)
      (io-write-uNbe u8 as-path-type stream-out)
      (io-write-uNbe u8 as-path-length stream-out)
      (io-write-uNbe-octets u16 (1- (/ attribute-length 2)) as-path-list stream-out))))

(defun AS-PATH-io-write (4-octet-asn-flag obj stream-out)
  (if 4-octet-asn-flag
      (AS-PATH-io-write-4-octet-asn obj stream-out)
      (AS-PATH-io-write-2-octet-asn obj stream-out)))

(defun AS-PATH-prepend (obj asn-u32 &optional (repeat-count 1))
  "Takes AS-PATH object and returns copy with ASN-U32 prepended to path REPEAT times"
   (destructuring-bind (attribute-type attribute-length as-path-type as-path-length . as-path-list)
       (cdr obj)
     (AS-PATH-make attribute-type
		   (if (> attribute-length 0)
		       (+ attribute-length (* 4 repeat-count))
		       (+ 2 (* 4 repeat-count)))
		   as-path-type
		   (+ as-path-length repeat-count)
		   (if (= repeat-count 1)
		       (cons asn-u32 as-path-list)
		       (labels ((rec (as-path-list count)
				  (if (<= count 0)
				      as-path-list
				      (rec (cons asn-u32 as-path-list) (1- count)))))
			 (rec as-path-list repeat-count))))))

(defun AS-PATH-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X #x~V,'0X #x~2,'0X #x~2,'0X ~{#x~8,'0X~^ ~})"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (if (PATH-ATTRIB-get-flag-extended-len-bit obj) 4 2) 
	  (PATH-ATTRIB-get-attribute-length obj)                 ; u8|u16
	  (AS-PATH-get-type obj)                                 ; u8
	  (AS-PATH-get-length obj)                               ; u8
          (AS-PATH-get-value obj)))                              ; list [ u16 | u32 ]

(set-pprint-dispatch '(cons (member AS-PATH)) #'AS-PATH-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

;; TODO if trailing AS_SET exists (path-type == as-set && attribute-length > (2 + (4 * length))) should print the AS_SET in brackets {}
(defun AS-PATH-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~4,'0X ~D [~W] ~D [~{~/sbgp:u32-pprint-asn/~^ ~}]]~)"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (PATH-ATTRIB-get-attribute-length obj)                 ; u8|u16
	  (case (AS-PATH-get-type obj)                           
	    (1 'AS-SET)
	    (2 'AS-SEQUENCE)
	    (t 'UNASSIGNED))
	  (AS-PATH-get-length obj)                               ; u8
          (AS-PATH-get-value obj)))                              ; list [ u16 | u32 ]

(set-pprint-dispatch '(cons (member AS-PATH)) #'AS-PATH-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'AS-PATH 'pprint-1) #'AS-PATH-pprint-1)
(setf (get 'AS-PATH 'pprint-2) #'AS-PATH-pprint-2)
(setf (get 'AS-PATH 'zhash) #'AS-PATH-zhash)






