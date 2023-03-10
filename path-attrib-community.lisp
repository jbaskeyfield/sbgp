(in-package :sbgp)

(defun COMMUNITY-get-value (obj) "-> list u32" (cdddr obj))

(defun COMMUNITY-zhash (octet-offset obj)
  (logxor (zhash-integer u16 octet-offset (PATH-ATTRIB-get-attribute-type-field obj))
	  (zhash-integer u16 (+ 2 octet-offset) (PATH-ATTRIB-get-attribute-length obj))
	  (zhash-list u32 (+ 4 octet-offset) (COMMUNITY-get-value obj))))

(defun COMMUNITY-make (attribute-type attribute-length list-u32)
  "Returns tagged list ('COMMUNITY attribute-type attribute-length . list-u32)"
  (let ((obj (cons 'COMMUNITY
		   (cons attribute-type  
			 (cons attribute-length
		               list-u32)))))
    (if *path-attrib-cache*
	(CACHE-lookup *path-attrib-cache*
		      obj
		      (COMMUNITY-zhash 0 obj))
	obj)))

(defun COMMUNITY-make-new (list-u32)
  (let ((attribute-length (* 4 (length list-u32))))
    (COMMUNITY-make (if (> attribute-length 255)
			#xD008  ; Optional-bit(#x8000) set; Transitive-bit(#x4000) set; Partial-bit(#x2000) not set; Extended-Length(#x1000) set; Type 8
			#xC008) ; Optional-bit(#x8000) set; Transitive-bit(#x4000) set; Partial-bit(#x2000) not set; Extended-Length(#x1000) not set; Type
		    attribute-length
		    list-u32)))

(defun COMMUNITY-valid1-p (obj)
   "Test list elements are of correct type"
  (and (consp obj)
       (>= (length obj) 3)
       (u16-p (PATH-ATTRIB-get-attribute-type-field obj))
       (u16-p (PATH-ATTRIB-get-attribute-length obj))
       (listp (COMMUNITY-get-value obj))
       (every #'u32-p (COMMUNITY-get-value obj))))

(deftype COMMUNITY () '(and (cons (member COMMUNITY)) (satisfies COMMUNITY-valid1-p)))

(defun COMMUNITY-typep (obj) (typep obj 'COMMUNITY))

(defun COMMUNITY-valid2-p (obj)
   "Test list values are within allowed range"
  (and (eq 'COMMUNITY (TL-get-name obj))
       (= #xC008 (logandc1 #x1000 (PATH-ATTRIB-get-attribute-type-field obj)))   ; type field masked with extended length bit
       (= (PATH-ATTRIB-get-attribute-length obj)
	  (* 4 (length (COMMUNITY-get-value obj))))))

(defun COMMUNITY-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (+ (if (PATH-ATTRIB-get-flag-extended-len-bit obj)                      ; attribute-type & attribute-length [ 3 or 4 bytes ]
	 4
	 3)
     (PATH-ATTRIB-get-attribute-length obj)))

(defun COMMUNITY-io-read (attribute-type attribute-length stream-in)
  (COMMUNITY-make attribute-type
		  attribute-length
		  (io-read-uNbe-octets u32 attribute-length stream-in)))

(defun COMMUNITY-io-write (obj stream-out)
  (destructuring-bind (attribute-type attribute-length . list-u32)
      (cdr obj)
    (io-write-uNbe u16 attribute-type stream-out)
    (if (uN-bit-set-p u16 attribute-type 3)           ; extended length bit set
	(io-write-uNbe u16 attribute-length stream-out)     
	(io-write-uNbe u8 attribute-length stream-out))

    (io-write-uNbe-octets u32 attribute-length list-u32 stream-out)))

(defun COMMUNITY-well-known-integer->symbol (i)
  (case i
    (23456              'AS_TRANS)                     ; [RFC4893]
    (#xffff0000 	'GRACEFUL_SHUTDOWN) 	       ; [RFC8326]
    (#xffff0001         'ACCEPT_OWN) 	               ; [RFC7611]
    (#xffff0002	        'ROUTE_FILTER_TRANSLATED_v4)   ; [draft-l3vpn-legacy-rtc]
    (#xffff0003 	'ROUTE_FILTER_v4) 	       ; [draft-l3vpn-legacy-rtc]
    (#xffff0004 	'ROUTE_FILTER_TRANSLATED_v6)   ; [draft-l3vpn-legacy-rtc]
    (#xffff0005 	'ROUTE_FILTER_v6) 	       ; [draft-l3vpn-legacy-rtc]
    (#xffff0006 	'LLGR_STALE) 	               ; [draft-uttaro-idr-bgp-persistence]
    (#xffff0007 	'NO_LLGR) 	               ; [draft-uttaro-idr-bgp-persistence]
    (#xffff0008 	'ACCEPT-OWN-NEXTHOP) 	       ; [draft-agrewal-idr-accept-own-nexthop]
    (#xffff0009 	'STANDBY-PE) 	               ; [RFC9026]
    (#xffff029A 	'BLACKHOLE) 	               ; [RFC7999]
    (#xffffff01 	'NO_EXPORT) 	               ; [RFC1997]
    (#xffffff02 	'NO_ADVERTISE) 	               ; [RFC1997]
    (#xffffff03 	'NO_EXPORT_SUBCONFED) 	       ; [RFC1997]
    (#xffffff04 	'NOPEER) 	                   ; [RFC3765]
    (t nil)))


(defun COMMUNITY-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X #x~V,'0X ~{#x~8,'0X~^ ~})"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (if (PATH-ATTRIB-get-flag-extended-len-bit obj) 4 2) 
	  (PATH-ATTRIB-get-attribute-length obj)                 ; [ u8 | u16 ]
	  (COMMUNITY-get-value obj)))                            ; list u32
	  
(set-pprint-dispatch '(cons (member COMMUNITY)) #'COMMUNITY-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

;; TODO update u32-print-community or pprint-2 to print well known values as strings or symbols
(defun COMMUNITY-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~4,'0X ~D [~{~/sbgp:u32-pprint-community/~^ ~}]]~)"
	  (TL-get-name obj)                             ; symbol
	  (PATH-ATTRIB-get-attribute-type-field obj)             ; u16
	  (PATH-ATTRIB-get-attribute-length obj)                 ; [ u8 | u16 ]
	  (COMMUNITY-get-value obj)))                            ; list u32

(set-pprint-dispatch '(cons (member COMMUNITY)) #'COMMUNITY-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'COMMUNITY 'pprint-1) #'COMMUNITY-pprint-1)
(setf (get 'COMMUNITY 'pprint-2) #'COMMUNITY-pprint-2)
(setf (get 'COMMUNITY 'zhash) #'COMMUNITY-zhash)
