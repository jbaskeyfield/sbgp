(in-package :sbgp)

;;; BGP-CAP COMMON FUNCTIONS 1

(defparameter +BGP-CAP-all-types+ '(BGP-CAP-UNKNOWN BGP-CAP-MP-EXTENSIONS BGP-CAP-ROUTE-REFRESH BGP-CAP-4-OCTET-ASN) "list of all defined BGP Capability types")  

(defun BGP-CAP-get-cap-type (obj)    "-> u8"      (cadr obj))
(defun BGP-CAP-get-cap-length (obj)  "-> u8"      (caddr obj))

(defun BGP-CAP-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (+ 2 (BGP-CAP-get-cap-length obj)))

(defun BGP-CAP-UNKNOWN-make (cap-type cap-length bytes)
  (list 'BGP-CAP-UNKNOWN
	cap-type
        cap-length
	bytes))

(defun BGP-CAP-UNKNOWN-get-bytes (obj) "-> BYTES"  (cadddr obj))

(defun BGP-CAP-UNKNOWN-io-read (cap-type cap-length stream-in)
  (list 'BGP-CAP-UNKNOWN
	cap-type
	cap-length
	(BYTES-io-read cap-length stream-in)))

(defun BGP-CAP-UNKNOWN-io-write (obj stream-out)
  (destructuring-bind (cap-type cap-length bytes)
      (cdr obj)
    (io-write-uNbe u8 cap-type stream-out)
    (io-write-uNbe u8 cap-length stream-out)
    (BYTES-io-write bytes stream-out)))

(deftype BGP-CAP-UNKNOWN () '(cons (member BGP-CAP-UNKNOWN)))


(defun BGP-CAP-UNKNOWN-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~A ~D ~D ~/sbgp:bytes-pprint-1/)"
	  (TL-get-name obj)
	  (BGP-CAP-get-cap-type obj)
	  (BGP-CAP-get-cap-length obj)
	  (BGP-CAP-UNKNOWN-get-bytes obj)))

(set-pprint-dispatch 'BGP-CAP-UNKNOWN #'BGP-CAP-UNKNOWN-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun BGP-CAP-UNKNOWN-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~A ~D ~D ~/sbgp:bytes-pprint-2/]~)"
	  (TL-get-name obj)
	  (BGP-CAP-get-cap-type obj)
	  (BGP-CAP-get-cap-length obj)
	  (BGP-CAP-UNKNOWN-get-bytes obj)))

(set-pprint-dispatch '(cons (member BGP-CAP-UNKNOWN)) #'BGP-CAP-UNKNOWN-pprint-2 0 *sbgp-pprint-dispatch-table-2*)
	

;;; BGP-CAP-MP-EXTENSIONS
;;; Type 1 - Multiprotocol Extensions for BGP-4 [RFC2858]

(defun BGP-CAP-MP-EXTENSIONS-make (afisafi)
  (list 'BGP-CAP-MP-EXTENSIONS
        1   ; Type 1
	4   ; length = 4 bytes AFI/SAFI field, 24bit AFISAFI object (shared with NLRI object) has extra 8 bit reserved field. 
	afisafi))

(defun BGP-CAP-MP-EXTENSIONS-get-afisafi (obj) "-> AFISAFI" (cadddr obj))

(defun BGP-CAP-MP-EXTENSIONS-io-read (cap-type cap-length stream-in)
  (list 'BGP-CAP-MP-EXTENSIONS
	cap-type
	cap-length
	(let ((afi (io-read-uNbe u16 stream-in)))     ; 16 bit afi 
	  (io-read-uNbe u8 stream-in)                 ; reserved 8 bits
	  (let ((safi (io-read-uNbe u8 stream-in)))   ; 8 bit safi
	    (AFISAFI-make afi safi)))))

(defun BGP-CAP-MP-EXTENSIONS-io-write (obj stream-out)
  (destructuring-bind (cap-type cap-length afisafi)
      (cdr obj)
    (io-write-uNbe u8 cap-type stream-out)
    (io-write-uNbe u8 cap-length stream-out)
    (io-write-uNbe u16 (AFISAFI-get-afi afisafi) stream-out)
    (io-write-uNbe u8 0 stream-out)
    (io-write-uNbe u8 (AFISAFI-get-safi afisafi) stream-out)))     

(deftype BGP-CAP-MP-EXTENSIONS () '(cons (member BGP-CAP-MP-EXTENSIONS)))

;;; BGP-CAP-ROUTE-REFRESH
;;; Type 2 - Route Refresh Capability for BGP-4 [RFC2918]

(defun BGP-CAP-ROUTE-REFRESH-make ()
  (list 'BGP-CAP-ROUTE-REFRESH
	2   ; Type 2
	0)) ; Zero bytes)

(defun BGP-CAP-ROUTE-REFRESH-io-read (cap-type cap-length)
  ;; TODO: test valid
  (list 'BGP-CAP-ROUTE-REFRESH
	cap-type   ; Type 2
	cap-length))

(defun BGP-CAP-ROUTE-REFRESH-io-write (obj stream-out)
  (destructuring-bind (cap-type cap-length)
      (cdr obj)
    (io-write-uNbe u8 cap-type stream-out)
    (io-write-uNbe u8 cap-length stream-out)))

(deftype BGP-CAP-ROUTE-REFRESH () '(cons (member BGP-CAP-ROUTE-REFRESH)))


(defun BGP-CAP-ROUTE-REFRESH-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~A ~D ~D)"
	  (TL-get-name obj)
	  (BGP-CAP-get-cap-type obj)
	  (BGP-CAP-get-cap-length obj)))

(set-pprint-dispatch 'BGP-CAP-ROUTE-REFRESH #'BGP-CAP-ROUTE-REFRESH-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun BGP-CAP-ROUTE-REFRESH-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~A ~D ~D]~)"
	  (TL-get-name obj)
	  (BGP-CAP-get-cap-type obj)
	  (BGP-CAP-get-cap-length obj)))

(set-pprint-dispatch 'BGP-CAP-ROUTE-REFRESH #'BGP-CAP-ROUTE-REFRESH-pprint-2 0 *sbgp-pprint-dispatch-table-2*)


;;; BGP-CAP-4-OCTET-ASN
;;; Type 65 -  Support for 4-octet AS number capability [RFC6793]

(defun BGP-CAP-MP-EXTENSIONS-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~A ~D ~D #x~6,'0X)"
	  (TL-get-name obj)
	  (BGP-CAP-get-cap-type obj)
	  (BGP-CAP-get-cap-length obj)
	  (BGP-CAP-MP-EXTENSIONS-get-afisafi obj)))

(set-pprint-dispatch 'BGP-CAP-MP-EXTENSIONS #'BGP-CAP-MP-EXTENSIONS-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun BGP-CAP-MP-EXTENSIONS-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~A ~D ~D #x~6,'0X]~)"
	  (TL-get-name obj)
	  (BGP-CAP-get-cap-type obj)
	  (BGP-CAP-get-cap-length obj)
	  (BGP-CAP-MP-EXTENSIONS-get-afisafi obj)))

(set-pprint-dispatch 'BGP-CAP-MP-EXTENSIONS #'BGP-CAP-MP-EXTENSIONS-pprint-2 0 *sbgp-pprint-dispatch-table-2*)


;;; BGP-CAP-4-OCTET-ASN
;;; Type 65 -  Support for 4-octet AS number capability [RFC6793]

(defun BGP-CAP-4-OCTET-ASN-make (asn4)
  (list 'BGP-CAP-4-OCTET-ASN
	65   ; Type 65
	4    ; 4 octet ASN field
	asn4))

(defun BGP-CAP-4-OCTET-ASN-get-asn4 (obj) "-> u32"    (cadddr obj))
       
(defun BGP-CAP-4-OCTET-ASN-io-read (cap-type cap-length stream-in)
  (list 'BGP-CAP-4-OCTET-ASN
	cap-type
	cap-length
	(io-read-uNbe u32 stream-in)))

(defun BGP-CAP-4-OCTET-ASN-io-write (obj stream-out)
  (destructuring-bind (cap-type cap-length asn4)
      (cdr obj)
    (io-write-uNbe u8 cap-type stream-out)
    (io-write-uNbe u8 cap-length stream-out)
    (io-write-uNbe u32 asn4 stream-out)))

(deftype BGP-CAP-4-OCTET-ASN () '(cons (member BGP-CAP-4-OCTET-ASN)))


(defun BGP-CAP-4-OCTET-ASN-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~A ~D ~D #x~8,'0X)"
	  (TL-get-name obj)
	  (BGP-CAP-get-cap-type obj)
	  (BGP-CAP-get-cap-length obj)
	  (BGP-CAP-4-OCTET-ASN-get-asn4 obj)))

(set-pprint-dispatch 'BGP-CAP-4-OCTET-ASN #'BGP-CAP-4-OCTET-ASN-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun BGP-CAP-4-OCTET-ASN-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~A ~D ~D [~/sbgp:u32-pprint-asn/]]~)"
	  (TL-get-name obj)
	  (BGP-CAP-get-cap-type obj)
	  (BGP-CAP-get-cap-length obj)
	  (BGP-CAP-4-OCTET-ASN-get-asn4 obj)))

(set-pprint-dispatch 'BGP-CAP-4-OCTET-ASN #'BGP-CAP-4-OCTET-ASN-pprint-2 0 *sbgp-pprint-dispatch-table-2*)
	  

;;; BGP-CAP COMMON FUNCTIONS 2

(defun BGP-CAP-io-read (octets stream-in)
  (let* ((type (io-read-uNbe u8 stream-in))
	 (length (io-read-uNbe u8 stream-in)))
    (if (= length (- octets 2)) ;; length of BGP-CAP should match OPEN-OPT-PARAM length
        (case type
	  (1  (BGP-CAP-MP-EXTENSIONS-io-read type length stream-in))  ; 1  Multiprotocol Extensions for BGP-4  [RFC2858]
	  (2  (BGP-CAP-ROUTE-REFRESH-io-read type length))            ; 2  Route Refresh Capability for BGP-4 [RFC2918]
	  (65 (BGP-CAP-4-OCTET-ASN-io-read type length stream-in))    ; 65  Support for 4-octet AS number capability [RFC6793]
	  (t  (BGP-CAP-UNKNOWN-io-read type length stream-in)))
	;; TODO: error handling and catch custom error
	(error "BGP-CAP-io-read, length = ~S, octets = ~S. length of BGP-CAP should be OPEN-OPT-PARAM - 2" length octets))))

(defun BGP-CAP-io-write (obj stream-out)
  (typecase obj
    (BGP-CAP-MP-EXTENSIONS  (BGP-CAP-MP-EXTENSIONS-io-write obj stream-out))
    (BGP-CAP-ROUTE-REFRESH  (BGP-CAP-ROUTE-REFRESH-io-write obj stream-out))
    (BGP-CAP-4-OCTET-ASN    (BGP-CAP-4-OCTET-ASN-io-write obj stream-out))
    (BGP-CAP-UNKNOWN        (BGP-CAP-UNKNOWN-io-write obj stream-out))))

(deftype BGP-CAP () '(cons (member BGP-CAP-UNKNOWN BGP-CAP-MP-EXTENSIONS BGP-CAP-ROUTE-REFRESH BGP-CAP-4-OCTET-ASN)))

