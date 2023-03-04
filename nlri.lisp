(in-package :sbgp)

;;; AFISAFI. AFI/SAFI 3 byte field as formatted in MP_REACH_NLRI.
;;; | AFI (2 octets) | SAFI (1 octet) |
;;; Note: The 4 byte field in BGP-CAP shares this object but is read/written with 8 bit zero padding
;;; between the AFI and SAFI. AFISAFI-io-read/write just performs single 3 byte read/write so is only suitable for MP_REACH_NLRI. 

;;; 3 byte constants
(defconstant +AFISAFI-ipv4-unicast+   #x000101)
(defconstant +AFISAFI-ipv6-unicast+   #x000201)
(defconstant +AFISAFI-ipv4-multicast+ #x000102)
(defconstant +AFISAFI-ipv6-multicast+ #x000202)
(defconstant +AFISAFI-ipv4-mpls+      #x000104)
(defconstant +AFISAFI-ipv6-mpls+      #x000204)
(defconstant +AFISAFI-ipv4-mpls-vpn+  #x000180)
(defconstant +AFISAFI-ipv6-mpls-vpn+  #x000280)
(defconstant +AFISAFI-ipv4-vpn-mcast+ #x000181)
(defconstant +AFISAFI-ipv6-vpn-mcast+ #x000281)
(defconstant +AFISAFI-ipv4-flowspec+  #x000185)
(defconstant +AFISAFI-ipv6-flowspec+  #x000285)

;;; AFI / SAFI values suitable for passing to AFISAFI-make
(defconstant +AFI-ipv4+           1)
(defconstant +AFI-ipv6+           2)

(defconstant +SAFI-unicast+	  1)
(defconstant +SAFI-multicast+	  2)
(defconstant +SAFI-mpls+	  4)
(defconstant +SAFI-mpls_vpn+	  128)
(defconstant +SAFI-vpn_multicast+ 129)
(defconstant +SAFI-flow+          133)

(defun AFISAFI-make (afi safi)
  "Returns u24 [16-bits AFI] [8-bits SAFI]"
  (+ (ash (logand #xffff afi) 8)
     (logand #xff safi)))

(defun AFISAFI-get-afi (atom-u24)
  "Returns integer, upper 2 bytes of ATOM-U24"
  (logand #xffff (ash atom-u24 -8)))

(defun AFISAFI-get-safi (atom-u24)
  "Returns integer, lower byte of ATOM-U24"
  (logand #xff atom-u24))

(defun AFISAFI-valid1-p (obj)
  (u24-p obj))

(defun AFISAFI-io-read (stream-in)
  "Reads 3 byte AFI-SAFI field (as formatted in MP_REACH_NLRI)"
  (io-read-uNbe u24 stream-in))

(defun AFISAFI-io-write (atom-u24 stream-out)
  "Reads 3 byte AFI-SAFI field (as formatted in MP_REACH_NLRI)"
  (io-write-uNbe u24 atom-u24 stream-out))


;;; NLRI, NLRI-WITHDRAWL

(defun NLRI-get-afisafi (obj)            "-> AFISAFI"  (cadr obj))
(defun NLRI-get-prefix-length-bits (obj) "-> u8"       (caddr obj))
(defun NLRI-get-prefix (obj)             "-> list-u56" (cdddr obj))

(defun NLRI-zhash (octet-offset obj)
  (zhash-tagged-list '(1 7) octet-offset obj))

(defun NLRI-make (afisafi prefix-length prefix)
  "Returns tagged list ('NLRI [AFISAFI] [u8] . [list-u56])"
  (let ((obj (cons 'NLRI
		   (cons afisafi
			 (cons prefix-length prefix)))))
    (if *nlri-cache*
	(NLRI-CACHE-ro-lookup *nlri-cache*
			      obj
			      (NLRI-zhash 0 obj))
	obj)))

(defun NLRI-make-new-ipv4-unicast (ipv4-address prefix-len-bits)
  (NLRI-make +AFISAFI-ipv4-unicast+
	     prefix-len-bits
	     (cons (ash (IPV4-get-value ipv4-address) 24)  ; right shift u32 ipv4 address value to top of u56
	           nil)))

(defun NLRI-make-new-ipv6-unicast (ipv6-address prefix-len-bits)
  (NLRI-make +AFISAFI-ipv6-unicast+
	     prefix-len-bits
	     (map 'list
		  #'logand
		  (list-uN-make-bit-mask u56 3 prefix-len-bits)
		  (list-un->list-um-subseq u32 u56 0 (ceiling (/ prefix-len-bits 8)) (IPV6-get-value ipv6-address)))))

(defun NLRI-WITHDRAWL-make (afisafi prefix-length-bits prefix)
  "Returns tagged list ('NLRI-WITHDRAWL [AFISAFI] [u8] . [list-u56])"
  (cons 'NLRI-WITHDRAWL
	(cons afisafi
	      (cons prefix-length-bits prefix))))

(defun NLRI-valid1-p (obj)
  (and (consp obj)
       (>= (length obj) 4)
       (<= (length obj) 8)              
       (u24-p (NLRI-get-afisafi obj))
       (u8-p (NLRI-get-prefix-length-bits obj))
       (every #'u56-p (NLRI-get-prefix obj))))

(deftype NLRI () '(and (cons (member NLRI)) (satisfies NLRI-valid1-p)))
(deftype NLRI-WITHDRAWL () '(and (cons (member NLRI-WITHDRAWL)) (satisfies NLRI-valid1-p)))

(defun NLRI-typep (obj) (typep obj 'NLRI))
(defun NLRI-WITHDRAWL-typep (obj) (typep obj 'NLRI-WITHDRAWL))

(defun NLRI/NLRI-WITHDRAWL-equal (n1 n2)
  "Equal function to be used whrn comparing NLRI with NLRI-WITHDRWAL. Ignores head of list"
  (equal (cdr n1) (cdr n2)))

(defun NLRI-ipv4-unicast-valid2-p (obj)
  (and (eq 'NLRI (TL-get-name obj))
       (= (NLRI-get-afisafi obj) +AFISAFI-ipv4-unicast+)
       (>= (NLRI-get-prefix-length-bits obj) 0)
       (<= (NLRI-get-prefix-length-bits obj) 32)
       (= (length obj) 4)))

(defun NLRI-ipv6-unicast-valid2-p (obj)
  (and (eq 'NLRI (TL-get-name obj))
       (= (NLRI-get-afisafi obj) +AFISAFI-ipv4-unicast+)
       (>= (NLRI-get-prefix-length-bits obj) 0)
       (<= (NLRI-get-prefix-length-bits obj) 128)
       (<= (length obj) 6)))


(defun NLRI-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (1+ (ceiling (/ (NLRI-get-prefix-length-bits obj) 8))))

(defun NLRI-io-read (afisafi stream-in)
  (let ((prefix-length-bits (io-read-uNbe u8 stream-in)))
    (if (> prefix-length-bits 0)
	(NLRI-make afisafi
		   prefix-length-bits
		   (io-read-uNbe-octets u56
					(ceiling (/ prefix-length-bits 8))
					stream-in))
        (NLRI-make afisafi
		   0
		   0))))

(defun NLRI-io-read-list (octets afisafi stream-in)
  (labels ((recurse (bytes-read)
	     (if (>= bytes-read octets)
		 '()
		 (let ((elem (NLRI-io-read afisafi stream-in)))
		   (cons elem
			 (recurse (+ bytes-read (NLRI-get-io-rw-octets elem))))))))
    (recurse 0)))

(defun NLRI-WITHDRAWL-io-read (afisafi stream-in)
  (let ((prefix-length-bits (io-read-uNbe u8 stream-in)))
    (if (> prefix-length-bits 0)
	(NLRI-WITHDRAWL-make afisafi
		   prefix-length-bits
		   (io-read-uNbe-octets u56
					(ceiling (/ prefix-length-bits 8))
					stream-in))
        (NLRI-WITHDRAWL-make afisafi
		   0
		   0))))

(defun NLRI-WITHDRAWL-io-read-list (octets afisafi stream-in)
  (labels ((recurse (bytes-read)
	     (if (>= bytes-read octets)
		 '()
		 (let ((elem (NLRI-WITHDRAWL-io-read afisafi stream-in)))
		   (cons elem
			 (recurse (+ bytes-read (NLRI-get-io-rw-octets elem))))))))
    (recurse 0)))

(defun NLRI-io-write (obj stream-out)
  (let ((prefix-length-bits (NLRI-get-prefix-length-bits obj)))
    (io-write-uNbe u8
		   prefix-length-bits
		   stream-out)
    (if (> prefix-length-bits 0)
	(io-write-uNbe-octets u56
			      (ceiling (/ prefix-length-bits 8))
			      (NLRI-get-prefix obj)
			      stream-out))))
  
(defun NLRI-bit-set-p (obj bit-position-from-MSB)  ; indexed from 0 as MSB
  (list-uN-bit-set-p u56
		     (NLRI-get-prefix obj)
		     bit-position-from-MSB))

(defun NLRI->IPV4 (obj)
  (IPV4-make (ash (first (NLRI-get-prefix obj)) -24)))

(defun NLRI->IPV6 (obj)
  (IPV6-make (list-uN->list-uM-subseq u56 u32 0 16 (NLRI-get-prefix obj))))

(defun NLRI->BYTES (obj)
  (BYTES-make (ceiling (/ (NLRI-get-prefix-length-bits obj) 8))
	      (NLRI-get-prefix obj)))


(defun NLRI-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~A #x~6,'0X ~D ~{#x~14,'0X~^ ~})"
	  (TL-get-name obj)
	  (NLRI-get-afisafi obj)
	  (NLRI-get-prefix-length-bits obj)
	  (NLRI-get-prefix obj)))

(set-pprint-dispatch 'NLRI #'NLRI-pprint-1 0 *sbgp-pprint-dispatch-table-1*)
(set-pprint-dispatch 'NLRI-WITHDRAWL #'NLRI-pprint-1 0 *sbgp-pprint-dispatch-table-1*)
    
(defun NLRI-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (case (AFISAFI-get-afi (NLRI-get-afisafi obj))
    (1
     (format port "~([~A #x~6,'0X ~/sbgp:u32-pprint-ipv4//~D]~)"
	     (TL-get-name obj)
	     (NLRI-get-afisafi obj)
	     (IPV4-get-value (NLRI->IPV4 obj))
	     (NLRI-get-prefix-length-bits obj)))
    (2
     (format port "~([~A #x~6,'0X ~/sbgp:list-u32-pprint-ipv6//~D]~)"
	     (TL-get-name obj)
	     (NLRI-get-afisafi obj)
	     (IPV6-get-value (NLRI->IPV6 obj))
	     (NLRI-get-prefix-length-bits obj)))
    (t
     (format port "~((~A #x~6,'0X ~/sbgp:BYTES-pprint-2/)~)"
	     (TL-get-name obj)
	     (NLRI-get-afisafi obj)
	     (NLRI->BYTES obj)))))
 	     
(set-pprint-dispatch 'NLRI #'NLRI-pprint-2 0 *sbgp-pprint-dispatch-table-2*)
(set-pprint-dispatch 'NLRI-WITHDRAWL #'NLRI-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'NLRI 'pprint-1) #'NLRI-pprint-1)
(setf (get 'NLRI 'pprint-2) #'NLRI-pprint-2)
(setf (get 'NLRI 'zhash) #'NLRI-zhash)

(setf (get 'NLRI-WITHDRWAL 'pprint-1) #'NLRI-pprint-1)
(setf (get 'NLRI-WITHDRAWL 'pprint-2) #'NLRI-pprint-2)
(setf (get 'NLRI-WITHDRAWL 'zhash) #'NLRI-zhash)
