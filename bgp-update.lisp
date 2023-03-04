(in-package :sbgp)

(defun BGP-UPDATE-make (withdrawn-routes-length withdrawn-routes path-attributes-length path-attributes nlri)
  (list 'BGP-UPDATE
	withdrawn-routes-length    ; u16
	withdrawn-routes           ; list of NLRI
	path-attributes-length     ; u16
	path-attributes            ; list of PATH-ATTRIB
	nlri))                     ; list of NLRI

(defun BGP-UPDATE-make-new (4-octet-asn-flag withdrawn-routes path-attributes nlri-list)
  "Returns two values: BGP-UPDATE object and number of bytes to read/write to network.
If mp-extensions-flag is T, update message is formatted as per rfc4760 with MP_REACH_NLRI and MP_UNREACH_NLRI"
  (let ((withdrawn-routes-length (if withdrawn-routes
				     (loop for nlri in withdrawn-routes
					   sum (NLRI-get-io-rw-octets nlri))
				     0))
	(path-attributes-length (loop for path-attrib in path-attributes
				      sum (PATH-ATTRIB-get-io-rw-octets 4-octet-asn-flag path-attrib)))
	(nlri-list-length (if nlri-list
			      (loop for nlri in withdrawn-routes
				    sum (NLRI-get-io-rw-octets nlri))
			      0)))
    (values (BGP-UPDATE-make withdrawn-routes-length
			     withdrawn-routes
			     path-attributes-length
			     path-attributes
			     nlri-list)
	    (+ 4
	       withdrawn-routes-length
	       path-attributes-length
	       nlri-list-length))))

(defun BGP-UPDATE-get-name (obj)             "-> symbol"              (car obj))
(defun BGP-UPDATE-get-withdrawn-len (obj)    "-> u16"                 (cadr obj))
(defun BGP-UPDATE-get-withdrawn-routes (obj) "-> list of NLRI"        (caddr obj))
(defun BGP-UPDATE-get-path-attrib-len (obj)  "-> u16"                 (cadddr obj))
(defun BGP-UPDATE-get-path-attributes (obj)  "-> list of PATH-ATTRIB" (car (cddddr obj)))
(defun BGP-UPDATE-get-nlri (obj)             "-> list of NLRI"        (cadr (cddddr obj)))

(defun BGP-UPDATE-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (+ 4
     (BGP-UPDATE-get-withdrawn-len obj)
     (BGP-UPDATE-get-path-attrib-len obj)
     (loop for nlri in (BGP-UPDATE-get-nlri obj)
	   sum (NLRI-get-io-rw-octets nlri))))

(defun BGP-UPDATE-io-read (4-octet-asn-flag message-length stream-in)
  (let* ((withdrawn-routes-length (io-read-uNbe u16 stream-in))

	 (withdrawn-routes (NLRI-WITHDRAWL-io-read-list withdrawn-routes-length
							+AFISAFI-ipv4-unicast+
							stream-in))
	 
	 (path-attributes-length (io-read-uNbe u16 stream-in))

	 (path-attributes (PATH-ATTRIB-io-read-list path-attributes-length
						    4-octet-asn-flag
						    stream-in))
	 
	 (nlri-len-octets (- message-length
			     withdrawn-routes-length
			     path-attributes-length
			     4))

	 (nlri (NLRI-io-read-list nlri-len-octets
				  +AFISAFI-ipv4-unicast+
				  stream-in)))
    (list 'BGP-UPDATE
	  withdrawn-routes-length
	  withdrawn-routes
	  path-attributes-length
	  path-attributes
	  nlri)))

(defun BGP-UPDATE-io-write (4-octet-asn-flag obj stream-out)
  (destructuring-bind (withdrawn-routes-length withdrawn-routes path-attributes-length path-attributes nlri)
      (cdr obj)
    (io-write-uNbe u16 withdrawn-routes-length stream-out)
    (dolist (elem withdrawn-routes)
      (NLRI-io-write elem stream-out))
    
    (io-write-uNbe u16 path-attributes-length stream-out)
    (dolist (elem path-attributes)
      (PATH-ATTRIB-io-write 4-octet-asn-flag elem stream-out))

    (dolist (elem nlri)
      (NLRI-io-write elem stream-out))))


(defun BGP-UPDATE-valid2-p (obj)
  (if obj t nil))
#|  (and (= (length obj) 6)
       (u16-p (BGP-UPDATE-get-withdrawn-len obj))
       (every #'NLRI-WITHDRAWL-typep (BGP-UPDATE-get-withdrawn-routes obj))
       (u16-p (BGP-UPDATE-get-path-attrib-len obj))
       (every #'PATH-ATTRIB-typep (BGP-UPDATE-get-path-attributes obj))
       (every #'NLRI-typep (BGP-UPDATE-get-nlri obj))))|#

(deftype BGP-UPDATE () '(cons (member BGP-UPDATE)))


(defun BGP-UPDATE-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~4,'0X (~{~W~^ ~}) #x~4,'0X (~{~W~^ ~}) (~{~W~^ ~}))"
	  (TL-get-name obj)             ; symbol
	  (BGP-UPDATE-get-withdrawn-len obj)    ; u16
	  (BGP-UPDATE-get-withdrawn-routes obj) ; list of NLRI
	  (BGP-UPDATE-get-path-attrib-len obj)  ; u16
	  (BGP-UPDATE-get-path-attributes obj)  ; list of PATH-ATTRIB
	  (BGP-UPDATE-get-nlri obj)))           ; list of NLRI

(set-pprint-dispatch '(cons (member BGP-UPDATE)) #'BGP-UPDATE-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun BGP-UPDATE-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W ~D [~{~W~^ ~}] ~D [~{~W~^ ~}] [~{~W~^ ~}]]~)"
	  (TL-get-name obj)             ; symbol
	  (BGP-UPDATE-get-withdrawn-len obj)    ; u16
	  (BGP-UPDATE-get-withdrawn-routes obj) ; list of NLRI
	  (BGP-UPDATE-get-path-attrib-len obj)  ; u16
	  (BGP-UPDATE-get-path-attributes obj)  ; list of PATH-ATTRIB
	  (BGP-UPDATE-get-nlri obj)))           ; list of NLRI

(set-pprint-dispatch '(cons (member BGP-UPDATE)) #'BGP-UPDATE-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

