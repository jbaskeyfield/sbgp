(in-package :sbgp)

;;; BYTES. byte-blob list type

(defun BYTES-get-length-octets (obj) "-> integer"  (cadr obj))
(defun BYTES-get-value (obj)         "-> list-u56" (cddr obj))

(defun BYTES-zhash (octet-offset obj)
  (logxor (zhash-integer u16 octet-offset (BYTES-get-length-octets obj))
	  (zhash-list u56 (+ 2 octet-offset) (BYTES-get-value obj))))

(defun BYTES-make (length-octets list-u56)
  "Returns tagged list ('BYTES [integer] . [list-u56]) "
  (cons 'BYTES
	(cons length-octets list-u56)))

(defun BYTES-make-new-ascii (ascii-string)
  "Returns new BYTES object from passed ASCII-STRING" 
  (let* ((list-u8 (map 'list #'char-code (coerce ascii-string 'list)))
	 (length-octets (length list-u8))
	 (list-u56 (list-uN->list-uM u8 u56 list-u8)))
    (BYTES-make length-octets list-u56)))

(defun BYTES-make-new-vector-u8 (vector-u8)
  "Returns new BYTES object from passed simple vector" 
  (BYTES-make (length vector-u8)
	      (list-uN->list-uM u8 u56 (coerce vector-u8 'list))))

(defun BYTES-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (u16-p (BYTES-get-length-octets obj))
       (every #'u56-p (BYTES-get-value obj))))

(deftype BYTES () '(and (cons (member BYTES)) (satisfies BYTES-valid1-p)))
(defun BYTES-typep (obj) (typep obj 'BYTES))

(defun BYTES-valid2-p (obj)
  "Test list values are within allowed range"
  (and (eq 'BYTES (TL-get-name obj))
       (and (>= (BYTES-get-length-octets obj) 0)
	    (<= (BYTES-get-length-octets obj) 4077)) ;; maximum message length (4096) minus header length (19)
       (= (length (BYTES-get-value obj))
	  (ceiling (/ (BYTES-get-length-octets obj) u56)))))

(defun BYTES-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (BYTES-get-length-octets obj))

(defun BYTES-io-read (length-octets stream-in)
  (BYTES-make length-octets
	      (io-read-uNbe-octets u56 length-octets stream-in)))

(defun BYTES-io-write (obj stream-out)
  (io-write-uNbe-octets u56
			(BYTES-get-length-octets obj)
			(BYTES-get-value obj)
			stream-out))

(defun BYTES-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~A ~D ~{#x~14,'0X~^ ~})" (TL-get-name obj) (BYTES-get-length-octets obj) (BYTES-get-value obj)))

(set-pprint-dispatch 'BYTES #'BYTES-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun BYTES-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~A ~D ~{#x~14,'0X~^ ~}]~)" (TL-get-name obj) (BYTES-get-length-octets obj) (BYTES-get-value obj)))

(set-pprint-dispatch '(cons (member BYTES)) #'BYTES-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

(setf (get 'BYTES 'pprint-1) #'BYTES-pprint-1)
(setf (get 'BYTES 'pprint-2) #'BYTES-pprint-2)
(setf (get 'BYTES 'zhash) #'BYTES-zhash)
