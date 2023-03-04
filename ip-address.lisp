;;; IPV4 and IPV6 address types
;;; (IPV4 u32), (IPV6 u32 u32 u32 u32)


(in-package :sbgp)

;;; IPV4. IPv4 Address type

;; format: (IPV4 value-u32)

(defun IPV4-get-value (obj) "-> u32" (cadr obj))

(defun IPV4-zhash (octet-offset obj)
  (zhash-integer u32 octet-offset (IPV4-get-value obj)))

(defun IPV4-make (atom-u32)
  "Returns tagged list ('IPV4 [u32])"
  (list 'IPV4 atom-u32))

(defun IPV4-make-new (atom-u32)
  (IPV4-make atom-u32))


(defun IPV4-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (= (length obj) 2)
       (u32-p (IPV4-get-value obj))))

(deftype IPV4 () '(and (cons (member IPV4)) (satisfies IPV4-valid1-p)))
(defun IPV4-typep (obj) (typep obj 'IPV4))

(defun IPV4-valid2-p (obj)
  "Test list values are within allowed range"
  (and (eq 'IPV4 (TL-get-name obj))
       (IPV4-valid1-p obj)))

(defun IPV4-io-read (stream-in)
  (IPV4-make (io-read-uNbe u32 stream-in)))

(defun IPV4-io-write (obj stream-out)
  (io-write-uNbe u32 (IPV4-get-value obj) stream-out))

(defun IPV4->vector-u8 (obj)
  (coerce (integer->list-un u8 4 (IPV4-get-value obj)) 'vector))

(defun vector-u8->IPV4 (vector-u8)
  (IPV4-make (list-uN->integer u8 (coerce vector-u8 'list))))


;;; IPV6. IPV6 address type

;; format: (IPV6 u32 u32 u32 u32)

(defun IPV6-get-value (obj) "-> list-u32" (cdr obj))

(defun IPV6-zhash (octet-offset obj)
  (zhash-list u32 octet-offset (IPV6-get-value obj)))

(defun IPV6-make (list-u32)
  "Returns tagged list ('IPV6 . [list-u32])"
  (cons 'IPV6 list-u32))

(defun IPV6-make-new (list-u32)
  (IPV6-make list-u32))

(defun IPV6-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (= (length obj) 5)
       (every #'u32-p (IPV6-get-value obj))))

(deftype IPV6 () '(cons (member IPV6)))
(defun IPV6-typep (obj) (typep obj 'IPV6))

(defun IPV6-valid2-p (obj)
  "Test list values are within allowed range"
  (eq 'IPV6 (TL-get-name obj)))

(defun IPV6-io-read (stream-in)
  (IPV6-make (io-read-uNbe-octets u32 16 stream-in)))

(defun IPV6-io-write (obj stream-out)
  (io-write-uNbe-octets u32 16 (IPV6-get-value obj) stream-out))

(defun IPV6->vector-u8 (obj)
  (coerce (list-uN->list-uM u32 u8 (IPV6-get-value obj))
	  'vector))

(defun vector-u8->IPV6 (vector-u8)
  (IPV6-make (list-uN->list-uM u8 u32 (coerce vector-u8 'list))))



(defun IPV4-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~A #x~8,'0X)" (TL-get-name obj) (IPV4-get-value obj)))

(set-pprint-dispatch 'IPV4 #'IPV4-pprint-1  0 *sbgp-pprint-dispatch-table-1*)

(defun IPV4-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~A ~/sbgp:u32-pprint-ipv4/]~)" (TL-get-name obj) (IPV4-get-value obj)))

(set-pprint-dispatch 'IPV4 #'IPV4-pprint-2  0 *sbgp-pprint-dispatch-table-2*)

(defun IPV6-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~A ~{#x~8,'0X~^ ~})" (TL-get-name obj) (IPV6-get-value obj)))

(set-pprint-dispatch 'IPV6 #'IPV6-pprint-1  0 *sbgp-pprint-dispatch-table-1*)

(defun IPV6-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  "Output modified by *sbgp-pprint-ipv6-display-format* = :fixed-width | :compressed (default)"
  (format port "~([~A ~/sbgp:list-u32-pprint-ipv6/]~)"
	  (TL-get-name obj)
	  (IPV6-get-value obj)))

(set-pprint-dispatch 'IPV6 #'IPV6-pprint-2 0 *sbgp-pprint-dispatch-table-2*)



(setf (get 'IPV4 'pprint-1) #'IPV4-pprint-1)
(setf (get 'IPV4 'pprint-2) #'IPV4-pprint-2)
(setf (get 'IPV4 'zhash) #'IPV4-zhash)

(setf (get 'IPV6 'pprint-1) #'IPV6-pprint-1)
(setf (get 'IPV6 'pprint-2) #'IPV6-pprint-2)
(setf (get 'IPV6 'zhash) #'IPV6-zhash)
