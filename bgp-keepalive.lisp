(in-package :sbgp)

(defun BGP-KEEPALIVE-make ()
  (list 'BGP-KEEPALIVE))

(defun BGP-KEEPALIVE-get-name (obj)   "-> symbol"          (car obj))

(deftype BGP-KEEPALIVE () '(cons (member BGP-KEEPALIVE)))

(defun BGP-KEEPALIVE-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W)" (TL-get-name obj)))

(set-pprint-dispatch '(cons (member BGP-KEEPALIVE)) #'BGP-KEEPALIVE-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun BGP-KEEPALIVE-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W]~)" (TL-get-name obj)))

(set-pprint-dispatch '(cons (member BGP-KEEPALIVE)) #'BGP-KEEPALIVE-pprint-2 0 *sbgp-pprint-dispatch-table-2*)
