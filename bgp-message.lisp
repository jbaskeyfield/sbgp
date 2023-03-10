(in-package :sbgp)

(defun MSG-HEADER-get-marker (obj) "-> list u32" (cadr obj))
(defun MSG-HEADER-get-length (obj) "-> u16"      (caddr obj))
(defun MSG-HEADER-get-type (obj)   "-> u8"       (cadddr obj))

(defun MSG-HEADER-make (length type)
  (list 'MSG-HEADER
	(list #xffffffff #xffffffff #xffffffff #xffffffff)
	(+ length 19)
	type))
     
(defun MSG-HEADER-io-read (stream-in)
  (list 'MSG-HEADER
	(io-read-uNbe-octets u32 16 stream-in)  ; marker
	(io-read-uNbe u16 stream-in)            ; length
	(io-read-uNbe u8 stream-in)))           ; type

(defun MSG-HEADER-io-write (obj stream-out)
  (destructuring-bind (marker length type)
      (cdr obj)
    (io-write-uNbe-octets u32 16 marker stream-out)
    (io-write-uNbe u16 length stream-out)
    (io-write-uNbe u8 type stream-out)))

;; RFC4271 p12 Section 4.1. Message Header Format
(defun MSG-HEADER-valid2-p (obj)
  (and (consp obj)
       (= (length obj) 4)
       (eq 'MSG-HEADER (TL-get-name obj))
       (equal (MSG-HEADER-get-marker obj)'(#xffffffff #xffffffff #xffffffff #xffffffff))
       (and (>= (MSG-HEADER-get-length obj) 19)
	    (<= (MSG-HEADER-get-length obj) 4096))
       (and (>= (MSG-HEADER-get-type obj) 1)
	    (<= (MSG-HEADER-get-type obj) 5))))

(deftype MSG-HEADER () '(cons (member MSG-HEADER)))


(defun MSG-HEADER-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W (~{#x~8,'0X~^ ~}) #x~4,'0X #x~2,'0X)" 
	  (TL-get-name obj)   ; symbol
	  (MSG-HEADER-get-marker obj)  ; list u32
	  (MSG-HEADER-get-length obj)  ; u16
	  (MSG-HEADER-get-type obj)))    ; u8

(set-pprint-dispatch '(cons (member MSG-HEADER)) #'MSG-HEADER-pprint-1 0 *sbgp-pprint-dispatch-table-1*)
		     
(defun MSG-HEADER-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W [~{#x~8,'0X~^ ~}] ~D ~D]~)" 
	  (TL-get-name obj)   ; symbol
	  (MSG-HEADER-get-marker obj)  ; list u32
	  (MSG-HEADER-get-length obj)  ; u16
	  (MSG-HEADER-get-type obj)))    ; u8

(set-pprint-dispatch '(cons (member MSG-HEADER)) #'MSG-HEADER-pprint-2 0 *sbgp-pprint-dispatch-table-2*)


(defun BGP-MESSAGE-get-name (obj)     "-> symbol"     (car obj))
(defun BGP-MESSAGE-get-header (obj)   "-> MSG-HEADER" (cadr obj))
(defun BGP-MESSAGE-get-message-body (obj)  "-> [ ERROR-Reserved | BGP-OPEN | BGP-UPDATE | BGP-NOTIFICATION | BGP-KEEPALIVE | BGP-ROUTE-REFRESH | ERROR-Unassigned ]" (caddr obj))

(defun BGP-MESSAGE-make-new (message-body)
  "Makes new BGP-MESSAGE. Calculates size of passed MESSAGE-BODY [BGP-OPEN|BGP-UPDATE|...] and adds MSG-HEADER"
  
  (let ((message-type 
	  (case (BGP-MESSAGE-get-name message-body)
	    (BGP-OPEN          1)
	    (BGP-UPDATE        2)
	    (BGP-NOTIFICATION  3)
	    (BGP-KEEPALIVE     4)
	    (BGP-ROUTE-REFRESH 5)
	    (t (error "BGP-MESSAGE-make-new message-body = ~S, expected list (BGP-OPEN|BGP-UPDATE|BGP-NOTIFICATION|BGP-KEEPALIVE|BGP-ROUTE-REFRESH..." message-body))))
	(message-length
	  (case (BGP-MESSAGE-get-name message-body)
	    (BGP-OPEN     (BGP-OPEN-get-io-rw-octets message-body))
	    
	    (BGP-UPDATE   (BGP-UPDATE-get-io-rw-octets message-body))
	    (BGP-KEEPALIVE 0)
	    (t  (error "BGP-MESSAGE-make-new not implemented type ~S" (BGP-MESSAGE-get-name message-body))))))
    
    (list 'BGP-MESSAGE
	  (MSG-HEADER-make message-length message-type)
	  message-body)))

(defun BGP-MESSAGE-get-io-rw-octets (obj)
  (MSG-HEADER-get-length (BGP-MESSAGE-get-header obj)))

(defun BGP-MESSAGE-io-read (4-octet-asn-flag stream-in)
  (let* ((header (MSG-HEADER-io-read stream-in))
	 (message-length (- (MSG-HEADER-get-length header)
			    19)))
	(let ((message (case (MSG-HEADER-get-type header)
		    (0   (BYTES-io-read message-length stream-in))
		    (1   (BGP-OPEN-io-read message-length stream-in))
		    (2   (BGP-UPDATE-io-read 4-octet-asn-flag message-length stream-in))
		    (3   (BGP-NOTIFICATION-io-read message-length stream-in))
		    (4   (BGP-KEEPALIVE-make))
		    (5   (BGP-ROUTE-REFRESH-io-read message-length stream-in))
		    (t   (BYTES-io-read message-length stream-in)))))
	  (list 'BGP-MESSAGE header message))))

(defun BGP-MESSAGE-io-write (4-octet-asn-flag obj stream-out)
  (destructuring-bind (header message)
      (cdr obj)
    (MSG-HEADER-io-write header stream-out)
    (case (MSG-HEADER-get-type header)
      (0   (BYTES-io-write message stream-out))                       ; ERROR-Reserved
      (1   (BGP-OPEN-io-write message stream-out))
      (2   (BGP-UPDATE-io-write 4-octet-asn-flag message stream-out))
      (3   (BGP-NOTIFICATION-io-write message stream-out))
      (4   nil)                                                       ; BGP-KEEPALIVE
      (5   (BGP-ROUTE-REFRESH-io-write message stream-out))
      (t   (BYTES-io-write message stream-out)))))                    ; ERROR-Unassigned

(deftype BGP-MESSAGE () '(cons (member BGP-MESSAGE)))

(defun BGP-MESSAGE-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~_(~W~_~W~_~W)" 
	  (TL-get-name obj)       ; symbol
	  (BGP-MESSAGE-get-header obj)     ; MSG-HEADER
	  (BGP-MESSAGE-get-message-body obj)))  ; -> [ ERROR-Reserved | BGP-OPEN | BGP-UPDATE | BGP-NOTIFICATION | BGP-KEEPALIVE | BGP-ROUTE-REFRESH | ERROR-Unassigned ]
(set-pprint-dispatch '(cons (member BGP-MESSAGE)) #'BGP-MESSAGE-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun BGP-MESSAGE-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~_[~W~_~W~_~W]" 
	  (TL-get-name obj)       ; symbol
	  (BGP-MESSAGE-get-header obj)     ; MSG-HEADER
	  (BGP-MESSAGE-get-message-body obj)))  ; -> [ ERROR-Reserved | BGP-OPEN | BGP-UPDATE | BGP-NOTIFICATION | BGP-KEEPALIVE | BGP-ROUTE-REFRESH | ERROR-Unassigned ]
(set-pprint-dispatch '(cons (member BGP-MESSAGE)) #'BGP-MESSAGE-pprint-2 0 *sbgp-pprint-dispatch-table-2*)



