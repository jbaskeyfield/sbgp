(in-package :sbgp)

(defparameter *sbgp-pprint-dispatch-table-1* (copy-pprint-dispatch nil))
(defparameter *sbgp-pprint-dispatch-table-2* (copy-pprint-dispatch nil))

(defun set-pprint-default ()
  (setf *print-pprint-dispatch* (copy-pprint-dispatch nil)))
(defun set-pprint-1 ()
  (setf *print-pprint-dispatch* *sbgp-pprint-dispatch-table-1*))
(defun set-pprint-2 ()
  (setf *print-pprint-dispatch* *sbgp-pprint-dispatch-table-2*))

(defun pprint-1 (obj &optional (stream-out t))
  (let ((pprint-fn (cond ((TL-typep obj)
			  (get (TL-get-name obj) 'pprint-1))
			 ((TV-typep obj)
			  (get (TV-get-name obj) 'pprint-1))
			 (t
			  nil))))
    (if pprint-fn
	(funcall pprint-fn stream-out obj)
	(format stream-out "~S" obj))))

(defun pprint-2 (obj &optional (stream-out t))
  (let ((pprint-fn (cond ((TL-typep obj)
			  (get (TL-get-name obj) 'pprint-2))
			 ((TV-typep obj)
			  (get (TV-get-name obj) 'pprint-2))
			 (t
			  nil))))
    (if pprint-fn
	(funcall pprint-fn stream-out obj)
	(format stream-out "~S" obj))))


(defparameter *sbgp-pprint-ipv6-display-format* :compressed
  ":fixed-width | :compressed (default)")

(defparameter *sbgp-pprint-asn-display-format* :asdot+
  ":asplain | :asdot | :asdot+ (default)")

(defparameter *sbgp-pprint-community-display-format* :new-format
  ":hex | :dec | :new-format (default)")


;;; PRETTY PRINTER HELPER FUNCTIONS

;;; IPV4 

(defun u32-pprint-ipv4 (port x &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  "Passed integer X (u32). Prints IPV4 address in dotted decimal format to output stream PORT."
  (format port "~D.~D.~D.~D"
	  (logand #xff (ash x -24))
	  (logand #xff (ash x -16))
	  (logand #xff (ash x -8))
	  (logand #xff x)))


;;; BGP COMMUNITY FORMATS: new-format 30:20 ; hexadecimal 0x1E0014 ; decimal     1966100

(defun u32-pprint-community (port x &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  "Prints integer X as BGP community. Output modified by *sbgp-pprint-community-display-format* = :hex | :dec | :new-format (default)"
  (case  *sbgp-pprint-community-display-format* 
    (:hex  (format port "#x~8,'0X" x))
    (:dec  (format port "~D" x))
    (t     (format port "~D:~D" (logand #xffff (ash x -16)) (logand #xffff x)))))


;;; ASN FORMATS (as per RFC5396): asplain ; asdot+ ; asdot

(defun u32-pprint-asn (port x &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  "Prints integer X as RFC5396 ASN. Output modified by *sbgp-pprint-asn-display-format*  = :asplain | :asdot | :asdot+ (default)"
  (case *sbgp-pprint-asn-display-format* 
    (:asplain (format port "~D" x))
    (:asdot   (if (< x 65536)
		  (format port "~D" x)
		  (format port "~D.~D" (logand #xffff (ash x -16)) (logand #xffff x))))
    (t  (format port "~D.~D" (logand #xffff (ash x -16)) (logand #xffff x)))))


;;; IPV6

(defun list-u16-pprint-ipv6-compressed (port eight-tuple &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  "Passed list EIGHT-TUPLE (list of u16, length 8). Prints IPV6 address in compressed (RFC5952) format to output stream PORT."

  (let ((zero-sequences
	  
	  ;; returns list of 3-tuples ((<seq-length> <start-index> <end-index>)... or 'nil', if no zeros found in list. 
	  (labels ((rec-zero-seq (index                           ; current position in list
				  previous-element-zero           ; [ t | nil ] was there a zero in the previous element
				  zero-start-index                ; [ nil | integer ] index set on start of zero sequence
				  seq-length                      ; length of current sequence of zeros
				  lst)
		     (if (null lst)
			 (if previous-element-zero
			     (cons (list seq-length zero-start-index index) nil)                           ; (0->nil) final element is zero
			     nil)                                                                          ; (0->nil) final element is non-zero
			 (let ((elem (car lst)))
			   (if previous-element-zero
			       (if (= elem 0)
				   (rec-zero-seq (1+ index) t zero-start-index (1+ seq-length) (cdr lst))  ; (0->0) continuing zero seq.
				   (cons (list seq-length zero-start-index index)                          ; (0->1) end of zero seq.
					 (rec-zero-seq (1+ index) nil nil 0 (cdr lst))))
			       (if (= elem 0)
				   (rec-zero-seq (1+ index) t index 1 (cdr lst))                           ; (1->0) start of new seq.
				   (rec-zero-seq (1+ index) nil nil 0 (cdr lst))))))))                     ; (1->1) continue non-zero 
	    (rec-zero-seq 0 nil nil 0 eight-tuple))))
    
    (let ((long-sequences
	    ;; remove sequences < 2 elements (16-bits) long
	    (remove-if #'(lambda (x) (< (car x) 2)) zero-sequences)))

      (let ((sorted-sequences
	      ;; sorted by first element (sequence length) then by second element (position in list)
	      (sort long-sequences
		    #'(lambda (x y) (if (> (first x) (first y))
					t
					(if (= (first x) (first y))
					    (< (second x) (second y))
					    nil))))))
	
	(let ((longest-sequence (car sorted-sequences)))
	  (cond (longest-sequence
		 (let ((start-skip (cadr longest-sequence))
		       (end-skip (caddr longest-sequence)))
		   (format port "~(~{~X~^:~}::~{~X~^:~}~)"
			   (subseq eight-tuple 0 start-skip)
			   (subseq eight-tuple end-skip))))
		(t
		 (format port "~(~{~X~^:~}~)" eight-tuple))))))))

(defun list-u32-pprint-ipv6 (port list-u32 &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  "Prints list of u32 as IPV6 format (RFC5952). Output modified by *sbgp-pprint-ipv6-display-format*  = :fixed-width | :compressed (default)"
  (case *sbgp-pprint-ipv6-display-format*
    (:fixed-width
     (format port "~(~{~4,'0X~^:~}~)" (list-uN->list-uM u32 u16 list-u32)))
    (t
     (list-u16-pprint-ipv6-compressed port (list-uN->list-uM u32 u16 list-u32)))))






			    
