#|
Unsigned integers [u8,u16,u32,u56] and untagged lists of integers
|#
(in-package :uint)

(defconstant u8  1)
(defconstant u16 2)
(defconstant u24 3)
(defconstant u32 4)
(defconstant u48 6)
(defconstant u56 7)

(defun u8-p (x)
  "Test that X is a FIXNUM and in range #x0 to #xFF"
  (and (typep x 'fixnum)
       (= 0 (logandc1 #xff x))))

(defun u16-p (x)
  "Test that X is a FIXNUM and in range #x0 to #xFFFF"
  (and (typep x 'fixnum)
       (= 0 (logandc1 #xffff x))))

(defun u24-p (x)
  "Test that X is a FIXNUM and in range #x0 to #xFFFFFF"
  (and (typep x 'fixnum)
       (= 0 (logandc1 #xffffff x))))

(defun u32-p (x)
  "Test that X is a FIXNUM and in range #x0 to #xFFFFFFFF"
  (and (typep x 'fixnum)
       (= 0 (logandc1 #xffffffff x))))

(defun u48-p (x)
  "Test that X is a FIXNUM and in range #x0 to #xFFFFFFFFFFFF"
  (and (typep x 'fixnum)
       (= 0 (logandc1 #xffffffffffff x))))

(defun u56-p (x)
  "Test that X is a FIXNUM and in range #x0 to #xFFFFFFFFFFFFFF"
  (and (typep x 'fixnum)
       (= 0 (logandc1 #xffffffffffffff x))))

(defun io-read-uNbe (N-bytes stream-in)
  "Reads N-BYTES from binary input stream STREAM-IN (input stream assumed to be big-endian byte order). 
Returns -> integer"
  (labels ((iterate (accum index)
	     (if (<= index 0)
		 accum
		 (let ((x (read-byte stream-in)))
		   (if x
		       (iterate (+ x (ash accum 8))
				(1- index)))))))
    (iterate 0 N-bytes)))

(defun io-write-uNbe (N-bytes x stream-out)
  "Writes N-BYTES from integer X to binary output stream STREAM-OUT (written in big-endian byte order).
Returns -> passed integer X"
  (labels ((iterate (index)
	     (when (>= index 0)
	       (write-byte (logand #xff
				   (ash x (* -8 index)))
			   stream-out)
	       (iterate (1- index)))))
    (iterate (1- N-bytes))
    x))

(defun io-read-uNbe-octets (N-bytes octets stream-in)
  "Reads OCTETS bytes from binary input stream STREAM-IN (input stream assumed to be big-endian byte order).
Returns -> list of unsigned integers of element size N-BYTES.
Note: If OCTETS is not an exact multiple of element size N-BYTES, the final list element will be left shifted
so bytes read from stream occupy most significant bytes.
Example: (io-read-uNbe-octets 4 5 str) -> #x12345678 #x9a000000 (final list element padded with zeros)"
  (labels ((iterate (octet-counter)
	     (cond ((<= octet-counter 0) nil)
		   ((< octet-counter N-bytes) (cons (ash (io-read-uNbe octet-counter stream-in)
							 (* 8 (- N-bytes octet-counter))) 
						    nil))
		   (t (cons (io-read-uNbe N-bytes stream-in)
			    (iterate (- octet-counter N-bytes)))))))
    (iterate octets)))

(defun io-write-uNbe-octets (N-bytes octets l stream-out)
  "Writes OCTETS bytes from list of integers L to binary output stream STREAM-OUT (written in big-endian byte order).
Returns -> passed list L"
  (labels ((iterate (octet-counter l)
	     (if (and (not (null l)) (> octet-counter 0))
		 (cond ((< octet-counter N-bytes)
			(io-write-uNbe octet-counter
				       (ash (car l)
					    (* -8 (- N-bytes octet-counter)))
				       stream-out))
		       (t
			(io-write-uNbe N-bytes (car l) stream-out)
			(iterate (- octet-counter N-bytes) (cdr l)))))))
    (iterate octets l)
    l))



(defun uN-bit-set-p (N-bytes x bit-position-from-MSB)
  "Test bit position indexed from most significant bit in N byte integer [0 1 2 3 4 5 6 7 ...] => t | nil"
  ;; offset from LSB is (((8 * num-bytes) - 1) - bit-position-from-MSB)
  ;; vis. bit position 0 in 3 byte field => shift of ((8 * 3) - 1) - 0) = 23
  ;; bit position 23 in 3 byte field => shift of ((8 * 3) - 1) - 23) = 0"

  (let ((offset-from-LSB  (- (* 8 N-bytes)
			     1
			     bit-position-from-MSB)))
    (if (< offset-from-LSB 0)
	nil
	(logbitp offset-from-LSB x))))

(defun list-uN-bit-set-p (N-bytes l bit-position-from-MSB)
  (labels ((rec (l bit-offset)
	     (cond ((null l)
		    nil)
		   ((>= bit-offset (* 8 N-bytes))
		    (rec (cdr l) (- bit-offset (* N-bytes 8))))
		   (t (uN-bit-set-p N-bytes (car l) bit-offset)))))
    (rec l bit-position-from-MSB)))

(defun uN-make-bit-mask (N-bytes num-bits)
  (ash (- (ash 1 num-bits) 1)
       (- (* 8 N-bytes) num-bits)))

(defun list-uN-make-bit-mask (N-bytes len num-bits)
  (let* ((bits-per-cons (* 8 N-bytes))
	 (all-ones-element (1- (ash 1 bits-per-cons)))
	 (modulo-num-bits (mod num-bits bits-per-cons)))
    
    (labels ((iter (mask-bit-counter output-bit-counter)
	       (cond ((<= output-bit-counter 0)
		      nil)
		     ((>= mask-bit-counter bits-per-cons)
		      (cons all-ones-element
			    (iter (- mask-bit-counter bits-per-cons)
				  (- output-bit-counter bits-per-cons))))
		     ((> mask-bit-counter 0)
		      (cons (uN-make-bit-mask N-bytes modulo-num-bits)
			    (iter (- mask-bit-counter bits-per-cons)
				  (- output-bit-counter bits-per-cons))))
		     (t
		      (cons 0
			    (iter (- mask-bit-counter bits-per-cons)
				  (- output-bit-counter bits-per-cons)))))))
      (iter num-bits (* len bits-per-cons)))))

(defun integer->list-uN (N-bytes len x)
  "Expands integer X into list of N-byte integers, of list length LEN. Input is 'left-aligned' into output cells.
Least signigicant bytes of input integer always map to last element of output list with padding on the right."
  (let* ((bits-per-cons (* 8 N-bytes))
	 (input-mask (1- (ash 1 bits-per-cons))))

    (labels ((iter (input-integer shift-counter accum)
	       (cond ((<= shift-counter 0)  accum)                          ; list complete

		     ((<= input-integer 0)  (iter 0                          ; run out of bits in input integer
						  (1- shift-counter)         ; fill remaining list elements with zeros
						  (cons 0 accum)))
		     (t  (iter (ash input-integer (- bits-per-cons))
			       (1- shift-counter)
			       (cons (logand input-mask input-integer) accum))))))
      (iter x len nil))))

(defun list-uN->integer (N-bytes l)
  (labels ((iter (accum l)
	     (if (null l)
		 accum
		 (iter (+ (car l)
			  (ash accum
			       (* N-bytes 8)))
		       (cdr l)))))
    (iter 0 l)))

(defun list-uN-truncate-to-octets (N-bytes octets l)
  "Returns list of N-BYTE integers equivalent to truncating bytevector down to total OCTETS in length.
Used to truncate prefixes passed to make-NLRI etc"
  (labels ((rec (octet-counter l)
	     (cond ((or (null l)
			(<= octet-counter 0))
		    nil)
		   ((< octet-counter N-bytes)
		    (let* ((input-mask (1- (ash 1 (* 8 octet-counter))))
			   (output-bitshift (* 8 (- N-bytes octet-counter))))
		      
		      (cons (ash
			     (logand (ash (car l) (- output-bitshift))    ; shift input number right before masking
				     input-mask)
			     output-bitshift)                             ; shift back to left so most significant bits saved

			    nil)))
		   (t
		    (cons (car l) (rec (- octet-counter N-bytes) (cdr l)))))))
    (rec octets l)))

(defun list-uN->list-uM-subseq (N M start-octet len-octets l)
  "Maps list of N byte integers to list of M byte integers.
START-OCTET can be an arbitary position within the input list, or if negative will pad output
list with leading zeros. Trailing zeros will be added to output if input is exhausted.
N,M = [ u8 | u16 | u32 | u56 ]. Passed list L is a list of type N.
Returns -> list of M byte integers.
For less general mapping use (list-uN->list-uM N M l) that is indexed from start of input."
  (labels ((rec (l input-octet-position output-octet-count accum)
	     (let ((N-byte-offset (- N (mod input-octet-position N) 1))
		   (M-byte-offset (- M (mod output-octet-count M) 1)))
	       
	       (cond ((>= output-octet-count len-octets)                             ; reached end of output list
		      (if (= M-byte-offset (1- M))
			  nil
			  (cons (ash accum (* 8 (1+ M-byte-offset))) nil)))

		     ((< input-octet-position 0)                                     ; pad leading zeros
		      (if (= M-byte-offset 0)
			  (cons 0
				(rec l
				     (1+ input-octet-position)
				     (1+ output-octet-count)
				     0))
			  (rec l
			       (1+ input-octet-position)
			       (1+ output-octet-count)
			       0)))

		     ((null l)                                                       ; reached end of input list, pad trailing zeros
		      (if (= M-byte-offset 0) 
			  (cons (ash accum 8)
				(rec l
				     (1+ input-octet-position)
				     (1+ output-octet-count)
				     0))
			  (rec l
			       (1+ input-octet-position)
			       (1+ output-octet-count)
			       (ash accum 8))))
		     
		     ((<= start-octet input-octet-position)                          ; collect digits from input list
		      (if (= M-byte-offset 0)
			  (cons (+ (ash accum 8)
				   (logand #xff
					   (ash (car l) (* -8 N-byte-offset))))
				(rec (if (= N-byte-offset 0)
					 (cdr l)
					 l)
				     (1+ input-octet-position)
				     (1+ output-octet-count)
				     0))
			  (rec (if (= N-byte-offset 0)
				   (cdr l)
				   l)
			       (1+ input-octet-position)
			       (1+ output-octet-count)
			       (+ (ash accum 8)
				  (logand #xff
					  (ash (car l) (* -8 N-byte-offset)))))))

		     (t                                                              ; skip over input cells until start-octet
		      (rec (if (= N-byte-offset 0)
			       (cdr l)
			       l)
			   (1+ input-octet-position)
			   output-octet-count
			   0))))))
    (rec l
	 (if (< start-octet 0)
	     start-octet
	     0)
	 0
	 0)))

(defun list-uN->list-uM (N M l)
  "Converts list L of N byte integers to list of M bytes integers. 
N,M = [ u8 | u16 | u32 | u56 ]. Passed list L is a list of type N.
Returns -> list of element type M"
  (labels ((rec (l input-octet-position output-octet-count accum)
	     (let ((N-byte-offset (- N (mod input-octet-position N) 1))
		   (M-byte-offset (- M (mod output-octet-count M) 1)))
	       
	       (cond ((null l)                                                          ; reached end of input list, pad trailing zeros
		      (if (= M-byte-offset (1- M))
			  nil
			  (cons (ash accum (* 8 (+ M-byte-offset 1))) nil)))
		     
		     (t                                                                 ; collect digits from input list
		      (if (= M-byte-offset 0)
			  (cons (+ (ash accum 8)
				   (logand #xff
					   (ash (car l) (* -8 N-byte-offset))))
				(rec (if (= N-byte-offset 0)
					 (cdr l)
					 l)
				     (1+ input-octet-position)
				     (1+ output-octet-count)
				     0))
			  (rec (if (= N-byte-offset 0)
				   (cdr l)
				   l)
			       (1+ input-octet-position)
			       (1+ output-octet-count)
			       (+ (ash accum 8)
				  (logand #xff
					  (ash (car l) (* -8 N-byte-offset)))))))))))
    (rec l 0 0 0)))

