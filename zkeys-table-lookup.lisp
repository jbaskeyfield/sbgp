#|
zobrist/tabular hashing algorithm, using array of 256 x 256 x 56 bit random integers
hashing performed per byte => column size is 256
hash function steps to next row as reads each byte => wraps on mod 256 bytes

hash function steps to next 'row' when moving to next byte being hashed
'column' is indexed on byte being hashed to get xor mask.

key/mask size = 56 bits, to keep each xor mask within single fixnum word on 64 bit machine.
key size needs to be >= n, where 2^n is size of the cache table. 

lowest n bits of resulting hash is used as index to cache array.
|#
(in-package :sbgp)

(defconstant +zkeys-num-rows+ 256)
(defconstant +zkeys-num-columns+ 256)
(defconstant +zkeys-num-bits+ 56)
(defconstant +zkeys-value-limit+ (ash 1 +zkeys-num-bits+))

(defparameter *zkeys-filename* "zobrist-keys.txt")

(defun make-zkeys-table ()
  "make simple vector of length '(* +zkeys-num-columns+ +zkeys-num-rows+)'"
  (make-array (* +zkeys-num-columns+ +zkeys-num-rows+)
	      :initial-element 0))

(defparameter *zkeys-table* (make-zkeys-table))

(defun zkeys-table-fill-random ()
  "populate *zkeys-table* with random numbers"
  (do ((i 0 (1+ i)))
      ((>= i (* +zkeys-num-columns+ +zkeys-num-rows+)) *zkeys-table*)  
    (setf (aref *zkeys-table* i)
	  (random +zkeys-value-limit+))))

(defun zkeys-table-fill-test ()
  "populate *zkeys-table* with sequential integers (for debugging)"
  (do ((i 0 (1+ i)))
      ((>= i (* +zkeys-num-columns+ +zkeys-num-rows+)) *zkeys-table*)  
    (setf (aref *zkeys-table* i)
	  i)))

(defun zkeys-table-read-from-file (&optional (filespec *zkeys-filename*))
  "read *zkeys-table* values from text file"
  (let ((str-in (open filespec
		      :direction :input
		      :if-does-not-exist nil)))

    (cond (str-in (format t "reading keys from file ~S ... " filespec)
		  (setf *zkeys-table* (read str-in))
		  (close str-in)
		  (format t "OK~%")
		  (cond ((typep *zkeys-table* `(simple-vector ,(* +zkeys-num-columns+ +zkeys-num-rows+)))
			 t)
			(t
			 (format t "ERROR: read form is not of type ~S~%"
				 `(simple-vector ,(* +zkeys-num-columns+ +zkeys-num-rows+)))
			 nil)))
	  (t (format t "ERROR: unable to open file ~S for reading~%" filespec)
	     nil))))

(defun zkeys-table-write-to-file (&optional (filespec *zkeys-filename*))
  "write *zkeys-table* to text file"
  
  (cond ((typep *zkeys-table* `(simple-vector ,(* +zkeys-num-columns+ +zkeys-num-rows+)))
	 (let ((str-out (open filespec
			      :direction :output
			      :if-exists :supersede)))
	   (cond (str-out (format t "writing keys to file ~S ... " filespec)
			  (prin1 *zkeys-table* str-out)
			  (close str-out)
			  (format t "OK~%")
			  t)
		 (t (format t "ERROR: unable to open file ~S for writing~%" filespec)
		    nil))))
	(t
	 (format t "ERROR: *zkeys-table* is not of type ~S~%"
		 `(simple-vector ,(* +zkeys-num-columns+ +zkeys-num-rows+)))
	 nil)))

;; TODO: this is only called from zhash-integer. move inline or compiler directive?
(defun zhash-lookup (octet-offset byte-value)
  #| DEBUG_START
  (format t "(zhash-lookup octet-offset:~A byte-value:~X)" octet-offset byte-value)
  (format t " key index: ~A value: ~A~%"
	  (+ byte-value (* +zkeys-num-columns+ (mod octet-offset +zkeys-num-rows+)))
	  (aref *zkeys-table* (+ byte-value
		      (* +zkeys-num-columns+
			 (mod octet-offset +zkeys-num-rows+)))))
  DEBUG_END |#
	  
  (aref *zkeys-table* (+ byte-value
		      (* +zkeys-num-columns+
			 (mod octet-offset +zkeys-num-rows+)))))

(defun zhash-integer (num-bytes octet-offset x)
  "hashing performed from LSB to MSB across passed integer size 'num-bytes'"
  (labels ((rec (hash byte-count octet-pos input-val)
	     (if (<= byte-count 0)
		 hash
		 (rec (logxor hash
			      (zhash-lookup octet-pos
					    (logand #xff input-val)))
		      (1- byte-count)
		      (1+ octet-pos)
		      (ash input-val -8)))))
    
    (rec 0 num-bytes octet-offset x)))

(defun zhash-list (bytes-per-integer octet-offset lst)
  "hash flat list of integers, skipping over elements not integerp. => values hash (cons hash lst)"
  (labels ((rec (hash octet-pos l)
	     (if (null l)
		 hash
		 (rec (logxor hash (zhash-integer bytes-per-integer
						  octet-pos
						  (car l)))
		      (+ octet-pos bytes-per-integer)
		      (cdr l)))))
    (rec 0 octet-offset lst)))

(defun zhash-tagged-list (specification start-offset obj)
  (do ((spec specification (if (cdr spec)
			       (cdr spec)
			       spec))
       (lst (cdr obj) (cdr lst))
       (offset start-offset (+ offset (car spec)))
       (hash 0 (logxor hash
		       (zhash-integer (car spec)
				      offset
				      (car lst)))))
      ((null lst) (values hash offset))
    ;;(format t "~&spec: ~S lst: ~S offset: ~S hash: ~S~%" spec lst offset hash)
    ;;(sleep 1)
    ))

(defun zhash (octet-offset obj)
  (let ((zhash-fn (if (TL-typep obj)
		      (get (TL-get-name obj) 'zhash)
		      nil)))
    (if zhash-fn
	(funcall zhash-fn octet-offset obj)
	0)))

;;; TODO. logic to read from file if it exists and is valid. otherwise, generate new keys and try to save.

;; initialise the global hash keys table *zkeys-table*
;;(zkeys-table-fill-random)
;;(zkeys-table-write-to-file)
(zkeys-table-read-from-file)
