(in-package :sbgp)

(defconstant +FIFOQ-length+ (expt 2 8))                   ;; Length of queues must be power of 2 and match +FIFOQ-modulus-mask+
(defconstant +FIFOQ-modulus-mask+ (1- +FIFOQ-length+))    ;; as modular arithmetic mapping index to array position is performed by bitwise-AND. Mask is length of queue minus 1

(defun FIFOQ-make ()
  (list (gensym "FIFOQ")
	(cons 0         ; queue head/input/write index position  (index position of next push)
	      0)        ; queue tail/output/read index position  (index position of next pop)
	(make-array +FIFOQ-length+ :initial-element nil)))

(defmacro FIFOQ-get-name          (obj) "-> symbol"   `(car ,obj))
(defmacro FIFOQ-get-indexes       (obj) "-> cons"     `(cadr ,obj))
(defmacro FIFOQ-get-queue-vector  (obj) "-> vector"   `(cadddr ,obj))

(defun FIFOQ-get-queue-count (obj) (- (car (FIFOQ-get-indexes obj))   ; head pos
				      (cdr (FIFOQ-get-indexes obj)))) ; tail pos
(defun FIFOQ-empty-p (obj) (= (car (FIFOQ-get-indexes obj))   ; head pos
			      (cdr (FIFOQ-get-indexes obj)))) ; tail pos
(defun FIFOQ-full-p (obj) (>= (- (car (FIFOQ-get-indexes obj))   ; head pos
				 (cdr (FIFOQ-get-indexes obj)))  ; tail pos
			      +FIFOQ-length+))

(defun FIFOQ-push (obj filoq)
  (destructuring-bind (indexes queue)
      (cdr filoq)
    (if (>= (- (car indexes) (cdr indexes)) +FIFOQ-length+)
	nil
	(let ((head-index (logand (car indexes) +FIFOQ-modulus-mask+)))
	  (sb-ext:compare-and-swap (svref queue head-index) nil obj)            ; atomic / ordered
	  (sb-ext:atomic-incf (car indexes))                                    ; atomic / ordered
	  t))))

(defun FIFOQ-pop (filoq)
  (destructuring-bind (indexes queue)
      (cdr filoq)
    (if (<= (- (car indexes) (cdr indexes)) 0)
	(values nil nil)
	(let* ((tail-index (logand (cdr indexes) +FIFOQ-modulus-mask+))
	       (return-value (svref queue tail-index)))
          (sb-ext:compare-and-swap (svref queue tail-index) return-value nil)   ; atomic / ordered
	  (sb-ext:atomic-incf (cdr indexes))                                    ; atomic / ordered
	  (values return-value t)))))

;;; QUEUE-PAIR-make, QUEUE-PAIR-get-A-end, QUEUE-PAIR-get-B-end are called by T-THREAD-make only (to created connection between parent and newly spawned child thread
(defun QUEUE-PAIR-make ()
  (let* ((a-b (FIFOQ-make))
	 (b-a (FIFOQ-make))
	 (name (gensym "DUPLEXQ"))
	 (name-a-end (intern (concatenate 'string (string name) "-A")))
	 (name-b-end (intern (concatenate 'string (string name) "-B"))))
  (list name
	(list name-a-end
	      b-a           ; Rx at A-end
	      a-b)          ; Tx at A-end
	(list name-b-end
	      a-b           ; Rx at B-end
	      b-a))))       ; Tx at B-end

(defmacro QUEUE-PAIR-get-A-end (duplexq-pair) `(cadr ,duplexq-pair))
(defmacro QUEUE-PAIR-get-B-end (duplexq-pair) `(caddr ,duplexq-pair))

;;; QUEUE-LOOPBACK-make is called by define-T-THREAD-LOOP-FUNCTION only (to create %timers-queue)
(defun LOOPBACK-QUEUE-make ()
  (let* ((a-b (FIFOQ-make))
	 (name (gensym "LOOPBACKQ"))
	 (name-a-end (intern (concatenate 'string (string name) "-RXTX"))))
     (list name-a-end
	      a-b           ; Rx at A-end
	      a-b)))        ; Tx at A-end


(defmacro QUEUE-get-name (duplexq) `(car ,duplexq))
(defmacro QUEUE-get-Rx (duplexq) `(cadr ,duplexq))
(defmacro QUEUE-get-Tx (duplexq) `(caddr ,duplexq))

;;; NOTE: QUEUE-send/receive should only be called in threads via the macros QUEUE-send/receive-message 
(defmacro QUEUE-send (duplexq obj) `(FIFOQ-push ,obj (QUEUE-get-Tx ,duplexq)))
(defmacro QUEUE-receive (duplexq) `(FIFOQ-pop (QUEUE-get-Rx ,duplexq)))

(defun QUEUE-get-rx-queue-count (duplexq) (FIFOQ-get-queue-count (QUEUE-get-rx duplexq)))
(defun QUEUE-get-tx-queue-count (duplexq) (FIFOQ-get-queue-count (QUEUE-get-tx duplexq)))				  
(defun QUEUE-rx-empty-p (duplexq) (FIFOQ-empty-p (QUEUE-get-rx duplexq)))  ; check if we can send
(defun QUEUE-tx-full-p (duplexq) (FIFOQ-full-p (QUEUE-get-tx duplexq)))    ; check if we can receive


;;; Messages on queues are lists of following format
;;; (MSG COMMAND ARG1 ARG2 ARG3 ...
;;; To prevent modification to messages (and hopefully make parsing in loop case functions) following wrapper functions used for 'ca*r'

(defun MSG-get-command (message) "-> symbol (event or command)"  (cadr message))
(defun MSG-get-arg1 (message)    "-> second element of message"  (caddr message))
(defun MSG-get-arg2 (message)    "-> third element of message"   (cadddr message))
(defun MSG-get-arg3 (message)    "-> fourth element of message"  (car (cddddr message)))
(defun MSG-get-arg4 (message)    "-> fifth element of message"   (cadr (cddddr message)))
(defun MSG-get-arg5 (message)    "-> sixth element of message"   (caddr (cddddr message)))
(defun MSG-get-arg6 (message)    "-> second element of message"  (cadddr (cddddr message)))

(defun MSG-make (command &rest arguments)
  (cons 'MSG
	(cons command
	      arguments)))

(defun MSG-remove-arg1-arg2 (obj)
  "Removes first and second arguments. Passed message of format ('MSG 'TO 'THREAD-ID <message> ...), returns ('MSG <message> ...)"
  (cons 'MSG
	(cdddr obj)))

(defun MSG-valid1-p (obj)
  "Test list elements are of correct type"
  (and (consp obj)
       (>= (length obj) 2)))

(deftype MSG () '(and (cons (member MSG)) (satisfies MSG-valid1-p)))
(defun MSG-typep (obj) (typep obj 'MSG))

(defun MSG-valid2-p (obj)
  "Test list values are within allowed range"
  (eq 'MSG (TL-get-name obj))
  (symbolp (cadr obj)))

