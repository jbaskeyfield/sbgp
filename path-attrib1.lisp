(in-package :sbgp)

(defun PATH-ATTRIB-get-attribute-type-field (obj)   "-> u16"       (cadr obj))
(defun PATH-ATTRIB-get-attribute-length (obj)
  "-> [ u8 (default) | u16 (if 'flag-extended-len-bit' is set)"    (caddr obj))

(defun PATH-ATTRIB-get-type-subfield (obj)
  (logand #xff (PATH-ATTRIB-get-attribute-type-field obj)))

(defun PATH-ATTRIB-get-flag-optional-bit (obj)
  (uN-bit-set-p u16 (PATH-ATTRIB-get-attribute-type-field obj) 0))

(defun PATH-ATTRIB-get-flag-transitive-bit (obj)
  (uN-bit-set-p u16 (PATH-ATTRIB-get-attribute-type-field obj) 1))

(defun PATH-ATTRIB-get-flag-partial-bit (obj)
  (uN-bit-set-p u16 (PATH-ATTRIB-get-attribute-type-field obj) 2))

(defun PATH-ATTRIB-get-flag-extended-len-bit (obj)
  (uN-bit-set-p u16 (PATH-ATTRIB-get-attribute-type-field obj) 3))

(define-symbol-macro +PATH-ATTRIB-optional-bit+        #x8000)
(define-symbol-macro +PATH-ATTRIB-transitive-bit+      #x4000)
(define-symbol-macro +PATH-ATTRIB-partial-bit+         #x2000)
(define-symbol-macro +PATH-ATTRIB-extended-length-bit+ #x1000)

#|
See path-attrib2.lisp for other PATH-ATTRIB funcions

PATH-ATTRIB-io-read
PATH-ATTRIB-io-write

|#
  
