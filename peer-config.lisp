(in-package :sbgp)

;; static config
(defun PEER-CONFIG-get-thread-name (peer-config)         "-> symbol"            (cadr peer-config))
(defun PEER-CONFIG-get-flags (peer-config)               "-> u56"               (caddr peer-config))
(defun PEER-CONFIG-get-peer-ip-address (peer-config)     "-> IPV4 | IPV6"       (cadddr peer-config))
(defun PEER-CONFIG-get-peer-router-id (peer-config)      "-> IPV4"              (car (cddddr peer-config)))
(defmacro PEER-CONFIG-get-peer-router-id! (peer-config)  "-> IPV4"              `(car (cddddr ,peer-config)))
(defun PEER-CONFIG-get-peer-asn (peer-config)            "-> u32"               (cadr (cddddr peer-config)))
(defun PEER-CONFIG-get-hold-time (peer-config)           "-> integer (seconds)" (caddr (cddddr peer-config)))
(defun PEER-CONFIG-get-address-family-list (peer-config) "-> list of AFISAFI"   (cadddr (cddddr peer-config)))

(defconstant +PEER-CONFIG-flag-ebgp+      #x01)
(defconstant +PEER-CONFIG-flag-rr-client+ #x02)

(defun PEER-CONFIG-ebgp-flag-setp (peer-config)
  (= +PEER-CONFIG-flag-ebgp+ (logand +PEER-CONFIG-flag-ebgp+
				     (PEER-CONFIG-get-flags peer-config))))
(defun PEER-CONFIG-rr-client-flag-setp (peer-config)
  (= +PEER-CONFIG-flag-rr-client+ (logand +PEER-CONFIG-flag-rr-client+
				     (PEER-CONFIG-get-flags peer-config))))

(defun PEER-CONFIG-make (thread-name
			 &key (flags (+ +PEER-CONFIG-flag-ebgp+ +PEER-CONFIG-flag-rr-client+))
			   peer-ip-address
			   peer-router-id
			   peer-asn
			   (hold-time 90)
			   (address-family-list (list +AFISAFI-ipv4-unicast+
						      +AFISAFI-ipv6-unicast+)))
  (list 'PEER-CONFIG
	thread-name
	flags
	peer-ip-address
	peer-router-id
	peer-asn
	hold-time
	address-family-list))
  
;; negotiated session state config
(defmacro PEER-SESSION-STATE-get-4-octet-asn-flag (session-state)          "-> [t|nil]"           `(svref ,session-state 1))
(defmacro PEER-SESSION-STATE-get-peer-advertised-hold-time (session-state) "-> integer"           `(svref ,session-state 2))
(defmacro PEER-SESSION-STATE-get-peer-router-id (session-state)            "-> IPV4"              `(svref ,session-state 3))
(defmacro PEER-SESSION-STATE-get-peer-asn (session-state)                  "-> u32"               `(svref ,session-state 4))
(defmacro PEER-SESSION-STATE-get-peer-advertised-caps (session-state)      "-> list of BGP-CAP-"  `(svref ,session-state 5))
(defmacro PEER-SESSION-STATE-get-common-capability-set (session-state)     "-> list of BGP-CAP-"  `(svref ,session-state 6))

(defun PEER-SESSION-STATE-make ()
  (let ((session-state (make-array 7 :initial-element nil)))
    (setf (svref session-state 0) 'PEER-SESSION-STATE)
    session-state))
    
