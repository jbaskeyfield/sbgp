(in-package :sbgp)

;; static config
(defun PEER-CONFIG-get-peer-name (peer-config)           "-> symbol"            (cadr peer-config))
(defun PEER-CONFIG-get-peer-ip-address (peer-config)     "-> IPV4 | IPV6"       (caddr peer-config))
(defun PEER-CONFIG-get-peer-asn (peer-config)            "-> u32"               (cadddr peer-config))
(defun PEER-CONFIG-get-hold-time (peer-config)           "-> integer (seconds)" (car (cddddr peer-config)))
(defun PEER-CONFIG-get-address-family-list (peer-config) "-> list of AFISAFI"   (cadr (cddddr peer-config)))
(defun PEER-CONFIG-get-capability-set (peer-config)      "-> list of BGP-CAP-"  (caddr (cddddr peer-config)))

(defun PEER-CONFIG-make (peer-name
			 &key peer-ip-address
			   peer-asn
			   (hold-time 90)
			   (address-family-list (list +AFISAFI-ipv4-unicast+
						      +AFISAFI-ipv6-unicast+))
			   (capability-set (cons (BGP-CAP-4-OCTET-ASN-make peer-asn)
						 (cons (BGP-CAP-ROUTE-REFRESH-make)
						       (loop for afisafi in address-family-list
							     collect (BGP-CAP-MP-EXTENSIONS-make afisafi))))))
  (list 'PEER-CONFIG
	peer-name
	peer-ip-address
	peer-asn
	hold-time
	address-family-list
	capability-set))
  
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
    
