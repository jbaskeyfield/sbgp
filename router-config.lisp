(in-package :sbgp)

(defun ROUTER-CONFIG-get-router-name (router-config)        "-> symbol"                        (cadr router-config))
(defun ROUTER-CONFIG-get-router-id (router-config)          "-> IPV4"                          (caddr router-config))
(defun ROUTER-CONFIG-get-local-asn (router-config)          "-> u32"                           (cadddr router-config))

(defun ROUTER-CONFIG-make (name-symbol &key router-id local-asn)
  (list 'ROUTER-CONFIG
	name-symbol
	router-id
	local-asn))

