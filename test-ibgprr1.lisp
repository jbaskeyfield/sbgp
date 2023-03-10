#|
    '(router-config        ROUTER1                           ;; => name of thread "ROUTER1"
      router-id            (IPV4 #x0a0a0a01)                 ;; set
      asn                  65533                             ;; set
      listener-list        ((IPV4 #x0a0a0a01) 179)           ;; add / remove
      peer-list            (PEER1 PEER2))                    ;; add / remove

    '(peer-name            PEER1                             ;; => name of thread "ROUTER1-PEER1"
      remote-asn           65533
      peer-ip-address      (IPV4 #x0a0a0a0a)
      address-family-list  (ipv4-unicast ipv6-unicast))

    '(peer-config          PEER2                             ;; => name of thread "ROUTER1-PEER2"
      remote-asn           65533
      peer-ip-address      (IPV4 #x0a0a0a0b)
      address-family-list  (ipv4-unicast ipv6-unicast))
|#

(in-package :sbgp)
(setf *t-thread-debug-messages-force-output* t)

(defmacro rx (q) `(queue-receive ,q))
(defmacro tx (q &body msg) `(queue-send ,q (MSG-make ,@msg)))

(defparameter t1 (THREAD-make 'ROUTER1 nil #'ROUTER-thread-loop))
(defparameter q1 (thread-get-control-queue t1))

(tx q1 'to 'tcpserver 'add 'listener '(ipv4 #x7f000001) 179)
(tx q1 'to 'tcpserver 'add 'listener '(ipv4 #x0a0a0a01) 179)
(tx q1 'add 'peer 'PEER1)
(tx q1 'add 'peer 'PEER2)

(tx q1 'set 'router-config (ROUTER-CONFIG-make 'ROUTER1
					       :router-id '(ipv4 #x0a0a0a01)
					       :local-asn 65533))
(tx q1 'set 'peer-config (PEER-CONFIG-make 'ROUTER1-PEER1
					   :peer-asn 65533
					   :flags +PEER-CONFIG-flag-rr-client+
					   :peer-ip-address '(ipv4 #x0a0a0a0a)))
(tx q1 'set 'peer-config (PEER-CONFIG-make 'ROUTER1-PEER2
					   :peer-asn 65533
					   :flags +PEER-CONFIG-flag-rr-client+
					   :peer-ip-address '(ipv4 #x0a0a0a0b)))

(defparameter router1-rib-loc (RIB-LOC-make 5))
(defparameter router1-peer1-rib-adj-in (RIB-ADJ-make 5))
(defparameter router1-peer1-rib-adj-out (RIB-ADJ-make 5))

(defparameter router1-peer2-rib-adj-in (RIB-ADJ-make 5))
(defparameter router1-peer2-rib-adj-out (RIB-ADJ-make 5))

(tx q1 'set 'rib-loc router1-rib-loc)
(tx q1 'to 'router1-peer1 'set 'rib-adj-in router1-peer1-rib-adj-in)
(tx q1 'to 'router1-peer1 'set 'rib-adj-out router1-peer1-rib-adj-out)
(tx q1 'to 'router1-peer2 'set 'rib-adj-in router1-peer2-rib-adj-in)
(tx q1 'to 'router1-peer2 'set 'rib-adj-out router1-peer2-rib-adj-out)

(tx q1 'to 'router1-peer1 'get 'rib-adj-in)

(tx q1 'to 'router1-peer1 'Event-1-ManualStart)
(tx q1 'to 'router1-peer2 'Event-1-ManualStart)




;;; Single peer test

(in-package :sbgp)
(setf *t-thread-debug-messages-force-output* t)

(defmacro rx (q) `(queue-receive ,q))
(defmacro tx (q &body msg) `(queue-send ,q (MSG-make ,@msg)))

(defparameter t1 (THREAD-make 'ROUTER1 nil #'ROUTER-thread-loop))
(defparameter q1 (thread-get-control-queue t1))

(tx q1 'to 'tcpserver 'add 'listener '(ipv4 #x7f000001) 179)
(tx q1 'to 'tcpserver 'add 'listener '(ipv4 #x0a0a0a01) 179)
(tx q1 'add 'peer 'PEER1)

(tx q1 'set 'router-config (ROUTER-CONFIG-make 'ROUTER1
					       :router-id '(ipv4 #x0a0a0a01)
					       :local-asn 65533))
(tx q1 'set 'peer-config (PEER-CONFIG-make 'ROUTER1-PEER1
					   :peer-asn 65533
					   :flags +PEER-CONFIG-flag-rr-client+
					   :peer-ip-address '(ipv4 #x0a0a0a0a)))

(defparameter router1-rib-loc (RIB-LOC-make 5))
(defparameter router1-peer1-rib-adj-in (RIB-ADJ-make 5))
(defparameter router1-peer1-rib-adj-out (RIB-ADJ-make 5))

(tx q1 'set 'rib-loc router1-rib-loc)
(tx q1 'to 'router1-peer1 'set 'rib-adj-in router1-peer1-rib-adj-in)
(tx q1 'to 'router1-peer1 'set 'rib-adj-out router1-peer1-rib-adj-out)


(tx q1 'to 'router1-peer1 'set 'debug-fsm-state *standard-output*)
(tx q1 'to 'router1-peer1 'set 'debug-fsm-events *standard-output*)

(tx q1 'to 'router1-peer1 'Event-1-ManualStart)
