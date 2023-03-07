
(defpackage :iana-assignments
  (:nicknames :iana)
  (:use :common-lisp)
  (:export
   #:+BGP-ORIGIN+
   #:+BGP-MESSAGE-TYPES+
   #:+BGP-OPEN+
   #:+BGP-CAP+
   #:+AFI+
   #:+SAFI+
   #:+BGP-PATH-ATTRIBUTES+
   #:+BGP-ERROR-NOTIFICATION+
   #:+BGP-ERROR+
   #:+OPEN-MESSAGE-ERROR+
   #:+UPDATE-MESSAGE-ERROR+
   #:+BGP-FSM-ERROR+
   #:+BGP-CEASE-NOTIFICATION+
   #:+BGP-ROUTE-REFRESH-MESSAGE-ERROR+
   #:+BGP-ORF-TYPES+
   #:+BGP-ROUTE-REFRESH+
   ))
(load "iana-assignments.lisp")

(defpackage :uint
  (:use :common-lisp)
  (:export
   #:u8 #:u16 #:u24 #:u32 #:u48 #:u56
   #:u8-p #:u16-p #:u24-p #:u32-p #:u48-p #:u56-p
   #:io-read-uNbe
   #:io-write-uNbe
   #:io-read-uNbe-octets
   #:io-write-uNbe-octets
   #:uint-print
   #:uN-bit-set-p
   #:list-uN-bit-set-p
   #:uN-make-bit-mask
   #:list-uN-make-bit-mask
   #:integer->list-uN
   #:list-uN->integer
   #:list-uN-truncate-to-octets
   #:list-uN->list-uM-subseq
   #:list-uN->list-uM
   ))
(load "uint.lisp")            ;; Unsigned integers [u8,u16,u32,u56] and untagged lists of integers

(defpackage :sbgp
  (:nicknames :s9)
  (:use :common-lisp :uint)
  (:export ))

(load "globals.lisp")
(load "tagged-lists.lisp")
(load "zkeys-table-lookup.lisp")

(load "pprint.lisp")
(load "cache.lisp")                        ;; CACHE   - cache table for path attributes (netiorx/*-io-read) and lists path-attributes (peer/rib-adj-entry-make)

(load "ip-address.lisp")                   ;; IPV4, IPV6
(load "bytes.lisp")                        ;; BYTES

(load "nlri-cache1.lisp")                  ;; NLRI-CACHE
(load "nlri.lisp")                         ;; AFISAFI, NLRI, NLRI-WITHDRAWL
(load "nlri-cache2.lisp")                  ;; NLRI-CACHE (functions that are dependent on nlri.lisp)

(load "path-attrib1.lisp")                 ;; PATH-ATTRIB (common functions)
(load "path-attrib-unknown.lisp")          ;; PATH-ATRIB-UNKNOWN
(load "path-attrib-origin.lisp")           ;; ORIGIN
(load "path-attrib-as-path.lisp")          ;; AS-PATH
(load "path-attrib-next-hop.lisp")         ;; NEXT-HOP
(load "path-attrib-multi-exit-disc.lisp")  ;; MULTI-EXIT-DISC
(load "path-attrib-local-pref.lisp")       ;; LOCAL-PREF
(load "path-attrib-atomic-aggregate.lisp") ;; ATOMIC-AGGREGATE
(load "path-attrib-aggregator.lisp")       ;; AGGREGATOR
(load "path-attrib-community.lisp")        ;; COMMUNITY
(load "path-attrib-mp-reach-nlri.lisp")    ;; MP-REACH-NLRI
(load "path-attrib-mp-unreach-nlri.lisp")  ;; MP-UNREACH-NLRI
(load "path-attrib-large-community.lisp")  ;; LARGE-COMMUNITY
(load "path-attrib-originator-id.lisp")    ;; ORIGINATOR-ID, CLUSTER-LIST
(load "path-attrib2.lisp")                 ;; PATH-ATTRIB (calls above path-attrib fns.)

(load "bgp-cap.lisp")                      ;; BGP-CAP, BGP-CAP-UNKNOWN, BGP-CAP-MP-EXTENSIONS, BGP-CAP-ROUTE-REFRESH, BGP-CAP-4-OCTET-ASN
(load "open-opt-param.lisp")               ;; OPEN-OPT-PARAM
(load "bgp-open.lisp")                     ;; BGP-OPEN
(load "bgp-update.lisp")                   ;; BGP-UPDATE
(load "bgp-notification.lisp")             ;; BGP-NOTIFICATION
(load "bgp-keepalive.lisp")                ;; BGP-KEEPALIVE
(load "bgp-route-refresh.lisp")            ;; BGP-ROUTE-REFRESH

(load "bgp-message.lisp")                  ;; MSG-HEADER, BGP-MESSAGE

(load "mrt-bgpdump.lisp")                  ;; MRT-COMMON-HEADER, MRT...


;;; Threading and fifo queues
(load "t-queues.lisp")
(load "t-threads.lisp")

(load "sbgp-update.lisp")                  ;; SBGP-UPDATE

(load "list-utils.lisp")                   ;; remove-if!, remove-sublist-elements-if!
(load "rib-adj.lisp")                      ;; RIB-ADJ-ENTRY, RIB-ADJ
(load "rib-loc.lisp")                      ;; RIB-ENTRY, RIB-ENTRY-TABLE, RIB-LOC

(load "tcpserver-timers.lisp")             
(load "tcpserver.lisp")                    ;; TCPSERVER-thread-loop
(load "netio.lisp")                        ;; NETIORX-thread-loop, NETIOTX-thread-loop

(load "router-config.lisp")                ;; ROUTER-CONFIG
(load "router-timers.lisp")                ;; ROUTER-TIMERS
(load "peer-config.lisp")                  ;; PEER-CONFIG
(load "peer-timers.lisp")                  ;; PEER-TIMERS

(load "fsm-timers.lisp")
(load "fsm-attrib.lisp")
(load "peer.lisp")                         ;; PEER-thread-loop

(load "bintree.lisp")

(load "router.lisp")                       ;; ROUTER-thread-loop

(load "mrt-utils.lisp")                    ;; mrt-read

