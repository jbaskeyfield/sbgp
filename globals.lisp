(in-package :sbgp)


(defparameter *rr-polling-bucket-size* 5 "Maximum number of sequential reads from threads queue when polling list of queues")
(defparameter *t-thread-debug-messages-flag* t)
(defparameter *t-thread-debug-messages-force-output* t "Unbuffered debug logging. If set to T, 'force-output' is run after each message is written to debug log file")
;;(defparameter *t-thread-debug-messages-directory* nil
;;(defparameter *t-thread-debug-messages-filename-prefix* nil
;;

;;; Special variables - shadowed by each thread to point to cache table unique per thread.
;;; Global value of these variables should always be nil. DO NOT CHANGE

;; *path-attrib-cache* CACHE table object used by NETIORX threads.
;; <PATH-ATTRIB>-make functions (called by <PATH-ATTRIB>-io-read) will optionally query this cache before returning newly created object. Each thread making path attributes should be sent its own CACHE object.
(defparameter *path-attrib-cache* nil "CACHE used by <PATH-ATTRIB>-make functions - shadowed in each NETIORX thread to unique CACHE table")

;; *path-attrib-list-cache* CACHE object used by PEER threads.
;; RIB-ADJ-ENTRY-make function will optionally query this cache before returning a newly created object.
(defparameter *path-attrib-list-cache* nil "CACHE used by RIB-ADJ-ENTRY-make - shadowed in each PEER thread to unique CACHE table")

(defparameter *nlri-cache* nil "CACHE populated by ROUTER thread. Read only access in PEER threads")


(defparameter +all-update-types+ '(IPV4 IPV6 BYTES NLRI NLRI-WITHDRAWL PATH-ATTRIB-UNKNOWN ORIGIN AS-PATH NEXT-HOP MULTI-EXIT-DISC LOCAL-PREF ATOMIC-AGGREGATE AGGREGATOR COMMUNITY MP-REACH-NLRI MP-UNREACH-NLRI LARGE-COMMUNITY))
(defparameter +all-nlri-types+ '(NLRI NLRI-WITHDRAWL))
(defparameter +all-hashed-path-attrib-types+ '(BYTES PATH-ATTRIB-UNKNOWN ORIGIN AS-PATH NEXT-HOP MULTI-EXIT-DISC LOCAL-PREF ATOMIC-AGGREGATE AGGREGATOR COMMUNITY MP-REACH-NLRI MP-UNREACH-NLRI LARGE-COMMUNITY))
