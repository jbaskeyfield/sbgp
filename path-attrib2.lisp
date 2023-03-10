(in-package :sbgp)

#| -make & -get-value are handled by functions from different PATH-ATTRIB 'sub-types'
This 'parent-type' of all attribs is used for common functions and io-read/write
See path-attrib1.lisp for:
PATH-ATTRIB-make
PATH-ATTRIB-get-name
PATH-ATTRIB-get-attribute-type-field
PATH-ATTRIB-get-attribute-length
PATH-ATTRIB-get-type
PATH-ATTRIB-get-flag-optional-bit
PATH-ATTRIB-get-flag-transitive-bit
PATH-ATTRIB-get-flag-partial-bit
PATH-ATTRIB-get-flag-extended-len-bit (obj)
PATH-ATTRIB-get-io-rw-octets (obj)
|#

(defun PATH-ATTRIB-get-io-rw-octets (4-octet-asn-flag obj)
  (case (PATH-ATTRIB-get-type-subfield obj)
    (2 ; AS-PATH
     (AS-PATH-get-io-rw-octets 4-octet-asn-flag obj))
    (7 ; AGGREGATOR
     (AGGREGATOR-get-io-rw-octets 4-octet-asn-flag obj))
    (t
     (+ (if (PATH-ATTRIB-get-flag-extended-len-bit obj)
	     4
	     3)
	(PATH-ATTRIB-get-attribute-length obj)))))

(defun PATH-ATTRIB-io-read (4-octet-asn-flag stream-in)
  (let* ((attribute-type (io-read-uNbe u16 stream-in))
	 
	 (attribute-length (if (uN-bit-set-p u16 attribute-type 3)  ; extended-length bit flag set
			       (io-read-uNbe u16 stream-in)
			       (io-read-uNbe u8 stream-in)))
	 
	 (type-subfield (logand #xff attribute-type)))
    (case type-subfield
      (1  (ORIGIN-io-read attribute-type attribute-length stream-in))                              ;  ORIGIN [RFC4271]
      (2  (AS-PATH-io-read 4-octet-asn-flag attribute-type attribute-length stream-in))            ;  AS_PATH [RFC4271]
      (3  (NEXT-HOP-io-read attribute-type attribute-length +AFISAFI-ipv4-unicast+ stream-in))     ;  NEXT_HOP [RFC4271]
      (4  (MULTI-EXIT-DISC-io-read attribute-type attribute-length stream-in))                     ;  MULTI_EXIT_DISC [RFC4271]
      (5  (LOCAL-PREF-io-read attribute-type attribute-length stream-in))                          ;  LOCAL_PREF [RFC4271]
      (6  (ATOMIC-AGGREGATE-io-read attribute-type attribute-length))                              ;  ATOMIC_AGGREGATE [RFC4271]
      (7  (AGGREGATOR-io-read 4-octet-asn-flag attribute-type attribute-length stream-in))         ;  AGGREGATOR [RFC4271]
      (8  (COMMUNITY-io-read attribute-type attribute-length stream-in))                           ;  COMMUNITY [RFC1997]
      (9  (ORIGINATOR-ID-io-read attribute-type attribute-length stream-in))                       ;  ORIGINATOR_ID [RFC4456]
      (10 (CLUSTER-LIST-io-read attribute-type attribute-length stream-in))                        ;  CLUSTER_LIST [RFC4456]
      (14 (MP-REACH-NLRI-io-read attribute-type attribute-length stream-in))                       ;  MP_REACH_NLRI [RFC4760]
      (15 (MP-UNREACH-NLRI-io-read attribute-type attribute-length stream-in))                     ;  MP_UNREACH_NLRI [RFC4760]
      (32 (LARGE-COMMUNITY-io-read attribute-type attribute-length stream-in))                     ;  LARGE_COMMUNITY [RFC8092]
      (t (PATH-ATTRIB-UNKNOWN-io-read attribute-type attribute-length stream-in)))))

(defun PATH-ATTRIB-io-read-list (octets 4-octet-asn-flag stream-in)
  (labels ((recurse (bytes-read)
	     (if (>= bytes-read octets)
		 nil
		 (let ((elem (PATH-ATTRIB-io-read 4-octet-asn-flag stream-in)))
		   (cons elem
			 (recurse (+ bytes-read (PATH-ATTRIB-get-io-rw-octets 4-octet-asn-flag elem))))))))
    (recurse 0)))

(defun PATH-ATTRIB-io-write (4-octet-asn-flag obj stream-out)
  (case (PATH-ATTRIB-get-type-subfield obj)
    (1  (ORIGIN-io-write obj stream-out))                          ;  ORIGIN [RFC4271]
    (2  (AS-PATH-io-write 4-octet-asn-flag obj stream-out))        ;  AS_PATH [RFC4271]
    (3  (NEXT-HOP-io-write obj stream-out))                        ;  NEXT_HOP [RFC4271]
    (4  (MULTI-EXIT-DISC-io-write obj stream-out))                 ;  MULTI_EXIT_DISC [RFC4271]
    (5  (LOCAL-PREF-io-write obj stream-out))                      ;  LOCAL_PREF [RFC4271]
    (6  (ATOMIC-AGGREGATE-io-write obj stream-out))                ;  ATOMIC_AGGREGATE [RFC4271]
    (7  (AGGREGATOR-io-write 4-octet-asn-flag obj stream-out))     ;  AGGREGATOR [RFC4271]
    (8  (COMMUNITY-io-write obj stream-out))                       ;  COMMUNITY [RFC1997]
    (9  (ORIGINATOR-ID-io-write obj stream-out))                   ;  ORIGINATOR_ID [RFC4456]
    (10 (CLUSTER-LIST-io-write obj stream-out))                    ;  CLUSTER_LIST [RFC4456]
    (14 (MP-REACH-NLRI-io-write obj stream-out))                   ;  MP_REACH_NLRI [RFC4760]
    (15 (MP-UNREACH-NLRI-io-write obj stream-out))                 ;  MP_UNREACH_NLRI [RFC4760]
    (32 (LARGE-COMMUNITY-io-write obj stream-out))                 ;  LARGE_COMMUNITY [RFC8092]
    (t (PATH-ATTRIB-UNKNOWN-io-write obj stream-out))))

(defun PATH-ATTRIB-valid1-p (obj)
   "Test list elements are of correct type"
  (and (consp obj)
       (case (TL-get-name obj)
	 (ORIGIN                 (ORIGIN-valid1-p obj))              
	 (AS-PATH		 (AS-PATH-valid1-p obj))             
	 (NEXT-HOP		 (NEXT-HOP-valid1-p obj))            
	 (MULTI-EXIT-DISC	 (MULTI-EXIT-DISC-valid1-p obj))     
	 (LOCAL-PREF		 (LOCAL-PREF-valid1-p obj))          
	 (ATOMIC-AGGREGATE	 (ATOMIC-AGGREGATE-valid1-p obj))    
	 (AGGREGATOR		 (AGGREGATOR-valid1-p obj))          
	 (COMMUNITY		 (COMMUNITY-valid1-p obj))
	 (ORIGINATOR-ID		 (ORIGINATOR-ID-valid1-p obj))
	 (CLUSTER-LIST		 (CLUSTER-LIST-valid1-p obj))
	 (MP-REACH-NLRI	         (MP-REACH-NLRI-valid1-p obj))       
	 (MP-UNREACH-NLRI	 (MP-UNREACH-NLRI-valid1-p obj))     
	 (LARGE-COMMUNITY	 (LARGE-COMMUNITY-valid1-p obj))     
	 (PATH-ATTRIB-UNKNOWN    (PATH-ATTRIB-UNKNOWN-valid1-p obj)) 
	 (t nil))))

(defun PATH-ATTRIB-valid2-p (obj)
  "Test list values are within allowed range"
(and (PATH-ATTRIB-valid1-p obj)
     (case (TL-get-name obj)
	 (ORIGIN                 (ORIGIN-valid2-p obj))              
	 (AS-PATH		 (AS-PATH-valid2-p obj))             
	 (NEXT-HOP		 (NEXT-HOP-valid2-p obj))            
	 (MULTI-EXIT-DISC	 (MULTI-EXIT-DISC-valid2-p obj))     
	 (LOCAL-PREF		 (LOCAL-PREF-valid2-p obj))          
	 (ATOMIC-AGGREGATE	 (ATOMIC-AGGREGATE-valid2-p obj))    
	 (AGGREGATOR		 (AGGREGATOR-valid2-p obj))          
	 (COMMUNITY		 (COMMUNITY-valid2-p obj))
	 (ORIGINATOR-ID		 (ORIGINATOR-ID-valid2-p obj))
	 (CLUSTER-LIST		 (CLUSTER-LIST-valid2-p obj))
	 (MP-REACH-NLRI	         (MP-REACH-NLRI-valid2-p obj))       
	 (MP-UNREACH-NLRI	 (MP-UNREACH-NLRI-valid2-p obj))     
	 (LARGE-COMMUNITY	 (LARGE-COMMUNITY-valid2-p obj))     
	 (PATH-ATTRIB-UNKNOWN    (PATH-ATTRIB-UNKNOWN-valid2-p obj)) 
	 (t nil))))

(defun PATH-ATTRIB-zhash (obj)
  (case (TL-get-name obj)
    (ORIGIN		  (ORIGIN-zhash 0 obj))
    (AS-PATH		  (AS-PATH-zhash 0 obj))             
    (NEXT-HOP		  (NEXT-HOP-zhash 0 obj))
    (COMMUNITY		  (COMMUNITY-zhash 0 obj)) 
    (MULTI-EXIT-DISC	  (MULTI-EXIT-DISC-zhash 0 obj))     
    (LOCAL-PREF		  (LOCAL-PREF-zhash 0 obj))
    (ORIGINATOR-ID        (ORIGINATOR-ID-zhash 0 obj))
    (CLUSTER-LIST	  (CLUSTER-LIST-zhash 0 obj))
    (ATOMIC-AGGREGATE	  (ATOMIC-AGGREGATE-zhash 0 obj))    
    (AGGREGATOR		  (AGGREGATOR-zhash 0 obj))          
    (LARGE-COMMUNITY	  (LARGE-COMMUNITY-zhash 0 obj))
    (PATH-ATTRIB-UNKNOWN  (PATH-ATTRIB-UNKNOWN-zhash 0 obj))
    (t 0))) 

(defun PATH-ATTRIB-zhash-list (list-path-attrib)
  "Returned combined 'logxor' of all path attributes in LIST-PATH-ATTRIB"
  (do ((lst list-path-attrib (cdr lst))
       (hash 0 (logxor hash (PATH-ATTRIB-zhash (car lst)))))
      ((null lst) hash)))

