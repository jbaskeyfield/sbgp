#|
1. Introduction

   Currently there does not exist a mechanism in BGP-4 [BGP-4] to
   dynamically request a re-advertisement of the Adj-RIB-Out from a BGP
   peer.  When the inbound routing policy for a peer changes, all
   prefixes from that peer must be somehow made available and then re-
   examined against the new policy. To accomplish this, a commonly used
   approach, known as 'soft-reconfiguration', is to store an unmodified
   copy of all routes from that peer at all times, even though routing
   policies do not change frequently (typically no more than a couple
   times a day). Additional memory and CPU are required to maintain
   these routes.

   This document proposes an alternative solution that avoids the
   additional maintenance cost. More specifically, it defines a new BGP
   capability termed 'Route Refresh Capability', which would allow the
   dynamic exchange of route refresh request between BGP speakers and
   subsequent re-advertisement of the respective Adj-RIB-Out.





Chen                        Standards Track                     [Page 1]

RFC 2918                Route Refresh for BGP-4           September 2000


2. Route Refresh Capability

   To advertise the Route Refresh Capability to a peer, a BGP speaker
   uses BGP Capabilities Advertisement [BGP-CAP]. This capability is
   advertised using the Capability code 2 and Capability length 0.

   By advertising the Route Refresh Capability to a peer, a BGP speaker
   conveys to the peer that the speaker is capable of receiving and
   properly handling the ROUTE-REFRESH message (as defined in Section 3)
   from the peer.

3. Route-REFRESH Message

   The ROUTE-REFRESH message is a new BGP message type defined as
   follows:

          Type: 5 - ROUTE-REFRESH

          Message Format: One <AFI, SAFI> encoded as

                  0       7      15      23      31
                  +-------+-------+-------+-------+
                  |      AFI      | Res.  | SAFI  |
                  +-------+-------+-------+-------+

          The meaning, use and encoding of this <AFI, SAFI> field is the
          same as defined in [BGP-MP, sect. 7]. More specifically,

               AFI  - Address Family Identifier (16 bit).

               Res. - Reserved (8 bit) field. Should be set to 0 by the
                      sender and ignored by the receiver.

               SAFI - Subsequent Address Family Identifier (8 bit).

4. Operation

   A BGP speaker that is willing to receive the ROUTE-REFRESH message
   from its peer should advertise the Route Refresh Capability to the
   peer using BGP Capabilities advertisement [BGP-CAP].

   A BGP speaker may send a ROUTE-REFRESH message to its peer only if it
   has received the Route Refresh Capability from its peer.  The <AFI,
   SAFI> carried in such a message should be one of the <AFI, SAFI> that
   the peer has advertised to the speaker at the session establishment
   time via capability advertisement.





Chen                        Standards Track                     [Page 2]

RFC 2918                Route Refresh for BGP-4           September 2000


   If a BGP speaker receives from its peer a ROUTE-REFRESH message with
   the <AFI, SAFI> that the speaker didn't advertise to the peer at the
   session establishment time via capability advertisement, the speaker
   shall ignore such a message.  Otherwise, the BGP speaker shall re-
   advertise to that peer the Adj-RIB-Out of the <AFI, SAFI> carried in
   the message, based on its outbound route filtering policy.

5. Security Considerations

   This extension to BGP does not change the underlying security issues.

6. Acknowledgments

   The concept of Route Refresh proposed is similar to the one used in
   IDRP.

   The author would like to thank Yakov Rekhter, Ravi Chandra, Srihari
   Ramachandra and Bruce Cole for their review and comments.

7. References

   [BGP-4]   Rekhter, Y. and T. Li, "A Border Gateway Protocol 4 (BGP-
             4)", RFC 1771, March 1995.

   [BGP-MP]  Bates, T., Chandra, R., Katz, D. and Y. Rekhter,
             "Multiprotocol Extensions for BGP-4", RFC 2858, June 2000.

   [BGP-CAP] Chandra, R. and J. Scudder, "Capabilities Advertisement
             with BGP-4", RFC 2842, May 2000.
|#

(in-package :sbgp)

(defun BGP-ROUTE-REFRESH-make (afi safi)
  (list 'BGP-ROUTE-REFRESH
	(AFISAFI-make afi safi)))

(defun BGP-ROUTE-REFRESH-get-afisafi (obj) "-> AFISAFI" (cadr obj))

(defun BGP-ROUTE-REFRESH-get-io-rw-octets () 4)

(defun BGP-ROUTE-REFRESH-io-read (message-length stream-in)
  (if (not (= message-length 4))
      (error "BGP-ROUTE-REFRESH-io-read, message-length = ~S, (expected 4)" message-length)
      (let ((afi (io-read-uNbe u16 stream-in)))
	(io-read-uNbe u8 stream-in)  ; discard reserved octets
	(let ((safi (io-read-uNbe u8 stream-in)))
	    (list 'BGP-ROUTE-REFRESH
		  (AFISAFI-make afi safi))))))

(defun BGP-ROUTE-REFRESH-io-write (obj stream-out)
  (let ((afisafi (BGP-ROUTE-REFRESH-get-afisafi obj)))
    (io-write-uNbe u16 (AFISAFI-get-afi afisafi) stream-out)
    (io-write-uNbe u8 0 stream-out)
    (io-write-uNbe u8 (AFISAFI-get-safi afisafi) stream-out)))

(deftype BGP-ROUTE-REFRESH () '(cons (member BGP-ROUTE-REFRESH)))

(defun BGP-ROUTE-REFRESH-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~6,'0X)"
	  (TL-get-name obj)
	  (BGP-ROUTE-REFRESH-get-afisafi obj)))

(set-pprint-dispatch '(cons (member BGP-ROUTE-REFRESH)) #'BGP-ROUTE-REFRESH-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun BGP-ROUTE-REFRESH-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W #x~6,'0X]~)"
	  (TL-get-name obj)
	  (BGP-ROUTE-REFRESH-get-afisafi obj)))

(set-pprint-dispatch '(cons (member BGP-ROUTE-REFRESH)) #'BGP-ROUTE-REFRESH-pprint-2 0 *sbgp-pprint-dispatch-table-2*)
