
#|
RFC 4271                         BGP-4                      January 2006
4.2.  OPEN Message Format

After a TCP connection is established, the first message sent by each
side is an OPEN message.  If the OPEN message is acceptable, a
KEEPALIVE message confirming the OPEN is sent back.

In addition to the fixed-size BGP header, the OPEN message contains
the following fields:

0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+
|    Version    |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|     My Autonomous System      |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|           Hold Time           |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         BGP Identifier                        |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
| Opt Parm Len  |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                                               |
|             Optional Parameters (variable)                    |
|                                                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Version:

This 1-octet unsigned integer indicates the protocol version
number of the message.  The current BGP version number is 4.

My Autonomous System:

This 2-octet unsigned integer indicates the Autonomous System
number of the sender.

Hold Time:

This 2-octet unsigned integer indicates the number of seconds
the sender proposes for the value of the Hold Timer.  Upon
receipt of an OPEN message, a BGP speaker MUST calculate the
value of the Hold Timer by using the smaller of its configured
Hold Time and the Hold Time received in the OPEN message.  The
Hold Time MUST be either zero or at least three seconds.  An
implementation MAY reject connections on the basis of the Hold

Time.  The calculated value indicates the maximum number of
seconds that may elapse between the receipt of successive
KEEPALIVE and/or UPDATE messages from the sender.

BGP Identifier:

This 4-octet unsigned integer indicates the BGP Identifier of
the sender.  A given BGP speaker sets the value of its BGP
Identifier to an IP address that is assigned to that BGP
speaker.  The value of the BGP Identifier is determined upon
startup and is the same for every local interface and BGP peer.

Optional Parameters Length:

This 1-octet unsigned integer indicates the total length of the
Optional Parameters field in octets.  If the value of this
field is zero, no Optional Parameters are present.

Optional Parameters:

This field contains a list of optional parameters, in which
each parameter is encoded as a <Parameter Type, Parameter
Length, Parameter Value> triplet.

0                   1
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-...
|  Parm. Type   | Parm. Length  |  Parameter Value (variable)
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-...

Parameter Type is a one octet field that unambiguously
identifies individual parameters.  Parameter Length is a one
octet field that contains the length of the Parameter Value
field in octets.  Parameter Value is a variable length field
that is interpreted according to the value of the Parameter
Type field.

[RFC3392] defines the Capabilities Optional Parameter.

The minimum length of the OPEN message is 29 octets (including the
message header).
|#

(in-package :sbgp)

(defun BGP-OPEN-get-version (obj)             "-> u8"     (cadr obj))
(defun BGP-OPEN-get-my-as (obj)               "-> u16"    (caddr obj))
(defun BGP-OPEN-get-hold-time (obj)           "-> u16"    (cadddr obj))
(defun BGP-OPEN-get-bgp-id (obj)              "-> IPV4"   (car (cddddr obj)))
(defun BGP-OPEN-get-opt-param-len (obj)       "-> u8"     (cadr (cddddr obj)))
(defun BGP-OPEN-get-optional-parameters (obj) "-> list-OPEN-OPT-PARAM" (caddr (cddddr obj)))

(defun BGP-OPEN-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (+ 10 (BGP-OPEN-get-opt-param-len obj)))

(defun BGP-OPEN-io-read (message-length stream-in)
  (let* ((version (io-read-uNbe u8 stream-in))        ; version
	 (my-as (io-read-uNbe u16 stream-in))         ; my-as
	 (hold-time (io-read-uNbe u16 stream-in))     ; hold-time
	 (bgp-id (IPV4-io-read stream-in))            ; bgp-id
	 (opt-param-len (io-read-uNbe u8 stream-in))) ; opt-param-len
    (if (not (= message-length
		(+ opt-param-len 10)))
	(error "BGP-OPEN-io-read, message-length = ~S, opt-param-len = ~S expected (= message-length (+ opt-param-len 10))" message-length opt-param-len)
	(list 'BGP-OPEN
	      version
	      my-as
	      hold-time
	      bgp-id
	      opt-param-len
	      (OPEN-OPT-PARAM-io-read-list opt-param-len stream-in)))))

(defun BGP-OPEN-io-write (obj stream-out)
  (destructuring-bind (version my-as hold-time bgp-id opt-param-len optional-parameters)
      (cdr obj)
    (io-write-uNbe u8 version stream-out)                    ; version
    (io-write-uNbe u16 my-as stream-out)                     ; my-as
    (io-write-uNbe u16 hold-time stream-out)                 ; hold-time
    (IPV4-io-write bgp-id stream-out)                        ; bgp-id
    (io-write-uNbe u8 opt-param-len stream-out)              ; opt-param-len
    (dolist (oop optional-parameters)                        ; optional-parameters
      (OPEN-OPT-PARAM-io-write oop stream-out))))               

(defun BGP-OPEN-make (my-as hold-time my-bgp-id capabilities-list)
  "Arguments: MY-AS - u16 or zero if 4 octet AS and ASN > 65525
HOLD-TIME - u16 time in seconds
BGP-ID - IPV4 object
CAPABILITIES-LIST - list of objects made by BGP-CAP-*-make
Returns -> BGP-OPEN object serializable by BGP-OPEN-io-write"
  (let* ((open-opt-params
	   (loop for cap in capabilities-list collect (OPEN-OPT-PARAM-make cap)))
	 (open-opt-params-length
	   (loop for opt-param in open-opt-params sum (OPEN-OPT-PARAM-get-io-rw-octets opt-param))))
    (list 'BGP-OPEN
	  4
	  my-as
	  hold-time
	  my-bgp-id
	  open-opt-params-length
	  open-opt-params)))

(defun BGP-OPEN-valid2-p (obj)
 (if obj t nil))
  #|
  (and (= (length obj) 7)
       (u8-p (BGP-OPEN-get-version obj))
       (= 4 (BGP-OPEN-get-version obj))
       (u16-p (BGP-OPEN-get-my-as obj))
       (u16-p (BGP-OPEN-get-hold-time obj))
       (typep (BGP-OPEN-get-bgp-id obj) 'IPV4)
       (u8-p (BGP-OPEN-get-opt-param-len obj))
       (every #'OPEN-OPT-PARAM-typep
	      (BGP-OPEN-get-optional-parameters obj))))
|#
(deftype BGP-OPEN () '(cons (member BGP-OPEN)))

(defun BGP-OPEN-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~A ~D #x~4,'0X ~D ~/sbgp:ipv4-pprint-1/ ~D ~W)"
	  (TL-get-name obj)                  ; symbol
	  (BGP-OPEN-get-version obj)               ; u8   
	  (BGP-OPEN-get-my-as obj)                 ; u16   
	  (BGP-OPEN-get-hold-time obj)             ; u16   
	  (BGP-OPEN-get-bgp-id obj)                ; IPV4  
	  (BGP-OPEN-get-opt-param-len obj)         ; u8   
	  (BGP-OPEN-get-optional-parameters obj))) ; OPEN-OPT-PARAM list

(set-pprint-dispatch '(cons (member BGP-OPEN)) #'BGP-OPEN-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun BGP-OPEN-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~A ~D [~/sbgp:u32-pprint-asn/] ~D ~/sbgp:ipv4-pprint-2/ ~D~) ~W]"
	  (TL-get-name obj)                  ; symbol
	  (BGP-OPEN-get-version obj)               ; u8   
	  (BGP-OPEN-get-my-as obj)                 ; u16   
	  (BGP-OPEN-get-hold-time obj)             ; u16   
	  (BGP-OPEN-get-bgp-id obj)                ; IPV4  
	  (BGP-OPEN-get-opt-param-len obj)         ; u8   
	  (BGP-OPEN-get-optional-parameters obj))) ; OPEN-OPT-PARAM list

(set-pprint-dispatch '(cons (member BGP-OPEN)) #'BGP-OPEN-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

