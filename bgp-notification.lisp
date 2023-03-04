#|
RFC 4271 
4.5.  NOTIFICATION Message Format

A NOTIFICATION message is sent when an error condition is detected.
The BGP connection is closed immediately after it is sent.

In addition to the fixed-size BGP header, the NOTIFICATION message
contains the following fields:

0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
| Error code    | Error subcode |   Data (variable)             |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Error Code:

This 1-octet unsigned integer indicates the type of
NOTIFICATION.  The following Error Codes have been defined:

Error Code       Symbolic Name               Reference

1         Message Header Error             Section 6.1

2         OPEN Message Error               Section 6.2

3         UPDATE Message Error             Section 6.3

4         Hold Timer Expired               Section 6.5

5         Finite State Machine Error       Section 6.6

6         Cease                            Section 6.7

Error subcode:

This 1-octet unsigned integer provides more specific
information about the nature of the reported error.  Each Error
Code may have one or more Error Subcodes associated with it.
If no appropriate Error Subcode is defined, then a zero
(Unspecific) value is used for the Error Subcode field.

Message Header Error subcodes:

1 - Connection Not Synchronized.
2 - Bad Message Length.
3 - Bad Message Type.

OPEN Message Error subcodes:

1 - Unsupported Version Number.
2 - Bad Peer AS.
3 - Bad BGP Identifier.
4 - Unsupported Optional Parameter.
5 - [Deprecated - see Appendix A].
6 - Unacceptable Hold Time.

UPDATE Message Error subcodes:

1 - Malformed Attribute List.
2 - Unrecognized Well-known Attribute.
3 - Missing Well-known Attribute.
4 - Attribute Flags Error.
5 - Attribute Length Error.
6 - Invalid ORIGIN Attribute.
7 - [Deprecated - see Appendix A].
8 - Invalid NEXT_HOP Attribute.
9 - Optional Attribute Error.
10 - Invalid Network Field.
11 - Malformed AS_PATH.

Data:

This variable-length field is used to diagnose the reason for
the NOTIFICATION.  The contents of the Data field depend upon
the Error Code and Error Subcode.  See Section 6 for more
details.

Note that the length of the Data field can be determined from
the message Length field by the formula:

Message Length = 21 + Data Length

The minimum length of the NOTIFICATION message is 21 octets
(including message header).

|#

(in-package :sbgp)

(defun BGP-NOTIFICATION-make (error-code error-subcode data)
  (list 'BGP-NOTIFICATION
	error-code
	error-subcode
	data))

(defun BGP-NOTIFICATION-get-error-code (obj)    "-> u8"     (cadr obj))
(defun BGP-NOTIFICATION-get-error-subcode (obj) "-> u8"     (caddr obj))
(defun BGP-NOTIFICATION-get-data (obj)          "-> BYTES"  (cadddr obj))

(defun BGP-NOTIFICATION-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (+ 2 (BYTES-get-io-rw-octets obj)))

(defun BGP-NOTIFICATION-io-read (message-length stream-in)
  (list 'BGP-NOTIFICATION
	(io-read-uNbe u8 stream-in)
	(io-read-uNbe u8 stream-in)
	(BYTES-io-read (- message-length 2) stream-in)))

(defun BGP-NOTIFICATION-io-write (obj stream-out)
  (destructuring-bind (code subcode data)
      (cdr obj)
    (io-write-uNbe u8 code stream-out)
    (io-write-uNbe u8 subcode stream-out)
    (BYTES-io-write data stream-out)))

(deftype BGP-NOTIFICATION () '(cons (member BGP-NOTIFICATION)))


(defun BGP-NOTIFICATION-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W #x~2,0X #x~2,0X ~/sbgp:bytes-pprint-1/)"
	  (TL-get-name obj)          ; symbol
	  (BGP-NOTIFICATION-get-error-code obj)    ; u8
	  (BGP-NOTIFICATION-get-error-subcode obj) ; u8
	  (BGP-NOTIFICATION-get-data obj)))        ; BYTES

(set-pprint-dispatch '(cons (member BGP-NOTIFICATION)) #'BGP-NOTIFICATION-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun BGP-NOTIFICATION-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W ~D ~D~) ~/sbgp:bytes-pprint-2/]"
	  (TL-get-name obj)          ; symbol
	  (BGP-NOTIFICATION-get-error-code obj)    ; u8
	  (BGP-NOTIFICATION-get-error-subcode obj) ; u8
	  (BGP-NOTIFICATION-get-data obj)))        ; BYTES

(set-pprint-dispatch '(cons (member BGP-NOTIFICATION)) #'BGP-NOTIFICATION-pprint-2 0 *sbgp-pprint-dispatch-table-2*)
  
