#|
RFC 4271
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
|#
#|
RFC 3392         Capabilities Advertisement with BGP-4     November 2002


4. Capabilities Optional Parameter (Parameter Type 2):

   This is an Optional Parameter that is used by a BGP speaker to convey
   to its BGP peer the list of capabilities supported by the speaker.

   The parameter contains one or more triples <Capability Code,
   Capability Length, Capability Value>, where each triple is encoded as
   shown below:

       +------------------------------+
       | Capability Code (1 octet)    |
       +------------------------------+
       | Capability Length (1 octet)  |
       +------------------------------+
       | Capability Value (variable)  |
       +------------------------------+

   The use and meaning of these fields are as follows:

      Capability Code:

         Capability Code is a one octet field that unambiguously
         identifies individual capabilities.

      Capability Length:

         Capability Length is a one octet field that contains the length
         of the Capability Value field in octets.

      Capability Value:

         Capability Value is a variable length field that is interpreted
         according to the value of the Capability Code field.

   BGP speakers SHOULD NOT include more than one instance of a
   capability with the same Capability Code, Capability Length, and
   Capability Value.  Note however, that processing of multiple
   instances of such capability does not require special handling, as
   additional instances do not change the meaning of announced
   capability.

   BGP speakers MAY include more than one instance of a capability (as
   identified by the Capability Code) with non-zero Capability Length
   field, but with different Capability Value, and either the same or
   different Capability Length.  Processing of these capability
   instances is specific to the Capability Code and MUST be described in
   the document introducing the new capability.
|#

#|
BGP Open Optional Parameter Types
0 	Reserved 	             [RFC5492]
1 	Authentication (deprecated)  [RFC4271][RFC5492]
2 	Capabilities 	             [RFC5492]
3-254 	Unassigned 	
255 	Extended Length 	     [RFC9072]
|#

(in-package :sbgp)

(defun OPEN-OPT-PARAM-make (bgp-cap-object)
  (list 'OPEN-OPT-PARAM
	2
	(BGP-CAP-get-io-rw-octets bgp-cap-object)
	bgp-cap-object))

(defun OPEN-OPT-PARAM-get-type (obj)   "-> u8"              (cadr obj))
(defun OPEN-OPT-PARAM-get-length (obj) "-> u8"              (caddr obj))
(defun OPEN-OPT-PARAM-get-value (obj)  "-> BGP-CAP"         (cadddr obj))

(defun OPEN-OPT-PARAM-get-io-rw-octets (obj)
  "Returns -> integer. Size of object in bytes when written to binary output stream."
  (+ 2 (OPEN-OPT-PARAM-get-length obj)))

(defun OPEN-OPT-PARAM-io-read (stream-in)
  (let* ((type (io-read-uNbe u8 stream-in))
	 (length (io-read-uNbe u8 stream-in))
	 (value (case type
		  (2 (BGP-CAP-io-read length stream-in))
		  (t (BYTES-io-read length stream-in)))))
    (list 'OPEN-OPT-PARAM type length value)))

(defun OPEN-OPT-PARAM-io-write (obj stream-out)
  (destructuring-bind (type length value)
      (cdr obj)
    (io-write-uNbe u8 type stream-out)
    (io-write-uNbe u8 length stream-out)
    (case type
	 (2 (BGP-CAP-io-write value stream-out))
	 (t (BYTES-io-write value stream-out)))))
    
(defun OPEN-OPT-PARAM-io-read-list (octets stream-in)
  (labels ((recurse (bytes-read)
	     (if (>= bytes-read octets)
		 nil
		 (let ((elem (OPEN-OPT-PARAM-io-read stream-in)))
		   (cons elem
		         (recurse (+ bytes-read (OPEN-OPT-PARAM-get-io-rw-octets elem))))))))
    (recurse 0)))

(deftype OPEN-OPT-PARAM () '(cons (member OPEN-OPT-PARAM)))

(defun OPEN-OPT-PARAM-pprint-1 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "(~W ~D ~D ~W)"
	  (TL-get-name obj)
	  (OPEN-OPT-PARAM-get-type obj)
	  (OPEN-OPT-PARAM-get-length obj)
	  (OPEN-OPT-PARAM-get-value obj)))

(set-pprint-dispatch 'OPEN-OPT-PARAM #'OPEN-OPT-PARAM-pprint-1 0 *sbgp-pprint-dispatch-table-1*)

(defun OPEN-OPT-PARAM-pprint-2 (port obj &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format port "~([~W ~D ~D~) ~W]"
	  (TL-get-name obj)
	  (OPEN-OPT-PARAM-get-type obj)
	  (OPEN-OPT-PARAM-get-length obj)
	  (OPEN-OPT-PARAM-get-value obj)))

(set-pprint-dispatch 'OPEN-OPT-PARAM #'OPEN-OPT-PARAM-pprint-2 0 *sbgp-pprint-dispatch-table-2*)

