(in-package :sbgp)

;;; MRT Common Header
#|
Header Field Descriptions:

Timestamp:

A 4-octet field whose integer value is the number of seconds,
excluding leap seconds, elapsed since midnight proleptic
Coordinated Universal Time (UTC).  This representation of time
is sometimes called "UNIX time" [POSIX].  This time format
cannot represent time values prior to January 1, 1970.  The
latest UTC time value that can be represented by a 4-octet
integer value is 03:14:07 on January 19, 2038, which is
represented by the hexadecimal value 7FFFFFFF.  Implementations
that wish to create MRT records after this date will need to
provide an alternate EPOCH time base for the Timestamp field.
Mechanisms for indicating this alternate EPOCH are currently
outside the scope of this document.

Type:

A 2-octet field that indicates the Type of information
contained in the Message field.  Types 0 through 4 are
informational messages pertaining to the state of an MRT
collector, while Types 5 and higher are used to convey routing
information.

Subtype:

A 2-octet field that is used to further distinguish message
information within a particular record Type.

Length:

A 4-octet message length field.  The Length field contains the
number of octets within the message.  The Length field does not
include the length of the MRT Common Header.

Message:

A variable-length message.  The contents of this field are
context dependent upon the Type and Subtype fields.

0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                           Timestamp                           |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|             Type              |            Subtype            |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                             Length                            |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                      Message... (variable)
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|#

(defun MRT-COMMON-HEADER-make (timestamp type subtype length)
  "Returns -> `(MRT-Common-Header timestamp[u32] type[u16] subtype[u16] length[u32])"
  (list 'MRT-COMMON-HEADER
	timestamp                  ; u32
	type                       ; u16
	subtype                    ; u16
	length))                   ; u32

(defun MRT-COMMON-HEADER-get-name (obj)      "-> symbol"  (car obj))
(defun MRT-COMMON-HEADER-get-timestamp (obj) "-> u32"     (cadr obj))
(defun MRT-COMMON-HEADER-get-type (obj)      "-> u16"     (caddr obj))
(defun MRT-COMMON-HEADER-get-subtype (obj)   "-> u16"     (cadddr obj))
(defun MRT-COMMON-HEADER-get-length (obj)    "-> u32"     (car (cdddr obj)))

(defun MRT-COMMON-HEADER-io-read (port)
  (MRT-COMMON-HEADER-make (io-read-uNbe u32 port)    ; u32
			  (io-read-uNbe u16 port)    ; u16
			  (io-read-uNbe u16 port)    ; u16
			  (io-read-uNbe u32 port)))  ; u32

(defun MRT-COMMON-HEADER-io-write (obj port)
  (destructuring-bind (timestamp type subtype length)
      (cdr obj)
    (io-write-uNbe u32 timestamp port)
    (io-write-uNbe u16 type port)
    (io-write-uNbe u16 subtype port)
    (io-write-uNbe u32 length port)))

(deftype MRT-COMMON-HEADER () '(cons (member MRT-COMMON-HEADER)))

;;; Extended Timestamp MRT Header
#|
Several MRT format record types support a variant type with an
extended timestamp field.  The purpose of this field is to support
measurements at sub-second resolutions.  This field, Microsecond
Timestamp, contains an unsigned 32BIT offset value in microseconds,
which is added to the Timestamp field value.  The Timestamp field
remains as defined in the MRT Common Header.  The Microsecond
Timestamp immediately follows the Length field in the MRT Common
Header and precedes all other fields in the message.  The Microsecond
Timestamp is included in the computation of the Length field value.
The Extended Timestamp MRT Header is illustrated below.


0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                           Timestamp                           |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|             Type              |            Subtype            |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                             Length                            |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                      Microsecond Timestamp                    |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                      Message... (variable)
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|#

(defun MRT-EXTENDED-TIMESTAMP-make (timestamp)
  (list 'MRT-EXTENDED-TIMESTAMP
	timestamp))                    ; u32

(defun MRT-EXTENDED-TIMESTAMP-get-name (obj)      "-> symbol"  (car obj))
(defun MRT-EXTENDED-TIMESTAMP-get-timestamp (obj) "-> u32" (cadr obj))

(defun MRT-EXTENDED-TIMESTAMP-io-read (port)
  (MRT-EXTENDED-TIMESTAMP-make (io-read-uNbe u32 port)))      ; u32

(defun MRT-EXTENDED-TIMESTAMP-io-write (obj port)
  (destructuring-bind (timestamp)
      (cdr obj)
    (io-write-uNbe u32 timestamp port)))

(deftype MRT-EXTENDED-TIMESTAMP () '(cons (member MRT-EXTENDED-TIMESTAMP)))

#|
4.  MRT Types

The following MRT Types are currently defined for the MRT format.
The MRT Types that contain the "_ET" suffix in their names identify
those types that use an Extended Timestamp MRT Header.  The Subtype
and Message fields in these types remain as defined for the MRT Types
of the same name without the "_ET" suffix.

11   OSPFv2
12   TABLE_DUMP
13   TABLE_DUMP_V2
16   BGP4MP
17   BGP4MP_ET
32   ISIS
33   ISIS_ET
48   OSPFv3
49   OSPFv3_ET
|#

;;; TABLE_DUMP_V2 Type

#|
4.3.  TABLE_DUMP_V2 Type

The TABLE_DUMP_V2 Type updates the TABLE_DUMP Type to include 4-byte
Autonomous System Number (ASN) support and full support for BGP
multiprotocol extensions.  It also improves upon the space efficiency
of the TABLE_DUMP Type by employing an index table for peers and
permitting a single MRT record per Network Layer Reachability
Information (NLRI) entry.  The following subtypes are used with the
TABLE_DUMP_V2 Type.

1    PEER_INDEX_TABLE
2    RIB_IPV4_UNICAST
3    RIB_IPV4_MULTICAST
4    RIB_IPV6_UNICAST
5    RIB_IPV6_MULTICAST
6    RIB_GENERIC
|#


;;; PEER_INDEX_TABLE Subtype

#|
4.3.1.  PEER_INDEX_TABLE Subtype

An initial PEER_INDEX_TABLE MRT record provides the BGP ID of the
collector, an OPTIONAL view name, and a list of indexed peers.
Following the PEER_INDEX_TABLE MRT record, a series of MRT records is
used to encode RIB table entries.  This series of MRT records uses
subtypes 2-6 and is separate from the PEER_INDEX_TABLE MRT record
itself and includes full MRT record headers.  The RIB entry MRT
records MUST immediately follow the PEER_INDEX_TABLE MRT record.

The header of the PEER_INDEX_TABLE Subtype is shown below.  The View
Name is OPTIONAL and, if not present, the View Name Length MUST be
set to 0.  The View Name encoding MUST follow the UTF-8
transformation format [RFC3629].

0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                      Collector BGP ID                         |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|       View Name Length        |     View Name (variable)      |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|          Peer Count           |    Peer Entries (variable)
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Figure 5: PEER_INDEX_TABLE Subtype

The format of the Peer Entries is shown below.  The PEER_INDEX_TABLE
record contains Peer Count number of Peer Entries.

0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|   Peer Type   |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         Peer BGP ID                           |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                   Peer IP Address (variable)                  |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                        Peer AS (variable)                     |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Figure 6: Peer Entries

The Peer Type, Peer BGP ID, Peer IP Address, and Peer AS fields are
repeated as indicated by the Peer Count field.  The position of the
peer in the PEER_INDEX_TABLE is used as an index in the subsequent
TABLE_DUMP_V2 MRT records.  The index number begins with 0.

The Peer Type field is a bit field that encodes the type of the AS
and IP address as identified by the A and I bits, respectively,
below.

0 1 2 3 4 5 6 7
+-+-+-+-+-+-+-+-+
| | | | | | |A|I|
+-+-+-+-+-+-+-+-+

Bit 6: Peer AS number size:  0 = 16 bits, 1 = 32 bits
Bit 7: Peer IP Address family:  0 = IPv4,  1 = IPv6

Figure 7: Peer Type Field

The MRT records that follow the PEER_INDEX_TABLE MRT record consist
of the subtypes listed below and contain the actual RIB table
entries.  They include a header that specifies a sequence number, an
NLRI field, and a count of the number of RIB entries contained within
the record.
|#

;;; MRT-PEER_INDEX_TABLE-Entry

(defun MRT-PEER-INDEX-TABLE-ENTRY-make (index-number peer-type peer-bgp-id peer-ip-address peer-as)
  (list `MRT-PEER-INDEX-TABLE-ENTRY
	index-number                ; integer - entries indexed from 0 in order as read from stream
	peer-type                   ; u8
	peer-bgp-id                 ; IPV4
	peer-ip-address             ; IPV4 | IPV6
	peer-as))                   ; u16 | u32

(defun MRT-PEER-INDEX-TABLE-ENTRY-get-name (obj)            "-> symbol"  (car obj))
(defun MRT-PEER-INDEX-TABLE-ENTRY-get-index-number (obj)    "-> integer"   (cadr obj))
(defun MRT-PEER-INDEX-TABLE-ENTRY-get-peer-type (obj)       "-> u8"        (caddr obj))
(defun MRT-PEER-INDEX-TABLE-ENTRY-get-peer-bgp-id (obj)     "-> IPV4"      (cadddr obj))
(defun MRT-PEER-INDEX-TABLE-ENTRY-get-peer-ip-address (obj) "-> IPV4 | IPV6" (car (cddddr obj)))
(defun MRT-PEER-INDEX-TABLE-ENTRY-get-peer-as (obj)         "-> u16 | u32"   (cadr (cddddr obj)))

(defun MRT-PEER-INDEX-TABLE-ENTRY-is-4-byte-asn-p (obj)
  (uN-bit-set-p u8 (MRT-PEER-INDEX-TABLE-ENTRY-get-peer-type obj) 6))

(defun MRT-PEER-INDEX-TABLE-ENTRY-is-ipv6-p (obj)
  (uN-bit-set-p u8 (MRT-PEER-INDEX-TABLE-ENTRY-get-peer-type obj) 7))

(defun MRT-PEER-INDEX-TABLE-ENTRY-io-read (index-number port)
  (let ((peer-type (io-read-uNbe u8 port)))
    (MRT-PEER-INDEX-TABLE-ENTRY-make
	  index-number
	  peer-type                                 ; u8
	  (IPV4-io-read port)                       ; IPV4
	  (if (uN-bit-set-p u8 peer-type 7)         ; IPV4 | IPV6
	      (IPV6-io-read port)
	      (IPV4-io-read port))
	  (if (uN-bit-set-p u8 peer-type 6)         ; u16 | u32 
	      (io-read-uNbe u32 port)
	      (io-read-uNbe u16 port)))))

(defun MRT-PEER-INDEX-TABLE-ENTRY-io-write (obj port)
  (destructuring-bind (peer-type peer-bgp-id peer-ip-address peer-as)
      (cddr obj)
    (io-write-uNbe u8 peer-type port)
    (IPV4-io-write peer-bgp-id port)
    (if (uN-bit-set-p u8 peer-type 7)               ; IPV4 | IPV6
	(IPV6-io-write peer-ip-address port)
	(IPV4-io-write peer-ip-address port))
    (if (uN-bit-set-p u8 peer-type 6)               ; u16 | u32
	(io-write-uNbe u32 peer-as port)
	(io-write-uNbe u16 peer-as port))))

(deftype MRT-PEER-INDEX-TABLE-ENTRY () '(cons (member MRT-PEER-INDEX-TABLE-ENTRY)))

;;; MRT-PEER_INDEX_TABLE

(defun MRT-PEER-INDEX-TABLE-make (collector-bgp-id view-name-length view-name peer-count peer-entries)
  (list `MRT-PEER-INDEX-TABLE
	collector-bgp-id                             ; IPV4
	view-name-length                             ; u16
	view-name                                    ; BYTES (view-name-length)
	peer-count                                   ; u16
	peer-entries))                               ; list of PEER-INDEX-TABLE-ENTRY

(defun MRT-PEER-INDEX-TABLE-get-name (obj)              "-> symbol"                          (car obj))
(defun MRT-PEER-INDEX-TABLE-get-collector-bgp-id (obj)  "-> IPV4"                            (cadr obj))
(defun MRT-PEER-INDEX-TABLE-get-view-name-length (obj)  "-> u16"                             (caddr obj))
(defun MRT-PEER-INDEX-TABLE-get-view-name (obj)         "-> BYTES (view-name-length)"        (cadddr obj))
(defun MRT-PEER-INDEX-TABLE-get-peer-count (obj)        "-> u16"                             (car (cddddr obj)))
(defun MRT-PEER-INDEX-TABLE-get-peer-entries (obj)      "-> list of PEER-INDEX-TABLE-ENTRY"  (cadr (cddddr obj)))

(defun MRT-PEER-INDEX-TABLE-io-read (port)
  (let* ((collector-bgp-id (IPV4-io-read port))      ; IPV4
	 (view-name-length (io-read-uNbe u16 port))  ; u16
	 (view-name (BYTES-io-read view-name-length
				   port))            ; BYTES (view-name-length)
	 (peer-count (io-read-uNbe u16 port))        ; u16
	 (peer-entries                               ; list of PEER-INDEX-TABLE-ENTRY
	   (labels ((recurse (index-number)
		      (if (< index-number peer-count)
			  (cons (MRT-PEER-INDEX-TABLE-ENTRY-io-read index-number port)
				(recurse (1+ index-number)))
			  nil)))
	     (recurse 0))))
    
    (MRT-PEER-INDEX-TABLE-make collector-bgp-id
			       view-name-length
			       view-name
			       peer-count
			       peer-entries)))

(defun MRT-PEER-INDEX-TABLE-io-write (obj port)
  (destructuring-bind (collector-bgp-id view-name-length view-name peer-count peer-entries)
      (cdr obj)
    (IPV4-io-write collector-bgp-id port)
    (io-write-uNbe u16 view-name-length port)
    (BYTES-io-write view-name port)
    (io-write-uNbe u16 peer-count port)
    (dolist (elem peer-entries) (MRT-PEER-INDEX-TABLE-ENTRY-io-write elem port))))

(deftype MRT-PEER-INDEX-TABLE () '(cons (member MRT-PEER-INDEX-TABLE)))

;;; RIB Entries

#|
4.3.4.  RIB Entries
The RIB Entries are repeated Entry Count times.  These entries share
a common format as shown below.  They include a Peer Index from the
PEER_INDEX_TABLE MRT record, an originated time for the RIB Entry,
and the BGP path attribute length and attributes.  All AS numbers in
the AS_PATH attribute MUST be encoded as 4-byte AS numbers.

0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|         Peer Index            |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         Originated Time                       |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|      Attribute Length         |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                    BGP Attributes... (variable)
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Figure 10: RIB Entries

TODO:
There is one exception to the encoding of BGP attributes for the BGP
MP_REACH_NLRI attribute (BGP Type Code 14) [RFC4760].  Since the AFI,
SAFI, and NLRI information is already encoded in the RIB Entry Header
or RIB_GENERIC Entry Header, only the Next Hop Address Length and
Next Hop Address fields are included.  The Reserved field is omitted.
The attribute length is also adjusted to reflect only the length of
the Next Hop Address Length and Next Hop Address fields.
|#

(defun MRT-RIB-ENTRY-make (peer-index originated-time attribute-length bgp-attributes)
  (list 'MRT-RIB-ENTRY
	peer-index                      ; u16
	originated-time                 ; u32
	attribute-length                ; u16
	bgp-attributes))                ; list of PATH-ATTRIB

(defun MRT-RIB-ENTRY-get-name (obj)              "-> symbol"               (car obj))
(defun MRT-RIB-ENTRY-get-peer-index (obj)        "-> u16"                  (cadr obj))
(defun MRT-RIB-ENTRY-get-originated-time (obj)   "-> u32"                  (caddr obj))
(defun MRT-RIB-ENTRY-get-attribute-length (obj)  "-> u16"                  (cadddr obj))
(defun MRT-RIB-ENTRY-get-bgp-attributes (obj)    "-> list of PATH-ATTRIB"  (car (cddddr obj)))

(defun MRT-RIB-ENTRY-io-read (port)
  (let* ((peer-index       (io-read-uNbe u16 port))
	 (originated-time  (io-read-uNbe u32 port))
	 (attribute-length (io-read-uNbe u16 port))
	 (bgp-attributes   (PATH-ATTRIB-io-read-list attribute-length
						     t
						     port)))
    (MRT-RIB-ENTRY-make peer-index
			originated-time
			attribute-length
			bgp-attributes)))

(defun MRT-RIB-ENTRY-io-write (obj port)
  (destructuring-bind (peer-index originated-time attribute-length bgp-attributes)
      (cdr obj)
    (io-write-uNbe u16 peer-index port)
    (io-write-uNbe u32 originated-time port)
    (io-write-uNbe u16 attribute-length port)
    (dolist (elem bgp-attributes)
      (PATH-ATTRIB-io-write t elem port))))

(deftype MRT-RIB-ENTRY () '(cons (member MRT-RIB-ENTRY)))

#|
4.3.2.  AFI/SAFI-Specific RIB Subtypes
AFI/SAFI-Specific RIB Subtypes
2    RIB_IPV4_UNICAST
3    RIB_IPV4_MULTICAST
4    RIB_IPV6_UNICAST
5    RIB_IPV6_MULTICAST

The AFI/SAFI-specific RIB Subtypes consist of the RIB_IPV4_UNICAST,
RIB_IPV4_MULTICAST, RIB_IPV6_UNICAST, and RIB_IPV6_MULTICAST
Subtypes.  These specific RIB table entries are given their own MRT
TABLE_DUMP_V2 subtypes as they are the most common type of RIB table
instances, and providing specific MRT subtypes for them permits more
compact encodings.  These subtypes permit a single MRT record to
encode multiple RIB table entries for a single prefix.  The Prefix
Length and Prefix fields are encoded in the same manner as the BGP
NLRI encoding for IPv4 and IPv6 prefixes.  Namely, the Prefix field
contains address prefixes followed by enough trailing bits to make
the end of the field fall on an octet boundary.  The value of
trailing bits is irrelevant.

0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         Sequence Number                       |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
| Prefix Length |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                        Prefix (variable)                      |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|         Entry Count           |  RIB Entries (variable)
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|#

(defun MRT-RIB-make (af-type sequence-number nlri entry-count rib-entries)
  (list af-type            ; symbol { MRT-RIB-IPV4-UNICAST | MRT-RIB-IPV4-MULTICAST | MRT-RIB-IPV6-UNICAST | MRT-RIB-IPV6-MULTICAST  }
	sequence-number    ; u32
	nlri               ; NLRI
	entry-count        ; u16
	rib-entries))      ; list of MRT-RIB-ENTRY

(defun MRT-RIB-get-af-type (obj)     "-> symbol { MRT-RIB-IPV4-UNICAST | MRT-RIB-IPV4-MULTICAST | MRT-RIB-IPV6-UNICAST | MRT-RIB-IPV6-MULTICAST  }" (car obj))
(defun MRT-RIB-get-sequence-number (obj) "-> u32"                    (cadr obj))
(defun MRT-RIB-get-nlri (obj)            "-> NLRI"                   (caddr obj))
(defun MRT-RIB-get-entry-count (obj)     "-> u16"                    (cadddr obj))
(defun MRT-RIB-get-rib-entries (obj)     "-> list of MRT-RIB-ENTRY"  (car (cddddr obj)))

(defun MRT-RIB-io-read (af-type port)
  "af-type is symbol RIB-IPV4-UNICAST | RIB-IPV4-MULTICAST | RIB-IPV6-UNICAST | RIB-IPV6-MULTICAST"
  (let* ((sequence-number  (io-read-uNbe u32 port))
	 (nlri             (case af-type
			     (MRT-RIB-IPV4-UNICAST   (NLRI-io-read +AFISAFI-ipv4-unicast+ port))
			     (MRT-RIB-IPV4-MULTICAST (NLRI-io-read +AFISAFI-ipv4-multicast+ port))
			     (MRT-RIB-IPV6-UNICAST   (NLRI-io-read +AFISAFI-ipv6-unicast+ port))
			     (MRT-RIB-IPV6-MULTICAST (NLRI-io-read +AFISAFI-ipv6-multicast+ port))))
	 (entry-count      (io-read-uNbe u16 port))
	 (rib-entries      (loop repeat entry-count
				 collect (MRT-RIB-ENTRY-io-read port))))
    (MRT-RIB-make af-type
		  sequence-number   
		  nlri
		  entry-count       
		  rib-entries)))

(defun MRT-RIB-io-write (obj port)
  (destructuring-bind (sequence-number nlri entry-count rib-entries)
      (cdr obj)
    (io-write-uNbe u32 sequence-number port)
    (NLRI-io-write nlri port)
    (io-write-uNbe u16 entry-count port)
    (dolist (elem rib-entries) (MRT-RIB-ENTRY-io-write elem port))))

(deftype MRT-RIB () '(cons (member MRT-RIB)))

;;; RIB_GENERIC Subtype

#|
4.3.3.  RIB_GENERIC Subtype
The RIB_GENERIC header is shown below.  It is used to cover RIB
entries that do not fall under the common case entries defined above.
It consists of an AFI, Subsequent AFI (SAFI), and a single NLRI
entry.  The NLRI information is specific to the AFI and SAFI values.
An implementation that does not recognize particular AFI and SAFI
values SHOULD discard the remainder of the MRT record.

0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         Sequence Number                       |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|    Address Family Identifier  |Subsequent AFI |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|     Network Layer Reachability Information (variable)         |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|         Entry Count           |  RIB Entries (variable)
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|#

(defun MRT-RIB-GENERIC-make (sequence-number afi safi nlri entry-count rib-entries)
  (list `MRT-RIB-GENERIC
	sequence-number         ; u32
	afi                     ; u16
	safi                    ; u8
	nlri                    ; NLRI
	entry-count             ; u16
	rib-entries))           ; list of MRT-RIB-ENTRY

(defun MRT-RIB-GENERIC-get-name (obj)             "-> symbol"                 (car obj))
(defun MRT-RIB-GENERIC-get-sequence-number (obj)  "-> u32"                    (cadr obj))
(defun MRT-RIB-GENERIC-get-afi (obj)              "-> u16"                    (caddr obj))
(defun MRT-RIB-GENERIC-get-safi (obj)             "-> u8"                     (cadddr obj))
(defun MRT-RIB-GENERIC-get-nlri (obj)             "-> NLRI"                   (car (cddddr obj)))
(defun MRT-RIB-GENERIC-get-entry-count (obj)      "-> u16"                    (cadr (cddddr obj)))
(defun MRT-RIB-GENERIC-get-rib-entries  (obj)     "-> list of MRT-RIB-ENTRY"  (caddr (cddddr obj)))

(defun MRT-RIB-GENERIC-io-read (port)
  (let* ((sequence-number  (io-read-uNbe u32 port))
	 (afi              (io-read-uNbe u16 port))
	 (safi             (io-read-uNbe u8 port))
	 (nlri             (NLRI-io-read (AFISAFI-make afi safi) port))
	 (entry-count      (io-read-uNbe u16 port))
	 (rib-entries      (loop repeat entry-count
				 collect (MRT-RIB-ENTRY-io-read port))))
    (MRT-RIB-GENERIC-make sequence-number
			  afi        
			  safi       
			  nlri       
			  entry-count
			  rib-entries)))

(defun MRT-RIB-GENERIC-io-write (obj port)
  (destructuring-bind (sequence-number afi safi nlri entry-count rib-entries)
      (cdr obj)
    (io-write-uNbe u32 sequence-number port)
    (io-write-uNbe u16 afi port)
    (io-write-uNbe u8 safi port)
    (NLRI-io-write nlri port)
    (io-write-uNbe u16 entry-count port)
    (dolist (elem rib-entries) (MRT-RIB-ENTRY-io-write elem port))))

(deftype MRT-RIB-GENERIC () '(cons (member MRT-RIB-GENERIC)))

#|
4.4.  BGP4MP Type

This type was initially defined in the Zebra software package for the
BGP protocol with multiprotocol extension support as defined by RFC
4760 [RFC4760].  The BGP4MP Type has six Subtypes, which are defined
as follows:

0    BGP4MP_STATE_CHANGE
1    BGP4MP_MESSAGE
4    BGP4MP_MESSAGE_AS4
5    BGP4MP_STATE_CHANGE_AS4
6    BGP4MP_MESSAGE_LOCAL
7    BGP4MP_MESSAGE_AS4_LOCAL
|#



#|
4.4.1.  BGP4MP_STATE_CHANGE Subtype

This message is used to encode state changes in the BGP finite state
machine (FSM).  The BGP FSM states are encoded in the Old State and
New State fields to indicate the previous and current state.  In some
cases, the Peer AS Number may be undefined.  In such cases, the value
of this field MAY be set to zero.  The format is illustrated below:

0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|         Peer AS Number        |        Local AS Number        |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|        Interface Index        |        Address Family         |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                      Peer IP Address (variable)               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                      Local IP Address (variable)              |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|            Old State          |          New State            |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Figure 11: BGP4MP_STATE_CHANGE Subtype

The FSM states are defined in RFC 4271 [RFC4271], Section 8.2.2.
Both the Old State value and the New State value are encoded as
2-octet numbers.  The state values are defined numerically as
follows:

1    Idle
2    Connect
3    Active
4    OpenSent
5    OpenConfirm
6    Established

The BGP4MP_STATE_CHANGE message also includes Interface Index and
Address Family fields.  The Interface Index provides the interface
number of the peering session.  The index value is OPTIONAL and MAY
be zero if unknown or unsupported.  The Address Family indicates what
types of addresses are in the address fields.  At present, the
following AFI Types are supported:

1    AFI_IPv4
2    AFI_IPv6
|#

(defun MRT-BGP4MP-STATE-CHANGE-make (peer-as-number local-as-number interface-index address-family peer-ip-address local-ip-address old-state new-state)
  (list 'MRT-BGP4MP-STATE-CHANGE
	peer-as-number    ; u16    
	local-as-number   ; u16 
	interface-index   ; u16 
	address-family    ; u16 
	peer-ip-address   ; IPV4 | IPV6 
	local-ip-address  ; IPV4 | IPV6   
	old-state         ; u16 	  
	new-state))       ; u16

(defun MRT-BGP4MP-STATE-CHANGE-get-name (obj)              "-> symbol"       (car obj))
(defun MRT-BGP4MP-STATE-CHANGE-get-peer-as-number (obj)    "-> u16"          (cadr obj))    
(defun MRT-BGP4MP-STATE-CHANGE-get-local-as-number (obj)   "-> u16"          (caddr obj))
(defun MRT-BGP4MP-STATE-CHANGE-get-interface-index (obj)   "-> u16"          (cadddr obj)) 
(defun MRT-BGP4MP-STATE-CHANGE-get-address-family (obj)    "-> u16"          (car (cddddr obj)))
(defun MRT-BGP4MP-STATE-CHANGE-get-peer-ip-address (obj)   "-> IPV4 | IPV6"  (cadr (cddddr obj)))
(defun MRT-BGP4MP-STATE-CHANGE-get-local-ip-address (obj)  "-> IPV4 | IPV6"  (caddr (cddddr obj)))
(defun MRT-BGP4MP-STATE-CHANGE-get-old-state (obj)         "-> u16"          (cadddr (cddddr obj))) 	  
(defun MRT-BGP4MP-STATE-CHANGE-get-new-state (obj)         "-> u16"          (car (cddddr (cddddr obj))))

(defun MRT-BGP4MP-STATE-CHANGE-io-read (port)
  (let* ((peer-as-number     (io-read-uNbe u16 port))
	 (local-as-number    (io-read-uNbe u16 port))
	 (interface-index    (io-read-uNbe u16 port))
	 (address-family     (io-read-uNbe u16 port))
	 (peer-ip-address    (if (= address-family 1)
				 (IPV4-io-read port)
				 (IPV6-io-read port)))
	 (local-ip-address   (if (= address-family 1)
				 (IPV4-io-read port)
				 (IPV6-io-read port)))
	 (old-state          (io-read-uNbe u16 port))
	 (new-state          (io-read-uNbe u16 port)))

    (MRT-BGP4MP-STATE-CHANGE-make peer-as-number     
				  local-as-number    
				  interface-index    
				  address-family     
				  peer-ip-address    
				  local-ip-address   
				  old-state          	  
				  new-state)))

(defun MRT-BGP4MP-STATE-CHANGE-io-write (obj port)
  (destructuring-bind (peer-as-number local-as-number interface-index address-family peer-ip-address local-ip-address old-state new-state)
      (cdr obj)
    (io-write-uNbe u16 peer-as-number port)
    (io-write-uNbe u16 local-as-number port)
    (io-write-uNbe u16 interface-index port)
    (io-write-uNbe u16 address-family port)
    (if (= address-family 1)
	(IPV4-io-write peer-ip-address port)
	(IPV6-io-write peer-ip-address port))
    (if (= address-family 1)
	(IPV4-io-write local-ip-address port)
	(IPV6-io-write local-ip-address port))
    (io-write-uNbe u16 old-state port)
    (io-write-uNbe u16 new-state port)))

(deftype MRT-BGP4MP-STATE-CHANGE () '(cons (member MRT-BGP4MP-STATE-CHANGE)))

#|
4.4.2.  BGP4MP_MESSAGE Subtype

This subtype is used to encode BGP messages.  It can be used to
encode any Type of BGP message.  The entire BGP message is
encapsulated in the BGP Message field, including the 16-octet marker,
the 2-octet length, and the 1-octet type fields.  The BGP4MP_MESSAGE
Subtype does not support 4-byte AS numbers.  The AS_PATH contained in
these messages MUST only consist of 2-byte AS numbers.  The
BGP4MP_MESSAGE_AS4 Subtype updates the BGP4MP_MESSAGE Subtype in
order to support 4-byte AS numbers.  The BGP4MP_MESSAGE fields are
shown below:

0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|         Peer AS Number        |        Local AS Number        |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|        Interface Index        |        Address Family         |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                      Peer IP Address (variable)               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                      Local IP Address (variable)              |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                    BGP Message... (variable)
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Figure 12: BGP4MP_MESSAGE Subtype

The Interface Index provides the interface number of the peering
session.  The index value is OPTIONAL and MAY be zero if unknown or
unsupported.  The Address Family indicates what types of addresses
are in the subsequent address fields.  At present, the following AFI
Types are supported:

1    AFI_IPv4
2    AFI_IPv6

The Address Family value only applies to the IP addresses contained
in the MRT header.  The BGP4MP_MESSAGE Subtype is otherwise
transparent to the contents of the actual message that may contain
any valid AFI/SAFI values.  Only one BGP message SHALL be encoded in
the BGP4MP_MESSAGE Subtype.
|#


(defun MRT-BGP4MP-MESSAGE-make (peer-as-number local-as-number interface-index address-family peer-ip-address local-ip-address bgp-message)
  (list 'MRT-BGP4MP-MESSAGE
	peer-as-number        ; u16     
	local-as-number       ; u16
	interface-index       ; u16
	address-family        ; u16
	peer-ip-address       ; IPV4 | IPV6
	local-ip-address      ; IPV4 | IPV6
	bgp-message))         ; BGP-MESSAGE

(defun MRT-BGP4MP-MESSAGE-get-name (obj)             "-> symbol"       (car obj))
(defun MRT-BGP4MP-MESSAGE-get-peer-as-number (obj)   "-> u16"          (cadr obj))    
(defun MRT-BGP4MP-MESSAGE-get-local-as-number (obj)  "-> u16"          (caddr obj))
(defun MRT-BGP4MP-MESSAGE-get-interface-index (obj)  "-> u16"          (cadddr obj))
(defun MRT-BGP4MP-MESSAGE-get-address-family (obj)   "-> u16"          (car (cddddr obj)))
(defun MRT-BGP4MP-MESSAGE-get-peer-ip-address (obj)  "-> IPV4 | IPV6"  (cadr (cddddr obj)))
(defun MRT-BGP4MP-MESSAGE-get-local-ip-address (obj) "-> IPV4 | IPV6"  (caddr (cddddr obj)))
(defun MRT-BGP4MP-MESSAGE-get-bgp-message (obj)      "-> BGP-MESSAGE"  (cadddr (cddddr obj)))

(defun MRT-BGP4MP-MESSAGE-io-read (port)
  (let* ((peer-as-number     (io-read-uNbe u16 port))
	 (local-as-number    (io-read-uNbe u16 port))
	 (interface-index    (io-read-uNbe u16 port))
	 (address-family     (io-read-uNbe u16 port))
	 (peer-ip-address    (if (= address-family 1)
				 (IPV4-io-read port)
				 (IPV6-io-read port)))
	 (local-ip-address   (if (= address-family 1)
				 (IPV4-io-read port)
				 (IPV6-io-read port)))
	 (bgp-message        (BGP-MESSAGE-io-read nil port)))
    
    (MRT-BGP4MP-MESSAGE-make peer-as-number     
			     local-as-number    
			     interface-index    
			     address-family     
			     peer-ip-address    
			     local-ip-address 
			     bgp-message)))

(defun MRT-BGP4MP-MESSAGE-io-write (obj port)
  (destructuring-bind (peer-as-number local-as-number interface-index address-family peer-ip-address local-ip-address bgp-message)
      (cdr obj)
    (io-write-uNbe u16 peer-as-number port)
    (io-write-uNbe u16 local-as-number port)
    (io-write-uNbe u16 interface-index port)
    (io-write-uNbe u16 address-family port)
    (if (= address-family 1)
	(IPV4-io-write peer-ip-address port)
	(IPV6-io-write peer-ip-address port))
    (if (= address-family 1)
	(IPV4-io-write local-ip-address port)
	(IPV6-io-write local-ip-address port))
    (BGP-MESSAGE-io-write '() bgp-message port)))

(deftype MRT-BGP4MP-MESSAGE () '(cons (member MRT-BGP4MP-MESSAGE)))

#|
4.4.3.  BGP4MP_MESSAGE_AS4 Subtype

This subtype updates the BGP4MP_MESSAGE Subtype to support 4-byte AS
numbers.  The BGP4MP_MESSAGE_AS4 Subtype is otherwise identical to
the BGP4MP_MESSAGE Subtype.  The AS_PATH in these messages MUST only
consist of 4-byte AS numbers.  The BGP4MP_MESSAGE_AS4 fields are
shown below:

0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         Peer AS Number                        |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         Local AS Number                       |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|        Interface Index        |        Address Family         |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                      Peer IP Address (variable)               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                      Local IP Address (variable)              |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                    BGP Message... (variable)
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Figure 13: BGP4MP_MESSAGE_AS4 Subtype
|#

(defun MRT-BGP4MP-MESSAGE-AS4-make (peer-as-number local-as-number interface-index address-family peer-ip-address local-ip-address bgp-message)
  (list 'MRT-BGP4MP-MESSAGE-AS4
	peer-as-number     ; u32
	local-as-number    ; u32
	interface-index    ; u16
	address-family     ; u16
	peer-ip-address    ; IPV4 | IPV6
	local-ip-address   ; IPV4 | IPV6
	bgp-message))      ; BGP-MESSAGE

(defun MRT-BGP4MP-MESSAGE-AS4-get-name (obj)              "-> symbol"  (car obj))
(defun MRT-BGP4MP-MESSAGE-AS4-get-peer-as-number (obj)    "-> u32"          (cadr obj))
(defun MRT-BGP4MP-MESSAGE-AS4-get-local-as-number (obj)   "-> u32"          (caddr obj))
(defun MRT-BGP4MP-MESSAGE-AS4-get-interface-index (obj)   "-> u16"          (cadddr obj))
(defun MRT-BGP4MP-MESSAGE-AS4-get-address-family (obj)    "-> u16"          (car (cddddr obj)))
(defun MRT-BGP4MP-MESSAGE-AS4-get-peer-ip-address (obj)   "-> IPV4 | IPV6"  (cadr (cddddr obj)))
(defun MRT-BGP4MP-MESSAGE-AS4-get-local-ip-address (obj)  "-> IPV4 | IPV6"  (caddr (cddddr obj)))
(defun MRT-BGP4MP-MESSAGE-AS4-get-bgp-message (obj)       "-> BGP-MESSAGE"  (cadddr (cddddr obj)))

(defun MRT-BGP4MP-MESSAGE-AS4-io-read (port)
  (let* ((peer-as-number     (io-read-uNbe u32 port))
	 (local-as-number    (io-read-uNbe u32 port))
	 (interface-index    (io-read-uNbe u16 port))
	 (address-family     (io-read-uNbe u16 port))
	 (peer-ip-address    (if (= address-family 1)
				 (IPV4-io-read port)
				 (IPV6-io-read port)))
	 (local-ip-address   (if (= address-family 1)
				 (IPV4-io-read port)
				 (IPV6-io-read port)))
	 (bgp-message        (BGP-MESSAGE-io-read t port)))

    (MRT-BGP4MP-MESSAGE-AS4-make peer-as-number
				 local-as-number
				 interface-index
				 address-family
				 peer-ip-address
				 local-ip-address
				 bgp-message)))

(defun MRT-BGP4MP-MESSAGE-AS4-io-write (obj port)
  (destructuring-bind (peer-as-number local-as-number interface-index address-family peer-ip-address local-ip-address bgp-message)
      (cdr obj)
    (io-write-uNbe u32 peer-as-number port)
    (io-write-uNbe u32 local-as-number port)
    (io-write-uNbe u16 interface-index port)
    (io-write-uNbe u16 address-family port)
    (if (= address-family 1)
	(IPV4-io-write peer-ip-address port)
	(IPV6-io-write peer-ip-address port))
    (if (= address-family 1)
	(IPV4-io-write local-ip-address port)
	(IPV6-io-write local-ip-address port))
    (BGP-MESSAGE-io-write t bgp-message port)))

(deftype MRT-BGP4MP-MESSAGE-AS4 () '(cons (member MRT-BGP4MP-MESSAGE-AS4)))

#|
4.4.4.  BGP4MP_STATE_CHANGE_AS4 Subtype

This subtype updates the BGP4MP_STATE_CHANGE Subtype to support
4-byte AS numbers.  As with the BGP4MP_STATE_CHANGE Subtype, the BGP
FSM states are encoded in the Old State and New State fields to
indicate the previous and current state.  Aside from the extension of
the Peer and Local AS Number fields to 4 bytes, this subtype is
otherwise identical to the BGP4MP_STATE_CHANGE Subtype.  The
BGP4MP_STATE_CHANGE_AS4 fields are shown below:

0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         Peer AS Number                        |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                         Local AS Number                       |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|        Interface Index        |        Address Family         |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                      Peer IP Address (variable)               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                      Local IP Address (variable)              |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|            Old State          |          New State            |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

Figure 14: BGP4MP_STATE_CHANGE_AS4 Subtype
|#

(defun MRT-BGP4MP-STATE-CHANGE-AS4-make (peer-as-number local-as-number interface-index address-family peer-ip-address local-ip-address old-state new-state)
  (list 'MRT-BGP4MP-STATE-CHANGE-AS4
	peer-as-number           ; u32
	local-as-number          ; u32
	interface-index          ; u16
	address-family           ; u16
	peer-ip-address          ; IPV4 | IPV6
	local-ip-address         ; IPV4 | IPV6
	old-state                ; u16
	new-state))              ; u16

(defun MRT-BGP4MP-STATE-CHANGE-AS4-get-name (obj)              "-> symbol"  (car obj))
(defun MRT-BGP4MP-STATE-CHANGE-AS4-get-peer-as-number (obj)    "-> u32"          (cadr obj))
(defun MRT-BGP4MP-STATE-CHANGE-AS4-get-local-as-number (obj)   "-> u32"          (caddr obj))
(defun MRT-BGP4MP-STATE-CHANGE-AS4-get-interface-index (obj)   "-> u16"          (cadddr obj))
(defun MRT-BGP4MP-STATE-CHANGE-AS4-get-address-family (obj)    "-> u16"          (car (cddddr obj)))
(defun MRT-BGP4MP-STATE-CHANGE-AS4-get-peer-ip-address (obj)   "-> IPV4 | IPV6"  (cadr (cddddr obj)))
(defun MRT-BGP4MP-STATE-CHANGE-AS4-get-local-ip-address (obj)  "-> IPV4 | IPV6"  (caddr (cddddr obj)))
(defun MRT-BGP4MP-STATE-CHANGE-AS4-get-old-state (obj)         "-> u16"          (cadddr (cddddr obj)))
(defun MRT-BGP4MP-STATE-CHANGE-AS4-get-new-state (obj)         "-> u16"          (car (cddddr (cddddr obj))))

(defun MRT-BGP4MP-STATE-CHANGE-AS4-io-read (port)
  (let* ((peer-as-number     (io-read-uNbe u32 port))
	 (local-as-number    (io-read-uNbe u32 port))
	 (interface-index    (io-read-uNbe u16 port))
	 (address-family     (io-read-uNbe u16 port))
	 (peer-ip-address    (if (= address-family 1)
				 (IPV4-io-read port)
				 (IPV6-io-read port)))
	 (local-ip-address   (if (= address-family 1)
				 (IPV4-io-read port)
				 (IPV6-io-read port)))
	 (old-state          (io-read-uNbe u16 port))
	 (new-state          (io-read-uNbe u16 port)))

    (MRT-BGP4MP-STATE-CHANGE-AS4-make peer-as-number     
				      local-as-number    
				      interface-index    
				      address-family     
				      peer-ip-address    
				      local-ip-address 
				      old-state
				      new-state)))

(defun MRT-BGP4MP-STATE-CHANGE-AS4-io-write (obj port)
  (destructuring-bind (peer-as-number local-as-number interface-index address-family peer-ip-address local-ip-address old-state new-state)
      (cdr obj)
    (io-write-uNbe u32 peer-as-number port)
    (io-write-uNbe u32 local-as-number port)
    (io-write-uNbe u16 interface-index port)
    (io-write-uNbe u16 address-family port)
    (if (= address-family 1)
	(IPV4-io-write peer-ip-address port)
	(IPV6-io-write peer-ip-address port))
    (if (= address-family 1)
	(IPV4-io-write local-ip-address port)
	(IPV6-io-write local-ip-address port))
    (io-write-uNbe u16 old-state port)
    (io-write-uNbe u16 new-state port)))

(deftype MRT-BGP4MP-STATE-CHANGE-AS4 () '(cons (member MRT-BGP4MP-STATE-CHANGE-AS4)))

#|
4.4.5.  BGP4MP_MESSAGE_LOCAL Subtype

Implementations of MRT have largely focused on collecting remotely
generated BGP messages in a passive route collector role.  However,
for active BGP implementations, it can be useful to archive locally
generated BGP messages in addition to remote messages.  This subtype
is added to indicate a locally generated BGP message.  The fields
remain identical to the BGP4MP_MESSAGE type including the Peer and
Local IP and AS fields.  The Local fields continue to refer to the
local IP and AS number of the collector that generated the BGP
message, and the Peer IP and AS fields refer to the recipient of the
generated BGP messages.
|#


#|
4.4.6.  BGP4MP_MESSAGE_AS4_LOCAL Subtype

As with the BGP4MP_MESSAGE_LOCAL type, this type indicates locally
generated messages.  The fields are identical to the
BGP4MP_MESSAGE_AS4 message type.
|#

;;; BGP4MP_MESSAGE_LOCAL

(defun MRT-BGP4MP-MESSAGE-LOCAL-make (peer-as-number local-as-number interface-index address-family peer-ip-address local-ip-address bgp-message)
  (list 'MRT-BGP4MP-MESSAGE-LOCAL
	peer-as-number          ; u16    
	local-as-number         ; u16
	interface-index         ; u16
	address-family          ; u16
	peer-ip-address         ; IPV4 | IPV6
	local-ip-address        ; IPV4 | IPV6
	bgp-message))           ; BGP-MESSAGE

(defun MRT-BGP4MP-MESSAGE-LOCAL-get-name (obj)              "-> symbol"       (car obj))
(defun MRT-BGP4MP-MESSAGE-LOCAL-get-peer-as-number (obj)    "-> u16"          (cadr obj))    
(defun MRT-BGP4MP-MESSAGE-LOCAL-get-local-as-number (obj)   "-> u16"          (caddr obj))
(defun MRT-BGP4MP-MESSAGE-LOCAL-get-interface-index (obj)   "-> u16"          (cadddr obj))
(defun MRT-BGP4MP-MESSAGE-LOCAL-get-address-family (obj)    "-> u16"          (car (cddddr obj)))
(defun MRT-BGP4MP-MESSAGE-LOCAL-get-peer-ip-address (obj)   "-> IPV4 | IPV6"  (cadr (cddddr obj)))
(defun MRT-BGP4MP-MESSAGE-LOCAL-get-local-ip-address (obj)  "-> IPV4 | IPV6"  (caddr (cddddr obj)))
(defun MRT-BGP4MP-MESSAGE-LOCAL-get-bgp-message (obj)       "-> BGP-MESSAGE"  (cadddr (cddddr obj)))

(defun  MRT-BGP4MP-MESSAGE-LOCAL-io-read (port)
  (let* ((peer-as-number    (io-read-uNbe u16 port))
	 (local-as-number   (io-read-uNbe u16 port))
	 (interface-index   (io-read-uNbe u16 port))
	 (address-family    (io-read-uNbe u16 port))
	 (peer-ip-address   (if (= address-family 1)
			         (IPV4-io-read port)
				 (IPV6-io-read port)))
	 (local-ip-address  (if (= address-family 1)
				 (IPV4-io-read port)
				 (IPV6-io-read port)))
	 (bgp-message       (BGP-MESSAGE-io-read nil port)))
    
    (MRT-BGP4MP-MESSAGE-LOCAL-make peer-as-number     
				   local-as-number    
				   interface-index    
				   address-family     
				   peer-ip-address    
				   local-ip-address 
				   bgp-message)))

(defun MRT-BGP4MP-MESSAGE-LOCAL-io-write (obj port)
  (destructuring-bind (peer-as-number local-as-number interface-index address-family peer-ip-address local-ip-address bgp-message)
      (cdr obj)
    (io-write-uNbe u16 peer-as-number port)
    (io-write-uNbe u16 local-as-number port)
    (io-write-uNbe u16 interface-index port)
    (io-write-uNbe u16 address-family port)
    (if (= address-family 1)
	(IPV4-io-write peer-ip-address port)
	(IPV6-io-write peer-ip-address port))
    (if (= address-family 1)
	(IPV4-io-write local-ip-address port)
	(IPV6-io-write local-ip-address port))
    (BGP-MESSAGE-io-write '() bgp-message port)))

(deftype MRT-BGP4MP-MESSAGE-LOCAL () '(cons (member MRT-BGP4MP-MESSAGE-LOCAL)))

;;; BGP4MP_MESSAGE_AS4_LOCAL

(defun MRT-BGP4MP-MESSAGE-AS4-LOCAL-make (peer-as-number local-as-number interface-index address-family peer-ip-address local-ip-address bgp-message)
  (list 'MRT-BGP4MP-MESSAGE-AS4-LOCAL
	peer-as-number          ; u32    
	local-as-number         ; u32
	interface-index         ; u16
	address-family          ; u16
	peer-ip-address         ; IPV4 | IPV6
	local-ip-address        ; IPV4 | IPV6
	bgp-message))           ; BGP-MESSAGE

(defun MRT-BGP4MP-MESSAGE-AS4-LOCAL-get-name (obj)              "-> symbol"  (car obj))
(defun MRT-BGP4MP-MESSAGE-AS4-LOCAL-get-peer-as-number (obj)    "-> u32"          (cadr obj))    
(defun MRT-BGP4MP-MESSAGE-AS4-LOCAL-get-local-as-number (obj)   "-> u32"          (caddr obj))
(defun MRT-BGP4MP-MESSAGE-AS4-LOCAL-get-interface-index (obj)   "-> u16"          (cadddr obj))
(defun MRT-BGP4MP-MESSAGE-AS4-LOCAL-get-address-family (obj)    "-> u16"          (car (cddddr obj)))
(defun MRT-BGP4MP-MESSAGE-AS4-LOCAL-get-peer-ip-address (obj)   "-> IPV4 | IPV6"  (cadr (cddddr obj)))
(defun MRT-BGP4MP-MESSAGE-AS4-LOCAL-get-local-ip-address (obj)  "-> IPV4 | IPV6"  (caddr (cddddr obj)))
(defun MRT-BGP4MP-MESSAGE-AS4-LOCAL-get-bgp-message (obj)       "-> BGP-MESSAGE"  (cadddr (cddddr obj)))

(defun  MRT-BGP4MP-MESSAGE-AS4-LOCAL-io-read (port)
  (let* ((peer-as-number    (io-read-uNbe u32 port))
	 (local-as-number   (io-read-uNbe u32 port))
	 (interface-index   (io-read-uNbe u16 port))
	 (address-family    (io-read-uNbe u16 port))
	 (peer-ip-address   (if (= address-family 1)
			         (IPV4-io-read port)
				 (IPV6-io-read port)))
	 (local-ip-address  (if (= address-family 1)
				 (IPV4-io-read port)
				 (IPV6-io-read port)))
	 (bgp-message       (BGP-MESSAGE-io-read t port)))
    
    (MRT-BGP4MP-MESSAGE-AS4-LOCAL-make peer-as-number     
				       local-as-number    
				       interface-index    
				       address-family     
				       peer-ip-address    
				       local-ip-address 
				       bgp-message)))

(defun MRT-BGP4MP-MESSAGE-AS4-LOCAL-io-write (obj port)
  (destructuring-bind (peer-as-number local-as-number interface-index address-family peer-ip-address local-ip-address bgp-message)
      (cdr obj)
    (io-write-uNbe u32 peer-as-number port)
    (io-write-uNbe u32 local-as-number port)
    (io-write-uNbe u16 interface-index port)
    (io-write-uNbe u16 address-family port)
    (if (= address-family 1)
	(IPV4-io-write peer-ip-address port)
	(IPV6-io-write peer-ip-address port))
    (if (= address-family 1)
	(IPV4-io-write local-ip-address port)
	(IPV6-io-write local-ip-address port))
    (BGP-MESSAGE-io-write t bgp-message port)))

(deftype MRT-BGP4MP-MESSAGE-AS4-LOCAL () '(cons (member MRT-BGP4MP-MESSAGE-AS4-LOCAL)))

;;; MRT-MESSAGE

(defun MRT-MESSAGE-make (header extended-timestamp message)
  (list 'MRT-MESSAGE
	header             ; MRT-COMMON-HEADER
	extended-timestamp ; nil | MRT-EXTENDED-TIMESTAMP
	message))          ; BYTES | MRT-PEER-INDEX-TABLE | MRT-RIB | MRT-BGPMP-MESSAGE...

(defun MRT-MESSAGE-get-name (obj)                "-> symbol"                       (car obj))
(defun MRT-MESSAGE-get-header (obj)              "-> MRT-COMMON-HEADER"            (cadr obj))
(defun MRT-MESSAGE-get-extended-timestamp (obj)  "-> nil | MRT-EXTENDED-TIMESTAMP" (caddr obj))
(defun MRT-MESSAGE-get-message (obj)             "-> MRT-MESSAGE"                  (cadddr obj))

(defun MRT-MESSAGE-io-read (port)
  (let* ((header (MRT-COMMON-HEADER-io-read port))
	 (message-type (MRT-COMMON-HEADER-get-type header))
	 (message-subtype (MRT-COMMON-HEADER-get-subtype header))
	 (message-length (MRT-COMMON-HEADER-get-length header))

	 (extended-timestamp (if (= message-type 17)
		                 (MRT-EXTENDED-TIMESTAMP-io-read port)
				 nil))
	 
	 (message-octets (if (= message-type 17)
			     (- message-length 4)
			     message-length))
	 
	 (message (case message-type
		    (11  (BYTES-io-read message-octets port))                            ; OSPFv2
		    (12  (BYTES-io-read message-octets port))                            ; TABLE_DUMP_V1
		    (13  (case message-subtype                                          ; TABLE_DUMP_V2
			   (1  (MRT-PEER-INDEX-TABLE-io-read port))                   ; PEER_INDEX_TABLE
			   (2  (MRT-RIB-io-read 'MRT-RIB-IPV4-UNICAST port))          ; RIB_IPV4_UNICAST
			   (3  (MRT-RIB-io-read 'MRT-RIB-IPV4-MULTICAST port))        ; RIB_IPV4_MULTICAST
			   (4  (MRT-RIB-io-read 'MRT-RIB-IPV6-UNICAST port))          ; RIB_IPV6_UNICAST
			   (5  (MRT-RIB-io-read 'MRT-RIB-IPV6-MULTICAST port))        ; RIB_IPV6_MULTICAST
			   (6  (MRT-RIB-GENERIC-io-read port))                        ; RIB_GENERIC
			   (t  (BYTES-io-read message-octets port))))
		    
		    ((16 17)  (case message-subtype                                     ; BGP4MP & BGP4MP_AS4
				(0  (MRT-BGP4MP-STATE-CHANGE-io-read port))             ; BGP4MP_STATE_CHANGE
				(1  (MRT-BGP4MP-MESSAGE-io-read port))                  ; BGP4MP_MESSAGE
				(4  (MRT-BGP4MP-MESSAGE-AS4-io-read port))              ; BGP4MP_MESSAGE_AS4
				(5  (MRT-BGP4MP-STATE-CHANGE-AS4-io-read port))         ; BGP4MP_STATE_CHANGE_AS4
				(6  (MRT-BGP4MP-MESSAGE-LOCAL-io-read port))            ; BGP4MP_MESSAGE_LOCAL
				(7  (MRT-BGP4MP-MESSAGE-AS4-LOCAL-io-read port))        ; BGP4MP_MESSAGE_AS4_LOCAL
				(t  (BYTES-io-read message-octets port))))
		    
		    ((32 33 48 49)  (BYTES-io-read message-octets port))               ; ISIS ISIS_ET OSPFv3 OSPFv3_ET
		    (t  (BYTES-io-read message-octets port)))))

    (MRT-MESSAGE-make header extended-timestamp message)))

(defun MRT-MESSAGE-io-write (obj port)
  (destructuring-bind (header extended-timestamp message)
      (cdr obj)
    (let ((message-type (MRT-COMMON-HEADER-get-type header))
	  (message-subtype (MRT-COMMON-HEADER-get-subtype header)))
      (MRT-COMMON-HEADER-io-write header port)
      (if extended-timestamp
	  (MRT-EXTENDED-TIMESTAMP-io-write extended-timestamp port))
      (case message-type
	(11  (BYTES-io-write message port))                               ; OSPFv2
	(12  (BYTES-io-write message port))                               ; TABLE_DUMP_V1
	(13  (case message-subtype                                        ; TABLE_DUMP_V2
	       (1  (MRT-PEER-INDEX-TABLE-io-write message port))          ; PEER_INDEX_TABLE
	       (2  (MRT-RIB-io-write message port))                       ; RIB_IPV4_UNICAST
	       (3  (MRT-RIB-io-write message port))                       ; RIB_IPV4_MULTICAST
	       (4  (MRT-RIB-io-write message port))                       ; RIB_IPV6_UNICAST
	       (5  (MRT-RIB-io-write message port))                       ; RIB_IPV6_MULTICAST
	       (6  (MRT-RIB-GENERIC-io-write message port))               ; RIB_GENERIC
	       (t  (BYTES-io-write obj port))))
	
	((16 17)  (case message-subtype                                     ; BGP4MP & BGP4MP_AS4
		    (0  (MRT-BGP4MP-STATE-CHANGE-io-write message port))             ; BGP4MP_STATE_CHANGE
		    (1  (MRT-BGP4MP-MESSAGE-io-write message port))                  ; BGP4MP_MESSAGE
		    (4  (MRT-BGP4MP-MESSAGE-AS4-io-write message port))              ; BGP4MP_MESSAGE_AS4
		    (5  (MRT-BGP4MP-STATE-CHANGE-AS4-io-write message port))         ; BGP4MP_STATE_CHANGE_AS4
		    (6  (MRT-BGP4MP-MESSAGE-LOCAL-io-write message port))            ; BGP4MP_MESSAGE_LOCAL
		    (7  (MRT-BGP4MP-MESSAGE-AS4-LOCAL-io-write message port))        ; BGP4MP_MESSAGE_AS4_LOCAL
		    (t  (BYTES-io-write message port))))
	
	((32 33 48 49)  (BYTES-io-write message port))                 ; ISIS ISIS_ET OSPFv3 OSPFv3_ET
	(t  (BYTES-io-write message port))))))

(deftype MRT-MESSAGE () '(cons (member MRT-MESSAGE)))
