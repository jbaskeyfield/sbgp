(in-package :iana-assignments)

;; BGP ORIGIN 

(defparameter +BGP-ORIGIN+
  
   '(0 "IGP"
     1 "EGP"
     2 "Incomplete")
  "BGP ORIGIN [RFC4271]")

;;; BGP Message Types

(defparameter +BGP-MESSAGE-TYPES+
  
   '(0 "Reserved"	
     1 "OPEN [RFC4271]"
     2 "UPDATE [RFC4271]"
     3 "NOTIFICATION [RFC4271]"
     4 "KEEPALIVE [RFC4271]"
     5 "ROUTE-REFRESH [RFC2918]")
  "BGP Message Types")


;;; BGP OPEN Optional Parameter Types  "https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml"

(defparameter +BGP-OPEN+
  
   '(0  "Reserved [RFC5492]"
     1  "Authentication (deprecated) [RFC4271][RFC5492]"
     2  "Capabilities [RFC5492]"
     255  "Extended Length [RFC9072]")
  "BGP OPEN Optional Parameter Types")

;;; BGP Capability Codes  https://www.iana.org/assignments/capability-codes/capability-codes.xhtml#capability-codes-2

(defparameter +BGP-CAP+
  
   '(0  "Reserved [RFC5492]"
     1  "Multiprotocol Extensions for BGP-4 [RFC2858]"
     2  "Route Refresh Capability for BGP-4 [RFC2918]"
     3  "Outbound Route Filtering Capability [RFC5291]"
     4  "Multiple routes to a destination capability (deprecated) [RFC8277]"
     5  "Extended Next Hop Encoding [RFC8950]"
     6  "BGP Extended Message [RFC8654]"
     7  "BGPsec Capability [RFC8205]"
     8  "Multiple Labels Capability [RFC8277]"
     9  "BGP Role [RFC9234]"
     64  "Graceful Restart Capability [RFC4724]"
     65  "Support for 4-octet AS number capability [RFC6793]"
     66  "Deprecated (2003-03-06) "
     67  "Support for Dynamic Capability (capability specific) [draft-ietf-idr-dynamic-cap]"
     68  "Multisession BGP Capability [draft-ietf-idr-bgp-multisession]"
     69  "ADD-PATH Capability [RFC7911]"
     70  "Enhanced Route Refresh Capability [RFC7313]"
     71  "Long-Lived Graceful Restart (LLGR) Capability [draft-uttaro-idr-bgp-persistence]"
     72  "Routing Policy Distribution [draft-ietf-idr-rpd-04]"
     73  "FQDN Capability [draft-walton-bgp-hostname-capability]"
     128  "Prestandard Route Refresh (deprecated) [RFC8810]"
     129  "Prestandard Outbound Route Filtering (deprecated), prestandard Routing Policy Distribution (deprecated) [RFC8810]"
     130  "Prestandard Outbound Route Filtering (deprecated) [RFC8810]"
     131  "Prestandard Multisession (deprecated) [RFC8810]"
     184  "Prestandard FQDN (deprecated) [RFC8810]"
     185  "Prestandard OPERATIONAL message (deprecated) [RFC8810]"
     255  "Reserved [RFC8810]")
  "BGP Capability Codes")

;;;  AFI - Address Family Numbers  https://www.iana.org/assignments/address-family-numbers/address-family-numbers.xhtml#address-family-numbers-2

(defparameter +AFI+
  
   '(0  "Reserved"
     1  "IPv4"
     2  "IPv6"
     3  "NSAP"
     4  "HDLC (8-bit multidrop)"
     5  "BBN 1822"
     6  "802 (includes all 802 media plus Ethernet 'canonical format')"
     7  "E.163"
     8  "E.164 (SMDS, Frame Relay, ATM)"
     9  "F.69 (Telex)"
     10  "X.121 (X.25, Frame Relay)"
     11  "IPX"
     12  "Appletalk"
     13  "Decnet IV"
     14  "Banyan Vines"
     15  "E.164 with NSAP format subaddress [ATM Forum UNI 3.1. October 1995.][Andy_Malis]"
     16  "DNS (Domain Name System) "
     17  "Distinguished Name [Charles_Lynn]"
     18  "AS Number [Charles_Lynn]"
     19  "XTP over IP version 4 [Mike_Saul]"
     20  "XTP over IP version 6 [Mike_Saul]"
     21  "XTP native mode XTP [Mike_Saul]"
     22  "Fibre Channel World-Wide Port Name [Mark_Bakke]"
     23  "Fibre Channel World-Wide Node Name [Mark_Bakke]"
     24  "GWID [Subra_Hegde]"
     25  "AFI for L2VPN information [RFC4761][RFC6074]"
     26  "MPLS-TP Section Endpoint Identifier [RFC7212]"
     27  "MPLS-TP LSP Endpoint Identifier [RFC7212]"
     28  "MPLS-TP Pseudowire Endpoint Identifier [RFC7212]"
     29  "MT IP: Multi-Topology IP version 4 [RFC7307]"
     30  "MT IPv6: Multi-Topology IP version 6 [RFC7307]"
     31  "BGP SFC [RFC9015]"
     16384  "EIGRP Common Service Family [Donnie_Savage]"
     16385  "EIGRP IPv4 Service Family [Donnie_Savage]"
     16386  "EIGRP IPv6 Service Family [Donnie_Savage]"
     16387  "LISP Canonical Address Format (LCAF) [David_Meyer]"
     16388  "BGP-LS [RFC7752]"
     16389  "48-bit MAC [RFC7042]"
     16390  "64-bit MAC [RFC7042]"
     16391  "OUI [RFC7961]"
     16392  "MAC/24 [RFC7961]"
     16393  "MAC/40 [RFC7961]"
     16394  "IPv6/64 [RFC7961]"
     16395  "RBridge Port ID [RFC7961]"
     16396  "TRILL Nickname [RFC7455]"
     16397  "Universally Unique Identifier (UUID) [Nischal_Sheth]"
     16398  "Routing Policy AFI [draft-ietf-idr-rpd-02]"
     16399  "MPLS Namespaces [draft-kaliraj-bess-bgp-sig-private-mpls-labels-03]"
     65535  "Reserved ")
  "AFI - Address Family Numbers")


;;; Subsequent Address Family Identifiers (SAFI) Parameters  https://www.iana.org/assignments/safi-namespace/safi-namespace.xhtml

(defparameter +SAFI+
  
   '(0  "Reserved [RFC4760]"
     1  "Unicast Forwarding [RFC4760]"
     2  "Multicast Forwarding [RFC4760]"
     3  "Reserved [RFC4760]"
     4  "NLRI with MPLS Labels [RFC8277]"
     5  "MCAST-VPN [RFC6514]"
     6  "NLRI for Dynamic Placement of Multi-Segment Pseudowires [RFC7267]"
     7  "Encapsulation SAFI (OBSOLETE) [RFC9012]"
     8  "MCAST-VPLS [RFC7117]"
     9  "BGP SFC [RFC9015]"
     64  "Tunnel SAFI [Gargi_Nalawade][draft-nalawade-kapoor-tunnel-safi-01]"
     65  "Virtual Private LAN Service (VPLS) [RFC4761][RFC6074]"
     66  "BGP MDT SAFI [RFC6037]"
     67  "BGP 4over6 SAFI [RFC5747]"
     68  "BGP 6over4 SAFI [Yong_Cui]"
     69  "Layer-1 VPN auto-discovery information [RFC5195]"
     70  "BGP EVPNs [RFC7432]"
     71  "BGP-LS [RFC7752]"
     72  "BGP-LS-VPN [RFC7752]"
     73  "SR TE Policy SAFI [draft-previdi-idr-segment-routing-te-policy]"
     74  "SD-WAN Capabilities [draft-ietf-idr-sdwan-edge-discovery-03]"
     75  "Routing Policy SAFI [draft-ietf-idr-rpd-02]"
     76  "Classful-Transport SAFI [draft-kaliraj-idr-bgp-classful-transport-planes-00]"
     77  "Tunneled Traffic Flowspec [draft-ietf-idr-flowspec-nvo3-10]"
     78  "MCAST-TREE [draft-ietf-bess-bgp-multicast-03]"
     79  "BGP-DPS (Dynamic Path Selection) [https://eos.arista.com/eos-4-26-2f/dps-vpn-scaling-using-bgp][Venkit_Kasiviswanathan]"
     80  "BGP-LS-SPF [draft-ietf-lsvr-bgp-spf-15][Victor_Kuarsingh]"
     83  "BGP CAR [draft-dskc-bess-bgp-car-03]"
     84  "BGP VPN CAR [draft-dskc-bess-bgp-car-03]"
     85  "BGP-MUP SAFI [draft-mpmz-bess-mup-safi-00]"
     128  "MPLS-labeled VPN address [RFC4364][RFC8277][RFC9252]"
     129  "Multicast for BGP/MPLS IP VPNs [RFC6513][RFC6514]"
     132  "Route Target constrains [RFC4684]"
     133  "Dissemination of Flow Specification rules [RFC8955]"
     134  "L3VPN Dissemination of Flow Specification rules [RFC8955]"
     140  "VPN auto-discovery [draft-ietf-l3vpn-bgpvpn-auto]"
     255  "Reserved [RFC4760]")
  "Subsequent Address Family Identifiers (SAFI) Parameters")


;;; BGP Path Attributes  "https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml"

(defparameter +BGP-PATH-ATTRIBUTES+
  
   '(0  "Reserved "
     1  "ORIGIN [RFC4271]"
     2  "AS_PATH [RFC4271]"
     3  "NEXT_HOP [RFC4271]"
     4  "MULTI_EXIT_DISC [RFC4271]"
     5  "LOCAL_PREF [RFC4271]"
     6  "ATOMIC_AGGREGATE [RFC4271]"
     7  "AGGREGATOR [RFC4271]"
     8  "COMMUNITY [RFC1997]"
     9  "ORIGINATOR_ID [RFC4456]"
     10  "CLUSTER_LIST [RFC4456]"
     11  "DPA (deprecated) 'Destination Preference Attribute for BGP' [RFC6938]"
     12  "ADVERTISER  (historic) (deprecated) [RFC1863][RFC4223][RFC6938]"
     13  "RCID_PATH / CLUSTER_ID (Historic) (deprecated) [RFC1863][RFC4223][RFC6938]"
     14  "MP_REACH_NLRI [RFC4760]"
     15  "MP_UNREACH_NLRI [RFC4760]"
     16  "EXTENDED COMMUNITIES [Eric_Rosen][draft-ramachandra-bgp-ext-communities-00][RFC4360]"
     17  "AS4_PATH [RFC6793]"
     18  "AS4_AGGREGATOR [RFC6793]"
     19  "SAFI Specific Attribute (SSA) (deprecated) [Gargi_Nalawade][draft-kapoor-nalawade-idr-bgp-ssa-00][draft-nalawade-idr-mdt-safi-00][draft-wijnands-mt-discovery-00]"
     20  "Connector Attribute (deprecated) [RFC6037]"
     21  "AS_PATHLIMIT (deprecated) [draft-ietf-idr-as-pathlimit]"
     22  "PMSI_TUNNEL [RFC6514]"
     23  "Tunnel Encapsulation [RFC9012]"
     24  "Traffic Engineering [RFC5543]"
     25  "IPv6 Address Specific Extended Community [RFC5701]"
     26  "AIGP [RFC7311]"
     27  "PE Distinguisher Labels [RFC6514]"
     28  "BGP Entropy Label Capability Attribute (deprecated) [RFC6790][RFC7447]"
     29  "BGP-LS Attribute [RFC7752]"
     30  "Deprecated [RFC8093]"
     31  "Deprecated [RFC8093]"
     32  "LARGE_COMMUNITY [RFC8092]"
     33  "BGPsec_Path [RFC8205]"
     34  "BGP Community Container Attribute (TEMPORARY) [draft-ietf-idr-wide-bgp-communities]"
     35  "Only to Customer (OTC) [RFC9234]"
     36  "BGP Domain Path (D-PATH) (TEMPORARY) [draft-ietf-bess-evpn-ipvpn-interworking-06]"
     37  "SFP attribute [RFC9015]"
     38  "BFD Discriminator [RFC9026]"
     39  "Unassigned "
     40  "BGP Prefix-SID [RFC8669]"
     128  "ATTR_SET [RFC6368]"
     129  "Deprecated [RFC8093]"
     241  "Deprecated [RFC8093]"
     242  "Deprecated [RFC8093]"
     243  "Deprecated [RFC8093]"
     255  "Reserved for development [RFC2042]")
  "BGP Path Attributes")


;; BGP Error (Notification) Codes  "https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml"

(defparameter +BGP-ERROR-NOTIFICATION+
  
   '(0  "Reserved "
     1  "Message Header Error [RFC4271]"
     2  "OPEN Message Error [RFC4271]"
     3  "UPDATE Message Error [RFC4271]"
     4  "Hold Timer Expired [RFC4271]"
     5  "Finite State Machine Error [RFC4271]"
     6  "Cease [RFC4271]"
     7  "ROUTE-REFRESH Message Error [RFC7313]")
  "BGP Error (Notification) Codes")


;;; BGP Error Subcodes  "https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml"

(defparameter +BGP-ERROR+
  
   '(0  "Unspecific [RFC Errata 4493]"
     1  "Connection Not Synchronized [RFC4271]"
     2  "Bad Message Length [RFC4271]"
     3  "Bad Message Type [RFC4271]")
  "BGP Error Subcode")


;;; OPEN Message Error subcodes  "https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml"

(defparameter +OPEN-MESSAGE-ERROR+
  
   '(0  "Unspecific [RFC Errata 4493]"
     1  "Unsupported Version Number [RFC4271]"
     2  "Bad Peer AS [RFC4271]"
     3  "Bad BGP Identifier [RFC4271]"
     4  "Unsupported Optional Parameter [RFC4271]"
     5  "[Deprecated] [RFC4271]"
     6  "Unacceptable Hold Time [RFC4271]"
     7  "Unsupported Capability [RFC5492]"
     8  "Deprecated [RFC9234]"
     9  "Deprecated [RFC9234]"
     10  "Deprecated [RFC9234]"
     11  "Role Mismatch [RFC9234]")
  "OPEN Message Error subcodes")


;;; UPDATE Message Error subcodes  "https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml"

(defparameter +UPDATE-MESSAGE-ERROR+
  
   '(0  "Unspecific [RFC Errata 4493]"
     1  "Malformed Attribute List [RFC4271]"
     2  "Unrecognized Well-known Attribute [RFC4271]"
     3  "Missing Well-known Attribute [RFC4271]"
     4  "Attribute Flags Error [RFC4271]"
     5  "Attribute Length Error [RFC4271]"
     6  "Invalid ORIGIN Attribute [RFC4271]"
     7  "[Deprecated] [RFC4271]"
     8  "Invalid NEXT_HOP Attribute [RFC4271]"
     9  "Optional Attribute Error [RFC4271]"
     10  "Invalid Network Field [RFC4271]"
     11  "Malformed AS_PATH [RFC4271]")
  "UPDATE Message Error subcodes")


;;; BGP Finite State Machine Error Subcodes  "https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml"

(defparameter +BGP-FSM-ERROR+
  
   '(0  "Unspecified Error [RFC6608]"
     1  "Receive Unexpected Message in OpenSent State [RFC6608]"
     2  "Receive Unexpected Message in OpenConfirm State [RFC6608]"
     3  "Receive Unexpected Message in Established State [RFC6608]")
  "BGP Finite State Machine Error Subcodes")

;;; BGP Cease NOTIFICATION message subcodes  "https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml"

(defparameter +BGP-CEASE-NOTIFICATION+
  
   '(0  "Reserved "
     1  "Maximum Number of Prefixes Reached [RFC4486]"
     2  "Administrative Shutdown [RFC4486][RFC9003]"
     3  "Peer De-configured [RFC4486]"
     4  "Administrative Reset [RFC4486][RFC9003]"
     5  "Connection Rejected [RFC4486]"
     6  "Other Configuration Change [RFC4486]"
     7  "Connection Collision Resolution [RFC4486]"
     8  "Out of Resources [RFC4486]"
     9  "Hard Reset [RFC8538]"
     10  "BFD Down (TEMPORARY) [draft-ietf-idr-bfd-subcode-01]")
  "BGP Cease NOTIFICATION message subcodes")


;;; BGP ROUTE-REFRESH Message Error subcodes  "https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml"

(defparameter +BGP-ROUTE-REFRESH-MESSAGE-ERROR+
  
   '(0  "Reserved [RFC7313]"
     1  "Invalid Message Length [RFC7313]")
  "BGP ROUTE-REFRESH Message Error subcodes")


;;; BGP Outbound Route Filtering (ORF) Types  "https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml"

(defparameter +BGP-ORF-TYPES+
  
   '(  0  "Reserved [RFC5291]"
     64  "Address Prefix ORF [RFC5292]"
     65  "CP-ORF [RFC7543]")
  "BGP Outbound Route Filtering (ORF) Types")


;;; BGP Route Refresh Subcodes  "https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml"

(defparameter +BGP-ROUTE-REFRESH+
  
   '( 0  "Route-Refresh [RFC2918][RFC5291]"
     1  "BoRR [RFC7313]"
     2  "EoRR [RFC7313]"
     255  "Reserved [RFC7313]")
  "BGP Route Refresh Subcodes")
