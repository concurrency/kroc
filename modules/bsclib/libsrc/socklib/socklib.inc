--
--	socklib.inc - occam socket library definitions
--	Copyright (C) 2000 Fred Barnes (frmb2@ukc.ac.uk)
--
--	This program is free software; you can redistribute it and/or modify
--	it under the terms of the GNU General Public License as published by
--	the Free Software Foundation; either version 2 of the License, or
--	(at your option) any later version.
--
--	This program is distributed in the hope that it will be useful,
--	but WITHOUT ANY WARRANTY; without even the implied warranty of
--	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--	GNU General Public License for more details.
--
--	You should have received a copy of the GNU General Public License
--	along with this program; if not, write to the Free Software
--	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
--

--** @module sock

--{{{  SOCKET definition
--* The information which is known about a socket.
--
-- For most purposes, the address family will be [@ref AF.INET] for
-- IPv4, the socket type will be either [@ref SOCK.STREAM] for TCP or
-- [@ref SOCK.DGRAM] for UDP, and the protocol will be [@ref
-- IPPROTO.TCP] for TCP or [@ref IPPROTO.UDP] for UDP.
--
-- This structure does not accurately reflect the true types of the two
-- [@code port] and [@code addr] members.  Ports are typically unsigned
-- 16-bit numbers, while addresses are unsigned 32-bit numbers (for IPV4
-- at least).  They are incorrect for ease of compatability with the
-- corresponding structure in the C code; in addition, occam does not
-- provide unsigned 16-bit or 32-bit types.
DATA TYPE SOCKET
  PACKED RECORD
    INT fd:               --* File descriptor
    INT local.port:       --* Local port number
    INT remote.port:      --* Remote port number
    INT local.addr:       --* Local IPv4 address, in host order
    INT remote.addr:      --* Remote IPv4 address, in host order
    INT s.family:         --* Socket family (one of [@ref AF])
    INT s.type:           --* Socket type (one of [@ref SOCK])
    INT s.proto:          --* IP protocol (one of [@ref IPPROTO])
    INT error:            --* The error code of the last operation on this socket, if it failed
:
--}}}  

--*{{{  SOCK Socket types

--* Stream sockets (like TCP)
VAL SOCK.STREAM IS 1:
--* Datagram sockets (like UDP)
VAL SOCK.DGRAM IS 2:
VAL SOCK.RAW IS 3:
VAL SOCK.RDM IS 4:
VAL SOCK.SEQPACKET IS 5:
VAL SOCK.PACKET IS 10:
--*}}}  

--*{{{  AF Address families

VAL AF.UNSPEC IS 0:
VAL AF.UNIX IS 1:
VAL AF.LOCAL IS 1:
--* IPv4
VAL AF.INET IS 2:
VAL AF.AX25 IS 3:
VAL AF.IPX IS 4:
VAL AF.APPLETALK IS 5:
VAL AF.NETROM IS 6:
VAL AF.BRIDGE IS 7:
VAL AF.ATMPVC IS 8:
VAL AF.X25 IS 9:
VAL AF.INET6 IS 10:
VAL AF.ROSE IS 11:
VAL AF.DECNET IS 12:
VAL AF.NETBEUI IS 13:
VAL AF.SECURITY IS 14:
VAL AF.KEY IS 15:
VAL AF.NETLINK IS 16:
VAL AF.ROUTE IS AF.NETLINK:
VAL AF.PACKET IS 17:
VAL AF.ASH IS 18:
VAL AF.ECONET IS 19:
VAL AF.ATMSVC IS 20:
VAL AF.SNA IS 22:
VAL AF.IRDA IS 23:
VAL AF.MAX IS 32:
--*}}}  

--*{{{  PF Protocol families

VAL PF.UNSPEC IS AF.UNSPEC:
VAL PF.UNIX IS AF.UNIX:
VAL PF.LOCAL IS AF.LOCAL:
--* IPv4
VAL PF.INET IS AF.INET:
VAL PF.AX25 IS AF.AX25:
VAL PF.IPX IS AF.IPX:
VAL PF.APPLETALK IS AF.APPLETALK:
VAL PF.NETROM IS AF.NETROM:
VAL PF.BRIDGE IS AF.BRIDGE:
VAL PF.ATMPVC IS AF.ATMPVC:
VAL PF.X25 IS AF.X25:
VAL PF.INET6 IS AF.INET6:
VAL PF.ROSE IS AF.ROSE:
VAL PF.DECNET IS AF.DECNET:
VAL PF.NETBEUI IS AF.NETBEUI:
VAL PF.SECURITY IS AF.SECURITY:
VAL PF.KEY IS AF.KEY:
VAL PF.NETLINK IS AF.NETLINK:
VAL PF.ROUTE IS AF.ROUTE:
VAL PF.PACKET IS AF.PACKET:
VAL PF.ASH IS AF.ASH:
VAL PF.ECONET IS AF.ECONET:
VAL PF.ATMSVC IS AF.ATMSVC:
VAL PF.SNA IS AF.SNA:
VAL PF.IRDA IS AF.IRDA:
VAL PF.MAX IS AF.MAX:
--*}}}  

--*{{{  IPPROTO IP protocols

VAL IPPROTO.IP IS 0:
VAL IPPROTO.HOPOPTS IS 0:
VAL IPPROTO.ICMP IS 1:
VAL IPPROTO.IGMP IS 2:
VAL IPPROTO.IPIP IS 4:
--* TCP
VAL IPPROTO.TCP IS 6:
VAL IPPROTO.EGP IS 8:
VAL IPPROTO.PUP IS 12:
--* UDP
VAL IPPROTO.UDP IS 17:
VAL IPPROTO.IDP IS 22:
VAL IPPROTO.TP IS 29:
VAL IPPROTO.IPV6 IS 41:
VAL IPPROTO.ROUTING IS 43:
VAL IPPROTO.FRAGMENT IS 44:
VAL IPPROTO.RSVP IS 46:
VAL IPPROTO.GRE IS 47:
VAL IPPROTO.ESP IS 50:
VAL IPPROTO.AH IS 51:
VAL IPPROTO.ICMPV6 IS 58:
VAL IPPROTO.NONE IS 59:
VAL IPPROTO.DSTOPTS IS 60:
VAL IPPROTO.MTP IS 92:
VAL IPPROTO.ENCAP IS 98:
VAL IPPROTO.PIM IS 103:
VAL IPPROTO.COMP IS 108:
VAL IPPROTO.RAW IS 255:
VAL IPPROTO.MAX IS 256:
--*}}}  

--*{{{  IPPORT Well-known TCP and UDP ports

VAL IPPORT.ECHO IS 7:
VAL IPPORT.DISCARD IS 9:
VAL IPPORT.SYSTAT IS 11:
VAL IPPORT.DAYTIME IS 13:
VAL IPPORT.NETSTAT IS 15:
VAL IPPORT.FTP IS 21:
VAL IPPORT.TELNET IS 23:
VAL IPPORT.SMTP IS 25:
VAL IPPORT.TIMESERVER IS 37:
VAL IPPORT.NAMESERVER IS 42:
VAL IPPORT.WHOIS IS 43:
VAL IPPORT.MTP IS 57:
VAL IPPORT.TFTP IS 69:
VAL IPPORT.RJE IS 77:
VAL IPPORT.FINGER IS 79:
VAL IPPORT.TTYLINK IS 87:
VAL IPPORT.SUPDUP IS 95:
VAL IPPORT.EXECSERVER IS 512:
VAL IPPORT.LOGINSERVER IS 513:
VAL IPPORT.CMDSERVER IS 514:
VAL IPPORT.EFSSERVER IS 520:
VAL IPPORT.BIFFUDP IS 512:
VAL IPPORT.WHOSERVER IS 513:
VAL IPPORT.ROUTESERVER IS 520:
VAL IPPORT.RESERVED IS 1024:
VAL IPPORT.USERRESERVED IS 5000:
--*}}}  

--*{{{  INADDR IPv4 addresses

--* Any IPv4 address, when binding
VAL INADDR.ANY IS #00000000:
--* Broadcast to all reachable hosts
VAL INADDR.BROADCAST IS #FFFFFFFF:
VAL INADDR.NONE IS #FFFFFFFF:
--* The local machine, via the loopback interface
VAL INADDR.LOOPBACK IS #7F000001:
VAL INADDR.UNSPEC.GROUP IS #E0000000:
VAL INADDR.ALLHOSTS.GROUP IS #E0000001:
VAL INADDR.ALLRTRS.GROUP IS #E0000002:
VAL INADDR.MAX.LOCAL.GROUP IS #E00000FF:
--*}}}  

--*{{{  OPTION Boolean values for [@ref socket.setsockopt]

--* Boolean option turned off
VAL OPTION.OFF IS 0:
--* Boolean option turned on
VAL OPTION.ON IS 1:
--*}}}  

--*{{{  SOL Option levels for [@ref socket.setsockopt]

VAL SOL.IP IS 0:
--* Options for sockets of all protocols
VAL SOL.SOCKET IS 1:
--* Options for TCP sockets
VAL SOL.TCP IS 6:
--* Options for UDP sockets
VAL SOL.UDP IS 17:
VAL SOL.IPV6 IS 41:
VAL SOL.RAW IS 255:
VAL SOL.IPX IS 256:
VAL SOL.AX25 IS 257:
VAL SOL.ATALK IS 258:
VAL SOL.NETROM IS 259:
VAL SOL.ROSE IS 260:
VAL SOL.DECNET IS 261:
VAL SOL.X25 IS 262:
VAL SOL.PACKET IS 263:
VAL SOL.ATM IS 264:
VAL SOL.AAL IS 265:
VAL SOL.IRDA IS 266:
--*}}}  

--*{{{  SO Generic options for [@ref socket.setsockopt]

--* Enable debug tracing
VAL SO.DEBUG IS 1:
--* Allow addresses to be reused when binding
VAL SO.REUSEADDR IS 2:
--* Get socket type
VAL SO.TYPE IS 3:
--* Get pending error and clear
VAL SO.ERROR IS 4:
--* Bypass routing table lookup
VAL SO.DONTROUTE IS 5:
--* Allow transmission to broadcast addresses
VAL SO.BROADCAST IS 6:
--* Send buffer size
VAL SO.SNDBUF IS 7:
--* Receive buffer size
VAL SO.RCVBUF IS 8:
--* Send keep-alive messages periodically
VAL SO.KEEPALIVE IS 9:
--* Leave received out-of-band data inline
VAL SO.OOBINLINE IS 10:
VAL SO.NO.CHECK IS 11:
VAL SO.PRIORITY IS 12:
VAL SO.LINGER IS 13:
VAL SO.BSDCOMPAT IS 14:
VAL SO.PASSCRED IS 16:
VAL SO.PEERCRED IS 17:
--* Receive buffer low-water mark
VAL SO.RCVLOWAT IS 18:
--* Send buffer low-water mark
VAL SO.SNDLOWAT IS 19:
VAL SO.RCVTIMEO IS 20:
VAL SO.SNDTIMEO IS 21:
--*}}}  

--*{{{  TCP TCP-related options for [@ref socket.setsockopt]

--* Disable Nagle algorithm (don't delay small packets)
VAL TCP.NODELAY IS 1:
VAL TCP.MAXSEG IS 2:
--* While set, pause sending of partial frames
VAL TCP.CORK IS 3:
VAL TCP.KEEPIDLE IS 4:
--* Seconds between keep-alive messages
VAL TCP.KEEPINTVL IS 5:
VAL TCP.KEEPCNT IS 6:
VAL TCP.SYNCNT IS 7:
VAL TCP.LINGER2 IS 8:
--* Defer accept until data arrives
VAL TCP.DEFER.ACCEPT IS 9:
VAL TCP.WINDOW.CLAMP IS 10:
VAL TCP.INFO IS 11:
VAL TCP.QUICKACK IS 12:
--*}}}

--*{{{  MSG Flag constants for [@ref socket.sendto] and [@ref socket.recvfrom]

VAL MSG.OOB IS #01:
VAL MSG.PEEK IS #02:
VAL MSG.DONTROUTE IS #04:
VAL MSG.TRYHARD IS MSG.DONTROUTE:
VAL MSG.CTRUNC IS #08:
VAL MSG.PROXY IS #10:
VAL MSG.TRUNC IS #20:
VAL MSG.DONTWAIT IS #40:
VAL MSG.EOR IS #80:
VAL MSG.WAITALL IS #100:
VAL MSG.FIN IS #200:
VAL MSG.SYN IS #400:
VAL MSG.URG IS #800:
VAL MSG.RST IS #1000:
VAL MSG.ERRQUEUE IS #2000:
VAL MSG.NOSIGNAL IS #4000:
--*}}}  

--*{{{  SHUT Mode constants for [@ref socket.shutdown]

--* Disallow further receives
VAL SHUT.RD IS 0:
--* Disallow further sends
VAL SHUT.WR IS 1:
--* Disallow further sends and receives
VAL SHUT.RDWR IS 2:
--*}}}  
