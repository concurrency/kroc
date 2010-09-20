/*
 *	csock.c - C functions for occam socket library
 *	Copyright (C) 2000 Fred Barnes (frmb2@ukc.ac.uk)
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <stdio.h>
#include <stdlib.h>

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>

#include <netinet/tcp.h>

#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif

#ifdef HOSTOS_CYGWIN
#include <w32api/wtypes.h>
#include <w32api/winbase.h>
#endif

#ifdef OCCBUILD_KROC
#include "dmem_if.h"
#endif

/*{{{  constants/structures */
struct __occ_socket {
	int fd;
	int local_port;
	int remote_port;
	int local_addr;
	int remote_addr;
	int s_family;
	int s_type;
	int s_proto;
	int error;
#ifdef __GNUC__
	} __attribute__ ((packed));
#else
	};
	#warning Not GNU C - might get structure problems..
#endif

typedef struct __occ_socket occ_socket;

#ifndef HOST_NAME_MAX
#define HOST_NAME_MAX (256)
#endif
/*}}}*/

/*{{{  address family */
/* MUST MATCH socklib.inc */

#define OCC_AF_UNSPEC 0
#define OCC_AF_UNIX 1
#define OCC_AF_LOCAL 1
#define OCC_AF_INET 2
#define OCC_AF_AX25 3
#define OCC_AF_IPX 4
#define OCC_AF_APPLETALK 5
#define OCC_AF_NETROM 6
#define OCC_AF_BRIDGE 7
#define OCC_AF_ATMPVC 8
#define OCC_AF_X25 9
#define OCC_AF_INET6 10
#define OCC_AF_ROSE 11
#define OCC_AF_DECNET 12
#define OCC_AF_NETBEUI 13
#define OCC_AF_SECURITY 14
#define OCC_AF_KEY 15
#define OCC_AF_NETLINK 16
#define OCC_AF_ROUTE OCC_AF_NETLINK
#define OCC_AF_PACKET 17
#define OCC_AF_ASH 18
#define OCC_AF_ECONET 19
#define OCC_AF_ATMSVC 20
#define OCC_AF_SNA 22
#define OCC_AF_IRDA 23
#define OCC_AF_MAX 32

static int r_encode_address_family (int af) {
	switch (af) {
		case OCC_AF_UNSPEC:	return AF_UNSPEC;
		case OCC_AF_UNIX:	return AF_UNIX;
		/* case OCC_AF_LOCAL:	return AF_LOCAL; */
		case OCC_AF_INET:	return AF_INET;
		#ifdef AF_AX25
		case OCC_AF_AX25:	return AF_AX25;
		#endif /* AF_AX25 */
		case OCC_AF_IPX:	return AF_IPX;
		case OCC_AF_APPLETALK:	return AF_APPLETALK;
		#ifdef AF_NETROM
		case OCC_AF_NETROM:	return AF_NETROM;
		#endif /* AF_NETROM */
		#ifdef AF_BRIDGE
		case OCC_AF_BRIDGE:	return AF_BRIDGE;
		#endif /* AF_BRIDGE */
		#ifdef AF_ATMPVC
		case OCC_AF_ATMPVC:	return AF_ATMPVC;
		#endif /* AF_ATMPVC */
		#ifdef AF_X25
		case OCC_AF_X25:	return AF_X25;
		#endif /* AF_X25 */
		case OCC_AF_INET6:	return AF_INET6;
		#ifdef AF_ROSE
		case OCC_AF_ROSE:	return AF_ROSE;
		#endif /* AF_ROSE */
		#ifdef AF_DECNET
		case OCC_AF_DECNET:	return AF_DECNET;
		#endif /* AF_DECNET */
		#ifdef AF_NETBEUI
		case OCC_AF_NETBEUI:	return AF_NETBEUI;
		#endif /* AF_NETBEUI */
		#ifdef AF_SECURITY
		case OCC_AF_SECURITY:	return AF_SECURITY;
		#endif /* AF_SECURITY */
		#ifdef AF_KEY
		case OCC_AF_KEY:	return AF_KEY;
		#endif /* AF_KEY */
		/* case OCC_AF_NETLINK:	return AF_NETLINK */
		case OCC_AF_ROUTE:	return AF_ROUTE;
		#ifdef AF_PACKET
		case OCC_AF_PACKET:	return AF_PACKET;
		#endif /* AF_PACKET */
		#ifdef AF_ASH
		case OCC_AF_ASH:	return AF_ASH;
		#endif /* AF_ASH */
		#ifdef AF_ECONET
		case OCC_AF_ECONET:	return AF_ECONET;
		#endif /* AF_ECONET */
		#ifdef AF_ATMSVC
		case OCC_AF_ATMSVC:	return AF_ATMSVC;
		#endif /* AF_ATMSVC */
		case OCC_AF_SNA:	return AF_SNA;
		#ifdef AF_IRDA
		case OCC_AF_IRDA:	return AF_IRDA;
		#endif /* AF_IRDA */
		case OCC_AF_MAX:	return AF_MAX;
		default:
			return AF_UNSPEC;
	}
}
/*}}}*/

/*{{{  sockopt levels and options */
/* MUST MATCH socklib.inc */

#define OCC_SOL_IP 0
#define OCC_SOL_SOCKET 1
#define OCC_SOL_TCP 6
#define OCC_SOL_UDP 17
#define OCC_SOL_IPV6 41
#define OCC_SOL_RAW 255
#define OCC_SOL_IPX 256
#define OCC_SOL_AX25 257
#define OCC_SOL_ATALK 258
#define OCC_SOL_NETROM 259
#define OCC_SOL_ROSE 260
#define OCC_SOL_DECNET 261
#define OCC_SOL_X25 262
#define OCC_SOL_PACKET 263
#define OCC_SOL_ATM 264
#define OCC_SOL_AAL 265
#define OCC_SOL_IRDA 266

static int r_encode_sockopt_optlevel (int level) {
	if (level == OCC_SOL_SOCKET) {
		return SOL_SOCKET;
	} else {
		return level;
	}
}

#define OCC_SO_DEBUG 1
#define OCC_SO_REUSEADDR 2
#define OCC_SO_TYPE 3
#define OCC_SO_ERROR 4
#define OCC_SO_DONTROUTE 5
#define OCC_SO_BROADCAST 6
#define OCC_SO_SNDBUF 7
#define OCC_SO_RCVBUF 8
#define OCC_SO_KEEPALIVE 9
#define OCC_SO_OOBINLINE 10
#define OCC_SO_NO_CHECK 11
#define OCC_SO_PRIORITY 12
#define OCC_SO_LINGER 13
#define OCC_SO_BSDCOMPAT 14
#define OCC_SO_PASSCRED 16
#define OCC_SO_PEERCRED 17
#define OCC_SO_RCVLOWAT 18
#define OCC_SO_SNDLOWAT 19
#define OCC_SO_RCVTIMEO 20
#define OCC_SO_SNDTIMEO 21

#define OCC_TCP_NODELAY 1
#define OCC_TCP_MAXSEG 2
#define OCC_TCP_CORK 3
#define OCC_TCP_KEEPIDLE 4
#define OCC_TCP_KEEPINTVL 5
#define OCC_TCP_KEEPCNT 6
#define OCC_TCP_SYNCNT 7
#define OCC_TCP_LINGER2 8
#define OCC_TCP_DEFER_ACCEPT 9
#define OCC_TCP_WINDOW_CLAMP 10
#define OCC_TCP_INFO 11
#define OCC_TCP_QUICKACK 12

static int r_encode_sockopt_optname (int level, int opt)
{
	if (level == OCC_SOL_SOCKET) {
		switch (opt) {
			case OCC_SO_DEBUG: 	return SO_DEBUG;
			case OCC_SO_REUSEADDR:	return SO_REUSEADDR;
			case OCC_SO_TYPE:	return SO_TYPE;
			case OCC_SO_ERROR:	return SO_ERROR;
			case OCC_SO_DONTROUTE:	return SO_DONTROUTE;
			case OCC_SO_BROADCAST:	return SO_BROADCAST;
			case OCC_SO_SNDBUF:	return SO_SNDBUF;
			case OCC_SO_RCVBUF:	return SO_RCVBUF;
			case OCC_SO_KEEPALIVE:	return SO_KEEPALIVE;
			case OCC_SO_OOBINLINE:	return SO_OOBINLINE;
			#ifdef SO_NO_CHECK
			case OCC_SO_NO_CHECK:	return SO_NO_CHECK;
			#endif /* SO_NO_CHECK */
			#ifdef SO_PRIORITY
			case OCC_SO_PRIORITY:	return SO_PRIORITY;
			#endif /* SO_PRIORITY */
			case OCC_SO_LINGER:	return SO_LINGER;
			#ifdef SO_BSDCOMPAT
			case OCC_SO_BSDCOMPAT:	return SO_BSDCOMPAT;
			#endif /* SO_BSDCOMPAT */
			#ifdef SO_PASSCRED
			case OCC_SO_PASSCRED:	return SO_PASSCRED;
			#endif /* SO_PASSCRED */
			#ifdef SO_PEERCRED
			case OCC_SO_PEERCRED:	return SO_PEERCRED;
			#endif /* SO_PEERCRED */
			case OCC_SO_RCVLOWAT:	return SO_RCVLOWAT;
			case OCC_SO_SNDLOWAT:	return SO_SNDLOWAT;
			case OCC_SO_RCVTIMEO:	return SO_RCVTIMEO;
			case OCC_SO_SNDTIMEO:	return SO_SNDTIMEO;
			default:
				return opt;
		}
	} else if (level == OCC_SOL_TCP) {
		switch (opt) {
			case OCC_TCP_NODELAY:	return TCP_NODELAY;
			case OCC_TCP_MAXSEG:	return TCP_MAXSEG;
			#ifdef TCP_CORK
			case OCC_TCP_CORK:	return TCP_CORK;
			#endif /* TCP_CORK */
			#ifdef TCP_KEEPIDLE
			case OCC_TCP_KEEPIDLE:	return TCP_KEEPIDLE;
			#endif /* TCP_KEEPIDLE */
			#ifdef TCP_KEEPINTVL
			case OCC_TCP_KEEPINTVL:	return TCP_KEEPINTVL;
			#endif /* TCP_INTVL */
			#ifdef TCP_KEEPCNT
			case OCC_TCP_KEEPCNT:	return TCP_KEEPCNT;
			#endif /* TCP_KEEPCNT */
			#ifdef TCP_SYNCNT
			case OCC_TCP_SYNCNT:	return TCP_SYNCNT;
			#endif /* TCP_SYNCNT */
			#ifdef TCP_LINGER2
			case OCC_TCP_LINGER2:	return TCP_LINGER2;
			#endif /* TCP_LINGER2 */
			#ifdef TCP_DEFER_ACCEPT
			case OCC_TCP_DEFER_ACCEPT: return TCP_DEFER_ACCEPT;
			#endif /* TCP_DEFER_ACCEPT */
			#ifdef TCP_WINDOW_CLAMP
			case OCC_TCP_WINDOW_CLAMP: return TCP_WINDOW_CLAMP;
			#endif /* TCP_WINDOW_CLAMP */
			#ifdef TCP_INFO
			case OCC_TCP_INFO:	return TCP_INFO;
			#endif /* TCP_INFO */
			#ifdef TCP_QUICKACK
			case OCC_TCP_QUICKACK:	return TCP_QUICKACK;
			#endif /* TCP_QUICKACK */
			default:
				return opt;
		}
	} else {
		return opt;
	}
}
/*}}}*/

/*{{{  message flags */
/* MUST MATCH socklib.inc */
#define OCC_MSG_OOB 0x01
#define OCC_MSG_PEEK 0x02
#define OCC_MSG_DONTROUTE 0x04
#define OCC_MSG_TRYHARD OCC_MSG_DONTROUTE:
#define OCC_MSG_CTRUNC 0x08
#define OCC_MSG_PROXY 0x10
#define OCC_MSG_TRUNC 0x20
#define OCC_MSG_DONTWAIT 0x40
#define OCC_MSG_EOR 0x80
#define OCC_MSG_WAITALL 0x100
#define OCC_MSG_FIN 0x200
#define OCC_MSG_SYN 0x400
#define OCC_MSG_URG 0x800
#define OCC_MSG_RST 0x1000
#define OCC_MSG_ERRQUEUE 0x2000
#define OCC_MSG_NOSIGNAL 0x4000
#define OCC_MSG_MASK 0x7fff

static int r_encode_msg_flags (int flags)
{
	int ret = (flags & (~OCC_MSG_MASK));

	if (flags & OCC_MSG_OOB)
		ret |= MSG_OOB;
	if (flags & OCC_MSG_PEEK)
		ret |= MSG_PEEK;
	if (flags & OCC_MSG_DONTROUTE)
		ret |= MSG_DONTROUTE;
	if (flags & OCC_MSG_CTRUNC)
		ret |= MSG_CTRUNC;
	#ifdef MSG_PROXY
	if (flags & OCC_MSG_PROXY)
		ret |= MSG_PROXY;
	#endif /* MSG_PROXY */
	if (flags & OCC_MSG_TRUNC)
		ret |= MSG_TRUNC;
	if (flags & OCC_MSG_DONTWAIT)
		ret |= MSG_DONTWAIT;
	if (flags & OCC_MSG_EOR)
		ret |= MSG_EOR;
	if (flags & OCC_MSG_WAITALL)
		ret |= MSG_WAITALL;
	#ifdef MSG_FIN
	if (flags & OCC_MSG_FIN)
		ret |= MSG_FIN;
	#endif /* MSG_FIN */
	#ifdef MSG_SYN
	if (flags & OCC_MSG_SYN)
		ret |= MSG_SYN;
	#endif /* MSG_SYN */
	#ifdef MSG_URG
	if (flags & OCC_MSG_URG)
		ret |= MSG_URG;
	#endif /* MSG_URG */
	#ifdef MSG_RST
	if (flags & OCC_MSG_RST)
		ret |= MSG_RST;
	#endif /* MSG_RST */
	#ifdef MSG_ERRQUEUE
	if (flags & OCC_MSG_ERRQUEUE)
		ret |= MSG_ERRQUEUE;
	#endif /* MSG_ERRQUEUE */
	#ifdef MSG_NOSIGNAL
	if (flags & OCC_MSG_NOSIGNAL)
		ret |= MSG_NOSIGNAL;
	#endif /* MSG_NOSIGNAL */

	return ret;
}

/*}}}*/

/*
 *	Basic socket stuff
 */

/*{{{  static __inline__ void r_socket (occ_socket *sock)*/
/*
 *	void r_socket (occ_socket *sock)
 *	creates a new socket
 */
static __inline__ void r_socket (occ_socket *sock)
{
	sock->fd = socket (r_encode_address_family (sock->s_family), sock->s_type, sock->s_proto);
	sock->error = errno;
}
/*}}}*/
/*{{{  static __inline__ void r_close (occ_socket *sock)*/
/*
 *	void r_close (occ_socket *sock)
 *	closes a socket (or any file descriptor)
 */
static __inline__ void r_close (occ_socket *sock)
{
	if (sock->fd >= 0) {
		close (sock->fd);
		sock->fd = -1;
		sock->error = errno;
	} else {
		sock->error = ENOTCONN;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_read (occ_socket *sock, char *buffer, int buf_size, int count, int *result)*/
/*
 *	void r_read (occ_socket *sock, char *buffer, int buf_size, int count, int *result)
 *	performs a read
 */
static __inline__ void r_read (occ_socket *sock, char *buffer, int buf_size, int count, int *result)
{
	int x;

	if (sock->fd < 0) {
		*result = -1;
		sock->error = ENOTCONN;
	} else {
		x = (buf_size < count) ? buf_size : count;
		*result = read (sock->fd, buffer, x);
		sock->error = errno;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_fullread (occ_socket *sock, char *buffer, int buf_size, int count, int *result)*/
/*
 a	void r_fullread (occ_socket *sock, char *buffer, int buf_size, int count, int *result)
 *	performs a read, but makes sure as much as requested has been read
 */
static __inline__ void r_fullread (occ_socket *sock, char *buffer, int buf_size, int count, int *result)
{
	int x, r, in;

	if (sock->fd < 0) {
		*result = -1;
		sock->error = ENOTCONN;
	} else {
		x = count;
		in = 0;
		while (in < x) {
			r = read (sock->fd, buffer+in, x-in);
			if (r < 1) {
				*result = r;
				sock->error = errno;
				return;
			} else {
				in += r;
			}
		}
		*result = in;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_write (occ_socket *sock, char *buffer, int buf_size, int *result)*/
/*
 *	void r_write (occ_socket *sock, char *buffer, int buf_size, int *result)
 *	performs a write
 */
static __inline__ void r_write (occ_socket *sock, char *buffer, int buf_size, int *result)
{
	if (sock->fd < 0) {
		*result = -1;
		sock->error = ENOTCONN;
	} else {
		*result = write (sock->fd, buffer, buf_size);
		sock->error = errno;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_write_addr (occ_socket *sock, int addr, int size, int *result)*/
/*
 *	void r_write_addr (occ_socket *sock, int addr, int size, int *result)
 *	does a write (takes address)
 */
static __inline__ void r_write_addr (occ_socket *sock, int addr, int size, int *result)
{
	if (sock->fd < 0) {
		*result = -1;
		sock->error = ENOTCONN;
	} else {
		*result = write (sock->fd, (char *)addr, size);
		sock->error = errno;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_fullwrite (occ_socket *sock, char *buffer, int buf_size, int *result)*/
/*
 *	void r_fullwrite (occ_socket *sock, char *buffer, int buf_size, int *result)
 *	performs a write, but makes sure it's all gone out unless there's an error
 */
static __inline__ void r_fullwrite (occ_socket *sock, char *buffer, int buf_size, int *result)
{
	int x, r, out;

	if (sock->fd < 0) {
		*result = -1;
		sock->error = ENOTCONN;
	} else {
		x = buf_size;
		out = 0;
		while (out < x) {
			r = write (sock->fd, buffer+out, x-out);
			if (r < 1) {
				*result = r;
				sock->error = errno;
				return;
			} else {
				out += r;
			}
		}
		*result = out;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_fullwrite_addr (occ_socket *sock, int addr, int size, int *result)*/
/*
 *	void r_fullwrite_addr (occ_socket *sock, int addr, int size, int *result)
 *	performs a write (from address), makes sure it's all gone unless error
 */
static __inline__ void r_fullwrite_addr (occ_socket *sock, int addr, int size, int *result)
{
	int x, r, out;

	if (sock->fd < 0) {
		*result = -1;
		sock->error = ENOTCONN;
	} else {
		x = size;
		out = 0;
		while (out < x) {
			r = write (sock->fd, (char *)(addr + out), (x - out));
			if (r < 1) {
				*result = r;
				sock->error = errno;
				return;
			} else {
				out += r;
			}
		}
		*result = out;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_connect (occ_socket *sock, int *result)*/
/*
 *	void r_connect (occ_socket *sock, int *result)
 *	attempts a connection
 */
static __inline__ void r_connect (occ_socket *sock, int *result)
{
	struct sockaddr_in sin;

	if (sock->fd < 0) {
		*result = -1;
		sock->error = ENOTCONN;
	} else {
		sin.sin_family = r_encode_address_family (sock->s_family);
		sin.sin_port = htons (sock->remote_port);
		sin.sin_addr.s_addr = htonl (sock->remote_addr);
		*result = connect (sock->fd, (struct sockaddr *)&sin, sizeof(sin));
		sock->error = errno;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_listen (occ_socket *sock, int backlog, int *result)*/
/*
 *	void r_listen (occ_socket *sock, int backlog, int *result)
 *	listens on a socket
 */
static __inline__ void r_listen (occ_socket *sock, int backlog, int *result)
{
	*result = listen (sock->fd, backlog);
	sock->error = errno;
}
/*}}}*/
/*{{{  static __inline__ void r_bind (occ_socket *sock, int *result)*/
/*
 *	void r_bind (occ_socket *sock, int *result)
 *	binds a socket to the specified sock->local_port
 */
static __inline__ void r_bind (occ_socket *sock, int *result)
{
	struct sockaddr_in sin;

	sin.sin_family = r_encode_address_family (sock->s_family);
	sin.sin_port = htons (sock->local_port);
	sin.sin_addr.s_addr = htonl (sock->local_addr);
	*result = bind (sock->fd, (struct sockaddr *)&sin, sizeof(sin));
	sock->error = errno;
}
/*}}}*/
/*{{{  static __inline__ void r_accept (occ_socket *sock, occ_socket *client, int *result)*/
/*
 *	void r_accept (occ_socket *sock, occ_socket *client, int *result)
 *	accepts a connection
 */
static __inline__ void r_accept (occ_socket *sock, occ_socket *client, int *result)
{
	struct sockaddr_in sin;
	int sin_size = sizeof (struct sockaddr_in);
	int t_fd;

	t_fd = accept (sock->fd, (struct sockaddr *)&sin, (socklen_t *)&sin_size);
	sin_size = sizeof (sin);
	sock->error = errno;
	if (t_fd > -1) {
		client->fd = t_fd;
		client->remote_port = ntohs (sin.sin_port);
		client->remote_addr = ntohl (sin.sin_addr.s_addr);
		client->local_port = sock->local_port;
		client->local_addr = sock->local_addr;
		client->s_family = sock->s_family;
		client->s_type = sock->s_type;
		client->s_proto = sock->s_proto;
	}
	*result = t_fd;
}
/*}}}*/
/*{{{  static __inline__ void r_sendto (occ_socket *sock, char *message, int msg_len, int flags, int *result)*/
/*
 *	void r_sendto (occ_socket *sock, char *message, int msg_len, int flags, int *result)
 *	sends a packet (for UDP sockets mostly)
 */
static __inline__ void r_sendto (occ_socket *sock, char *message, int msg_len, int flags, int *result)
{
	struct sockaddr_in	sin;
	int			sin_size = sizeof (sin);

	if (sock->fd < 0) {
		*result = -1;
		sock->error = ENOTCONN;
	} else {
		sin.sin_port = htons (sock->remote_port);
		sin.sin_addr.s_addr = htonl (sock->remote_addr);
		sin.sin_family = r_encode_address_family (sock->s_family);
		*result = sendto (sock->fd, message, msg_len, flags, (struct sockaddr *)&sin, sin_size);
		sock->error = errno;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_recvfrom (occ_socket *sock, char *buf, int buf_len, int flags, int *result)*/
/*
 *	void r_recvfrom (occ_socket *sock, char *buf, int buf_len, int flags, int *result)
 *	receives a datagram from a socket
 */
static __inline__ void r_recvfrom (occ_socket *sock, char *buf, int buf_len, int flags, int *result)
{
	struct sockaddr_in sin;
	int sin_size = sizeof (sin);

	if (sock->fd < 0) {
		*result = -1;
		sock->error = ENOTCONN;
	} else {
		*result = recvfrom (sock->fd, buf, buf_len, flags, (struct sockaddr *)&sin, (socklen_t *)&sin_size);
		sock->remote_port = ntohs (sin.sin_port);
		sock->remote_addr = ntohl (sin.sin_addr.s_addr);
		sock->error = errno;
	}
}
/*}}}*/

/*{{{  static __inline__ void r_setsockopt (occ_socket *sock, int level, int option, int value, int *result)*/
/*
 *	void r_setsockopt (occ_socket *sock, int level, int option, int value, int *result)
 *	sets a socket option (only integer options can be set through this!)
 */
static __inline__ void r_setsockopt (occ_socket *sock, int level, int option, int value, int *result)
{
	if (sock->fd < 0) {
		*result = -1;
		sock->error = ENOTCONN;
	} else {
		*result = setsockopt (sock->fd, r_encode_sockopt_optlevel (level), r_encode_sockopt_optname (level, option), (const void *)&value, sizeof (value));
		sock->error = errno;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_getsockopt (occ_socket *sock, int level, int option, int *value, int *result)*/
/*
 *	void r_getsockopt (occ_socket *sock, int level, int option, int *value, int *result)
 *	gets a socket option (only integer options though!)
 */
static __inline__ void r_getsockopt (occ_socket *sock, int level, int option, int *value, int *result)
{
	if (sock->fd < 0) {
		*result = -1;
		sock->error = ENOTCONN;
	} else {
		int valuesize = sizeof (int);

		*result = getsockopt (sock->fd, r_encode_sockopt_optlevel (level), r_encode_sockopt_optname (level, option), (void *)value, (void *)&valuesize);
		if (valuesize != sizeof (int)) {
			*result = -1;	/* something meaningless probably */
		}
		sock->error = errno;
	}
}
/*}}}*/

/*
 *	resolution stuff
 */

/*{{{  static __inline__ void r_addr_of_host (char *hostname, int h_len, int *addr, int *result)*/
/*
 *	void r_addr_of_host (char *hostname, int h_len, int *addr, int *result)
 *	gets the address of a host, from the hostname
 */
static __inline__ void r_addr_of_host (char *hostname, int h_len, int *addr, int *result)
{
	char p_buffer [512];
	struct hostent *hp;

	if (h_len >= sizeof(p_buffer)) {
		*result = -1;
		*addr = 0;
	} else {
		memcpy ((char *)p_buffer, hostname, h_len);
		p_buffer[h_len] = '\0';
		hp = gethostbyname (p_buffer);
		if (!hp) {
			*result = -1;
			*addr = 0;
		} else {
			*addr = *(int *)(hp->h_addr);
			*addr = ntohl (*addr);
			*result = 0;
		}
	}
}
/*}}}*/
/*{{{  static __inline__ void r_addrs_of_host (char *hostname, int h_len, int *addrs, int addrslen, int *result)*/
/*
 *	void r_addrs_of_host (char *hostname, int h_len, int *addrs, int addrslen, int *result)
 *	gets all addresses of a host, from the hostname
 */
static __inline__ void r_addrs_of_host (char *hostname, int h_len, int *addrs, int addrslen, int *result)
{
	char p_buffer[512];
	struct hostent *hp;

	if (h_len >= 512) {
		*result = -1;
	} else {
		memcpy ((char *)p_buffer, hostname, h_len);
		p_buffer[h_len] = '\0';
		hp = gethostbyname (p_buffer);
		if (!hp) {
			*result = -1;
		} else {
			for (*result = 0; (*result < addrslen) && hp->h_addr_list[*result]; (*result)++) {
				addrs[*result] = ntohl (*(int *)(hp->h_addr_list[*result]));
			}
		}
	}
}
/*}}}*/
/*{{{  static __inline__ void r_naddrs_of_host (char *hostname, int h_len, int *result)*/
/*
 *	void r_naddrs_of_host (char *hostname, int h_len, int *result)
 *	gets the number of addresses associated with a particular host
 */
static __inline__ void r_naddrs_of_host (char *hostname, int h_len, int *result)
{
	char p_buffer[512];
	struct hostent *hp;

	if (h_len >= 512) {
		*result = -1;
	} else {
		memcpy ((char *)p_buffer, hostname, h_len);
		p_buffer[h_len] = '\0';
		hp = gethostbyname (p_buffer);
		if (!hp) {
			*result = -1;
		} else {
			for (*result = 0; hp->h_addr_list[*result]; (*result)++);
		}
	}
}
/*}}}*/
/*{{{  static __inline__ void r_host_of_addr (int addr, char *hostname, int h_len, int *a_len, int *result)*/
/*
 *	void r_host_of_addr (int addr, char *hostname, int h_len, int *a_len, int *result)
 *	gets the hostname of an address
 */
static __inline__ void r_host_of_addr (int addr, char *hostname, int h_len, int *a_len, int *result)
{
	struct hostent *hp;
	int naddr;

	naddr = htonl (addr);
	hp = gethostbyaddr ((const char *)&naddr, sizeof(int), AF_INET);
	if (!hp) {
		*a_len = 0;
		*result = -1;
	} else {
		if (strlen (hp->h_name) >= h_len) {
			*a_len = 0;
			*result = -1;
		} else {
			*a_len = strlen (hp->h_name);
			memcpy (hostname, hp->h_name, *a_len);
			*result = 0;
		}
	}
}
/*}}}*/
/*{{{  static __inline__ void r_ip_of_addr (int addr, char *ipaddr, int h_len, int *a_len, int *result)*/
/*
 *	void r_ip_of_addr (int addr, char *ipaddr, int h_len, int *a_len, int *result)
 *	gets the IP address of an address
 */
static __inline__ void r_ip_of_addr (int addr, char *ipaddr, int h_len, int *a_len, int *result)
{
	char *ch;
	struct in_addr inad;

	inad.s_addr = (unsigned int)htonl(addr);
	ch = inet_ntoa (inad);
	if (strlen (ch) >= h_len) {
		*a_len = 0;
		*result = -1;
	} else {
		*a_len = strlen (ch);
		memcpy (ipaddr, ch, *a_len);
		*result = 0;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_error (occ_socket *sock, char *buffer, int buf_size, int *length)*/
/*
 *	void r_error (occ_socket *sock, char *buffer, int buf_size, int *length)
 *	puts the error string associated with sock->error in `buffer'
 */
static __inline__ void r_error (occ_socket *sock, char *buffer, int buf_size, int *length)
{
	char *ch;
	int x;

	ch = strerror (sock->error);
	x = strlen (ch);
	if (x > buf_size) {
		x = buf_size;
	}
	memcpy (buffer, ch, x);
	*length = x;
}
/*}}}*/
/*{{{  static __inline__ void r_shutdown (occ_socket *sock, int how, int *result)*/
/*
 *	void r_shutdown (occ_socket *sock, int how, int *result)
 *	shuts down part of a socket connection (0 = no more input, 1 = no more output, 2 = both)
 */
static __inline__ void r_shutdown (occ_socket *sock, int how, int *result)
{
	if (sock->fd < 0) {
		*result = -1;
	} else {
		*result = shutdown (sock->fd, how);
	}
}
/*}}}*/
/*{{{  static __inline__ void r_getsockname (occ_socket *sock, int *result)*/
/*
 *	void r_getsockname (occ_socket *sock, int *result)
 *	gets the local name of a socket
 */
static __inline__ void r_getsockname (occ_socket *sock, int *result)
{
	struct sockaddr_in sin;
	int sin_size = sizeof (sin);

	if (sock->fd < 0) {
		*result = -1;
	} else {
		*result = getsockname (sock->fd, (struct sockaddr *)&sin, (socklen_t *)&sin_size);
		if (!*result) {
			sock->local_port = ntohs (sin.sin_port);
			sock->local_addr = ntohl (sin.sin_addr.s_addr);
		}
	}
}
/*}}}*/
/*{{{  static __inline__ void r_getpeername (occ_socket *sock, int *result)*/
/*
 *	void r_getpeername (occ_socket *sock, int *result)
 *	gets the remote name of a socket
 */
static __inline__ void r_getpeername (occ_socket *sock, int *result)
{
	struct sockaddr_in sin;
	int sin_size = sizeof (sin);

	if (sock->fd < 0) {
		*result = -1;
	} else {
		*result = getpeername (sock->fd, (struct sockaddr *)&sin, (socklen_t *)&sin_size);
		if (!*result) {
			sock->remote_port = ntohs (sin.sin_port);
			sock->remote_addr = ntohl (sin.sin_addr.s_addr);
		}
	}
}
/*}}}*/

#if defined(HOSTOS_CYGWIN)

/*{{{  static __inline__ void r_gethostname (char *name, int namelen, int *result)*/
/*
 *	gets the current hostname (win32 api)
 */
static __inline__ void r_gethostname (char *name, int namelen, int *result)
{
	char nbuf[HOST_NAME_MAX];
	unsigned long nlen = HOST_NAME_MAX - 1;

	if (GetComputerName (nbuf, &nlen) == 0) {
		*result = -1;
	} else {
		if (nlen > namelen) {
			*result = namelen;
			memcpy (name, nbuf, namelen);
		} else {
			*result = nlen;
			memcpy (name, nbuf, nlen);
		}
	}
}
/*}}}*/
/*{{{  static __inline__ void r_sethostname (char *name, int namelen, int *result)*/
/*
 *	sets the current hostname (win32 api)
 */
static __inline__ void r_sethostname (char *name, int namelen, int *result)
{
	char nbuf[HOST_NAME_MAX];
	int nlen;

	if (namelen >= HOST_NAME_MAX) {
		nlen = HOST_NAME_MAX - 1;
	} else {
		nlen = namelen;
	}
	memcpy (nbuf, name, nlen);
	nbuf[nlen] = '\0';

	if (SetComputerName (nbuf) == 0) {
		*result = -1;
	} else {
		*result = 0;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_getdomainname (char *name, int namelen, int *result) */
/*
 *	gets the current domainname (win32 api)
 */
static __inline__ void r_getdomainname (char *name, int namelen, int *result) 
{
	*result = 0;
}
/*}}}*/
/*{{{  static __inline__ void r_setdomainname (char *name, int namelen, int *result)*/
/*
 * 	sets the current domainname (...)
 */
static __inline__ void r_setdomainname (char *name, int namelen, int *result)
{
	*result = -1;
}
/*}}}*/

#else	/* !HOSTOS_CYGWIN */

/*{{{  static __inline__ void r_gethostname (char *name, int namelen, int *result)*/
/*
 *	gets the current hostname
 */
static __inline__ void r_gethostname (char *name, int namelen, int *result)
{
	char nbuf[HOST_NAME_MAX];

	if (gethostname (nbuf, HOST_NAME_MAX) < 0) {
		*result = -1;
	} else {
		int i = strlen (nbuf);

		if (i > namelen) {
			*result = namelen;
			memcpy (name, nbuf, namelen);
		} else {
			*result = i;
			memcpy (name, nbuf, i);
		}
	}
}
/*}}}*/
/*{{{  static __inline__ void r_sethostname (char *name, int namelen, int *result)*/
/*
 *	void r_sethostname (char *name, int namelen, int *result)
 *	sets the current hostname (requires root priv.)
 */
static __inline__ void r_sethostname (char *name, int namelen, int *result)
{
	char nbuf[HOST_NAME_MAX];

	if (namelen >= HOST_NAME_MAX) {
		namelen = HOST_NAME_MAX - 1;
		memcpy (nbuf, name, namelen);
		nbuf[namelen] = '\0';
	} else {
		memcpy (nbuf, name, namelen);
		nbuf[namelen] = '\0';
	}
	*result = sethostname (nbuf, namelen);
}
/*}}}*/
/*{{{  static __inline__ void r_getdomainname (char *name, int namelen, int *result) */
/*
 *	void r_getdomainname (char *name, int namelen, int *result)
 *	gets the current domainname (often not set..)
 */
static __inline__ void r_getdomainname (char *name, int namelen, int *result) 
{
	char nbuf[HOST_NAME_MAX];

	if (getdomainname (nbuf, HOST_NAME_MAX) < 0) {
		*result = -1;
	} else {
		int i = strlen (nbuf);

		if (i > namelen) {
			*result = namelen;
			memcpy (name, nbuf, namelen);
		} else {
			*result = 1;
			memcpy (name, nbuf, i);
		}
	}
}
/*}}}*/
/*{{{  static __inline__ void r_setdomainname (char *name, int namelen, int *result)*/
/*
 *	void r_setdomainname (char *name, int namelen, int *result)
 * 	sets the current domainname (...)
 */
static __inline__ void r_setdomainname (char *name, int namelen, int *result)
{
	char nbuf[HOST_NAME_MAX];

	if (namelen >= HOST_NAME_MAX) {
		namelen = HOST_NAME_MAX - 1;
		memcpy (nbuf, name, namelen);
		nbuf[namelen] = '\0';
	} else {
		memcpy (nbuf, name, namelen);
		nbuf[namelen] = '\0';
	}
	*result = setdomainname (nbuf, namelen);
}
	/*}}}*/

#endif	/* !HOSTOS_CYGWIN */

/*{{{  interface functions*/
/*
 *	This may not be entirely pleasant, but it makes things easier to read..
 */
/*{{{  regular socket stuff*/
void _sl_socket (int *w)		{ r_socket ((occ_socket *)(w[0])); }
void _sl_close (int *w)			{ r_close ((occ_socket *)(w[0])); }
void _sl_read (int *w)			{ r_read ((occ_socket *)(w[0]), (char *)(w[1]), (int)(w[2]), (int)(w[3]), (int *)(w[4])); }
void _sl_fullread (int *w)		{ r_fullread ((occ_socket *)(w[0]), (char *)(w[1]), (int)(w[2]), (int)(w[3]), (int *)(w[4])); }
void _sl_write (int *w)			{ r_write ((occ_socket *)(w[0]), (char *)(w[1]), (int)(w[2]), (int *)(w[3])); }
void _sl_write_addr (int *w)		{ r_write_addr ((occ_socket *)(w[0]), (int)(w[1]), (int)(w[2]), (int *)(w[3])); }
void _sl_fullwrite_addr (int *w)	{ r_fullwrite_addr ((occ_socket *)(w[0]), (int)(w[1]), (int)(w[2]), (int *)(w[3])); }
void _sl_fullwrite (int *w)		{ r_fullwrite ((occ_socket *)(w[0]), (char *)(w[1]), (int)(w[2]), (int *)(w[3])); }
void _sl_connect (int *w)		{ r_connect ((occ_socket *)(w[0]), (int *)(w[1])); }
void _sl_listen (int *w)		{ r_listen ((occ_socket *)(w[0]), (int)(w[1]), (int *)(w[2])); }
void _sl_bind (int *w)			{ r_bind ((occ_socket *)(w[0]), (int *)(w[1])); }
void _sl_accept (int *w)		{ r_accept ((occ_socket *)(w[0]), (occ_socket *)(w[1]), (int *)(w[2])); }
void _sl_sendto (int *w)		{ r_sendto ((occ_socket *)(w[0]), (char *)(w[1]), (int)(w[2]), (int)(w[3]), (int *)(w[4])); }
void _sl_recvfrom (int *w)		{ r_recvfrom ((occ_socket *)(w[0]), (char *)(w[1]), (int)(w[2]), (int)(w[3]), (int *)(w[4])); }
void _sl_error (int *w)			{ r_error ((occ_socket *)(w[0]), (char *)(w[1]), (int)(w[2]), (int *)(w[3])); }
void _sl_shutdown (int *w)		{ r_shutdown ((occ_socket *)(w[0]), (int)(w[1]), (int *)(w[2])); }
/*}}}*/
/*{{{  resolution/identity stuff*/
void _sl_setsockopt (int *w)		{ r_setsockopt ((occ_socket *)(w[0]), (int)(w[1]), (int)(w[2]), (int)(w[3]), (int *)(w[4])); }
void _sl_getsockopt (int *w)		{ r_getsockopt ((occ_socket *)(w[0]), (int)(w[1]), (int)(w[2]), (int *)(w[3]), (int *)(w[4])); }
void _sl_addr_of_host (int *w)		{ r_addr_of_host ((char *)(w[0]), (int)(w[1]), (int *)(w[2]), (int *)(w[3])); }
void _sl_addrs_of_host (int *w)		{ r_addrs_of_host ((char *)(w[0]), (int)(w[1]), (int *)(w[2]), (int)(w[3]), (int *)(w[4])); }
void _sl_naddrs_of_host (int *w)	{ r_naddrs_of_host ((char *)(w[0]), (int)(w[1]), (int *)(w[2])); }
void _sl_host_of_addr (int *w)		{ r_host_of_addr ((int)(w[0]), (char *)(w[1]), (int)(w[2]), (int *)(w[3]), (int *)(w[4])); }
void _sl_ip_of_addr (int *w)		{ r_ip_of_addr ((int)(w[0]), (char *)(w[1]), (int)(w[2]), (int *)(w[3]), (int *)(w[4])); }

void _sl_getsockname (int *w)		{ r_getsockname ((occ_socket *)(w[0]), (int *)(w[1])); }
void _sl_getpeername (int *w)		{ r_getpeername ((occ_socket *)(w[0]), (int *)(w[1])); }
void _sl_gethostname (int *w)		{ r_gethostname ((char *)(w[0]), (int)(w[1]), (int *)(w[2])); }
void _sl_sethostname (int *w)		{ r_sethostname ((char *)(w[0]), (int)(w[1]), (int *)(w[2])); }
void _sl_getdomainname (int *w)		{ r_getdomainname ((char *)(w[0]), (int)(w[1]), (int *)(w[2])); }
void _sl_setdomainname (int *w)		{ r_setdomainname ((char *)(w[0]), (int)(w[1]), (int *)(w[2])); }
/*}}}*/
/*}}}*/

