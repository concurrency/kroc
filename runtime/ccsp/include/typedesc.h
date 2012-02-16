/*
 *	typedesc.h -- type description constants from occ21
 *	Copyright (C) 2005 Fred Barnes <frmb@kent.ac.uk>
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

/* documentation for the encoding/etc. at
 * http://www.cs.kent.ac.uk/projects/ofa/kroc/cttd.html
 */

#ifndef __TYPEDESC_H
#define __TYPEDESC_H

/* generation 1 mobile type IDs for describing chan-types and protocols */
#define MTID_PRIM 0			/* simple types */
#define MTID_SEQPROTO 1			/* sequential protocol */
#define MTID_CHANTYPE 2			/* channel-type (contents-of) */
#define MTID_MCHANEND_IU 3		/* input-unshared mobile channel-end */
#define MTID_MCHANEND_OU 4		/* output-unshared mobile channel-end */
#define MTID_MCHANEND_IS 5		/* input-shared mobile channel-end */
#define MTID_MCHANEND_OS 6		/* output-shared mobile channel-end */
#define MTID_CHAN_I 7			/* input channel */
#define MTID_CHAN_O 8			/* output channel */
#define MTID_RECORD 9			/* record type */
#define MTID_TAGPROTO 10		/* variant protocol */
#define MTID_MARRAY 11			/* dynamic mobile array */
#define MTID_MOBILE 12			/* static mobile type */
#define MTID_ARRAY 13			/* fixed-size array type */
#define MTID_COUNTED 14			/* counted array protocol */
#define MTID_TAG 15			/* tag for a variant protocol */
#define MTID_MBARRIER 16		/* mobile barrier type */
#define MTID_MPROC 17			/* mobile process */
#define MTID_STYPE 18			/* structured type */
#define MTID_FIELD 19			/* field type */
#define MTID_UNKNOWN 255		/* unknown/bad */

#endif	/* !__TYPEDESC_H */

