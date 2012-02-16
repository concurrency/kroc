/*
 *	CCSP kernel C macro scheduler definitions
 *	Copyright (C) 1995, 1996, 1997 D.J. Beckett, D.C. Wood
 *	Copyright (C) 1998 Jim Moores
 *	Modifications (C) 2002 Fred Barnes <frmb@kent.ac.uk>
 *	Modifications (C) 2007 Carl Ritson <cgr@kent.ac.uk>
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

#ifndef __SCHED_CONSTS_H
#define __SCHED_CONSTS_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if 0
/* This file is included in assembler files so may not always be able
   to use the new ANSI C ## operator in the C pre-processor */
#ifdef __STDC__
#  define CAT(a,b) a ## b
#else
#  define _IDENT(a) a
#  define CAT(a,b) _IDENT(a)b
#endif

#ifdef HAVE_C_LEADING_UNDERSCORE
#  define MAKENAME(n) CAT(_,n)
#else
#  define MAKENAME(n) n
#endif
#endif

#include "ccsp_consts.h"

/* kernel features (mostly deprecated) */
#define KERNEL_RUN                      /* Allow position-independent calls */
#define IMPOSSIBLE			/* Check for impossible errors */
#undef  T9000                           /* Not fully implemented */
#undef  VPTR                            /* Under development */

/* workspace shift (bytes -> words) */
#define WSH		2		/* FIXME: should come from arch config */

/* scheduler sync flags */
#define SYNC_INTR_BIT	1
#define SYNC_TIME_BIT	2
#define SYNC_BMAIL_BIT	4		/* batch mail */
#define SYNC_PMAIL_BIT	5		/* process mail */
#define SYNC_WORK_BIT	6
#define SYNC_TQ_BIT	7		/* timer queue dirty */

#define SYNC_INTR 	(1 << SYNC_INTR_BIT)
#define SYNC_TIME 	(1 << SYNC_TIME_BIT)
#define SYNC_BMAIL	(1 << SYNC_BMAIL_BIT)
#define SYNC_PMAIL	(1 << SYNC_PMAIL_BIT)
#define SYNC_MAIL	(SYNC_BMAIL | SYNC_PMAIL)
#define SYNC_WORK	(1 << SYNC_WORK_BIT)
#define SYNC_TQ		(1 << SYNC_TQ_BIT)

/* batch constants */
#define BATCH_EMPTIED	0x40000000	/* must not make an int negative */
#define BATCH_PPD	8		/* per-process dispatch */
#define BATCH_PPD_SHIFT	3		/* per-process dispatch, as a bit-shift */
#define BATCH_MD_MASK	0x7f		/* maximum batch dispatches as a mask */

/* mwindow constants */
#define MWINDOW_BM_OFFSET	8
#define MWINDOW_HEAD(s)		((s) & 0xff)
#define MWINDOW_NEW_STATE(s,h)	((((s) | (0x100 << (h))) & ~0xff) | (h))
#define MWINDOW_STATE		0

/* runqueue constants */
#define RUNQUEUE_LOCAL		0x1
#define RUNQUEUE_PENDING 	0x2

/* priority defines */
#define MAX_PRIORITY_LEVELS 	(32)

#endif	/* !__SCHED_CONSTS_H */

