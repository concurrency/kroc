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

#ifndef __CCSP_CONSTS_H
#define __CCSP_CONSTS_H

/* Constants used in transputer instructions */
#define MostNeg 		0x80000000
#define NotProcess_p 		0
#if 0
#define Enabling_p 		NotProcess_p+1
#define Waiting_p 		NotProcess_p+2
#define Ready_p 		NotProcess_p+3
#define Buffered_p 		NotProcess_p+4
#endif
#define TimeSet_p 		NotProcess_p+1
#define TimeNotSet_p 		NotProcess_p+2
#define NoneSelected_o 		MostNeg		/* must not be 0 or a plausible address */

/* ALT flag bits */
#define ALT_ENABLING_BIT	30
#define ALT_ENABLING		(1 << ALT_ENABLING_BIT)
#define ALT_WAITING_BIT 	29
#define ALT_WAITING 		(1 << ALT_WAITING_BIT)
#define ALT_NOT_READY_BIT	28
#define ALT_NOT_READY		(1 << ALT_NOT_READY_BIT)
#define ALT_GUARDS		0xffffff

/* Workspace offsets */
#define SavedPriority	2
#define Count 		1
#define IptrSucc 	0
#define Temp 		0
#define Iptr 		-1
#define Link 		-2
#define Priofinity	-3
#define Pointer		-4
#define State		-4
#define TLink		-5
#define Time_f		-6
#define SchedPtr	-7	/* for CIF */
#define StackPtr	-7	/* for CIF */
#define BarrierPtr	-8	/* for CIF */
#define EscapePtr	-9	/* for CIF */

/* buffered channel constants */
#ifdef BUFFERED_CHANNELS
	#define BCTemp 0
	#define BCWaitInput 1
	#define BCWaitOutput 2
	#define BCFreeSlots 3
	#define BCSizeMask 4
	#define BCBufHead 5
	#define BCBufTail 6
	#define BCMagic 7
	#define BCBufStart 8

	#define BCMagicConst 0xfeedbeef
#endif	/* BUFFERED_CHANNELS */

/* priority and affinity macros */
#define AFFINITY_MASK (0xffffffe0)
#define AFFINITY_SHIFT (5)
#define PRIORITY_MASK (0x0000001f)
#define PHasAffinity(X) ((X) & AFFINITY_MASK)
#define PAffinity(X) (((X) & AFFINITY_MASK) >> AFFINITY_SHIFT)
#define PPriority(X) ((X) & PRIORITY_MASK)
#define BuildPriofinity(A,P) \
	( \
		(((A) << AFFINITY_SHIFT) & AFFINITY_MASK) \
		| \
		((P) & PRIORITY_MASK) \
	)

/* CIF constants */
#define CIF_STACK_ALIGN		4 /* words */
#define CIF_STACK_LINKAGE	1 /* words */
#define CIF_PROCESS_WORDS	8 /* words */

#endif /* __CCSP_CONSTS_H */

