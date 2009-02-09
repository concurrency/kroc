/*
 *	Interface to the deadlock detection stuff
 *	Copyright (C) 2000 Fred Barnes
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


#ifndef I386_DEADLOCK_H
#define I386_DEADLOCK_H

#ifdef __DEADLOCK_C
/* architecture dependant stuff */
#define I386_JUMP_INS	0xe9

#if 0
#define DEADLOCK_CODE_BLOCK(FN,PN,CP) \
	do { \
		int DCBtmp1, DCBtmp2; \
	__asm__ __volatile__ ( "\n" \
		"	call *%%eax		\n" \
		: "=a" (DCBtmp1), "=b" (DCBtmp2) \
		: "a" (CP) \
		: "cc", "memory"); \
		*(int *)&(FN) = DCBtmp1; \
		*(int *)&(PN) = DCBtmp2; \
	} while (0)
#endif
#define DEADLOCK_CODE_BLOCK(FN,PN,CP) \
	__asm__ __volatile__ ( "\n" \
		"	call *%%eax		\n" \
		: "=a" (FN), "=c" (PN) \
		: "a" (CP) \
		: "cc", "memory")

#endif	/* __DEADLOCK_C */

#endif	/* !I386_DEADLOCK_H */

