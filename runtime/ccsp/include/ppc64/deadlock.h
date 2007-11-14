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

#ifndef __DEADLOCK_H
#define __DEADLOCK_H

#ifdef __DEADLOCK_C
/* architecture dependant stuff */
#define PPC_JUMP_INS	0x48

#define DEADLOCK_CODE_BLOCK(FN,PN,CP) \
	__asm__ __volatile__ ( \
		"	mr	0, %2		\n" \
		"	mtlr	0		\n" \
		"	blrl			\n" \
		"	mr	%0, 6		\n" \
		"	mr	%1, 7		\n" \
		: "=g" (FN), "=g" (PN) \
		: "g" (CP) \
		: "cc", "memory", "6", "7", "lr")
#endif	/* __DEADLOCK_C */

#endif	/* !__DEADLOCK_H */

