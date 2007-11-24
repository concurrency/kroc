/*
 *	machine.h - specific target stuff
 *	Copyright (C) 2000-2004 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __MACHINE_H
#define __MACHINE_H

#define TARGET_DEFAULT		0x0
#define TARGET_X86		0x1
#define TARGET_MIPS		0x2
#define TARGET_SPARC		0x3
#define TARGET_POWERPC		0x4

#define CLASS_UNKNOWN		0x0

#define CLASS_386		0x386
#define CLASS_486		0x486
#define CLASS_586		0x586
#define CLASS_686		0x686

#define CLASS_R3000		0x3000
#define CLASS_R6000		0x6000
#define CLASS_R4300		0x4300
#define CLASS_R4600		0x4600
#define CLASS_R5000		0x5000
#define CLASS_NEVADA		0x1fff
#define CLASS_R8000		0x8000
#define CLASS_R10000		0x10000

#define CLASS_SPARCV8		0xbeef

#define CLASS_POWERPC		0xbeef0

#define OPTION_MMX		0x01
#define OPTION_CMOVC		0x02
#define OPTION_FCOMI		0x04
#define OPTION_CMPXCHG8		0x08
#define OPTION_FXSR		0x10
#define OPTION_XMM		0x20
#define OPTION_SYSENTER		0x40
#define OPTION_SSE2		0x80

#endif	/* !__MACHINE_H */

