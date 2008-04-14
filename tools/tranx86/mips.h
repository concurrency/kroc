/*
 *	mips.h -- MIPS related stuffs
 *	Copyright (C) 2002 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __MIPS_H
#define __MIPS_H

/* Register defines */
#define MIPS_REG_0   0 
#define MIPS_REG_2   2 
#define MIPS_REG_3   3 
#define MIPS_REG_4   4
#define MIPS_REG_5   5
#define MIPS_REG_11 11
#define MIPS_REG_12 12
#define MIPS_REG_13 13
#define MIPS_REG_14 14
#define MIPS_REG_15 15
#define MIPS_REG_24 24
#define MIPS_REG_25 25
#define MIPS_REG_29 29
#define MIPS_REG_31 31

/* Special registers */
#define MIPS_REG_HI -100
#define MIPS_REG_LO -101

/* Register name defiens */
#define MIPS_REG_ZERO MIPS_REG_0
#define MIPS_REG_V0 MIPS_REG_2
#define MIPS_REG_V1 MIPS_REG_3
#define MIPS_REG_A0 MIPS_REG_4
#define MIPS_REG_A1 MIPS_REG_5
#define MIPS_REG_SP MIPS_REG_29
#define MIPS_REG_RA	MIPS_REG_31

/* FIXME: (Possibly) Temporary register fudges */
#define MIPS_KROC_TMP_REG MIPS_REG_24

#endif	/* !__MIPS_H */

