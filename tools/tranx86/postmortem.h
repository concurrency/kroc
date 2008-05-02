/*
 *	postmortem.h - post mortem debugging constants
 *	Copyright (C) 2000 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __POSTMORTEM_H
#define __POSTMORTEM_H

#define PMOP_INVALID 0
#define PMOP_ADD 1
#define PMOP_SUB 2
#define PMOP_MUL 3
#define PMOP_DIV 4
#define PMOP_REM 5
#define PMOP_LADD 6
#define PMOP_LSUB 7
#define PMOP_ADC 8
#define PMOP_STOP 9
#define PMOP_LDIV 10
#define PMOP_FMUL 11
#define PMOP_FPCHKI32 12
#define PMOP_FPCHKI64 13

#define DLOP_INVALID 0
#define DLOP_IN 1
#define DLOP_OUT 2
#define DLOP_OUTBYTE 3
#define DLOP_OUTWORD 4
#define DLOP_ALTWT 5
#define DLOP_TALTWT 6
#define DLOP_XABLE 7
#define DLOP_SYNC 8

#define REOP_INVALID 0
#define REOP_SHIFT 1
#define REOP_CSNGL 2
#define REOP_CSUB0 3
#define REOP_CCNT1 4
#define REOP_CWORD 5

#endif	/* !__POSTMORTEM_H */

