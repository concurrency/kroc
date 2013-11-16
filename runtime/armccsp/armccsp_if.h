/*
 *	armccsp_if.h -- ARM/CCSP public interface for CIF style code
 *	Copyright (C) 2013 Fred Barnes, University of Kent <frmb@kent.ac.uk>
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
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 */

#ifndef __ARMCCSP_IF_H
#define __ARMCCSP_IF_H


typedef uint32_t *Workspace;
typedef void *Channel;


/* kernel call constants, must stay consistent with table in kernel.c */
#define CALL_CHANOUT 0


/* these should only ever be called in the context of the main program, i.e. once from main() */
extern Workspace ProcAllocInitial (const int paramwords, const int stackwords);

extern void ProcStartInitial_blind (Workspace p, void (*fcn)(Workspace, ...));
#define ProcStartInitial(P,FCN) ProcStartInitial_blind(P,(void (*)(Workspace, ...))FCN)


#endif	/* !__ARMCCSP_IF_H */

