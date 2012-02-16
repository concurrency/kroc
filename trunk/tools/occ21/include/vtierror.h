/* $Id: vtierror.h,v 1.1 1996/04/15 10:52:26 djb1 Exp $ */

/*
 *	tree error numbers
 *	Copyright (C) 1987 Inmos Limited
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


/*{{{  error codes */
/*#define VTI_OUT_OF_SPACE        1*/
/*#define VTI_STACK_OVERFLOW      2*/
/*#define VTI_STACK_UNDERFLOW     3*/
/*#define VTI_BAD_TAG             4*/
/*#define VTI_NAMETRANS_OVERFLOW  5*/
#define VTI_NAMETRANS_UNDERFLOW 6
/*#define VTI_TOO_MANY_NAMETRANS  7*/
#define VTI_ARRAY_SIZE_OVERFLOW 8
#define VTI_EXPAND_NOT_CONSTANT 9
/*}}}*/

/*{{{  error macros */
#define vtiabort(N,L)      msg_out(SEV_ABORT,  VTI,(N),(L))
/*}}}*/


