/* $Id: fnidef.h,v 1.1 1996/04/15 10:53:44 djb1 Exp $ */

/*
 *	defines common areas between test program and library
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


/* fni_canonical's can be combined into lists, represented by a list      */
/* structure.                                                             */

typedef struct fni_list_entry
{
  fni_list          cl_next;
  fni_canonical     cl_cfn;
} fni_list_entry;
