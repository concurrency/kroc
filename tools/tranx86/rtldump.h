/*
 *	rtldump.h - interface to RTL dumper
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


#ifndef __RTLDUMP_H
#define __RTLDUMP_H

extern int dump_textual_rtl (rtl_chain *rtl_code, char *outfile, arch_t *arch);
extern void dump_ins_chunk (FILE *stream, ins_chain *start, ins_chain *stop, arch_t *arch);

#endif	/* !__RTLDUMP_H */

