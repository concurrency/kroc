/*
 *	etcrtl.h - interface to etcrtl.c
 *	Copyright (C) 2000-2005 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __ETCRTL_H
#define __ETCRTL_H

extern rtl_chain *etc_to_rtl (etc_chain *etc_code, arch_t *arch);
extern int get_last_lab (void);

/* these are only called during the translation step from helper functions */
extern void add_to_ins_chain (ins_chain *ins);
extern void flush_ins_chain (void);
extern void add_to_rtl_chain (rtl_chain *rtl);
extern void add_chain_to_ins_chain (ins_chain *start);
extern void add_chain_to_rtl_chain (rtl_chain *start);
extern void add_rtl_to_spec_chain (rtl_chain *rtl, rtl_chain **head, rtl_chain **tail);
extern void declare_data_bytes (char *data, int len);
extern ins_chain *scan_ins_chain_back (int instr);

#endif	/* !__ETCRTL_H */

