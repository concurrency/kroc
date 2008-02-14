/*
 *	ccsp_if.h -- CCSP exported functions
 *	Copyright (C) 2007 Carl Ritson <cgr@kent.ac.uk>
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

#ifndef __CCSP_IF_H
#define __CCSP_IF_H

extern bool ccsp_init (void);
extern void ccsp_exit (int, bool);

extern void ccsp_default_exit_handler (int, bool);
extern void ccsp_set_exit_handler (void (*)(int, bool));

extern bool ccsp_blocked_on_external_event (void);
extern bool ccsp_external_event_is_bsc (void);
extern bool ccsp_external_event_is_ready (void);
extern void ccsp_set_external_event_hook (bool bsc, bool (*ready)(void), bool (*blocked)(void));

extern void ccsp_set_branding (char *);
extern void ccsp_set_error_mode (int);

extern void ccsp_kernel_entry (word *, word *);
extern void ccsp_occam_entry (void *, unsigned int, word, word *, word *);

extern void ccsp_cif_bcall0_stub (word *);
extern void ccsp_cif_bcall1_stub (word *);
extern void ccsp_cif_bcalln_stub (word *);

extern void ccsp_decode_debug_insert (int offset, const char **filename, int *line);
extern void ccsp_show_last_debug_insert (void);

extern void *ccsp_mt_alloc (word type, word size);
extern void ccsp_mt_release (void *ptr);
extern word *ccsp_proc_alloc (word flags, word stack);

#endif	/* !__CCSP_IF_H */

