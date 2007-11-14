/*
 *	krocif.h -- visible bits of the KRoC top-level interface library
 *	Copyright (C) 2006 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __KROCIO_H
#define __KROCIO_H

extern int init_occam_io (int tlpiface);
extern bool kbd_ready (void);
extern bool process_blocked_on_kbd (void);

extern word *kbd_chan_addr (void);
extern word *scr_chan_addr (void);
extern word *err_chan_addr (void);

extern word *kbd_workspace (void);
extern word *scr_workspace (void);
extern word *err_workspace (void);

extern void init_kbdio (int is_a_tty);
/* extern int kill_kbdio (void); */

#endif	/* !__KROCIO_H */

