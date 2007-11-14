/*
 *	stugtable.h -- for handling STUBENTRY's
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

#ifndef __STUBTABLE_H
#define __STUBTABLE_H

typedef struct TAG_stublist {
	struct TAG_stublist *next;
	int lab;			/* label for stub */
	ins_chain *stubcode;		/* what should be placed instead of `call lab' */
} stublist;

extern void add_to_stubtable (int lab, ins_chain *c_first, ins_chain *c_last);
extern int label_is_stub (int lab);
extern ins_chain *get_stubcode (tstate *ts, int lab);


#endif	/* !__STUBTABLE_H */

