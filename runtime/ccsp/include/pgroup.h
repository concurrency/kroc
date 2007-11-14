/*
 *	pgroup.h -- interface to process groups
 *	Copyright (C) 2005 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __PGROUP_H
#define __PGROUP_H

#if defined(PROCESS_GROUPS)

struct TAG_pgrpinfo {
	int nprocs;		/* number of processes in this group */
};

typedef struct TAG_pgrpinfo pgrpinfo_t;

extern pgrpinfo_t default_pgroup;


extern pgrpinfo_t *pgrp_newgrp (void);
extern void pgrp_freegrp (pgrpinfo_t *group);


#endif	/* PROCESS_GROUPS */
#endif	/* __PGROUP_H */

