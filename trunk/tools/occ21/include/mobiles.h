/*
 *	mobiles.h -- prototypes of MOBILE helpers
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

#ifndef __MOBILES_H
#define __MOBILES_H

extern treenode *mobile_getanontype (treenode *protocol, int lexlevel);
extern wordnode *mobile_getanonname_cli (wordnode *vname);
extern wordnode *mobile_getanonname_svr (wordnode *vname);
extern treenode *mobile_getanon_fromvar (treenode *var, BOOL server, BOOL withchan, treenode *varin);
extern treenode *mobile_getanontypes (void);

extern int mobile_gettypedesc (treenode *const type, unsigned int *buf, int *len, int max);
extern int mobile_typedescfixup_start (int lab);
extern int mobile_typedescfixup_finish (int lab, void (*gencomment)(const char *));

#endif	/* !__MOBILES_H */

