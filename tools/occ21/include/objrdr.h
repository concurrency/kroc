/* $Id: objrdr.h,v 1.1 1996/04/15 10:52:15 djb1 Exp $ */

/*
 *	Object file reading
 *	Copyright (C) 1987 Inmos Limted
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


/*{{{  function prototypes */
FILE *open_descfile (const char *name, int mode);
char *readdescriptorline(char *line, BOOL in_stdlib, int *line_len);

BOOL init_external (const char *name, const int dynext);
const char *readexternalline (int *line_len);

void addtoentrypointlist (treenode *nptr);
void patchdescriptors(const char *filename);

treenode *local_loadstdlib (const char *filename);

void init_obj_reader (void);

/*}}}*/

