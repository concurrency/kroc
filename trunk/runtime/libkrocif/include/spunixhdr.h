/*
 *	KRoC interface to SP library
 *	Copyright (C) 1996 Michael Poole
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


#ifndef __SPUNIXHDR_H
#define __SPUNIXHDR_H

void call_occam_exit (void);
void C_fopen (int *handle, int p_name, int p_mode);
void C_fflush (int *result, int handle);
void C_fclose (int *result, int handle);
void C_fread (int *result, int handle, int p_buffer, int SIZEbuffer, int *bytes_read);
void C_fgets (int *result, int handle, int p_buffer, int SIZEbuffer, int *bytes_read);
void C_fwrite (int *result, int handle, int p_buffer, int SIZEbuffer, int *bytes_written);
void C_fremove (int *result, int p_fname);
void C_frename (int *result, int p_oldname, int p_newname);
void C_fseek (int *result, int handle, int origin, int position);
void C_ftell (int *result, int handle, int *position);
void C_comdline (int *result, int all, int *len, int p_block, int SIZEblock);
void C_getenv (int *result, int p_envname, int *len, int p_block, int SIZEblock);
void C_time (int *loctime, int *UTCtime);
void C_system (int *result, int *status, int p_block);
void C_exit (int *result, int status);
void C_getkey (int *keyval);
void C_pollkey (int *result, int *keyval);

#endif	/* !__SPUNIXHDR_H */

