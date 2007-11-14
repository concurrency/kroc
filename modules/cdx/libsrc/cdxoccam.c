/*
 *	cdxoccam.c -- C side of occam bindings for cdxclient
 *	Copyright (C) 2005 Fred Barnes <frmb@kent.ac.uk>
 *	                   Adam Sampson <ats1@kent.ac.uk>
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

#include <stdlib.h>
#include <string.h>
#include "cdxclient.h"

/*
#PRAGMA EXTERNAL "PROC C.cdxclient.new (VAL []BYTE host, VAL INT port, width, height, RESULT INT handle) = 0"
#PRAGMA EXTERNAL "PROC C.cdxclient.send (VAL INT handle, VAL INT frame, offset, VAL []INT data, RESULT INT rc) = 0"
#PRAGMA EXTERNAL "PROC C.cdxclient.close (VAL INT handle, RESULT INT rc) = 0"
*/
void real_cdxclient_new (const char *host, int host_len, int port, int width, int height, struct cdxclient **ret) {
	char *c_host = malloc (host_len + 1);
	if (c_host == NULL) {
		*ret = NULL;
		return;
	}
	memcpy (c_host, host, host_len);
	c_host[host_len] = '\0';

	*ret = cdxclient_new (c_host, port, width, height, 32);

	free (c_host);
}
void _cdxclient_new (int *w) {
	real_cdxclient_new ((const char *) w[0], (int) w[1], (int) w[2], (int) w[3], (int) w[4], (struct cdxclient **) w[5]);
}
void real_cdxclient_send (struct cdxclient *c, int frame, int offset, int *data, int data_len, int *ret) {
	*ret = cdxclient_send (c, frame, offset * 4, (const unsigned char *) data, data_len * 4);
}
void _cdxclient_send (int *w) {
	real_cdxclient_send ((struct cdxclient *) w[0], (int) w[1], (int) w[2], (int *) w[3], (int) w[4], (int *) w[5]);
}
void real_cdxclient_close (struct cdxclient *c, int *ret) {
	*ret = cdxclient_close (c);
}
void _cdxclient_close (int *w) {
	real_cdxclient_close ((struct cdxclient *) w[0], (int *) w[1]);
}


