/*
 *	cdxclient.h -- cluster DXRaster client library
 *	Copyright (C) 2005-2006 Fred Barnes <frmb@kent.ac.uk>
 *	                        Adam Sampson <ats1@kent.ac.uk>
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

struct cdxclient;

struct cdxclient *cdxclient_new (const char *host, int port,
                                 int width, int height, int depth);
int cdxclient_send (struct cdxclient *c, int frame, int offset,
                    const unsigned char *data, int data_size);
int cdxclient_close (struct cdxclient *c);


