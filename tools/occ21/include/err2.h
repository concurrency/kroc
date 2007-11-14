/* $Id: err2.h,v 1.1 1996/04/15 10:52:03 djb1 Exp $ */

/*
 *	error string functions
 *	Copyright (C) 1992 Inmos Limited
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


/*{{{  functions */
PUBLIC const char *anymessagestring(int n);
PUBLIC const char *synmessagestring(int n);
PUBLIC const char *chkmessagestring(int n);
PUBLIC const char *usemessagestring(int n);
PUBLIC const char *vtimessagestring(int n);
/*}}}*/

/*{{{  side-effects pragmas */
#ifdef _ICC
#pragma IMS_nosideeffects (anymessagestring)
#pragma IMS_nosideeffects (synmessagestring)
#pragma IMS_nosideeffects (chkmessagestring)
#pragma IMS_nosideeffects (usemessagestring)
#pragma IMS_nosideeffects (vtimessagestring)
#endif
/*}}}*/
