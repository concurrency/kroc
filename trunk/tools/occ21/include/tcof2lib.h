/* $Id: tcof2lib.h,v 1.1 1996/04/15 10:52:21 djb1 Exp $ */

/*
 *	contents of tcofflib, TCOFF and LFF i/o
 *	Copyright (C) 1994 Inmos Limited
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

#ifndef TCOFFLIB_H
#define TCOFFLIB_H

#include "imstype.h"
#include "aiodefs.h"

/*{{{  TCOFF */
#define     tcoff_geti(aio_h, val)  tcoff_eoi_geti(aio_h, val, TRUE)

Aio_return_t    (tcoff_geti)( Aio_handle_t  aio_h,
                            INT32         *val);

Aio_return_t    tcoff_eoi_geti( Aio_handle_t  aio_h,
                                INT32         *val,
                                BOOL          report_eoi);

Aio_return_t    tcoff_nocheck_geti( Aio_handle_t  aio_h,
                                    INT32         *val);
                            
Aio_return_t    tcoff_puti( Aio_handle_t  aio_h,
                            INT32         val);


Aio_return_t    tcoff_gets( Aio_handle_t aio_h,
                            size_t       *str_len,
                            char         **str,
                            BOOL         do_alloc);

Aio_return_t    tcoff_puts( Aio_handle_t aio_h,
                            size_t       str_len,
                            char const   *str);


Aio_return_t    tcoff_putrec( Aio_handle_t   aio_h,
                              INT32          tag,
                              char const     *fmt,
                              ...);

Aio_return_t tcoff_scan_recfmt( Aio_handle_t	aio_h,
				INT32		*reclen,
				char const	*fmt, 
				va_list 	ap,
				BOOL		do_print );

size_t tcoff_size_int(INT32 val);
/*}}}*/

/*{{{  LFF */
Aio_return_t    lff_geti( Aio_handle_t  aio_h,
                          INT32         *val);


Aio_return_t    lff_puti( Aio_handle_t  aio_h,
                          INT32         val);


Aio_return_t    lff_gets( Aio_handle_t aio_h,
                          size_t       *str_len,
                          char         **str,
                          BOOL         do_alloc );

Aio_return_t    lff_puts( Aio_handle_t  aio_h,
                          size_t        str_len,
                          char const    *str);
/*}}}*/

const char *tcoff_version (void);  /* returns pointer to version string */

#endif  /* TCOFFLIB_H */
