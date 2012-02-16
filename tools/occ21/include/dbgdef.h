/* $Id: dbgdef.h,v 1.1 1996/04/15 10:52:02 djb1 Exp $ */

/*
 *	define DBG macros
 *	Copyright (C) 1994 Inmos limited
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

/*
 * To activate, define DBG when compiling and link in variant of library
 * that ends with suffix .dbg, e.g. sun4 aiolib with trace is called 
 * aiolib.a.dbg
 *
 * Ade 28/4/94 - Created
 */

#ifndef	DBGDEF_H
#define DBGDEF_H

#ifdef DBG

/* Switch tracing on    */

#define DBG_START       dbg_start();
#define DBG_FINISH      dbg_finish();
#define DBG_PRINT(arg)  dbg_print arg;
#define DBG_ENTER(arg)  dbg_enter arg;
#define DBG_RETURN(arg)  dbg_return arg;

void dbg_start(void);
void dbg_finish(void);
void dbg_print(char *fmt, ...);
void dbg_enter(char *func, char *fmt, ...);
void dbg_return(char *func, char *fmt, ...);

#else   
                                  /* DBG lines removed  - no trace */
#define DBG_START 
#define DBG_FINISH
#define DBG_PRINT(arg)
#define DBG_ENTER(arg)
#define DBG_RETURN(arg)

#endif  /* DBG */
#endif	/* DBGDEF_H */

