/*{{{  module header */

/*
 *	memory manager model library header file
 *	Copyright (C) 1993 Inmos Limited
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

/*}}}*/

#ifndef _MMMLIBH
/*{{{  not included header file */
#define _MMMLIBH

/*{{{  include files */
#ifndef _IMSTYPEH
#include "imstype.h"
#endif

#ifndef _IMSMISCH
#include "imsmisc.h"
#endif

#ifndef _IMSSTDH
#include "imsstd.h"
#endif
/*}}}*/

/*{{{  typedef definitions */
/*{{{  constant typedefs */
/*{{{  enum MMM_Status */
typedef enum MMM_Status
{
    /*{{{   */
    MMM_StatusNone,
    
    MMM_StatusOk,
    MMM_StatusNotOk
    /*}}}*/
}
MMM_StatusT;
/*}}}*/
/*}}}*/

/*{{{  structure typedefs */
/*{{{  struct MMM_Memory */
typedef struct MMM_Memory *MMM_MemoryT;
/*}}}*/
/*}}}*/
/*}}}*/

/*{{{  constant definitions */
#define MMM_NIL (-1) /* Undefined integer value */

/* Version has 3 byte fields: 0 - minor, 1 - major, 2 - release */

#define MMM_LIBRARY_VERSION 0X000001 /* Library version number */

#define MMM_INFO_NORMAL      0 /* Information level flags */
#define MMM_INFO_INFORMATION 1
#define MMM_INFO_VERBOSE     2
#define MMM_INFO_DEBUG       3
/*}}}*/

/*{{{  prototype definitions */
/*{{{  from mmmhigh.c */
EXTERNAL MMM_StatusT MMM_Open (VAL BIT32, MMM_MemoryT *);

EXTERNAL MMM_StatusT MMM_Close (MMM_MemoryT *);

EXTERNAL MMM_StatusT MMM_FreeChk (VAL MMM_MemoryT, POINTER, VAL WORD);

EXTERNAL MMM_StatusT MMM_MallocChk (VAL MMM_MemoryT, VAL WORD, POINTER *);

EXTERNAL MMM_StatusT MMM_CallocChk (VAL MMM_MemoryT, VAL WORD, VAL WORD, POINTER *);

EXTERNAL MMM_StatusT MMM_ReallocChk (VAL MMM_MemoryT, POINTER, VAL WORD, VAL WORD, POINTER *);

EXTERNAL MMM_StatusT MMM_MallocCpy (VAL MMM_MemoryT, VALPOINTER, VAL WORD, POINTER *);
/*}}}*/

/*{{{  from mmmstat.c */
EXTERNAL MMM_StatusT MMM_DumpStatistics (VAL MMM_MemoryT);
/*}}}*/
/*}}}*/
/*}}}*/
#endif
