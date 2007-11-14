/* $Id: usehdr.h,v 1.1 1996/04/15 10:52:24 djb1 Exp $ */

/*
 *	alias and usage checker
 *	Copyright (C) 1987 Inmos Limited
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

/*{{{  use_mode_t */
typedef enum {
    EXP_READ,
    EXP_WRITTEN,
    CHAN_INPUT,
    CHAN_OUTPUT,
    CHAN_XINPUT		/* only in undefined checker! (use4) */
} use_mode_t;
/*}}}*/

#define CHAN_UNUSED            0x0
#define CHAN_USE_INPUT         0x1
#define CHAN_USE_OUTPUT        0x2
#define CHAN_USE_BIDIRECTIONAL (CHAN_USE_INPUT | CHAN_USE_OUTPUT)

#define NFreeVarsOf(N)     ((varlist *)NCheckerOf(N))
#define SetNFreeVars(N,V)  SetNChecker(N,V)
#define NParamUseOf(N)     NFreeVarsOf(N)
#define SetNParamUse(N,V)  SetNFreeVars(N,V)

/*{{{  subscrlist */
struct subscrnode {
    struct subscrnode *sl_next;
    INT32              sl_first;
    INT32              sl_last;
};
typedef struct subscrnode subscrlist;

#define SLNextOf(T)  ((T)->sl_next)
#define SLFirstOf(T) ((T)->sl_first)
#define SLLastOf(T)  ((T)->sl_last)
#define SetSLNext(T,V)  ((T)->sl_next  = (V))
#define SetSLFirst(T,V) ((T)->sl_first = (V))
#define SetSLLast(T,V)  ((T)->sl_last  = (V))
/*}}}*/
/*{{{  varlist */
struct varnode {
    struct varnode *vl_next;
    treenode       *vl_name;
    subscrlist     *vl_read;     /* read list */
    subscrlist     *vl_written;  /* written list */
    subscrlist     *vl_input;    /* input list */
    subscrlist     *vl_output;   /* output list */
};
typedef struct varnode varlist;

#define VLNextOf(T)    ((T)->vl_next)
#define VLNameOf(T)    ((T)->vl_name)
#define VLReadOf(T)    ((T)->vl_read)
#define VLWrittenOf(T) ((T)->vl_written)
#define VLInputOf(T)   ((T)->vl_input)
#define VLOutputOf(T)  ((T)->vl_output)
#define SetVLNext(T,V)    ((T)->vl_next = (V))
#define SetVLName(T,V)    ((T)->vl_name = (V))
#define SetVLRead(T,V)    ((T)->vl_read = (V))
#define SetVLWritten(T,V) ((T)->vl_written = (V))
#define SetVLInput(T,V)   ((T)->vl_input = (V))
#define SetVLOutput(T,V)  ((T)->vl_output = (V))
/*}}}*/
/*{{{  abbrevlist */
struct abbrevnode {
    struct abbrevnode *al_next;
    treenode          *al_name;
    SOURCEPOSN         al_loc;    /* line of abbreviation */
    INT32              al_first;  /* first subscript accessed, -1 if unknown */
    INT32              al_last;   /* last subscript accessed */
    treenode          *al_subscripts; /* Tree representing subscripts used
                                         if al_first is -1 */
};
typedef struct abbrevnode abbrevlist;

#define ALNextOf(T)  ((T)->al_next)
#define ALNameOf(T)  ((T)->al_name)
#define ALLocOf(T)   ((T)->al_loc)
#define ALFirstOf(T) ((T)->al_first)
#define ALLastOf(T)  ((T)->al_last)
#define ALSubscriptsOf(T) ((T)->al_subscripts)
#define SetALNext(T,V)  ((T)->al_next = (V))
#define SetALName(T,V)  ((T)->al_name = (V))
#define SetALLoc(T,V)   ((T)->al_loc = (V))
#define SetALFirst(T,V) ((T)->al_first = (V))
#define SetALLast(T,V)  ((T)->al_last = (V))
#define SetALSubscripts(T,V) ((T)->al_subscripts = (V))
/*}}}*/
/*{{{  errorlist */
struct errornode {
    struct errornode *er_next;
    char              er_warning;
    char              er_code;
    SOURCEPOSN        er_locn;
    treenode         *er_p1;
};
typedef struct errornode errorlist;

#define ERNextOf(T)    ((T)->er_next)
#define ERWarnOf(T)    ((T)->er_warning)
#define ERCodeOf(T)    ((T)->er_code)
#define ERLocnOf(T)    ((T)->er_locn)
#define ERP1Of(T)      ((T)->er_p1)
#define SetERNext(T,V) ((T)->er_next = (V))
#define SetERWarn(T,V) ((T)->er_warning = (V))
#define SetERCode(T,V) ((T)->er_code = (V))
#define SetERLocn(T,V) ((T)->er_locn = (V))
#define SetERP1(T,V)   ((T)->er_p1 = (V))
/*}}}*/

#define ParamInputOn(N)    ((NParamUseOf(N) != NULL) && \
                           (VLInputOf(NParamUseOf(N)) != NULL))
#define ParamOutputOn(N)   ((NParamUseOf(N) != NULL) && \
                           (VLOutputOf(NParamUseOf(N)) != NULL))

#define MAXINDEX 0x7fffffff
