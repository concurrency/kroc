/* $Id: trandef.h,v 1.1 1996/04/15 10:52:22 djb1 Exp $ */

/*
 *	code generator tran definitions
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

#ifndef TRANDEF_H
#define TRANDEF_H

/*{{{  declarations */
/*{{{  abbreviation modes */
typedef enum
  {
    AM_CONST,     /* Known constant value */
    AM_ISRHS,     /* Overlaid onto rhs variable */
    AM_VAL,       /* 'Value'; stored in workspace (or vecspace) */
    AM_PTR,       /* 'Pointer'; a pointer is stored in workspace */
    AM_COPYINOUT, /* Copy in and copy out, into a local copy */
    AM_NOTHING,   /* No storage required (eg TIMER) */
  } abbrevmode_t;
/*}}}*/
/*{{{  vectorspace criteria */
/* An ARRAY object of size greater than MAX_WS_ARRAY_BYTES bytes will be placed
   in vector space if vector space is enabled, otherwise the object will be
   placed in workspace.
   A RECORD object of size greater than MAX_WS_RECORD_BYTES bytes will be placed
   in vector space if vector space is enabled, otherwise the object will be
   placed in workspace.
   Vector space should really be called 'large object space'

   Conceptually, RECORDs are always accessed by constant offsets, so records
   are better placed into local workspace. However, very large records
   should be placed into vectorspace.
   Arrays are more likely to be accessed via pointers (either passed by
   reference, or variable subscripts), so have less to gain by being
   in local workspace. That's why I have made this distinction here.

   CON, INMOS - 29/10/93.
*/
#define MAX_WS_ARRAY_BYTES   8
#ifdef OCCAM2_5
#define MAX_WS_RECORD_BYTES 64
#endif
/*}}}*/
/*{{{  constants used when generating input/output */
typedef enum
  {
    INP_INPUT,
    INP_TIMER_INPUT,
    INP_PORT_INPUT,
    INP_DELAYED_INPUT,
    INP_SKIP,
    INP_CASE_INPUT,
    INP_TAGGED_INPUT,
    INP_X_INPUT,
    INP_X_CASE_INPUT,
    INP_X_TAGGED_INPUT
  } inp_type_t;
/*}}}*/
/*{{{  access to local channels */
/* Optimising local channels when chanaspointer is true breaks
   when you try to RETYPE channels */
/* This has now been fixed to notice that special case */
#define OPTIMISE_LOCAL_CHANS TRUE
/*}}}*/
/*{{{  loading constants */
typedef enum
  {
    constformat_mint32_ldnlp,
    constformat_mint32_adc,
    constformat_mint32_not,
    constformat_mint16_ldnlp,
    constformat_mint16_adc,
    constformat_ldinf_ldnlp,
    constformat_ldinf_adc,
    constformat_table,
    constformat_ldc
  } constformat_t;
/*}}}*/
/*{{{  return styles for fn results / valofs, etc */
/* These are used to specify the fancy specialised return styles
   for some functions
*/
typedef enum {
    return_style_alu,  /* returns a single object on the ALU stack */
    return_style_fpu,  /* returns a single result on the FPU */
    return_style_other
  } return_style_t;
/*}}}*/
/*}}}*/

/*{{{  structures */
typedef abbrevmode_t (*abbrevmode_fn_t)(treenode *tptr);

typedef struct trans_params_s
  {
    BOOL simplify_structured_io; /* set to TRUE if required to simplify
                                    all i/o */
    BOOL simplify_counted_io;    /* Set to TRUE if required to simplify
                                    counted array communications */
    BOOL abbr_constructors;      /* set to TRUE if you want to process
                                    constructors */
    BOOL scale_by_bytes;         /* set to TRUE if arrayitem scaling should
                                    all be scaled by BYTEs */
    BOOL replpar_becomes_proc;   /* set to TRUE if we are required to insert
                                    an anonymous PROC for the body of a repl PAR */
    abbrevmode_fn_t abbrevmode_fn; /* Function to classify abbreviations */
  } trans_params_t;
/*}}}*/

/*{{{  routines */
treenode *trans_create_declaration(treenode ***insertpoint,
       BOOL *insertvalof,
       int lexlevel, const char *name,
       int decltag, int nametag,
       treenode *typetree,
       treenode *expression);

treenode *scaletreeof (treenode *type, int base_bytes, int lexlevel);
treenode *transformelement(const trans_params_t *const trans_params,
                           treenode *tptr, BOOL rangechecking, const int my_lexlevel);
void transsubscripts (const trans_params_t *const trans_params,
                      treenode **tptr, BOOL create_temps);
treenode *unknowndimsof (treenode *type);

void transmain (const trans_params_t *trans_params, treenode **tptr);

void augmentformals ( treenode *nptr, int current_lexlevel);
void augmentproctype ( treenode *nptr, int current_lexlevel);

BOOL isinvectorspace (treenode *nptr);
#ifdef MOBILES
BOOL isinmobilespace (treenode *nptr);
BOOL isdynmobilechantypetype (treenode *tptr);
BOOL isdynmobilechantype (treenode *nptr);
BOOL isdynmobilearray (treenode *nptr);
BOOL isdynmobilearraypiece (treenode *nptr);
BOOL isdynmobilearraytype (treenode *tptr);
int dynmobiledimensioncount (treenode *nptr);
BOOL isdynmobileproctypetype (treenode *tptr);
BOOL isdynmobileproctype (treenode *nptr);
BOOL isdynmobilebarrier (treenode *nptr);
BOOL isdynmobilebarriertype (treenode *tptr);
BOOL isanychantype (treenode *nptr);
BOOL isanyproctype (treenode *nptr);
BOOL isdeepmobiletype (treenode *tptr);
#endif
BOOL islocal(const treenode *tptr, int my_lexlevel);
BOOL issimplelocal (treenode *tptr, int my_lexlevel);
int whichpowerof2 (BIT32 x);
int inputtypeof (treenode *tptr);
BOOL isshortint (int type);
BOOL isshorttype (int type);
BOOL istargetintsize (int type);
BOOL istargetbytesize (int type);
BOOL issimplechan (treenode *nptr);
#ifdef MOBILES
BOOL ismobile (treenode *nptr);
#endif
BOOL ispointer (treenode *tptr);
BOOL fitsinregister (int type);
BOOL fitsinword (int type);
BOOL isdoublelength (int type);
BOOL isquadlength (int type);
BOOL isinconstanttable (treenode *tptr);
BOOL isaddressable (treenode *tptr);
BOOL isplaced (treenode *nptr);
BOOL iswsplaced (treenode *nptr);
BOOL mightbealiased(treenode *t);
BOOL word_length_record(treenode *const type);
BOOL isconstexpnd (treenode *tptr); /* defined in gen2.c */

int ilength (INT32 operand);
constformat_t constformat_of_const(int type, INT32 lo, INT32 hi);
BOOL shouldbeinconstanttable (treenode *tptr);
BOOL can_use_wsubdb(int b, int bpw, treenode *tptr);

BOOL usedin (treenode *t, treenode *tptr, int my_lexlevel);
/*BOOL usedinaddr (treenode *t, treenode *tptr);*/

const char *transmessagestring ( int n );
const char *abbrevmode_string(abbrevmode_t am);

return_style_t fn_return_style(treenode *nptr);
return_style_t valof_return_style(treenode *resultlist);
return_style_t result_return_style(treenode *type);
/*}}}*/

/*{{{  side-effects pragmas */
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (whichpowerof2)
#pragma IMS_nosideeffects (isshortint)
#pragma IMS_nosideeffects (isshorttype)
#pragma IMS_nosideeffects (istargetintsize)
#pragma IMS_nosideeffects (istargetbytesize)
#pragma IMS_nosideeffects (issimplechan)
#pragma IMS_nosideeffects (fitsinregister)
#pragma IMS_nosideeffects (fitsinword)
#pragma IMS_nosideeffects (isdoublelength)
#pragma IMS_nosideeffects (isquadlength)
#pragma IMS_nosideeffects (isinconstanttable)
#pragma IMS_nosideeffects (isaddressable)
#pragma IMS_nosideeffects (isplaced)
#pragma IMS_nosideeffects (iswsplaced)
#pragma IMS_nosideeffects (mightbealiased)
#pragma IMS_nosideeffects (ilength)
#pragma IMS_nosideeffects (constformat_of_const)
#pragma IMS_nosideeffects (can_use_wsubdb)
#pragma IMS_nosideeffects (fn_return_style)
#pragma IMS_nosideeffects (valof_return_style)
#pragma IMS_nosideeffects (result_return_style)
#pragma IMS_nosideeffects (abbrevmode_string)
#endif
/*}}}*/

#endif
