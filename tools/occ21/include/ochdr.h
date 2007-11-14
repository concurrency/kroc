/* $Id: ochdr.h,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	occam compiler configuration
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


/*{{{  VAX stuff */
#if defined(TARGET_OS_IS_VMS)
#ifndef ANSI
#ifndef DEC
#define defined(P) P
#endif
#endif
#endif
/*}}}  */

/*{{{  HOST_ALIGN_SIZE */
/* This constant is host dependent: if structures must be aligned on a
   4-byte boundary (as the t4 requires) it should be set to 4.
   For an n-byte alignment, set the constant to n.
   The Vax needs no alignment, so set to 1. */
#if 0
  #if defined(COMPILER_IS_LLL) || defined(TARGET_OS_IS_HELIOS) || defined(IMS)
    #define HOST_ALIGN_SIZE 4
  #else
    #define HOST_ALIGN_SIZE 1
  #endif
#endif

#ifdef target_cpu_alpha
  #define HOST_ALIGN_SIZE 8 /* always align to 8 byte boundary */
#else
  #define HOST_ALIGN_SIZE 4 /* always align to 4 byte boundary */
#endif
/*}}}  */

/*{{{  OC, or CONFIG */
#if defined(OC) || defined(CONFIG2) || defined(CONFIG3)
#else
 error Must define tool type on command line: OC, CONFIG2, or CONFIG3
#endif
/*}}}  */

/*{{{  DEBUG_MSG */
#ifdef DEBUG
  #define DEBUG_MSG(X) printf X
#else
  #define DEBUG_MSG(X)
#endif
/*}}}  */

/*{{{  implementation limits */
#define TABSIZE 8

#define MAXSTRING_SIZE      256
#define MAXNAMELENGTH       255

#define MAX_FILENAME_LENGTH 128

/*}}}  */

/*{{{  occam 2.5 */
/* #define this if you want the occam 2.5 extensions */
/* #define OCCAM2_5  */

#ifdef OCCAM2_5
  /* #define this if you want records of channels.
     NOTE - this isn't fully implemented yet! In particular, it needs
     lots of work on the usage checking.
  */
  /* #define RECORDS_OF_CHANNELS */
  /* frmb change -- this is enabled automatically with MOBILEs now */
#endif
/*}}}  */

/*{{{  checking of tree accesses */
/* If this is defined, tag checking is turned ON */
#if 1
#define CHECK_TREEACCESSES
#endif
/*}}}  */

/*{{{  inlining */
/* Now determined in configuration - see config.h */
/*}}}  */

/*{{{  USE_VAR */
/* This is used to 'use' unused variables (eg parameters) */
#define USE_VAR(var) (void)(var)
/*}}}  */
