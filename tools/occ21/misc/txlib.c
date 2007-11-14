/* $Id: txlib.c,v 1.3 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	compiler trasputer specific flags
 *	Copyright (C) 1991 Inmos limited
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
 * Ade 3/3/95 - Add init of pstring field in tx_setprocessor()
 *           -  Add tx_processorstring_from_txlib()
 *           -  Moved table entry for T801 after T805, T400 after T425.
 */

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "imstype.h"		/* IMPORTED */
#include "imsmisc.h"		/* IMPORTED */
#include "tcoff.h"		/* IMPORTED */
#include "cfbdefs.h" /* IMPORTED */	/* data for the configurer */

#include "txlib.h"
/*}}}  */

/*{{{  PRIVATE contants */
#define RMC_CORE_BITS (INSTR_RMC_CORE1 | INSTR_RMC_CORE2 | INSTR_RMC_CORE3)
/*}}}  */

/*{{{  ptypes maps command line string to processor type */
PRIVATE const struct proctype_struct
  /*{{{  structure definition */
{
	const char *string;	/* string representing processor type 'ptype' */
	BIT32 instr;		/* processor instruction attributes */
	BIT32 attr;		/* other processor attributes */
	int links;		/* number of links */
	int config_type;	/* processor type for configurer */
	BOOL class;		/* TRUE if the type is a class (not permitted configuring */
	BOOL Hseries;		/* TRUE if an H_series processor */
	/* note that H_series is not quite the same as (instr & ARCH_H) */
	int RAM;		/* amount of on-chip RAM (in bytes) */
	int PTYPE_value;	/* initialiser for C _PTYPE preprocessor symbol */
	BOOL specifiable_memstart;	/* Added for ST20, allows memstart to be specified to configurer */
	BOOL specifiable_links;	/* Added for ST20, allows number of links to be specified to configurer */
}
ptable[] =
  /*}}}  */
/*{{{  definitions of T and F*/
#define T TRUE
#define F FALSE
/*}}}  */
{
	{
	"TA", TA_INSTR, TA_ATTRIB, 4, CFB_PROCESSOR_TYPE_TA, T, F, 2048, 'A', FALSE, FALSE}
	, {
	"TB", TB_INSTR, TB_ATTRIB, 4, CFB_PROCESSOR_TYPE_TB, T, F, 2048, 'B', FALSE, FALSE}
	,
#if 0
	{
	"TC", TC_INSTR, TC_ATTRIB, 4, CFB_PROCESSOR_TYPE_TC, T, F, 4096, 'C', FALSE, FALSE}
	,
#endif
	{
	"T2", T212_INSTR, T212_ATTRIB, 4, CFB_PROCESSOR_TYPE_T212, T, F, 2048, '2', FALSE, FALSE}
	, {
	"T212", T212_INSTR, T212_ATTRIB, 4, CFB_PROCESSOR_TYPE_T212, F, F, 2048, '2', FALSE, FALSE}
	,
/* Note T222 after T212 (its that alphabetically, but just a warning :-) */
	{
	"T222", T222_INSTR, T222_ATTRIB, 4, CFB_PROCESSOR_TYPE_T222, F, F, 4096, '2', FALSE, FALSE}
	, {
	"T225", T225_INSTR, T225_ATTRIB, 4, CFB_PROCESSOR_TYPE_T225, F, F, 4096, '3', FALSE, FALSE}
	, {
	"T3", T225_INSTR, T225_ATTRIB, 4, CFB_PROCESSOR_TYPE_T225, T, F, 4096, '3', FALSE, FALSE}
	,
/* Note that the M212 is not in alphabetical order, so that when you ask
   for a T212's string given the type and attributes, you don't get told
   M212
*/
	{
	"M212", T212_INSTR, T212_ATTRIB, 2, CFB_PROCESSOR_TYPE_M212, F, F, 2048, '2', FALSE, FALSE}
	, {
	"T4", T414_INSTR, T414_ATTRIB, 4, CFB_PROCESSOR_TYPE_T414, T, F, 2048, '4', FALSE, FALSE}
	, {
	"T414", T414_INSTR, T414_ATTRIB, 4, CFB_PROCESSOR_TYPE_T414, F, F, 2048, '4', FALSE, FALSE}
	, {
	"T425", T425_INSTR, T425_ATTRIB, 4, CFB_PROCESSOR_TYPE_T425, F, F, 4096, '5', FALSE, FALSE}
	,
/* Note T400 after T425 */
	{
	"T400", T400_INSTR, T400_ATTRIB, 2, CFB_PROCESSOR_TYPE_T400, F, F, 2048, '5', FALSE, FALSE}
	, {
	"T450", T450_INSTR, T450_ATTRIB, 4, CFB_PROCESSOR_TYPE_T450, F, F, 16384, 450, FALSE, FALSE}
	, {
	"ST20", T450_INSTR, T450_ATTRIB, 4, CFB_PROCESSOR_TYPE_ST20, F, F, 16384, 450, TRUE, TRUE}
	, {
	"T5", T425_INSTR, T425_ATTRIB, 4, CFB_PROCESSOR_TYPE_T425, T, F, 4096, '5', FALSE, FALSE}
	, {
	"T8", T800_INSTR, T800_ATTRIB, 4, CFB_PROCESSOR_TYPE_T800, T, F, 4096, '8', FALSE, FALSE}
	, {
	"T800", T800_INSTR, T800_ATTRIB, 4, CFB_PROCESSOR_TYPE_T800, F, F, 4096, '8', FALSE, FALSE}
	, {
	"T805", T805_INSTR, T805_ATTRIB, 4, CFB_PROCESSOR_TYPE_T805, F, F, 4096, '9', FALSE, FALSE}
	,
/* Note T801 after T805 */
	{
	"T801", T801_INSTR, T801_ATTRIB, 4, CFB_PROCESSOR_TYPE_T801, F, F, 4096, '9', FALSE, FALSE}
	, {
	"T9", T805_INSTR, T805_ATTRIB, 4, CFB_PROCESSOR_TYPE_T805, T, F, 4096, '9', FALSE, FALSE}
	, {
	"T9000", T9000_INSTR, T9000_ATTRIB, 4, CFB_PROCESSOR_TYPE_T9000, F, T, 16384, 9000, FALSE, FALSE}
	, {
	"T9000GAMMA", T9000GAMMA_INSTR, T9000GAMMA_ATTRIB, 4, CFB_PROCESSOR_TYPE_T9000_GAMMA_D, F, T, 16384, 9000, FALSE, FALSE}
	, {
	"T9000GAMMAE", T9000GAMMAE_INSTR, T9000GAMMAE_ATTRIB, 4, CFB_PROCESSOR_TYPE_T9000_GAMMA_E, F, T, 16384, 9000, FALSE, FALSE}
	,
		/*{ "H1G",        H1_INSTR,         H1_ATTRIB,         4, CFB_PROCESSOR_TYPE_T9000,         F, T, 16384, 9000, FALSE, FALSE }, */
		/*{ "H1L",        H1_INSTR,         H1_ATTRIB,         4, CFB_PROCESSOR_TYPE_T9000,         F, T, 16384, 9000, FALSE, FALSE }, */
		/*{ "ZH1G",       T805_INSTR,       T805_ATTRIB,       4, CFB_PROCESSOR_TYPE_T805,          F, T, 16384, 9000, FALSE, FALSE }, */
		/*{ "ZH1L",       T805_INSTR,       T805_ATTRIB,       4, CFB_PROCESSOR_TYPE_T805,          F, T, 16384, 9000, FALSE, FALSE } */
	{
	"AXP", AXP_INSTR, T800_ATTRIB | ATTRIB_WORD_64, 4, CFB_PROCESSOR_TYPE_T800, F, F, 4096, 'D', F, F}
};

/*}}}  */

/*{{{  PUBLIC variables */
txlib_t tx_global;

/*}}}  */

/*{{{  PRIVATE BOOL eqstrprefix(const char *s1, const char *s2) */
PRIVATE BOOL
eqstrprefix (const char *s1, const char *s2)
/* checks that the two strings match as far as possible, and that any
   overlap is purely spaces */
{
	/* Check that the first characters match */
	while (((*s2) != '\0') && ((*s1) != '\0'))
		if ((*s2++) != (*s1++))
			return FALSE;

	/* Now check that any trailing chars are spaces */
	while ((*s1) != '\0')
		if ((*s1++) != ' ')
			return FALSE;
	while ((*s2) != '\0')
		if ((*s2++) != ' ')
			return FALSE;
	return TRUE;
}

/*}}}  */
/*{{{  PRIVATE int get_index */
PRIVATE int
get_index (const char *s)
{
	int i;
	for (i = 0; i < (sizeof (ptable) / sizeof (struct proctype_struct)); i++)
		if (eqstrprefix (s, ptable[i].string))
			return i;
	return (-1);
}

/*}}}  */

/*{{{  PUBLIC BIT32 tx_setprocessor */
/* set processor dependant attributes */
/* pass in name of processor, in upper case */
/* This is also called by the configuration stuff */
PUBLIC BIT32
tx_setprocessor (txlib_t * const tx, const char *const s)
{
	const int i = get_index (s);
	/*printf("In tx_setprocessor: %s\n", s); */
	if (i >= 0) {
		const struct proctype_struct *const pentry = &ptable[i];
		const BIT32 instr = pentry->instr;
		const BIT32 attr = pentry->attr;
		const BOOL Hseries = pentry->Hseries;
		/*{{{  Set main flags */
		tx->ptype = instr;
		tx->pattr = attr;
		tx->pstring = pentry->string;

		/* note that H_series is not quite the same as (instr & ARCH_H) */
		/* Or, rather, wasn't when we had G-processes, and ZH1L etc */
		tx->hseries = Hseries;
		tx->lprocess = Hseries;
		tx->tseries = !Hseries;
		/*}}}  */
		/*{{{  Set the instruction timings */
		tx->t9000_timings = Hseries;
		/* A processor has rmc timings if any of the RMC_CORE bits are set */
		tx->rmc_timings = !Hseries && ((instr & RMC_CORE_BITS) != 0);
		/*}}}  */
		/*{{{  Set the flags indicating which instructions are available */
		tx->hasfpucore = Hseries | ((instr & INSTR_FPU_CORE) != 0);
		tx->hasdup = Hseries | ((instr & INSTR_DUP) != 0);
		tx->haswsubdb = Hseries | ((instr & INSTR_WSUBDB) != 0);
		tx->hasfpsupport = !Hseries & ((instr & INSTR_FP_SUPPORT) != 0);
		tx->hasmove2d = Hseries | ((instr & INSTR_MOVE2D) != 0);
		tx->hascrc = Hseries | ((instr & INSTR_CRC) != 0);
		tx->hasbitops = Hseries | ((instr & INSTR_BITOPS) != 0);
		tx->hasfmul = Hseries | ((instr & INSTR_FMUL) != 0);
		tx->hastimerdisable = !Hseries & ((instr & INSTR_TIMER_DISABLE) != 0);
		tx->hasdebugsupport = !Hseries & ((instr & INSTR_DEBUG_SUPPORT) != 0);
		tx->haslddevid = Hseries | ((instr & INSTR_LDDEVID) != 0);
		tx->haspop = Hseries | ((instr & INSTR_POP) != 0);
		tx->hasfptesterr = !Hseries & ((instr & INSTR_FPTESTERR) != 0);
		tx->hasldmemstartval = Hseries | ((instr & INSTR_LDMEMSTARTVAL) != 0);
		tx->hasdirectfp = Hseries;	/* IE don't use FPENTRY */
		tx->hasfprem = Hseries;
		tx->hasfpsqrt = Hseries;
		/* We haven't yet defined which of the RMC_CORE_BITS relate to specific
		   instructions, so for any of the RMC core instructions, ALL of the
		   RMC_CORE_BITS must be set. */
		tx->hasshortintops = Hseries | ((instr & RMC_CORE_BITS) == RMC_CORE_BITS);
		tx->hasgtu = Hseries | ((instr & RMC_CORE_BITS) == RMC_CORE_BITS);
		tx->hasdevaccess = Hseries | ((instr & INSTR_NO_DEVICE) == 0);

		/* use VIN and VOUT for counted array protocols */
		tx->hasvariableio = (((instr & ARCH_H) != 0) && tx->lprocess);

		tx->hastseriesscheduler = !Hseries;
		tx->hastserieserrors = !Hseries;
		tx->hastseriesfperrors = !Hseries & tx->hasfpucore;
		tx->hasfpentry = !Hseries & tx->hasfpucore;
		tx->hasfpremfirst = !Hseries & tx->hasfpucore;

		tx->hast9gammaprobs = Hseries & ((instr & H_INSTR_T9000_GAMMA) != 0);
		tx->hast9gammaeprobs = Hseries & ((instr & H_INSTR_T9000_GAMMAE) != 0);

		tx->hasrmccore = ((instr & RMC_CORE_BITS) == RMC_CORE_BITS);
		/*}}}  */
		/*{{{  Set word length etc */
		if ((((instr & ARCH_T) != 0) && ((attr & ATTRIB_WORD_16) == 0)) || ((instr & ARCH_H) != 0)) {
			tx->bpw = 4;
			tx->mint = 0x80000000;
		} else if (((instr & ARCH_T) != 0) && ((attr & ATTRIB_WORD_16) != 0)) {
			tx->bpw = 2;
			tx->mint = 0x8000;
		} else
			abort ();

		tx->specifiable_memstart = pentry->specifiable_memstart;
		tx->specifiable_links = pentry->specifiable_links;

		tx->links = pentry->links;
		tx->cfbtype = pentry->config_type;
		tx->isclass = pentry->class;
		tx->memstart = tx->hasmove2d ? 28 : 18;
		tx->ram = pentry->RAM;
		/*}}}  */
		/*{{{  Set the C specific stuff */
		tx->ptype_symbol = pentry->PTYPE_value;
		/*}}}  */
		if (instr == UNKNOWN_PROCESSOR_TYPE)
			abort ();
		return (instr);
	}
	return (UNKNOWN_PROCESSOR_TYPE);
}

/*}}}  */
/*{{{  PUBLIC BOOL  tx_testprocessor */
PUBLIC BOOL
tx_testprocessor (const char *const string)
{
	return get_index (string) >= 0;
}

/*}}}  */
/*{{{  PUBLIC const char *tx_processorstring */
PUBLIC const char *
tx_processorstring (const BIT32 p, BIT32 a)
{
	int i;
	const int mask = ATTRIB_WORD_MASK | ATTRIB_MEMSTART_MASK;
	a &= mask;
	for (i = 0; i < (sizeof (ptable) / sizeof (struct proctype_struct)); i++)
		if ((ptable[i].instr == p) && ((ptable[i].attr & mask) == a))
			return (ptable[i].string);
	return NULL;
}

/*}}}  */
/*{{{  PUBLIC const char *tx_processorstring_not_class */
PUBLIC const char *
tx_processorstring_not_class (const BIT32 p, BIT32 a)
{
	int i;
	const int mask = ATTRIB_WORD_MASK | ATTRIB_MEMSTART_MASK;
	a &= mask;
	for (i = 0; i < (sizeof (ptable) / sizeof (struct proctype_struct)); i++)
		if ((ptable[i].instr == p) && ((ptable[i].attr & mask) == a) && !ptable[i].class)
			return (ptable[i].string);
	return NULL;
}

/*}}}  */
/*{{{  PUBLIC const char *tx_processorstring_from_devid */
PUBLIC const char *
tx_processorstring_from_devid (const int deviceid)
{
	int i;
	for (i = 0; i < (sizeof (ptable) / sizeof (struct proctype_struct)); i++)
		if (ptable[i].config_type == deviceid)
			return (ptable[i].string);
	return NULL;
}

/*}}}  */
/*{{{  PUBLIC const char *tx_processorstring_from_txlib */
/*
 * Use in preference to tx_processorstring() to distinguish between
 * ST20 and T450.
 */
PUBLIC const char *
tx_processorstring_from_txlib (txlib_t * tx)
{
	const char *res = NULL;

	if (tx)
		res = tx->pstring;

	return res;
}

/*}}}  */
