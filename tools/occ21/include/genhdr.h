/* $Id: genhdr.h,v 1.1 1996/04/15 10:52:08 djb1 Exp $ */

/*
 *	code generator definitions
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

/*{{{  special namenode values */
/* Workspace offset of name which doesn't have an offset */
#define NO_SLOT 0x80000000
/*}}}*/

/*{{{  offsets in mt_cb_pony_state_t */
#define PONY_TYPEDESC 0
#define PONY_UIOHOOK 1
#define PONY_STATE 2
#define PONY_STATESEM 3
/*}}}*/
/*{{{  static link, vector space, constant pointer constants */
#define MAX_LEX_LEVELS           254
#define STATICLINK_NOT_USED      0xFF    /* an illegal lexical level */
#define CONSTANTPOINTER_NOT_USED NO_SLOT /* an impossible offset */

/* Fields within an entry in staticlinkoffsets */
#define OFFSET_BITS 0x7fffffff
#define REPL_FLAG   0x80000000

/*}}}*/
/*{{{  expression evaluation constants */
#define MAXREGS 3
#define MAXFPUREGS 3
#define MANY_REGS (-1)
/*}}}*/
/*{{{  expression modes */
#define P_EXP     1   /* operand is an expression */
#define P_TEMP    2   /* operand is an evaluated temporary */
#define P_TEMPPTR 3   /* operand is an evaluated temporary pointer */
#define P_PTR     4   /* operand is a pointer */
/*}}}*/
/*{{{  constants for below workspace data requirements */
#if 0
#define DS_MIN  2   /* All processes need at least two slots */
#define DS_IO   3   /* Processes using channels need three slots */
#define DS_WAIT 5   /* Processes using timers need five slots */
#else
/* if L_process is 1 (TRUE), then add 1 to each of these: */

/* frmb: i've moved some of this out of here; want a bit more control
 *       over workspace allocation at run-time.  in gen13.c
 */
extern int genhdr_ds_min (void);
extern int genhdr_ds_io (void);
extern int genhdr_ds_wait (void);

#define DS_MIN	(genhdr_ds_min())
#define DS_IO	(genhdr_ds_io())
#define DS_WAIT	(genhdr_ds_wait())

extern int genhdr_w_time_slot (void);

#define W_TIME_SLOT	(genhdr_w_time_slot ())

#endif	/* !0 */

#if 0
#define DEFAULT_HIGH_PRI_TRAP_OFFSET 20   /* H1 L-process's high priority trap handler */
                                  /* is at this many words from low pri one */
#else
#define DEFAULT_HIGH_PRI_TRAP_OFFSET -2   /* H1 L-process's high priority trap handler */
                                  /* is a pointer at this many words from low pri one */
#endif

/* Three below workspace slots contain the register parameters */
#define REG_PARAMS MAXREGS
/* One below workspace slot contains the return address */
#define INS_EXTRA  ((INT32)1)
/* requirements for recursive stuff */
#define RECURSIVE_WS -1
#define RECURSIVE_VS -2
#ifdef MOBILES
	#define RECURSIVE_MS -3
	#define RECURSIVE_SLOTS 3
#else
	#define RECURSIVE_SLOTS 2
#endif

#define RECURSIVE_SAVED_WS 0	/* in the allocated workspace */
#define MIN_RECURSIVE_SLOTS 2	/* for the saved Wptr (+ result) */

/* offsets for FORK call setup things */
#define FORK_SETUP_TEMP -1
#define FORK_SETUP_TEMPVAL -2
#define FORK_SETUP_TEMP_VS -3
#ifdef MOBILES
	#define FORK_SETUP_TEMP_MS -4
	#define FORK_SETUP_SLOTS 4
#else
	#define FORK_SETUP_SLOTS 3	/* in the dispatching workspace for temporaries */
#endif

#define MIN_FORK_SLOTS 1	/* in the allocated workspace */
#define FORK_BARRIER 0		/* for the barrier pointer */

/* offsets for dynmically called PROCs */
#define DYNCALL_SETUP_TEMP_WS -1
#define DYNCALL_SETUP_TEMP_VS -2
#define DYNCALL_SETUP_TEMP_NAME -3
#define DYNCALL_SETUP_TEMP_WSBASE -4

#define DYNCALL_SETUP_SLOTS 4		/* in the parent workspace for temporaries */

#define DYNCALL_SAVED_WS 0	/* in the allocated workspace */
#define MIN_DYNCALL_SLOTS 2	/* for the saved Wptr */

/*}}}*/
/*{{{  constants for MOBILE process offsets*/
#define MPP_WPTR 0		/* Wptr of mobile process when inactive, of invoking process when active */
#define MPP_IPTR 1		/* Iptr of mobile process when inactive */
#define MPP_AIPTR 2		/* Iptr of invoking process when active */
#define MPP_MAPCHAIN 3		/* workspace-map chain */
#define MPP_WSBASE 4		/* WS base */
#define MPP_WSSIZE 5		/* WS size (bytes) */
#define MPP_VSBASE 6		/* VS base */
#define MPP_MSBASE 7		/* MS base */
#define MPP_BARRIER 8		/* barrier: mobile type pointer */
#if 0
#define MPP_BFPTR 8		/* barrier: suspended processes queue (Fptr) */
#define MPP_BBPTR 9		/* barrier: Bptr */
#define MPP_BECNT 10		/* barrier: enrolled-process count */
#define MPP_BCNT 11		/* barrier: count */
#endif
#define MPP_TYPEHASH 12		/* typehash for the process type */
#define MPP_CODEMAP 13		/* statically created map that describes code used by the process */

#define MPP_VSSIZE 14		/* VS size (bytes) */
#define MPP_MSHOOK 15		/* MS hook pointer (in static mobilespace) */
#define MPP_MSSIZE 16		/* MS size (bytes) */
#define MPP_MSDESC 17		/* MS descriptor block (advanced) */

#define MPP_SLOTS 14		/* basic number of slots required for a MOBILE process */
#define MPP_SLOTS_VS 15		/* those requiring vectorspace */
#define MPP_SLOTS_MS 18		/* and MOBILESPACE */


#define MPA_SETUP_SLOTS 3	/* in the dispatching workspace for temporaries */
#define MPA_SETUP_TEMP -1	/* temporary workspace pointer */
#define MPA_SETUP_TEMP_VS -2	/* temporary vectorspace pointer */
#define MPA_SETUP_TEMP_MS -3	/* temporary mobilespace pointer */

#define MIN_MPA_SLOTS 0		/* in the allocated workspace (i.e. nothing) */

/*}}}*/
/*{{{  used in gen8 (types of PROC call)*/
#define PROC_NONE 0
#define PROC_REC 0x01		/* recursive PROC call */
#define PROC_FORKED 0x02	/* FORKed PROC call */
#define PROC_MPA 0x04		/* mobile process activation */
#define PROC_DYNCALL 0x08	/* dynamic PROC call */

/*}}}*/
/*{{{  special label constants */
#define NOLAB (-1)
/*}}}  */
/*{{{  constants used when generating replicators */
/* Offsets from replicator workspace position of the components of a
   replicator */
#define REPL_BASE 0
#define REPL_COUNT 1
#define REPL_STEP 2
/*#define REPL_SELECTED 2*/ /* For an ALT replicator, the selected value */
/*}}}*/
/*{{{  constants used when generating repl PAR */
/* Offsets from wptr when setting up repl PAR */
#if defined(PROCESS_PRIORITY)
	#define MIN_REPLPAR_SLOTS 4  /* Minimum number of slots required to set up */
	                             /* a repl PAR: par join, count, priority & tempws  */
	#define REPLPAR_WS_TEMP   3  /* Temp. slot for calculating repl wptrs */
	#define REPLPAR_VS_TEMP   4  /* Temp. slot for calculating repl vsps  */
	#ifdef MOBILES
	#define REPLPAR_MS_TEMP   5  /* Temp. slot for calculating repl msps */
	#endif
#else	/* !PROCESS_PRIORITY */
	#define MIN_REPLPAR_SLOTS 3  /* Minimum number of slots required to set up */
	                             /* a repl PAR: par join, count & tempws  */
	#define REPLPAR_WS_TEMP   2  /* Temp. slot for calculating repl wptrs */
	#define REPLPAR_VS_TEMP   3  /* Temp. slot for calculating repl vsps  */
	#ifdef MOBILES
	#define REPLPAR_MS_TEMP   4  /* Temp. slot for calculating repl msps */
	#endif
#endif	/* !PROCESS_PRIORITY */

/* Special slots in a replicated PAR process' workspace */
/* These are above any variables local to the process */
#define MIN_REPLPAR_SPECIALS 2 /* Minimum number of special slots */
#define REPLPAR_REPLICATOR 0 /* Local value of replicator */
#define REPLPAR_STATICLINK 1 /* Link to workspace of invoking process */
#define REPLPAR_VSP        2 /* Vector space pointer (may not be needed) */
#ifdef MOBILES
#define REPLPAR_MSP        3 /* mobile-space pointer */
#endif
/*}}}*/
/*{{{  constants used inside predefines  */
#define INFINITY (0x7F800000)
/*}}}*/
/*{{{  constants used for T9000 error handling */
/*
(See T9000 specification part 2, section 30.6.2 "Reporting causes of traps",
in a table under the heading "Indication of the particular error which has
occurred"). The value 5 is named "err_interr".
*/
#define ERR_INTERR 5
/*}}}*/
/*{{{  constants used for label handling */
#define NO_LABEL (-1) /* any value which isn't a valid label number */
/*}}}*/
/*{{{  constants used to adjust the behaviour of 'prod' */
/* If a constant is smaller than this, it should be moved to the rhs */
#define TIMES_RHS_IS_FASTER    16

/* If a constant is greater than this, it should be moved to the lhs */
/* the reasoning behind this value is that it is 'half' the width of an INT */
/* These are calculated at run-time according to the type of the operand! */
/*#define TIMES_LHS_IS_FASTER_32 0x10000*/
/*}}}*/

/* This is used to check for links being placed on the top of the
   debugger's links */
#define NO_OF_LINKS 4

/*{{{  constants for workspace map types */
#define WSMAP_INVALID		0x00 
#define WSMAP_CHANWORD		0x01
#define WSMAP_CHANPTR		0x02
#define WSMAP_FIRSTPARAM	0x03
#define WSMAP_CHANARRAY		0x04
#define WSMAP_GENPTR		0x05
#define WSMAP_MOB_DA		0x06
#define WSMAP_MOB_CT		0x07
#define WSMAP_MOB_PT		0x08
#define WSMAP_VSPTR		0x09
#define WSMAP_FB		0x0a
#define WSMAP_MSPTR		0x0b
#define WSMAP_MPP		0x0c
#define WSMAP_MOB_SHADOW	0x0d
#define WSMAP_STATICLINK	0x0e
#define WSMAP_CODEPTR		0x0f

#define WSMAP_FLAG_VS		0x80	/* in-vectorspace flag */

/*}}}*/


