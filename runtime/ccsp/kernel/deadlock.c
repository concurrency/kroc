/*
 *	deadlock.c -- run-time deadlock debugging
 *	Also includes run-time Floating Point error stuff
 *	Copyright (C) 2000-2002 Fred Barnes  <frmb2@ukc.ac.uk>
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef RMOX_BUILD
	#include <rmox_if.h>
#else
	#include <stdio.h>
	#include <setjmp.h>
#endif

#define __DEADLOCK_C

#include <kernel.h>
#include <rts.h>
#include <deadlock.h>
#include <arch/atomics.h>


static unsigned char	*lowest_code_address;
static char 		*ws_ptrs[MAX_WORKSPACES];
static int 		ws_sizes[MAX_WORKSPACES];
static int 		num_ws;
static atomic_t 	deadlock_output;
static int		debug_dead;
#if !defined(RMOX_BUILD)
static jmp_buf		segenv;
static void		(*old_segv_handler)(int);
#endif	/* !defined(RMOX_BUILD) */

#define INS_X86_JMPIMMS8	(0xeb)
#define INS_X86_JMPIMMS32	(0xe9)
#define INS_SPARC_JMPIMMS	(0x48)
#define INS_PPC_JMPIMMS		(0x48)

/*{{{  void ccsp_deadlock_init (void)*/
/*
 * 	initialises deadlock debugging data
 */
void ccsp_deadlock_init (void)
{
	att_init (&deadlock_output, 0);
	debug_dead = 0;
	num_ws = 0;
}
/*}}}*/
/*{{{  void ccsp_give_ws_code (char *ws, int ws_bytes, unsigned char *codeptr)*/
/*
 *	used to add a workspace for deadlock debugging
 */
void ccsp_give_ws_code (char *ws, int ws_bytes, unsigned char *codeptr)
{
	int slot;

	if (!ws || !ws_bytes || !codeptr) {
		BMESSAGE ("%s: ws, ws_bytes or codeptr given as 0.  Ignoring workspace.\n", __FILE__);
		return;
	}
	if (num_ws == MAX_WORKSPACES) {
		for (slot=0; slot < MAX_WORKSPACES; slot++) {
			if (!ws_ptrs[slot]) {
				break;
			}
		}
		if (slot == MAX_WORKSPACES) {
			/* can be fixed by changing constant and re-compiling the RTS */
/*			fprintf (stderr, "CCSP: %s: no more room!  Ignoring workspace.  (increase MAX_WORKSPACES in deadlock.h)\n", __FILE__); */
			return;
		}
	} else {
		slot = num_ws++;
	}
	debug_dead++;
	ws_ptrs[slot] = ws;
	ws_sizes[slot] = ws_bytes;
	if ((unsigned int)codeptr < (unsigned int)lowest_code_address) {
		lowest_code_address = codeptr;
	}
	return;
}
/*}}}*/
/*{{{  void ccsp_take_ws (char *ws)*/
/*
 *	removes a workspace added with `ccsp_give_ws_code'
 */
void ccsp_take_ws (char *ws)
{
	int slot;

	if (!ws) {
		BMESSAGE ("%s: ws given as 0.  Ignoring.\n", __FILE__);
		return;
	}
	for (slot=1; slot<num_ws; slot++) {
		if (ws_ptrs[slot] == ws) {
			break;
		}
	}
	if (slot == num_ws) {
		BMESSAGE ("%s: cannot remove workspace, not found.  Ignoring.\n", __FILE__);
		return;
	}
	ws_ptrs[slot] = 0;
	ws_sizes[slot] = 0;
	if (slot == (num_ws - 1)) {
		num_ws--;
	}
	return;
}
/*}}}*/

#if !defined(RMOX_BUILD)
/*{{{  static void segv_handler (int signum)*/
/*
 *	SIGSEGV handler -- this is used while searching for deadlocked processes (i.e. if we get a false hit)
 */
static void segv_handler (int signum)
{
	signal (SIGSEGV, segv_handler);
	longjmp (segenv, 1);
	return;
}
/*}}}*/
#endif	/* !defined(RMOX_BUILD) */

#if !defined(RMOX_BUILD)
/*{{{  static void debug_deadlock_out (FILE *stream, char *ops, char *proc, char *file, int line)*/
/*
 * sends out debugging info on 'stream'
 */
static void debug_deadlock_out (FILE *stream, char *ops, char *proc, char *file, int line)
{
	#if defined(DEADLOCK_TERSE)
		fprintf (stream, "%s(%d)-%s  PROC %s (...)\n", file, line, ops, proc);
	#elif defined(DEADLOCK_BRIEF)
		fprintf (stream, "\t%s:%d\n", file, line);
		fprintf (stream, "\t\tPROC %s () instruction %s\n", proc, ops);
	#else
		fprintf (stream, "\n  Instruction \"%s\"\n", ops);
		fprintf (stream, "    in PROC \"%s\"\n", proc);
		fprintf (stream, "    in file \"%s\"\n", file);
		fprintf (stream, "    near line %d\n", line);
	#endif
	return;
}
/*}}}*/
#endif

#if !defined(RMOX_BUILD)
/*{{{  static int deadlock_debug (int *iws_ptr, int bytes, int *did_print)*/
/*
 * Performs a WS search over `iws_ptr' for `bytes' bytes
 * Returns number-of-matches on success, -1 on failure (ie, program compiled without -X5)
 */
static int deadlock_debug (int *iws_ptr, int bytes, int *did_print)
{
	char *search, *debug_filename, *debug_procname;
	static unsigned char *codeptr;
	static int *search_int;
	int *search_limit;
	static int found;
	int debug_dlop, debug_kentry, debug_line, debug_file, debug_proc;
	unsigned char ins_1, ins_2;
	static char *dlops[] = { "INVALID", "IN", "OUT", "OUTBYTE", "OUTWORD", "ALTWT", "TALTWT", "XABLE", "SYNC" };
	static int num_dlops = 9;

	found = 0;
	if (((int) iws_ptr) & 0x03) {
		search = (char *)(((unsigned int)iws_ptr & 0xfffffffc) + 4);
	} else {
		search = (char *)iws_ptr;
	}
	old_segv_handler = signal (SIGSEGV, segv_handler);
	/* Nicely aligned workspace */
	search_int = (int *)((unsigned int)search + 8);
	search_limit = (int *)((unsigned int)search + (unsigned int)bytes);
	#ifdef DEBUG_DEBUG
		BMESSAGE ("%s: searching from 0x%lx to 0x%lx...\n", __FILE__, (unsigned long int)search_int, (unsigned long int)search_limit);
	#endif
	for (; search_int < search_limit; search_int++) {
		if ((int *)search_int[-2] == search_int) {
			/* Possibility */
			codeptr = (unsigned char *)search_int[-1];
			if (codeptr < lowest_code_address) {
				/* Nope.. */
				#ifdef DEBUG_DEBUG
					BMESSAGE ("%s: codeptr (0x%lx) < lowest_code_address (0x%lx)\n", __FILE__, (unsigned long int)codeptr,
						(unsigned long int)lowest_code_address);
				#endif
				continue;
			}
			if (!setjmp (segenv)) {
				/* Try dereferencing it... */
				ins_1 = *codeptr;
#if defined(TARGET_CPU_SPARC) || defined(TARGET_CPU_PPC64)
				ins_2 = *(codeptr + 4);		/* next word please */
#else
				ins_2 = *(codeptr + 1);
#endif
			} else {
				/* Nope, obviously not.. */
				#ifdef DEBUG_DEBUG
					BMESSAGE ("%s: SEGV while dereferencing at 0x%lx.  lowest_code_address = 0x%lx\n", __FILE__,
						(unsigned long int)codeptr, (unsigned long int)lowest_code_address);
				#endif
				continue;
			}
			/* They should be two halves */
#if defined(TARGET_CPU_SPARC)
			if (ins_1 != INS_SPARC_JMPIMMS) {
#elif defined(TARGET_CPU_PPC64)
			if (ins_1 != INS_PPC_JMPIMMS) {
#else
			if ((ins_1 != INS_X86_JMPIMMS8) && (ins_1 != INS_X86_JMPIMMS32)) {
#endif
				#ifdef DEBUG_DEBUG
					BMESSAGE ("%s: INS_X86_JMPIMMS8=0x%2.2x, INS_X86_JMPINNS32=0x%2.2x, read 0x%2.2x.\n", __FILE__, INS_X86_JMPIMMS8, INS_X86_JMPIMMS32, ins_1);
				#endif
				continue;
			}
#if defined(TARGET_CPU_SPARC) 
			if (ins_2 != 0x11) {
#elif defined(TARGET_CPU_PPC64)
			if (ins_2 != 0x60) {
#else
			if (ins_2 != 0x11) {
#endif
				#ifdef DEBUG_DEBUG
					BMESSAGE ("%s: jump (0x%2.2x) out of range (should be 0x11/0x60)\n", __FILE__, ins_2);
				#endif
				continue;
			}
			if (!setjmp (segenv)) {
				/* Could be likely.. */
#if defined(TARGET_CPU_SPARC) || defined(TARGET_CPU_PPC64)
				codeptr += 8;
#else
				if (ins_1 == INS_X86_JMPIMMS32) {
					codeptr += 5;
				} else {
					codeptr += 2;
				}
#endif
				/* codeptr should be pointing at 12 bytes of debugging info + 5 bytes of jump code */
#ifdef TARGET_BIGENDIAN
				ins_1 = codeptr[0];
				ins_2 = codeptr[1];
#else
				ins_1 = codeptr[3];
				ins_2 = codeptr[2];
#endif
				debug_kentry = ins_2;
				debug_dlop = ins_1;
#ifdef TARGET_BIGENDIAN
				ins_1 = codeptr[3];
				ins_2 = codeptr[2];
#else
				ins_1 = codeptr[0];
				ins_2 = codeptr[1];
#endif
				debug_line = (ins_2 << 8) + ins_1;
			} else {
				continue;
			}
			if ((debug_dlop >= num_dlops) || (debug_dlop <= 0)) {
				/* Well..  nearly */
				#ifdef DEBUG_DEBUG
					BMESSAGE ("%s: DLOP read as %d.  Bytes are: %2.2X %2.2X %2.2X %2.2X\n", __FILE__, debug_dlop, codeptr[0], codeptr[1],
						codeptr[2], codeptr[3]);
				#endif
				continue;
			}
			if (!setjmp (segenv)) {
				/* Next word is filename/procname */
				codeptr += 4;
				ins_1 = codeptr[0];
				ins_2 = codeptr[1];
				debug_proc = ((int)ins_2 << 8) + ins_1;
				ins_1 = codeptr[2];
				ins_2 = codeptr[3];
				debug_file = ((int)ins_2 << 8) + ins_1;
			} else {
				#ifdef DEBUG_DEBUG
					BMESSAGE ("%s: SEGV while deferencing at 0x%lx.  lowest_code_address = 0x%lx\n", __FILE__,
						(unsigned long int)codeptr, (unsigned long int)lowest_code_address);
				#endif
				continue;
			}
			if (!setjmp (segenv)) {
				/* Next word is some magic */
				codeptr += 4;
				ins_1 = codeptr[0];
				ins_2 = codeptr[1];
				if ((ins_1 != 0xde) || (ins_2 != 0xad)) {
					/* Incorrect first magic */
					#ifdef DEBUG_DEBUG
						BMESSAGE ("%s: Read magic as 0x%2.2x%2.2x, should have been 0xdead\n", __FILE__, ins_1, ins_2);
					#endif
					continue;
				}
				ins_1 = codeptr[2];
				ins_2 = codeptr[3];
				if ((ins_1 != 0xbe) || (ins_2 != 0xef)) {
					/* Incorrect second magic */
					#ifdef DEBUG_DEBUG
						BMESSAGE ("%s: Read magic as 0x%2.2x%2.2x, should have been 0xbeef\n", __FILE__, ins_1, ins_2);
					#endif
					continue;
				}
				/* Jump into codeptr... */
				codeptr += 4;
				ins_1 = *codeptr;
#if defined(TARGET_CPU_SPARC)
				if (ins_1 != SPARC_JUMP_INS) {
#elif defined(TARGET_CPU_PPC64)
				if (ins_1 != PPC_JUMP_INS) {
#else
				if (ins_1 != I386_JUMP_INS) {
#endif
					/* Not a jump :( */
					#ifdef DEBUG_DEBUG
						BMESSAGE ("%s: Expected opcode 0x.., found 0x%2.2x\n", __FILE__, ins_1);
					#endif
					continue;
				}
				DEADLOCK_CODE_BLOCK (debug_filename, debug_procname, codeptr);
			} else {
				#ifdef DEBUG_DEBUG
					BMESSAGE ("%s: SEGV while executing at 0x%lx\n", __FILE__, (unsigned long int)codeptr);
				#endif
				continue;
			}
			if (debug_filename < (char *)codeptr) {
				#ifdef DEBUG_DEBUG
					BMESSAGE ("%s: filename table (0x%lx) < codeptr (0x%lx)\n", __FILE__,
						(unsigned long int)debug_filename, (unsigned long int)codeptr);
				#endif
				continue;
			}
			if (debug_procname < (char *)codeptr) {
				#ifdef DEBUG_DEBUG
					BMESSAGE ("%s: procname table (0x%lx) < codeptr (0x%lx)\n", __FILE__,
						(unsigned long int)debug_procname, (unsigned long int)codeptr);
				#endif
				continue;
			}
			#ifdef DEBUG_DEBUG
			BMESSAGE ("%s: debug_file=0x%8.8x, debug_filename=0x%8.8x, debug_proc=0x%8.8x, debug_procname=0x%8.8x\n", __FILE__, (unsigned int)debug_file, (unsigned int)debug_filename, (unsigned int)debug_proc, (unsigned int)debug_procname);
			#endif
			/* Check them */
			if (debug_file >= *(int *)(debug_filename)) {
				#ifdef DEBUG_DEBUG
					BMESSAGE ("%s: filename %d out of range of table (%d)\n", __FILE__,
						debug_file, *(int *)(debug_filename));
				#endif
				continue;
			}
			if (debug_proc >= *(int *)(debug_procname)) {
				#ifdef DEBUG_DEBUG
					BMESSAGE ("%s: procname %d out of range of table (%d)\n", __FILE__,
						debug_proc, *(int *)(debug_procname));
				#endif
				continue;
			}
			/* Okay.. */
			debug_filename += *(int *)(debug_filename + ((1 + debug_file) * 4));
			debug_procname += *(int *)(debug_procname + ((1 + debug_proc) * 4));
			found++;
			if (!*did_print) {
				BMESSAGE ("Deadlock:\n");
				*did_print = 1;
			}
			debug_deadlock_out (stderr, dlops[debug_dlop], debug_procname, debug_filename, debug_line);
		}
	}
	signal (SIGSEGV, old_segv_handler);
	return found;
}
/*}}}*/
#endif	/* !defined(RMOX_BUILD) */

/*{{{  void ccsp_kernel_dead (int status)*/
/*
 *	called when the kernel deadlocks
 */
void ccsp_kernel_deadlock (void)
{
	if (att_val (&(ccsp_shutdown))) {
		ccsp_kernel_exit (0, 0);
	}

#if !defined(RMOX_BUILD)
	if (debug_dead) {
		int did_print = 0;

		if (att_cas (&deadlock_output, 0, 1)) {
			int found = 0;
			int i;

			for (i=0; i<num_ws; i++) {
				found += deadlock_debug ((int *)ws_ptrs[i], ws_sizes[i], &did_print);
			}
			if (!found) {
				BMESSAGE ("deadlock: no valid processes found in workspace(s)\n");
				debug_dead = 0;
			}
		} else {
			for (;;) {
				pause ();
			}
		}
	}
#else	/* defined(RMOX_BUILD) */
	ccsp_kernel_exit (2, 0);
#endif	/* defined(RMOX_BUILD) */

	if (!debug_dead) {
		ccsp_dead (0);
	} else {
		ccsp_dead_quiet (0);
	}
}
/*}}}*/

