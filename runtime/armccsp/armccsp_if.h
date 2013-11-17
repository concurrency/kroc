/*
 *	armccsp_if.h -- ARM/CCSP public interface for CIF style code
 *	Copyright (C) 2013 Fred Barnes, University of Kent <frmb@kent.ac.uk>
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
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 */

#ifndef __ARMCCSP_IF_H
#define __ARMCCSP_IF_H

/*{{{  types (that need to be visible to applications using CIF/CCSP only)*/
typedef uint32_t *Workspace;
typedef void *Channel;
typedef uint32_t word;


/*}}}*/
/*{{{  kernel call constants, must stay consistent with table in kernel.c*/
#define CALL_CHANOUT 0
#define CALL_CHANIN 1
#define CALL_SHUTDOWN 2
#define CALL_PAUSE 3
#define CALL_SETERR 4
#define CALL_SETERRM 5
#define CALL_MALLOC 6
#define CALL_MRELEASE 7

/*}}}*/
/*{{{  functions that should only ever be called in the context of the main program, i.e. from main()*/
extern int ccsp_init (void);
extern Workspace ProcAllocInitial (const int paramwords, const int stackwords);

extern void ProcStartInitial_blind (Workspace p, void (*fcn)(Workspace));
#define ProcStartInitial(P,FCN) ProcStartInitial_blind(P,(void (*)(Workspace))FCN)

#define WORKSPACE_SIZE(args,stack) \
	((args) + (stack) + 16)

/*}}}*/

/*{{{  ENTER_KERNEL(proc,call,args): abstraction for kernel entry proper*/
#define ENTER_KERNEL(p,c,a) do { \
	__asm__ __volatile__ ("							\n" \
		"	mov	r0, %0						\n" \
		"	add	r3, r0, #4		@ r3 = &(p->stack)	\n" \
		"	push	{lr}			@ save link-register	\n" \
		"	str	sp, [r3, #0]		@ p->stack = sp		\n" \
		"	add	r3, r0, #8		@ r3 = &(p->raddr)	\n" \
		"	adr	r2, 0f						\n" \
		"	str	r2, [r3, #0]		@ p->raddr = &.Lcont	\n" \
		"								\n" \
		"	ldr	r2, [r0, #0]		@ r2 = (p->sched)	\n" \
		"	ldr	r3, [r2, #0]		@ r3 = p->sched->stack	\n" \
		"	mov	sp, r3			@ switch stacks		\n" \
		"	mov	r1, %1						\n" \
		"	mov	r2, %2						\n" \
		"	bl	ccsp_entry(PLT)					\n" \
		"0:								\n" \
		"					@ note: when we get back\n" \
		"					@ r0 = process-desc	\n" \
		"	add	r3, r0, #4		@ r3 = &(p->stack)	\n" \
		"	ldr	r1, [r3, #0]		@ r1 = p->stack		\n" \
		"	mov	sp, r1			@ switch back		\n" \
		"	pop	{lr}			@ restore link-register	\n" \
		"								\n" \
		: : "r" (p), "r" (c), "r" (a) \
		: "memory", "cc", "r0", "r1", "r2", "r3"); \
	} while (0)

/*}}}*/

/*
 * BELOW: the CIF API for ARM/CCSP.
 */

/*{{{  static inline void ChanInit (Workspace p, Channel *c)*/
/*
 *	initialises a channel (blanks it).
 */
static inline void ChanInit (Workspace p, Channel *c)
{
	*c = NULL;
}
/*}}}*/
/*{{{  static inline void ChanOut (Workspace p, Channel *c, void *ptr, const int bytes)*/
/*
 *	channel output.
 */
static inline void ChanOut (Workspace p, Channel *c, void *ptr, const int bytes)
{
	void *dargs[4] = {(void *)p, (void *)c, ptr, (void *)bytes};
	int call = CALL_CHANOUT;

	ENTER_KERNEL (p, call, dargs);
}
/*}}}*/
/*{{{  static inline void ChanIn (Workspace p, Channel *c, void *ptr, const int bytes)*/
/*
 *	channel input.
 */
static inline void ChanIn (Workspace p, Channel *c, void *ptr, const int bytes)
{
	void *dargs[4] = {(void *)p, (void *)c, ptr, (void *)bytes};
	int call = CALL_CHANIN;

	ENTER_KERNEL (p, call, dargs);
}
/*}}}*/
/*{{{  static inline void Reschedule (Workspace p)*/
/*
 *	reschedules -- scheduler yield.
 */
static inline void Reschedule (Workspace p)
{
	void *dargs[1] = {(void *)p};
	int call = CALL_PAUSE;

	ENTER_KERNEL (p, call, dargs);
}
/*}}}*/
/*{{{  static inline void SetErr (Workspace p)*/
/*
 *	hard run-time error.
 */
static inline void SetErr (Workspace p)
{
	void *dargs[1] = {(void *)p};
	int call = CALL_SETERR;

	ENTER_KERNEL (p, call, dargs);
}
/*}}}*/
/*{{{  static inline void SetErrM (Workspace p)*/
/*
 *	hard run-time error (with message).
 */
static inline void SetErrM (Workspace p, const char *msg)
{
	void *dargs[2] = {(void *)p, (void *)msg};
	int call = CALL_SETERRM;

	ENTER_KERNEL (p, call, dargs);
}
/*}}}*/
/*{{{  static inline void Shutdown (Workspace p)*/
/*
 *	shuts down the run-time system; last thing an application does.
 */
static inline void Shutdown (Workspace p)
{
	void *dargs[1] = {(void *)p};
	int call = CALL_SHUTDOWN;

	ENTER_KERNEL (p, call, dargs);
}
/*}}}*/
/*{{{  static inline void *MAlloc (Workspace p, const int bytes)*/
/*
 *	dynamic memory allocation.
 */
static inline void *MAlloc (Workspace p, const int bytes)
{
	void *ptr;
	void *dargs[3] = {(void *)p, (void *)bytes, (void **)&ptr};
	int call = CALL_MALLOC;

	ENTER_KERNEL (p, call, dargs);

	return ptr;
}
/*}}}*/
/*{{{  static inline void MRelease (Workspace p, void *ptr)*/
/*
 *	dynamic memory release.
 */
static inline void MRelease (Workspace p, void *ptr)
{
	void *dargs[2] = {(void *)p, ptr};
	int call = CALL_MRELEASE;

	ENTER_KERNEL (p, call, dargs);
}
/*}}}*/


/*{{{  other things which are not macros, but defined in kfunc.c*/
extern Workspace LightProcInit (Workspace p, word *stack, const int nparams, const int stkwords);
extern void ProcParamAny (Workspace p, Workspace other, int paramno, void *arg);
extern void *ProcGetParamAny (Workspace p, int paramno);

#define ProcParam(p,o,n,a) do { ProcParamAny (p, o, n, (void *)a); } while (0)
#define ProcGetParam(p,n,T) ((T)ProcGetParamAny (p, n))

extern void ProcPar (Workspace p, int nprocs, ...);


/*}}}*/


/*
 * BELOW: assorted macros/etc. for use by the run-time system (keeps assembler chunks all in one place).
 */

/*{{{  static inline void ProcessResume (Workspace p)*/
/*
 *	called from the run-time to resume a process, ensures r0 = process-descriptor.
 */
static inline void ProcessResume (Workspace p)
{
	__asm__ __volatile__ ( \
		"	mov	r0, %0						\n" \
		"	add	r1, r0, #8		@ r1 = &(p->raddr)	\n" \
		"	ldr	r2, [r1, #0]		@ r2 = p->raddr		\n" \
		"	bx	r2						\n" \
		: : "r" (p) \
		: "memory", "cc", "r0", "r1", "r2", "r3");
}
/*}}}*/
/*{{{  static inline void RuntimeSaveStack (Workspace p)*/
/*
 *	takes a note of the stack-pointer and stores it in the global scheduler structure (accessed through 'p').
 */
static inline void RuntimeSaveStack (Workspace p)
{
	__asm__ __volatile__ ( \
		"	mov	r0, %0						\n" \
		"	ldr	r1, [r0, #0]		@ r1 = p->sched		\n" \
		"	str	sp, [r1, #0]		@ p->sched->stack = sp	\n" \
		: : "r" (p) \
		: "memory", "cc", "r0", "r1", "r2", "r3");
}
/*}}}*/
/*{{{  static inline void RuntimeSetEntry (Workspace p)*/
/*
 *	provides an entry-point for processes that can be ProcessResume()'d, simply
 *	branches off to ProcStartupCode (in kfunc.c).
 */
static inline void RuntimeSetEntry (Workspace p)
{
	__asm__ __volatile__ ( \
		"	mov	r0, %0						\n" \
		"	add	r1, r0, #8		@ r1 = &(p->raddr)	\n" \
		"	adr	r2, 0f						\n" \
		"	str	r2, [r1, #0]		@ p->raddr = .Lstart	\n" \
		"	b	1f						\n" \
		"0:								\n" \
		"	add	r3, r0, #4		@ r3 = &(p->stack)	\n" \
		"	ldr	r1, [r3, #0]		@ r1 = p->stack		\n" \
		"	mov	sp, r1			@ switch stack		\n" \
		"	bl	ProcStartupCode(PLT)	@ r0 = process-descriptor	\n" \
		"1:								\n" \
		"								\n" \
		: : "r" (p) \
		: "memory", "cc", "r0", "r1", "r2", "r3");
}
/*}}}*/

#endif	/* !__ARMCCSP_IF_H */

