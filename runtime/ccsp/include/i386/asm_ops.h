/*
 *	IA32 assembler inserts
 *	Copyright (C) 1999-2002 Fred Barnes  <frmb2@ukc.ac.uk>
 *	          (C) 2007      Carl Ritson  <cgr@kent.ac.uk>
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

#ifndef ASM_OPS_H
#define ASM_OPS_H

/*{{{  barriers */
#define compiler_barrier()	__asm__ __volatile__ ( ""       : : : "memory" )
#ifdef HAVE_IA32_SSE2
/* These are SSE2 instructions. */
#define strong_memory_barrier()	__asm__ __volatile__ ( "mfence"	: : : "memory" )
#define strong_read_barrier()  	__asm__ __volatile__ ( "lfence"	: : : "memory" )
#define strong_write_barrier()  __asm__ __volatile__ ( "sfence"	: : : "memory" )
#else
#define strong_memory_barrier()	compiler_barrier()
#define strong_read_barrier()  	compiler_barrier()
#define strong_write_barrier()  compiler_barrier()
#endif

#if MAX_RUNTIME_THREADS > 1
#define weak_memory_barrier() 	strong_memory_barrier()
#define weak_read_barrier() 	strong_read_barrier()
#define weak_write_barrier() 	compiler_barrier()
#else
#define weak_memory_barrier() 	compiler_barrier()
#define weak_read_barrier() 	compiler_barrier()
#define weak_write_barrier() 	compiler_barrier()
#endif /* MAX_RUNTIME_THREADS */
/*}}}*/

/*{{{  static inline unsigned int bsf (unsigned int v)*/
/* bit-scan forward */
static inline unsigned int bsf (unsigned int v)
{
	unsigned int res = 32;
	asm ("bsf %1,%0\n" : "=r" (res) : "r" (v) : "cc");
	return res;
}
/*}}}*/

/*{{{  static inline unsigned int bsr (unsigned int v)*/
/* bit-scan reverse */
static inline unsigned int bsr (unsigned int v)
{
	unsigned int res = 32;
	asm ("bsr %1,%0\n" : "=r" (res) : "r" (v) : "cc");
	return res;
}
/*}}}*/

/*{{{  static inline void cli (void)*/
/* clear interrupts enabled (for OOS build) */
static inline void cli (void) {
	__asm__ __volatile__ ("cli\n");
}
/*}}}*/

/*{{{  static inline void idle_cpu (void)*/
static inline void idle_cpu (void) {
	__asm__ __volatile__ ("	\n"
		"	pause	\n"
		"	pause	\n"
		"	pause	\n"
		"	pause	\n"
	);
}
/*}}}*/

/*{{{  static inline unsigned int one_if_nz (unsigned int value, unsigned int mask)*/
static inline unsigned int one_if_nz (unsigned int value, unsigned int mask) {
	unsigned char result;
	asm (	"			\n"
		"	test %1,%2	\n" 
		"	setnz %0	\n"
		: "=q" (result)
		: "ir" (mask), "r" (value)
		: "cc"
	);
	return (unsigned int) result;
}
/*}}}*/

/*{{{  static inline unsigned int one_if_z (unsigned int value, unsigned int mask)*/
static inline unsigned int one_if_z (unsigned int value, unsigned int mask) {
	unsigned char result;
	asm (	"			\n"
		"	test %1,%2	\n" 
		"	setz %0		\n"
		: "=q" (result)
		: "ir" (mask), "r" (value)
		: "cc"
	);
	return (unsigned int) result;
}
/*}}}*/

/*{{{  static inline unsigned int pick_random_bit (unsigned int mask)*/
static inline unsigned int pick_random_bit (unsigned int mask) {
#ifdef ENABLE_CPU_TIMERS
	unsigned int l, r;

	l = bsf (mask);
	r = bsr (mask);

	if (l != r) {
		unsigned int m, shift;

		m = 0xffffffff >> (32 - bsr ((r - l) << 1));

		/* generate a shift from the tsc */
		__asm__ __volatile__ (	"		\n"
			"	rdtsc			\n"
			"	andl %1, %%eax		\n"
			: "=a" (shift)
			: "r" (m)
			: "edx"
		);
		
		shift 	= (shift + l) & 0x1f;
		m 	= (mask >> shift) | (mask << (32 - shift));

		return (bsf (m) + shift) & 0x1f;
	} else {
		return l & 0x1f;
	}
#else
	return bsf (mask);
#endif
}
/*}}}*/

/*{{{  static inline void serialise (void)*/
/* serialises the instruction stream - the strongest form of barrier */
static inline void serialise (void) {
	__asm__ __volatile__ ("		\n"
		"	movl $0, %%eax	\n"
		"	cpuid		\n"
		: /* no outputs */
		: /* no inputs */
		: "cc", "memory", "eax", "ebx", "ecx", "edx"
	);
}
/*}}}*/

/*{{{  static inline void sti (void)*/
/* set interrupts enabled (for OOS build) */
static inline void sti (void) {
	__asm__ __volatile__ ("sti\n");
}
/*}}}*/

/*{{{  static inline void xmemcpy (void *src, void *dst, unsigned int count)*/
static inline void xmemcpy (void *src, void *dst, unsigned int count) {
	asm (	"		\n"
    		"	cld	\n"
		"	rep	\n"
    		"	movsb	\n"
		: /* no outputs */
		: "S" (src), "D" (dst), "c" (count)
	);
}
/*}}}*/

#endif /* !ASM_OPS_H */

