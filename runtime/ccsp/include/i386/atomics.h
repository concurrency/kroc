/*
 *	atomics.h -- atomic/synchronising operations
 *	Copyright (C) 2000-2004 Fred Barnes
 *	Modifications Copyright (C) 2007 Carl Ritson <cgr@kent.ac.uk>
 *	Some code based on include/asm-i386/spinlock.h in the Linux kernel
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

#include <inlining.h>

#ifndef I386_ATOMICS_H
#define I386_ATOMICS_H

#ifdef __GNUC__
#define _PACK_STRUCT __attribute__ ((packed))
#else
#warning "Unable to enforce alignment and packing on structures."
#define _PACK_STRUCT
#endif

typedef struct _atomic_t atomic_t;
struct _atomic_t {
	#if MAX_RUNTIME_THREADS > 1
	volatile unsigned int value;
	#else
	unsigned int value;
	#endif
} _PACK_STRUCT;

#undef _PACK_STRUCT

typedef struct { unsigned long a[100]; } __dummy_atomic_t;
#define __dummy_atomic(val) (*(__dummy_atomic_t *)(val))

#define att_init(X, V)	do { (X)->value = (V); } while (0)

/*{{{  unsigned int att_val (atomic_t *atval)*/
static INLINE unsigned int att_safe_val (atomic_t *atval)
{
	unsigned int result;

	__asm__ __volatile__ ("		\n"
		"	movl %1,%0	\n"
		: "=r" (result)
		: "m" (__dummy_atomic(atval))
	);

	return result;
}
#define att_unsafe_val(X) 	((X)->value)
/*}}}*/
/*{{{  void att_set (atomic_t *atval, unsigned int value)*/
static INLINE void att_safe_set (atomic_t *atval, unsigned int value)
{
	__asm__ __volatile__ ("		\n"
		"	movl %1,%0	\n"
		: "=m" (__dummy_atomic(atval))
		: "r" (value)
	);
}
#define att_unsafe_set(X,V)	do { (X)->value = (V); } while (0)
/*}}}*/
/*{{{  void att_inc (atomic_t *atval)*/
static INLINE void att_safe_inc (atomic_t *atval)
{
	__asm__ __volatile__ ("		\n" 
		"	lock; addl $1,%0\n"
		: "+m" (__dummy_atomic(atval))
		: /* no inputs */
		: "cc"
	);
}
#define att_unsafe_inc(X)	do { (X)->value++; } while (0)
/*}}}*/
/*{{{  void att_dec (atomic_t *atval)*/
static INLINE void att_safe_dec (atomic_t *atval)
{
	__asm__ __volatile__ ("		\n"
		"	lock; subl $1,%0\n"
		: "+m" (__dummy_atomic(atval))
		: /* no inputs */
		: "cc"
	);
}
#define att_unsafe_dec(X)	do { (X)->value--; } while (0)
/*}}}*/
/*{{{  unsigned int att_dec_z (atomic_t *atval)*/
static INLINE unsigned int att_safe_dec_z (atomic_t *atval)
{
	unsigned char result;

	__asm__ __volatile__ ("		\n"
		"	lock; subl $1,%0\n"
		"	setz %1		\n"
		: "+m" (__dummy_atomic(atval)), "=q" (result)
		: /* no inputs */
		: "cc"
	);

	return (unsigned int) result;
}
#define att_unsafe_dec_z(X)	(!(--((X)->value)))
/*}}}*/
/*{{{  void att_add (atomic_t *atval, unsigned int value)*/
static INLINE void att_safe_add (atomic_t *atval, unsigned int value)
{
	__asm__ __volatile__ ("		\n" 
		"	lock; addl %1,%0\n"
		: "+m" (__dummy_atomic(atval))
		: "ir" (value)
		: "cc"
	);
}
#define att_unsafe_add(X,V)	do { (X)->value += (V); } while (0)
/*}}}*/
/*{{{  void att_sub (atomic_t *atval, unsigned int value)*/
static INLINE void att_safe_sub (atomic_t *atval, unsigned int value)
{
	__asm__ __volatile__ ("		\n" 
		"	lock; subl %1,%0\n"
		: "+m" (__dummy_atomic(atval))
		: "ir" (value)
		: "cc"
	);
}
#define att_unsafe_sub(X,V)	do { (X)->value -= (V); } while (0)
/*}}}*/
/*{{{  unsigned int att_sub_z (atomic_t *atval, unsigned int value)*/
static INLINE unsigned int att_safe_sub_z (atomic_t *atval, unsigned int value)
{
	unsigned char result;

	__asm__ __volatile__ ("		\n"
		"	lock; subl %2,%0\n"
		"	setz %1		\n"
		: "+m" (__dummy_atomic(atval)), "=q" (result)
		: "ir" (value)
		: "cc"
	);

	return (unsigned int) result;
}

static INLINE unsigned int att_unsafe_sub_z (atomic_t *atval, unsigned int value)
{
	unsigned char result;

	__asm__ __volatile__ ("		\n"
		"	subl %2,%0	\n"
		"	setz %1		\n"
		: "+g" (atval->value), "=q" (result)
		: "ir" (value)
		: "cc"
	);

	return (unsigned int) result;
}
/*}}}*/
/*{{{  void att_or (atomic_t *atval, unsigned int bits)*/
static INLINE void att_safe_or (atomic_t *atval, unsigned int bits)
{
	__asm__ __volatile__ ("		\n"
		"	lock; orl %1,%0	\n"
		: "+m" (__dummy_atomic(atval))
		: "ir" (bits)
	);
}
#define att_unsafe_or(X,M)	do { (X)->value |= (M); } while (0)
/*}}}*/
/*{{{  void att_and (atomic_t *atval, unsigned int bits)*/
static INLINE void att_safe_and (atomic_t *atval, unsigned int bits)
{
	__asm__ __volatile__ ("		\n"
		"	lock; andl %1,%0\n"
		: "+m" (__dummy_atomic(atval))
		: "ir" (bits)
	);
}
#define att_unsafe_and(X,M)	do { (X)->value &= (M); } while (0)
/*}}}*/
/*{{{  unsigned int att_swap (atomic_t *atval, unsigned int nv)*/
static INLINE unsigned int att_safe_swap (atomic_t *atval, unsigned int nv)
{
	__asm__ __volatile__ ("		\n"
		"	xchgl %0,%1	\n"
		: "=r" (nv), "+m" (__dummy_atomic(atval))
		: "0" (nv)
		: "memory"
	);

	return nv;
}

static INLINE unsigned int att_unsafe_swap (atomic_t *atval, unsigned int nv) {
	unsigned int ov = atval->value;
	atval->value = nv;
	return ov;
}
/*}}}*/
/*{{{  unsigned int att_cas (atomic_t *atval, unsigned int ov, unsigned int nv) */
static INLINE unsigned int att_safe_cas (atomic_t *atval, unsigned int ov, unsigned int nv)
{
	unsigned int result;
	
	__asm__ __volatile__ ("			\n"
		"	lock; cmpxchgl %3,%1	\n"
		"	setz %%al		\n"
		"	andl $1, %%eax		\n"
		: "=a" (result), "+m" (__dummy_atomic(atval))
		: "0" (ov), "r" (nv)
		: "cc", "memory"
	);

	return result;
}

static INLINE unsigned int att_unsafe_cas (atomic_t *atval, unsigned int ov, unsigned int nv)
{
	if (atval->value == ov) {
		atval->value = nv;
		return 1;
	}
	return 0;
}
/*}}}*/
/*{{{  void att_set_bit (atomic_t *atval, unsigned int bit)*/
static INLINE void att_safe_set_bit (atomic_t *atval, unsigned int bit)
{
	__asm__ __volatile__ ("		\n"
		"	lock; btsl %1,%0\n"
		: "+m" (__dummy_atomic(atval))
		: "Ir" (bit)
		: "cc"
	);
}

static INLINE void att_unsafe_set_bit (atomic_t *atval, unsigned int bit)
{
	__asm__ __volatile__ ("		\n"
		"	btsl %1,%0	\n"
		: "+g" (atval->value)
		: "Ir" (bit)
		: "cc"
	);
}
/*}}}*/
/*{{{  void att_clear_bit (atomic_t *atval, unsigned int bit)*/
static INLINE void att_safe_clear_bit (atomic_t *atval, unsigned int bit)
{
	__asm__ __volatile__ ("		\n"
		"	lock; btrl %1,%0\n"
		: "+m" (__dummy_atomic(atval))
		: "Ir" (bit)
		: "cc"
	);
}

static INLINE void att_unsafe_clear_bit (atomic_t *atval, unsigned int bit)
{
	__asm__ __volatile__ ("		\n"
		"	btrl %1,%0	\n"
		: "+g" (atval->value)
		: "Ir" (bit)
		: "cc"
	);
}
/*}}}*/
/*{{{  unsigned int att_test_set_bit (atomic_t *atval, unsigned int bit)*/
static INLINE unsigned int att_safe_test_set_bit (atomic_t *atval, unsigned int bit)
{
	unsigned char result;

	__asm__ __volatile__ ("		\n"
		"	lock; btsl %2,%0\n"
		"	setc %1		\n"
		: "+m" (__dummy_atomic(atval)), "=q" (result)
		: "Ir" (bit)
		: "cc"
	);

	return result;
}

static INLINE unsigned int att_unsafe_test_set_bit (atomic_t *atval, unsigned int bit)
{
	unsigned char result;

	__asm__ __volatile__ ("		\n"
		"	btsl %2,%0	\n"
		"	setc %1		\n"
		: "+g" (atval->value), "=q" (result)
		: "Ir" (bit)
		: "cc"
	);

	return result;
}
/*}}}*/
/*{{{  unsigned int att_test_clear_bit (atomic_t *atval, unsigned int bit)*/
static INLINE unsigned int att_safe_test_clear_bit (atomic_t *atval, unsigned int bit)
{
	unsigned char result;

	__asm__ __volatile__ ("		\n"
		"	lock; btrl %2,%0\n"
		"	setc %1		\n"
		: "+m" (__dummy_atomic(atval)), "=q" (result)
		: "Ir" (bit)
		: "cc"
	);

	return result;
}

static INLINE unsigned int att_unsafe_test_clear_bit (atomic_t *atval, unsigned int bit)
{
	unsigned char result;

	__asm__ __volatile__ ("		\n"
		"	btrl %2,%0	\n"
		"	setc %1		\n"
		: "+g" (atval->value), "=q" (result)
		: "Ir" (bit)
		: "cc"
	);

	return result;
}
/*}}}*/

/*{{{  atomic mappings */
#if MAX_RUNTIME_THREADS > 1
#define att_val(X)		att_safe_val(X)
#define att_set(X,V)		att_safe_set(X, V)
#define att_inc(X)		att_safe_inc(X)
#define att_dec(X)		att_safe_dec(X)
#define att_dec_z(X)		att_safe_dec_z(X)
#define att_add(X,V)		att_safe_add(X, V)
#define att_sub(X,V)		att_safe_sub(X, V)
#define att_sub_z(X,V)		att_safe_sub_z(X, V)
#define att_and(X,M)		att_safe_and(X, M)
#define att_swap(X,V)		att_safe_swap(X, V)
#define att_cas(X,O,N)		att_safe_cas(X, O, N)
#define	att_set_bit(X,B) 	att_safe_set_bit(X, B)
#define att_clear_bit(X,B)	att_safe_clear_bit(X, B)
#define att_test_set_bit(X,B)	att_safe_test_set_bit(X, B)
#define att_test_clear_bit(X,B)	att_safe_test_clear_bit(X, B)
#else
#define att_val(X)		att_unsafe_val(X)
#define att_set(X,V)		att_unsafe_set(X, V)
#define att_inc(X)		att_unsafe_inc(X)
#define att_dec(X)		att_unsafe_dec(X)
#define att_dec_z(X)		att_unsafe_dec_z(X)
#define att_add(X,V)		att_unsafe_add(X, V)
#define att_sub(X,V)		att_unsafe_sub(X, V)
#define att_sub_z(X,V)		att_unsafe_sub_z(X, V)
#define att_or(X,M)		att_unsafe_or(X, M)
#define att_and(X,M)		att_unsafe_and(X, M)
#define att_swap(X,V)		att_unsafe_swap(X, V)
#define att_cas(X,O,N)		att_unsafe_cas(X, O, N)
#define	att_set_bit(X,B) 	att_unsafe_set_bit(X, B)
#define att_clear_bit(X,B)	att_unsafe_clear_bit(X, B)
#define att_test_set_bit(X,B)	att_unsafe_test_set_bit(X, B)
#define att_test_clear_bit(X,B)	att_unsafe_test_clear_bit(X, B)
#endif
/*}}}*/

/*{{{  word size atomics */
#define atw_val(W) 		((word) (att_val ((atomic_t *)((W)))))
#define atw_set(W,V) 		att_set ((atomic_t *)((W)), (unsigned int)(V))
#define atw_safe_val(W) 	((word) (att_safe_val ((atomic_t *)((W)))))
#define atw_safe_set(W,V)	att_safe_set ((atomic_t *)((W)), (unsigned int)(V))
#define atw_set(W,V) 		att_set ((atomic_t *)((W)), (unsigned int)(V))
#define atw_inc(W) 		att_inc ((atomic_t *)((W)))
#define atw_dec(W) 		att_dec ((atomic_t *)((W)))
#define atw_dec_z(W) 		(att_dec_z ((atomic_t *)((W))))
#define atw_add(W,V) 		att_add ((atomic_t *)((W)), (V))
#define atw_sub(W,V) 		att_sub ((atomic_t *)((W)), (V))
#define atw_sub_z(W,V) 		(att_sub_z ((atomic_t *)((W)), (V)))
#define atw_and(W,M)		att_and ((atomic_t *)((W)), (M))
#define atw_safe_swap(W,V)	((word) (att_safe_swap ((atomic_t *)((W)), (V))))
#define atw_swap(W,V)		((word) (att_swap ((atomic_t *)((W)), (V))))
#define atw_safe_cas(W,O,N)	(att_safe_cas ((atomic_t *)((W)), (O), (N)))
#define atw_cas(W,O,N)		(att_cas ((atomic_t *)((W)), (O), (N)))
#define	atw_set_bit(W,B) 	att_set_bit ((atomic_t *)((W)), (B))
#define atw_clear_bit(W,B)	att_clear_bit ((atomic_t *)((W)), (B))
#define atw_test_set_bit(W,B)	(att_test_set_bit ((atomic_t *)((W)), (B)))
#define atw_test_clear_bit(W,B)	(att_test_clear_bit ((atomic_t *)((W)), (B)))
/*}}}*/

#endif	/* !I386_ATOMICS_H */

