/*
 *	atomics.h -- atomic/synchronising operations
 *	Copyright (C) 2005 Fred Barnes
 *	Some code based on include/asm-ppc/spinlock.h in the Linux kernel
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

#ifndef __ATOMICS_H
#define __ATOMICS_H

/*
 *	Stuff for spinlocks
 */

#ifdef USE_PTHREADS
/*
 *	if we're using POSIX threads, use a pthread_mutex_t instead
 *	of the spinlock.
 */
typedef pthread_mutex_t spl_t;

#define spl_init(X)		pthread_mutex_init((X), NULL)
#define spl_lock_or_fail(X)	pthread_mutex_trylock((X))
#define spl_lock(X)		pthread_mutex_lock((X))
#define spl_unlock(X)		pthread_mutex_unlock((X))


#else	/* !USE_PTHREADS */

#ifdef __powerpc64__

/* do error -- just the 32-bit target at the moment */
#error not supporting 64-bit code yet

#endif	/* __powerpc64__ */

typedef struct {
	volatile unsigned int lock;
} spl_t;

#define SPL_UNLOCKED	(spl_t) { 0 }

typedef struct { unsigned long a[100]; } __dummy_lock_t;
#define __dummy_lock(lock) (*(__dummy_lock_t *)(lock))

#define spl_init(X)	do { *(X) = SPL_UNLOCKED; } while (0)

static inline int spl_lock_or_fail (spl_t *lock)
{
	unsigned int old, t;
	unsigned int mask = 1;

	__asm__ __volatile__("				\n"
		"	eieio				\n"
		"1:	lwarx	%0,0,%4			\n"
		"	or	%1,%0,%3		\n"
		"	stwcx.	%1,0,%4			\n"
		"	bne	1b			\n"
		"	sync				\n"
		: "=&r" (old), "=&r" (t), "=m" (lock->lock)
		: "r" (mask), "r" (&(lock->lock)), "m" (lock->lock)
		: "cc", "memory");

	return ((old & mask) == 0);
}

static inline void spl_lock (spl_t *lock)
{
	unsigned long tmp;

	__asm__ __volatile__(
		"					\n"
		"	b	1f	# spin_lock	\n"
		"2:	lwzx	%0,0,%1			\n"
		"	cmpwi	0,%0,0			\n"
		"	bne+	2b			\n"
		"1:	lwarx	%0,0,%1			\n"
		"	cmpwi	0,%0,0			\n"
		"	bne-	2b			\n"
		"	stwcx.	%2,0,%1			\n"
		"	bne-	2b			\n"
		"	isync				\n"
		: "=&r" (tmp)
		: "r" (&lock->lock), "r" (1)
		: "cr0", "memory");
}

static inline void spl_unlock (spl_t *lock)
{
	__asm__ __volatile__(
		"	eieio		# spin_unlock	\n"
		: /* no outputs */
		: /* no inputs */
		: "memory");
	lock->lock = 0;
}

#endif	/* !USE_PTHREADS */


/*
 *	Stuff for atomic INC/DEC
 */

typedef struct {
	volatile unsigned int value;
} atomic_t;

#define ATT_INITIAL	(atomic_t) { 0 }

typedef struct { unsigned long a[100]; } __dummy_atomic_t;
#define __dummy_atomic(val) (*(__dummy_atomic_t *)(val))


#define att_init(X)	do { (X)->value = ATT_INITIAL; } while (0)

extern inline int att_val (atomic_t *atval)
{
	return (int)atval->value;
}

extern inline void att_inc (atomic_t *atval)
{
	int t;

	__asm__ __volatile__(
		"						\n"
		"1:	lwarx	%0,0,%2		# atomic_inc	\n"
		"	addic	%0,0,1				\n"
		"	stwcx.	%0,0,%2				\n"
		"	bne-	1b				\n"
		: "=&r" (t), "=m" (atval->value)
		: "r" (&atval->value), "m" (atval->value)
		: "cc");
}

extern inline void att_dec (atomic_t *atval)
{
	int t;

	__asm__ __volatile__(
		"						\n"
		"1:	lwarx	%0,0,%2		# atomic_inc	\n"
		"	addic	%0,0,-1				\n"
		"	stwcx.	%0,0,%2				\n"
		"	bne-	1b				\n"
		: "=&r" (t), "=m" (atval->value)
		: "r" (&atval->value), "m" (atval->value)
		: "cc");
}

/*
 *	Other, generally useful, stuff
 */

#define barrier() __asm__ __volatile__ ("": : :"memory")

extern inline unsigned long XCHG_u32 (unsigned long val, volatile void *p)
{
	unsigned long prev;

	__asm__ __volatile__(
		"						\n"
		"1:	lwarx	%0,0,%2				\n"
		"	stwcx.	%3,0,%2				\n"
		"	bne-	1b				\n"
		: "=&r" (prev), "=m" (*(volatile unsigned long *)p)
		: "r" (p), "r" (val), "m" (*(volatile unsigned long *)p)
		: "cc", "memory");

	return prev;
}

#define XCHG(var,ptr) \
	{ var = XCHG_u32 (var, ptr); }


#endif	/* !__ATOMICS_H */

