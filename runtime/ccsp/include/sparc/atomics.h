/*
 *	atomics.h -- atomic/synchronising operations
 *	Copyright (C) 2004 Fred Barnes
 *	Some code based on include/asm-sparc/spinlock.h in the Linux kernel
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

typedef struct {
	volatile unsigned int lock;
} spl_t;

#define SPL_UNLOCKED	(spl_t) { 0 }

typedef struct { unsigned long a[100]; } __dummy_lock_t;
#define __dummy_lock(lock) (*(__dummy_lock_t *)(lock))

#define spl_init(X)	do { *(X) = SPL_UNLOCKED; } while (0)

static inline int spl_lock_or_fail (spl_t *lock)
{
	register int result;

	__asm__ __volatile__(
		"				\n"
		"	ldstub [%1], %0		\n"
		: "=r" (result)
		: "r" (lock)
		: "memory");
	return (result == 0);
}

static inline void spl_lock (spl_t *lock)
{
	__asm__ __volatile__(
		"				\n"
		"1:				\n"
		"	ldstub	[%1], %%g2	\n"
		"	orcc	%%g2, 0x0, %%g0	\n"
		"	bne,a	2f		\n"
		"	ldub	[%0], %%g2	\n"
		".subsection 2			\n"
		"2:				\n"
		"	orcc	%%g2, 0x0, %%g0	\n"
		"	bne,a	2b		\n"
		"	b,a	1b		\n"
		".previous			\n"
		: /* no outputs */
		: "r" (lock)
		: "g2", "cc", "memory");
}

static inline void spl_unlock (spl_t *lock)
{
	__asm__ __volatile__(
		"	stb	%%g0, [%0]	\n"
		: /* no outputs */
		: "r" (lock)
		: "memory");
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


#define att_init(X)	do { *(X) = ATT_INITIAL; } while (0)

extern inline int att_val (atomic_t *atval)
{
	register int result;

	__asm__ __volatile__ (
		"				\n"
		"	ld	[%1], %0	\n"
	: "=g" (result)
	: "r" (atval)
	: "memory");
	return result;
}

extern inline void att_inc (atomic_t *atval)
{
	__asm__ __volatile__(
		"				\n"
		"	ld	[%1], %%l0	\n"
		"	add	%%l0, 0x1, %%x0	\n"
		"	st	%%l0, [%1]	\n"
		: /* no output */
		: "r" (atval)
		: "memory", "l0");
}

extern inline void att_dec (atomic_t *atval)
{
	__asm__ __volatile__(
		"				\n"
		"	ld	[%1], %%l0	\n"
		"	sub	%%l0, 0x1, %%x0	\n"
		"	st	%%l0, [%1]	\n"
		: /* no output */
		: "r" (atval)
		: "memory", "l0");
}

/*
 *	Other, generally useful, stuff
 */

#define barrier() __asm__ __volatile__ ("": : :"memory")

#define XCHG(var,ptr) \
	__asm__ __volatile__ ( \
		"				\n" \
		"	swap	[%1], %0	\n" \
		: "=r" (var), "=r" (ptr) : "r" (var), "r" (ptr) : "cc", "memory")


#endif	/* !__ATOMICS_H */

