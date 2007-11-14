/*
 *	atomics.h -- atomic/synchronising operations.  MIPS version
 *	Copyright (C) 2002 Fred Barnes
 *	Some code based on include/asm-mips{,64}/spinlock.h in the Linux kernel
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

typedef struct {
	volatile unsigned int lock;
} spl_t;

#define SPL_UNLOCKED	(spl_t) { 0 }

typedef struct { unsigned long a[100]; } __dummy_lock_t;
#define __dummy_lock(lock) (*(__dummy_lock_t *)(lock))


#define spl_init(X)	do { *(X) = SPL_UNLOCKED; } while (0)

static inline int spl_lock_or_fail (spl_t *lock)
{
	unsigned int temp, result;

	__asm__ __volatile__("			\n" \
		" .set mips3		\n" \
		"	.set	noreorder	\n" \
		"1:	ll	%0, %1		\n" \
		"	or	%2, %0, %3	\n" \
		"	sc	%2, %1		\n" \
		"	beqz	%2, 1b		\n" \
		"	and	%2, %0, %3	\n" \
		"	.set	reorder		\n" \
		" .set mips0		\n" \
		: "=&r" (temp), "=m" (lock->lock), "=&r" (result) \
		: "r" (1), "m" (lock->lock) \
		: "memory");
	return (result == 0);
}

static inline void spl_lock (spl_t *lock)
{
	unsigned int temp;

	__asm__ __volatile__("			\n" \
		" .set mips3		\n" \
		"	.set noreorder	\n" \
		"1:	ll	%1, %2		\n" \
		"	bnez	%1, 1b		\n" \
		"	li	%1, 1		\n" \
		"	sc	%1, %0		\n" \
		"	beqz	%1, 1b		\n" \
		"	sync			\n" \
		"	.set reorder		\n" \
		" .set mips0		\n" \
		: "=o" (lock->lock), "=&r" (temp) \
		: "o" (lock->lock)
		: "memory");
}

static inline void spl_unlock (spl_t *lock)
{
	__asm__ __volatile__("			\n" \
		" .set mips3    \n" \
		"	.set noreorder		\n" \
		"	sync			\n" \
		"	sw	$0, %0		\n" \
		"	.set reorder		\n" \
		" .set mips0    \n" \
		: "=o" (lock->lock) \
		: "o" (lock->lock) \
		: "memory");
}


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
	return atval->value;
}

extern inline void att_inc (atomic_t *atval)
{
	unsigned int temp;

	__asm__ __volatile__("		\n" \
		" .set mips3	\n" \
		"1:	ll %0, %1	\n" \
		"	addu %0, %2	\n" \
		"	sc %0, %1	\n" \
		"	beqz %0, 1b	\n" \
		" .set mips0	\n" \
		: "=&r" (temp), "=m" (atval->value) \
		: "Ir" (1), "m" (atval->value) \
		);
}

extern inline void att_dec (atomic_t *atval)
{
	unsigned int temp;

	__asm__ __volatile__("		\n" \
		" .set mips3	\n" \
		"1:	ll %0, %1	\n" \
		"	subu %0, %2	\n" \
		"	sc %0, %1	\n" \
		"	beqz %0, 1b	\n" \
		" .set mips0	\n" \
		: "=&r" (temp), "=m" (atval->value) \
		: "Ir" (1), "m" (atval->value) \
		);
}

/*
 *	Other, generally useful, stuff
 */

#define barrier() __asm__ __volatile__ ("": : :"memory")

/* XCHG(var,ptr) swaps var and *ptr, 32-bit version.  from asm-mips64/system.h */

extern inline unsigned int xchg_u32_XCHG (volatile int *m, unsigned int val)
{
	unsigned int dummy;

	__asm__ __volatile__ ("		\n" \
		" .set mips3	\n" \
		"	.set noreorder	\n" \
		"	.set noat	\n" \
		"	ll %0, %3	\n" \
		"1:	move $1, %2	\n" \
		"	sc $1, %1	\n" \
		"	beqzl $1, 1b	\n" \
		"	ll %0, %3	\n" \
		"	.set at		\n" \
		"	.set reorder	\n" \
		" .set mips0	\n" \
		: "=r" (val), "=o" (*m), "=r" (dummy) \
		: "o" (*m), "2" (val) \
		: "memory");
	return val;
}
#define XCHG(var,ptr) \
	{ var = xchg_u32_XCHG(ptr,var); }

#endif	/* !__ATOMICS_H */

