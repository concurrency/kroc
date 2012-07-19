/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#include <stddef.h>
#include <scc.h>
#include <interrupt.h>

long get_tile_freq(tileid_typ tid)
{
	ulong gcbcfg;
	void *crb_base;

	crb_base = (void *)CRB_ADDR(X_TID(tid), Y_TID(tid));

	gcbcfg = *((ulong *)((ulong)crb_base + GCBCFG));
	switch (gcbcfg & TFS_TILE_FREQ_MASK) {
		case TFS_TILE_FREQ_800: return 800000000;
		case TFS_TILE_FREQ_533: return 533000000;
		case TFS_TILE_FREQ_400: return 400000000;
		case TFS_TILE_FREQ_320: return 320000000;
		case TFS_TILE_FREQ_266: return 266000000;
		case TFS_TILE_FREQ_228: return 228000000;
		case TFS_TILE_FREQ_200: return 200000000;
		case TFS_TILE_FREQ_178: return 178000000;
		case TFS_TILE_FREQ_160: return 160000000;
		case TFS_TILE_FREQ_145: return 145000000;
		case TFS_TILE_FREQ_133: return 133000000;
		case TFS_TILE_FREQ_123: return 123000000;
		case TFS_TILE_FREQ_114: return 114000000;
		case TFS_TILE_FREQ_106: return 106000000;
		case TFS_TILE_FREQ_100: return 100000000;
		default: return SYSERR;
	}
}

long get_mesh_freq(tileid_typ tid)
{
	ulong gcbcfg;
	void *crb_base;

	crb_base = (void *)CRB_ADDR(X_TID(tid), Y_TID(tid));

	gcbcfg = *((ulong *)((ulong)crb_base + GCBCFG));
	switch (gcbcfg & TFS_MESH_FREQ_MASK) {
		case TFS_MESH_FREQ_800: return 800000000;
		case TFS_MESH_FREQ_1600: return 1600000000;
		default: return SYSERR;
	}
}

/**
 * Read x86 time stamp counter. For timing stuff.
 * Typical usage:
 *     unsigned long long start, end;
 *     start = getticks();
 *     ... [code to time] ...
 *     end = getticks();
 *     printf("Execution time: %u cycles\n", end - start);
 * Note, libxc does not support printing an unsigned long long (%llu), so a 
 * better usage will check whether the upper half of the result of
 * (end - start) is 0.
 */ 
unsigned long long getticks()
{
    __asm__("pushl   %%ebx;":);
    __asm__("cpuid;":);
    __asm__("rdtsc;":);
    __asm__("movl    %%eax,-8(%%ebp);":);
    __asm__("movl    %%edx,-4(%%ebp);":);
    __asm__("popl    %%ebx;":);
}

void acquire_lock(int coreid)
{
	int tile, core;
	void *crb_base;
	volatile unsigned char *lock;

	tile = coreid / 2;
	core = coreid % 2;

	crb_base = (void *)CRB_ADDR(tile % 6, tile / 6); 
	lock = (char *)((ulong)crb_base + (core ? LOCK1 : LOCK0));

	/* The LOCK bit is clear-on-read i.e. we have the lock when reading a '1' 
	*/
	while (!(*lock & 0x1)) ; // might want to sleep or something? 
}

void release_lock(int coreid)
{
	int tile, core;
	void *crb_base;
	volatile unsigned char *lock;

	tile = coreid / 2;
	core = coreid % 2;

	crb_base = (void *)CRB_ADDR(tile % 6, tile / 6); 
	lock = (char *)((ulong)crb_base + (core ? LOCK1 : LOCK0));

	/* The LOCK bit is set by writing to the register */
	*lock = 0;
}

/* interrupt_core(int coreid, int lint):
 * Pulse the 'lint' pin (0 for LINT0 or 1 for LINT1) for the LAPIC 
 * connected to core 'coreid'.
 */
int interrupt_core(int coreid, int lint)
{
	if (!is_valid_coreid(coreid))
		return SYSERR;
	if (!(lint == 0 || lint == 1)) 
		return SYSERR;

	int tile, core;
	ulong mask;
	void *crb_base;
	ulong *glcfg_ptr;

	tile = coreid / 2;
	core = coreid % 2;
	mask = (lint ? 1 : 2); 

	crb_base = (void *)CRB_ADDR(tile % 6, tile / 6); 
	glcfg_ptr = (ulong *)((ulong)crb_base + (core ? GLCFG1 : GLCFG0));

	*glcfg_ptr |= mask;
	*glcfg_ptr &= ~mask;
	return OK; 
}
