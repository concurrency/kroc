/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#include <stddef.h>
#include <interrupt.h>
#include <scc.h>
#include <apic.h>
#include <clock.h>

tileid_typ _my_tileid;
int _my_coreid;

void platforminit(void);
static void initPrintBuf(void);
void enable_caching(void);

void platforminit()
{
	// Set global constants _my_tileid and _my_coreid
	ulong mytid_reg = *((ulong *)(CRB_OWN + MYTILEID));

	_my_tileid = (tileid_typ)(mytid_reg >> 3); 

	int x, y, core;
	core = mytid_reg & 0x7;
	x = (mytid_reg >> 3) & 0xF;
	y = (mytid_reg >> 7) & 0xF;

	_my_coreid = 12 * y + 2 * x + core;

	// Ensure my lock is released
	release_lock(get_my_coreid());

	initPrintBuf();
	init_idt(); // sets the gate descriptors in the IDT
	init_APIC(); // set up local APIC
#if RTCLOCK
	init_clock(); // set up and start the real-time clock
#endif /* RTCLOCK */

#ifdef RCCE_SUPPORT
	void init_heap(void);
	init_heap(); // set up a heap because RCCE needs malloc/free

	/* zero out the MPB to prepare for RCCE programs */
	volatile unsigned long *my_mpb;
	int i;
	my_mpb = (volatile unsigned long *)(MPB_OWN + (core ? MPB_SIZE : 0));
	for (i = 0; i < MPB_SIZE/sizeof(long); i++)
	{
		*(my_mpb + i) = 0;
	}
#endif

	enable_caching();
}


#define PRINTBUFSIZE 65536

static ulong *_head;
static ulong *_tail;
static char *_printbuf;

static void initPrintBuf() {
	// set print buffer head and tail to zero
	int coreid = get_my_coreid();
//  _head = (ulong *)(0x80000000 + coreid*16);
//  TODO: delete next line and uncomment prev line when 128-bit write 
//  restriction bug of sccKit 1.4.1 is fixed.
	_head = (ulong *)(0x80000000 + coreid*32);
//  _tail = _head + 2;
//  TODO: delete next line and uncomment prev line when 128-bit write 
//  restriction bug of sccKit 1.4.1 is fixed.
	_tail = _head + 4;
	_printbuf = (char *)(0x80001000 + coreid*PRINTBUFSIZE);

	*_head = 0;
	*_tail = 0;
}

int putc(int dummy, int ch) 
{
    *(_printbuf + *_head) = (char)ch;
    (*_head)++;
    if (*_head >= PRINTBUFSIZE) *_head = 0;
}
