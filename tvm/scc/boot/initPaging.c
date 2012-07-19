/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#include <paging.h>
#include <scc.h>

void initPaging(void);
void enable_paging(unsigned int);

/* initPaging: initialize system page table.  Everything is mapped flat, 
 * and private memory is cached. */
void initPaging()
{
	unsigned long addrMap, mloc;

	// Zero out the page tables
	void _asm_bzero(unsigned long base, unsigned long count);
	_asm_bzero(PTBASE, 1024*1024*4);

	// Fill in PD entries
	addrMap = PTBASE;
	for (mloc = 0; mloc < 1023*4; mloc+=4)
	{
		*(unsigned long *)mloc = addrMap | PTE_P | PTE_RW;
		addrMap += 0x1000;
	}

	// Last PD entry points to the PD itself.
	*(unsigned long *)mloc = PAGEDIRLOC | PTE_P | PTE_RW;

#define MAP(virt, phys, vlimit, flags) \
	addrMap = (phys); \
	for (mloc = (virt)/0x400+PTBASE; mloc < (vlimit)/0x400+PTBASE; mloc+=4) \
	{ \
		*(unsigned long *)mloc = addrMap | (flags); \
		addrMap += 0x1000; \
	}

	// Fill in PT entries
	// Private mem (0x0 -> same; < 0x29000000)
	MAP(0, 0, 0x29000000, PTE_P | PTE_RW);
	// SHM (0x80000000 -> same; < 0x84000000)
	MAP(0x80000000, 0x80000000, 0x84000000, PTE_P | PTE_RW | PTE_CD);
	// MPB (0xc0000000 -> same; < 0xd9000000)
	MAP(0xc0000000, 0xc0000000, 0xd9000000, PTE_P | PTE_RW | PTE_PMB);
	// CRB, FPGA, MCPC TCP/IP IF, and VRC (0xe0000000 -> same; < 0xfc000000)
	MAP(0xe0000000, 0xe0000000, 0xfc000000, PTE_P | PTE_RW | PTE_CD);
	// APIC and upper private mem (0xfee00000 -> same; < 0xffc00000)
	MAP(0xfee00000, 0xfee00000, 0xffc00000, PTE_P | PTE_RW | PTE_CD);

	enable_paging(PAGEDIRLOC);
}
