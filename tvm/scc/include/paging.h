/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#ifndef _PAGING_H_
#define _PAGING_H_

#define PTBASE 0x100000
#define PAGEDIRLOC 0x0
#define PAGESIZE 0x1000

#ifndef __ASSEMBLER__
#include <stddef.h>

struct pdentry {
	uint p : 1;   // Present
	uint rw : 1;  // Read/Write
	uint us : 1;  // User/Supervisor
	uint wt : 1;  // Write-through
	uint cd : 1;  // Cache disabled
	uint a : 1;   // Accessed
	uint res : 1; // Reserved (set to 0)
	uint ps : 1; // Page size (0 indicates 4KB)
	uint g : 1;   // Global page (ignored)
	uint avail : 3; // Available for system programmer use
	uint addr : 20; // Page frame address
};

struct ptentry {
	uint p : 1;   // Present
	uint rw : 1;  // Read/Write
	uint us : 1;  // User/Supervisor
	uint wt : 1;  // Write-through
	uint cd : 1;  // Cache disabled
	uint a : 1;   // Accessed
	uint d : 1;   // Dirty
	uint pmb : 1; // MPBT flag
	uint g : 1;   // Global page (reserved on P54C - set to 0)
	uint avail : 3; // Available for system programmer use
	uint addr : 20; // Page frame address
};

#define PTE_P   (1 << 0)
#define PTE_RW  (1 << 1)
#define PTE_US  (1 << 2)
#define PTE_WT  (1 << 3)
#define PTE_CD  (1 << 4)
#define PTE_A   (1 << 5)
#define PTE_D   (1 << 6)
#define PTE_PMB (1 << 7)
#define PTE_G   (1 << 8)

#endif /* __ASSEMBLER__ */

#endif /* _PAGING_H_ */
