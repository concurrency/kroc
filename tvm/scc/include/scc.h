/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

/* Most of these definitions are taken from rckos/rckApps/config.h from 
 * Intel's bareMetal BIOS, a part of SCC Linux.  bareMetal BIOS is 
 * Copyright (C) 2006, 2007 by Intel Corporation. */

#ifndef _SCC_H_
#define _SCC_H_

#define X_TID(tid) ((tid)&0x0f)
#define Y_TID(tid) ((tid)>>4)

#define is_valid_tileid(tid)	(X_TID(tid) >= 0 && X_TID(tid) <=5 \
								 && Y_TID(tid) >= 0 && Y_TID(tid) <= 3)
#define is_valid_coreid(cid)	(cid >= 0 && cid <= 47)

// Symbols for MPB addresses
#define MPB_X0_Y0 0xc0000000
#define MPB_X1_Y0 0xc1000000
#define MPB_X2_Y0 0xc2000000
#define MPB_X3_Y0 0xc3000000
#define MPB_X4_Y0 0xc4000000
#define MPB_X5_Y0 0xc5000000
#define MPB_X0_Y1 0xc6000000
#define MPB_X1_Y1 0xc7000000
#define MPB_X2_Y1 0xc8000000
#define MPB_X3_Y1 0xc9000000
#define MPB_X4_Y1 0xca000000
#define MPB_X5_Y1 0xcb000000
#define MPB_X0_Y2 0xcc000000
#define MPB_X1_Y2 0xcd000000
#define MPB_X2_Y2 0xce000000
#define MPB_X3_Y2 0xcf000000
#define MPB_X4_Y2 0xd0000000
#define MPB_X5_Y2 0xd1000000
#define MPB_X0_Y3 0xd2000000
#define MPB_X1_Y3 0xd3000000
#define MPB_X2_Y3 0xd4000000
#define MPB_X3_Y3 0xd5000000
#define MPB_X4_Y3 0xd6000000
#define MPB_X5_Y3 0xd7000000
#define MPB_OWN   0xd8000000
#define MPB_SIZE  0x2000
#define MPB_ADDR(core) (MPB_X0_Y0+(core/2)*0x1000000+(core%2)*MPB_SIZE)

/* Symbols for CRB addresses */
#define CRB_X0_Y0 0xe0000000
#define CRB_X1_Y0 0xe1000000
#define CRB_X2_Y0 0xe2000000
#define CRB_X3_Y0 0xe3000000
#define CRB_X4_Y0 0xe4000000
#define CRB_X5_Y0 0xe5000000
#define CRB_X0_Y1 0xe6000000
#define CRB_X1_Y1 0xe7000000
#define CRB_X2_Y1 0xe8000000
#define CRB_X3_Y1 0xe9000000
#define CRB_X4_Y1 0xea000000
#define CRB_X5_Y1 0xeb000000
#define CRB_X0_Y2 0xec000000
#define CRB_X1_Y2 0xed000000
#define CRB_X2_Y2 0xee000000
#define CRB_X3_Y2 0xef000000
#define CRB_X4_Y2 0xf0000000
#define CRB_X5_Y2 0xf1000000
#define CRB_X0_Y3 0xf2000000
#define CRB_X1_Y3 0xf3000000
#define CRB_X2_Y3 0xf4000000
#define CRB_X3_Y3 0xf5000000
#define CRB_X4_Y3 0xf6000000
#define CRB_X5_Y3 0xf7000000
#define CRB_OWN   0xf8000000
#define CRB_ADDR(x,y) (CRB_X0_Y0+(0x01000000*(x))+(0x01000000*6*(y)))

// Symbols for CRB sub-addresses
#define GLCFG0   0x010
#define GLCFG1   0x018
#define L2CFG0   0x020
#define L2CFG1   0x028
#define SENSOR   0x040
#define GCBCFG   0x080
#define MYTILEID 0x100
#define LOCK0    0x200
#define LOCK1    0x400
#define LUT0     0x00800
#define LUT1     0x01000

/* GCBCFG (global clock unit config reg) symbols */
#define GCU_TFS_MASK	0x3FFFF00

/* Mesh frequency settings  */
#define TFS_MESH_FREQ_MASK	0x7F000
#define TFS_MESH_FREQ_800	0x0E000
#define	TFS_MESH_FREQ_1600	0x07000

/* Tile frequency settings */
#define TFS_TILE_FREQ_MASK	0x3F80F00
#define TFS_TILE_FREQ_800	0x0700100
#define TFS_TILE_FREQ_533	0x0A80200
#define TFS_TILE_FREQ_400	0x0E00300
#define TFS_TILE_FREQ_320	0x1180400
#define TFS_TILE_FREQ_266	0x1500500
#define TFS_TILE_FREQ_228	0x1880600
#define TFS_TILE_FREQ_200	0x1C00700
#define TFS_TILE_FREQ_178	0x1F80800
#define TFS_TILE_FREQ_160	0x2300900
#define TFS_TILE_FREQ_145	0x2680A00
#define TFS_TILE_FREQ_133	0x2A00B00
#define TFS_TILE_FREQ_123	0x2D80C00
#define TFS_TILE_FREQ_114	0x3100D00
#define TFS_TILE_FREQ_106	0x3480E00
#define TFS_TILE_FREQ_100	0x3800F00

/* FPGA registers (new for sccKit >= 1.4.0) */
#define FPGA_REGS           0xf9000000
#define GTSC_LO             0x8224
#define GTSC_HI             0x8228
#define ATOMIC_CNT_INC(x)   (((x) < 48 ? 0xE000 : 0xF000) + (((x) % 48) * 8))
#define ATOMIC_CNT_INIT(x)  (((x) < 48 ? 0xE000 : 0xF000) + (((x) % 48) * 8) + 4)
/* NOTE: inter-core interrupt definitions omitted */

/* Instruction to invalidate L1 cache lines of message buffer type */
#define CL1INVMB __asm__ volatile ( ".byte 0x0f; .byte 0x0a;\n" )

typedef char	tileid_typ;

extern tileid_typ _my_tileid;
extern int _my_coreid;
static __inline tileid_typ get_my_tileid(void) { return _my_tileid; }
static __inline int get_my_coreid(void) { return _my_coreid; }

/* In scc.c: */
long get_tile_freq(tileid_typ);
long get_mesh_freq(tileid_typ);
void acquire_lock(int);
void release_lock(int);
int interrupt_core(int, int);
unsigned long long getticks(void);

#endif /* _SCC_H_ */
