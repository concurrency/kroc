/** \file bfin-registers.h
 * Blackfin register definitions
 *
 * (c) 11/2004 Martin Strubel <hackfin\@section5.ch>
 *
 * Contains the GDB internal register map used to communicate registers
 * between the GDB (client) and gdbproxy (debugging server).
 *
 * The register token numbers are directly encoded by register number and
 * group code so that the appropriate opcodes can be built easily.
 *
 * UGLY: There are gaps in the enumeration, which doesn't make the
 * registermap look contiguous when using 'info register' in GDB...
 *
 * Important: Make sure GDB includes the same file. Make also sure, that
 * REGISTER_NAMES (tm-bfin.h) match the bfin_registers{ } enumeration.
 *
 * Note that there are some non core registers that are not documented or
 * listed in the BF533 manuals. They were taken over from a very old
 * version of the GNU assembler. Probably they can be removed in future.
 *
 * TODO: Move this file to the GDB bfin distribution or use a translation
 * table on the gdbproxy side.
 *
 * IF YOU CHANGE THE REGISTER MAP, INCREASE THE VERSION NUMBER IN THE
 * FOLLOWING DEFINE !!!
 * 
 *     BFIN_REGISTER_MAP_VERSION
 *
 * You can only change register mappings above T_NOGROUP.
 *
 * $Id: bfin-registers.h 177 2006-03-21 11:55:34Z strubi $
 *
 */

#define BFIN_REGISTER_MAP_VERSION 0


#ifndef BFIN_REGISTER_DEFINITIONS_HEADER
#define BFIN_REGISTER_DEFINITIONS_HEADER

/* Stolen from binutils, since we don't want to include all the binutils
 * blurb..
 * Slightly modified, did some more compact encoding and added 'virtual'
 * registers. These are non-core registers which we would still like to see
 * via GDB..
 */

#define GROUP(x)    ((x >> 3) & 7)

#define T_REG_R       0x00
#define T_REG_P       (1 << 3)
#define T_REG_I       (2 << 3)
#define T_REG_M      ((2 << 3) + 4)
#define T_REG_B       (3 << 3)
#define T_REG_L      ((3 << 3) + 4)
#define T_REG_A       (4 << 3)
#define T_REG_LP      (6 << 3)
#define T_REG_X       (7 << 3)
#define T_NOGROUP     (8 << 3)   // all registers above this value don't
                                 // belong to a core register group


enum bfin_registers {
	REG_R0    = T_REG_R, REG_R1, REG_R2, REG_R3, REG_R4, REG_R5, REG_R6, REG_R7, 
	REG_P0    = T_REG_P, REG_P1, REG_P2, REG_P3, REG_P4, REG_P5, REG_SP, REG_FP,
	REG_I0    = T_REG_I, REG_I1, REG_I2, REG_I3,
	REG_M0    = T_REG_M, REG_M1, REG_M2, REG_M3, 
	REG_B0    = T_REG_B, REG_B1, REG_B2, REG_B3,
	REG_L0    = T_REG_L, REG_L1, REG_L2, REG_L3, 
	REG_A0x   = T_REG_A, REG_A0w, REG_A1x, REG_A1w,
	REG_ASTAT = T_REG_A + 6, REG_RETS,

	REG_LC0   = T_REG_LP, REG_LT0, REG_LB0,  REG_LC1, REG_LT1, REG_LB1,
	                  REG_CYCLES, REG_CYCLES2,

	REG_USP   = T_REG_X, REG_SEQSTAT, REG_SYSCFG,
	                  REG_RETI, REG_RETX, REG_RETN, REG_RETE, REG_EMUDAT,
  

// These don't belong to a core group
	REG_A0 = T_NOGROUP, REG_A1, 
	REG_CC,
	// Please do not insert any CORE MMR registers in here.
	// They can be accessed using GDB scripts
	REG_NUMBER // terminator
};

// Watchpoint unit
//
// #ifndef WPIACTL
// #define WPIACTL          0xffe07000
// #endif
#	define  WPAND        0x02000000
#	define  EMUSW5       0x01000000
#	define  EMUSW4       0x00800000
#	define  WPICNTEN5    0x00400000
#	define  WPICNTEN4    0x00200000
#	define  WPIAEN5      0x00100000
#	define  WPIAEN4      0x00080000
#	define  WPIRINV45    0x00040000
#	define  WPIREN45     0x00020000
#	define  EMUSW3       0x00010000
#	define  EMUSW2       0x00008000
#	define  WPICNTEN3    0x00004000
#	define  WPICNTEN2    0x00002000
#	define  WPIAEN3      0x00001000
#	define  WPIAEN2      0x00000800
#	define  WPIRINV23    0x00000400
#	define  WPIREN23     0x00000200
#	define  EMUSW1       0x00000100
#	define  EMUSW0       0x00000080
#	define  WPICNTEN1    0x00000040
#	define  WPICNTEN0    0x00000020
#	define  WPIAEN1      0x00000010
#	define  WPIAEN0      0x00000008
#	define  WPIRINV01    0x00000004
#	define  WPIREN01     0x00000002
#	define  WPPWR        0x00000001

// #ifndef WPDACTL
// #define WPDACTL          0xffe07100
// #endif
#	define WPDACC1(x)   (((x) << 12) & 0x00003000 )
#	define WPDSRC1(x)   (((x) << 10) & 0x00000c00 )
#	define WPDACC0(x)   (((x) << 8) & 0x00000300 )
#	define WPDSRC0(x)   (((x) << 6) & 0x000000c0 )
#		define WPDACC_W         0x1
#		define WPDACC_R         0x2
#	define WPDCNTEN1     0x00000020
#	define WPDCNTEN0     0x00000010
#	define WPDAEN1       0x00000008
#	define WPDAEN0       0x00000004
#	define WPDRINV01     0x00000002
#	define WPDREN01      0x00000001


// #ifndef WPSTAT
// #define WPSTAT           0xffe07200
// #endif
#	define STATDA1       0x00000080
#	define STATDA0       0x00000040
#	define STATIA5       0x00000020
#	define STATIA4       0x00000010
#	define STATIA3       0x00000008
#	define STATIA2       0x00000004
#	define STATIA1       0x00000002
#	define STATIA0       0x00000001


// These are defined in def_LPBlackfin.h
// #ifndef WPIA0
// #define WPIA0            0xffe07040
// #define WPIA1            0xffe07044
// #define WPIA2            0xffe07048
// #define WPIA3            0xffe0704c
// #define WPIA4            0xffe07050
// #define WPIA5            0xffe07054
// #endif

// #ifndef WPIACNT0
// #define WPIACNT0         0xffe07080
// #define WPIACNT1         0xffe07084
// #define WPIACNT2         0xffe07088
// #define WPIACNT3         0xffe0708c
// #define WPIACNT4         0xffe07090
// #define WPIACNT5         0xffe07094
// #endif

// #ifndef WPDA0
// #define WPDA0            0xffe07140
// #define WPDA1            0xffe07144
// #endif

// #ifndef WPDACNT0
// #define WPDACNT0         0xffe07180
// #define WPDACNT1         0xffe07184
// #endif



#endif

