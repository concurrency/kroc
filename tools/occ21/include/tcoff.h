/* Copyright 1994 INMOS Limited */

/*
 *	tcoff definitions
 *	Copyright (C) 1993-1995 Inmos Limited
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

/*  $Id: tcoff.h,v 1.1 1996/04/15 10:52:21 djb1 Exp $ */

/*
 * Ade 5/4/93  : H series uses same attribute defines as the T series
 * Ade 8/10/93 : New ATRRIB_WORD_64 and H_RESERVED_SET
 * CON 13/10/93: New values for T9000 gamma
 * Ade 24/6/94 : added CVS
 * Ade 6/9/04  : copied from component tcofflib to tcof2lib
 * Ade 15/11/94 : removed timer and debug attributes for T450
 * Ade  9/2/95 : added T9000_GAMMAE
 */

/*{{{  basic TCOFF tag values */
#define LINKABLE_TAG          1
#define START_MODULE_TAG      2
#define END_MODULE_TAG        3
#define SET_LOAD_POINT_TAG    4
#define ADJUST_POINT_TAG      5
#define LOAD_TEXT_TAG         6
#define LOAD_PREFIX_TAG       7
#define LOAD_EXPR_TAG         8
#define LOAD_ZEROS_TAG        9
#define ALIGN_TAG             10
#define SECTION_TAG           11
#define DEFINE_MAIN_TAG       12
#define LOCAL_SYMBOLS_TAG     13
#define DEFINE_LABEL_TAG      14
#define DEFINE_SYMBOL_TAG     15
#define KILL_ID_TAG           16
#define BYTE_PATCH_TAG        17
#define REP_START_TAG         18
#define REP_END_TAG           19
#define COMMENT_TAG           20
#define MESSAGE_TAG           21
#define LIB_INDEX_START_TAG   22
#define LIB_INDEX_END_TAG     23
#define INDEX_ENTRY_TAG       24
#define WORD_PATCH_TAG        25
#define DESCRIPTOR_TAG        26
#define VERSION_TAG           27
#define LINKED_UNIT_TAG       28
/* #define SPECIFY_ORIGIN_TAG    29  disused now */
#define SYMBOL_TAG            30
#define SPECIFIC_SYMBOL_TAG   31
#define LAST_TCOFF_TAG        31
/*}}}  */

/*{{{  encoding numbers */
#define PFX_1_NUMBER          251
#define PFX_2_NUMBER          252
#define PFX_4_NUMBER          253
#define PFX_8_NUMBER          254
#define SIGN_INDICATOR        255
/*}}}  */

/*{{{  expressions */
#define CO_VALUE_TAG          1
#define LP_VALUE_TAG          2
#define SV_VALUE_TAG          3
#define SS_VALUE_TAG          4
#define WL_VALUE_TAG          5
#define PLUS_OP               6
#define MINUS_OP              7
#define TIMES_OP              8
#define DIVIDE_OP             9
#define REM_OP                10
#define MAX_OP                11
#define MIN_OP                12
#define AP_VALUE_TAG          13
#define LAST_TCOFF_OP         13
/*}}}  */

/*{{{  messages */
#define NORMAL_MSG            1
#define WARNING_MSG           2
#define ERROR_MSG             3
/*}}}  */

/*{{{  BOOL */
#define BOOL_TRUE             1
#define BOOL_FALSE            0
/*}}}  */

/*{{{  sections */
#define WRITE_SECTION         0x1
#define READ_SECTION          0x2
#define EXECUTE_SECTION       0x4
#define DEBUG_SECTION         0x8
#define VIRTUAL_SECTION       0x10
/*}}}  */

/*{{{  usage */
#define LOCAL_USAGE           0x1
#define EXPORT_USAGE          0x2
#define IMPORT_USAGE          0x4
#define WEAK_USAGE            0x8
#define CONDITIONAL_USAGE     0x10
#define UNINDEXED_USAGE       0x20
#define PROVISIONAL_USAGE     0x40
#define ORIGIN_USAGE          0x80
#define ROUTINE_USAGE         0x100
#define DATA_USAGE            0x200
#define NOTYPE_USAGE          0x400
/*}}}  */

/*{{{  languages */
#define LANG_NOT_KNOWN        1
#define LANG_LINKED           2
#define LANG_OCCAM            3
#define LANG_ANSI_C           4
#define LANG_FORTRAN_77       5
#define LANG_ISO_PASCAL       6
#define LANG_MODULA_2         7
#define LANG_ADA              8
#define LANG_ASSEMBLER        9
#define LANG_OCCAM_HARNESS    10
/*}}}  */

/*{{{  symbol table tags */
#define SYMTAB_HEADER_TAG          1
#define SYMTAB_SYMBOL_TAG          2
#define SYMTAB_SPECIFIC_SYMBOL_TAG 3

#define SYMBOL_TABLE_MAGIC_TEXT "LINKER SYMBOL TABLE"
#define SYMBOL_TABLE_MAGIC_TEXT_LENGTH 19
/*}}}  */

/*{{{  processor type and attribute words */
/*{{{  architecture bits */
/* Architecture bits */

#define ARCH_MASK               0x01F00000 /* not part of TCOFF but useful */
#define ARCH_T                  0x00100000
#define ARCH_H                  0x00200000
#define ARCH_RESERVED_1         0x00400000 /* These bits should not be ...     */
#define ARCH_RESERVED_2         0x00800000 /* ... used before redefinition ... */
#define ARCH_RESERVED_3         0x01000000 /* ... as new architectures.        */  
/*}}}  */

/*{{{  T architecture bit definitions */
/* T architecture bit definitions */

#define ATTRIB_WORD_MASK        0x00007 /* not part of TCOFF but useful */
#define ATTRIB_WORD_16          0x00001
#define ATTRIB_WORD_32          0x00002
#define ATTRIB_WORD_64          0x00004
#define ATTRIB_MEMSTART_MASK    0x00038 /* not part of TCOFF but useful */
#define ATTRIB_MEMSTART18       0x00008
#define ATTRIB_MEMSTART28       0x00010
#define ATTRIB_MEMSTARTLEQ28    0x00000
#define ATTRIB_ERROR_MASK       0x00780 /* not part of TCOFF but useful */
#define ATTRIB_UNIVERSAL        0x00000
#define ATTRIB_HALT             0x00080
#define ATTRIB_STOP             0x00100
#define ATTRIB_IO_MASK          0x01800 /* not part of TCOFF but useful */
#define ATTRIB_INSTR_IO         0x00800
#define ATTRIB_CALL_IO          0x00000
#define ATTRIB_FPU_CALLING_MASK 0xC0000
#define ATTRIB_NON_FPU_CALLING  0x40000
#define ATTRIB_FPU_CALLING      0x80000
#define ATTRIB_RESERVED_SET     0x3E040

#define INSTR_CORE            0x0000001
#define INSTR_FMUL            0x0000002
#define INSTR_FP_SUPPORT      0x0000004
#define INSTR_DUP             0x0000008
#define INSTR_WSUBDB          0x0000010
#define INSTR_MOVE2D          0x0000020
#define INSTR_CRC             0x0000040
#define INSTR_BITOPS          0x0000080
#define INSTR_FPU_CORE        0x0000100
#define INSTR_FPTESTERR       0x0000200
#define INSTR_LDDEVID         0x0000400
#define INSTR_DEBUG_SUPPORT   0x0000800
#define INSTR_TIMER_DISABLE   0x0001000
#define INSTR_LDMEMSTARTVAL   0x0002000
#define INSTR_POP             0x0004000
#define INSTR_NO_SEMAPHORE    0x0008000
#define INSTR_NO_DEVICE       0x0010000
#define INSTR_RMC_CORE1       0x2000000
#define INSTR_RMC_CORE2       0x4000000
#define INSTR_RMC_CORE3       0x8000000
#define INSTR_RESERVED_SET    0x00E0000
/*}}}  */

/*{{{  T architecture processor definitions */
/* T architecture processor definitions */

/*{{{  T2 etc */
#define T212_ATTRIB           ( ATTRIB_WORD_16 | ATTRIB_MEMSTART18 \
			      | ATTRIB_NON_FPU_CALLING | ATTRIB_RESERVED_SET )

#define T212_INSTR            ( ARCH_T \
			      | INSTR_CORE | INSTR_NO_SEMAPHORE \
			      | INSTR_NO_DEVICE | INSTR_RESERVED_SET )

#define M212_ATTRIB           T212_ATTRIB

#define M212_INSTR            T212_INSTR

#define T222_ATTRIB           T212_ATTRIB

#define T222_INSTR            T212_INSTR

#define T225_ATTRIB           ( ATTRIB_WORD_16 | ATTRIB_MEMSTART18 \
			      | ATTRIB_NON_FPU_CALLING | ATTRIB_RESERVED_SET )

#define T225_INSTR            ( ARCH_T \
			      | INSTR_CORE | INSTR_DUP | INSTR_CRC \
			      | INSTR_BITOPS | INSTR_LDDEVID | INSTR_WSUBDB \
			      | INSTR_DEBUG_SUPPORT | INSTR_TIMER_DISABLE \
			      | INSTR_LDMEMSTARTVAL | INSTR_POP \
			      | INSTR_NO_SEMAPHORE | INSTR_NO_DEVICE \
			      | INSTR_RESERVED_SET )
/*}}}  */

/*{{{  T4 etc */
#define T400_ATTRIB           T425_ATTRIB

#define T400_INSTR            T425_INSTR

#define T414_ATTRIB           ( ATTRIB_WORD_32 | ATTRIB_MEMSTART18 \
			      | ATTRIB_NON_FPU_CALLING | ATTRIB_RESERVED_SET )

#define T414_INSTR            ( ARCH_T \
			      | INSTR_CORE | INSTR_FMUL | INSTR_FP_SUPPORT \
			      | INSTR_NO_SEMAPHORE | INSTR_NO_DEVICE \
			      | INSTR_RESERVED_SET )

#define T425_ATTRIB           ( ATTRIB_WORD_32 | ATTRIB_MEMSTART28 \
			      | ATTRIB_NON_FPU_CALLING | ATTRIB_RESERVED_SET )

#define T425_INSTR            ( ARCH_T \
			      | INSTR_CORE | INSTR_FMUL | INSTR_FP_SUPPORT \
			      | INSTR_DUP | INSTR_WSUBDB | INSTR_MOVE2D \
			      | INSTR_CRC | INSTR_BITOPS | INSTR_FPTESTERR \
			      | INSTR_LDDEVID | INSTR_DEBUG_SUPPORT \
			      | INSTR_TIMER_DISABLE | INSTR_LDMEMSTARTVAL \
			      | INSTR_NO_SEMAPHORE | INSTR_NO_DEVICE \
			      | INSTR_POP | INSTR_RESERVED_SET )

#define T450_ATTRIB           ( ATTRIB_WORD_32 \
			      | ATTRIB_NON_FPU_CALLING | ATTRIB_RESERVED_SET )

#define T450_INSTR            ( ARCH_T \
			      | INSTR_CORE | INSTR_FMUL | INSTR_FP_SUPPORT \
			      | INSTR_DUP | INSTR_WSUBDB | INSTR_MOVE2D \
			      | INSTR_CRC | INSTR_BITOPS | INSTR_FPTESTERR \
			      | INSTR_LDDEVID \
			      | INSTR_LDMEMSTARTVAL \
			      | INSTR_POP | INSTR_RMC_CORE1 \
			      | INSTR_RMC_CORE2 | INSTR_RMC_CORE3 \
			      | INSTR_RESERVED_SET )

/*}}}  */


/*{{{  T8 etc */
#define T800_ATTRIB           ( ATTRIB_WORD_32 | ATTRIB_MEMSTART28 \
			      | ATTRIB_FPU_CALLING | ATTRIB_RESERVED_SET )

#define T800_INSTR            ( ARCH_T \
			      | INSTR_CORE | INSTR_FMUL | INSTR_DUP \
			      | INSTR_WSUBDB | INSTR_MOVE2D | INSTR_CRC \
			      | INSTR_BITOPS | INSTR_FPU_CORE \
			      | INSTR_NO_SEMAPHORE | INSTR_NO_DEVICE \
			      | INSTR_FPTESTERR | INSTR_RESERVED_SET )

#define T801_ATTRIB           T805_ATTRIB

#define T801_INSTR            T805_INSTR

#define T805_ATTRIB           ( ATTRIB_WORD_32 | ATTRIB_MEMSTART28 \
			      | ATTRIB_FPU_CALLING | ATTRIB_RESERVED_SET )

#define T805_INSTR            ( ARCH_T \
			      | INSTR_CORE | INSTR_FMUL | INSTR_DUP \
			      | INSTR_WSUBDB | INSTR_MOVE2D | INSTR_CRC \
			      | INSTR_BITOPS | INSTR_FPU_CORE \
			      | INSTR_FPTESTERR | INSTR_LDDEVID \
			      | INSTR_DEBUG_SUPPORT | INSTR_TIMER_DISABLE \
			      | INSTR_LDMEMSTARTVAL | INSTR_POP \
			      | INSTR_NO_SEMAPHORE | INSTR_NO_DEVICE \
			      | INSTR_RESERVED_SET )
/*}}}  */

/*{{{  classes */
#define TA_ATTRIB             ( ATTRIB_WORD_32 | ATTRIB_MEMSTARTLEQ28 \
			      | ATTRIB_NON_FPU_CALLING | ATTRIB_RESERVED_SET )

#define TA_INSTR              ( ARCH_T \
			      | INSTR_CORE | INSTR_FMUL \
			      | INSTR_NO_SEMAPHORE | INSTR_NO_DEVICE \
			      | INSTR_RESERVED_SET )

#define TB_ATTRIB             ( ATTRIB_WORD_32 | ATTRIB_MEMSTARTLEQ28 \
			      | ATTRIB_NON_FPU_CALLING | ATTRIB_RESERVED_SET)

#define TB_INSTR              ( ARCH_T \
			      | INSTR_CORE | INSTR_FMUL | INSTR_FP_SUPPORT \
			      | INSTR_NO_SEMAPHORE | INSTR_NO_DEVICE \
			      | INSTR_RESERVED_SET )
/*}}}  */

/*{{{  T426 and T806 */
#define T426_ATTRIB           T425_ATTRIB
#define T426_INSTR            T425_INSTR

#define T806_ATTRIB           T805_ATTRIB
#define T806_INSTR            T805_INSTR
/*}}}  */
/*}}}  */

/*{{{  386 series definitions*/
#define INSTR_387             0x06000000
#define INSTR_386_32          0x02000000
#define INSTR_386_16          0x01000000
#define INSTR_088_16          0x00800000
#define I386_32_INSTR         ( INSTR_CORE | INSTR_RESERVED_SET | INSTR_DUP | INSTR_WSUBDB\
			      | INSTR_386_32)

#define I387_32_INSTR         ( I386_32_INSTR | INSTR_FPU_CORE | INSTR_FPTESTERR | INSTR_387)

#define I386_32_ATTRIB        ( ATTRIB_WORD_32 | ATTRIB_NON_FPU_CALLING \
			      |  ATTRIB_RESERVED_SET)

#define I386_16_INSTR         ( INSTR_CORE | INSTR_RESERVED_SET \
			      | INSTR_386_16)

#define I386_16_ATTRIB        ( ATTRIB_WORD_16 | ATTRIB_NON_FPU_CALLING \
			      |  ATTRIB_RESERVED_SET)

#define I088_16_INSTR         ( INSTR_CORE | INSTR_RESERVED_SET | INSTR_088_16 )

#define I088_16_ATTRIB        I386_16_ATTRIB

/*}}}  */

/*{{{  AXP definitions*/
/* Just enough to get started! we realy need to define a new architecture bit
   etc, but that needs more generalised design thought */
#define AXP_INSTR         ( INSTR_CORE | INSTR_RESERVED_SET | INSTR_DUP | INSTR_WSUBDB \
			    | INSTR_FPU_CORE | INSTR_FPTESTERR | INSTR_FMUL | ARCH_T)
/*}}}  */

/*{{{  H architecture bit definitions */
/* H architecture processor definitions */
/* The attribute word for H and T series use the same defines */

#define H_INSTR_RESERVED_SET  0x1FFF

#define H_INSTR_T9000_FULL    0x2000 /* has T9000 full spec instruction set */
#define H_INSTR_T9000_GAMMA   0x4000 /* has T9000 gamma instruction set/bugs */
#define H_INSTR_T9000_GAMMAE  0x8000 /* has T9000 gamma revision E instruction set/bugs */

#define H_ATTRIB_RESERVED_SET   ATTRIB_RESERVED_SET
/*}}}  */

/*{{{  H architecture processor definitions */
/*{{{  T9000 */
#define T9000_INSTR           ( ARCH_H | H_INSTR_RESERVED_SET \
				       | H_INSTR_T9000_FULL )

#define T9000_ATTRIB          ( ATTRIB_WORD_32 | ATTRIB_FPU_CALLING \
					| H_ATTRIB_RESERVED_SET )

#define H1_ATTRIB       T9000_ATTRIB    /* Define should be deprecated */
#define H1_INSTR        T9000_INSTR     /* Define should be deprecated */
/*}}}  */

/*{{{  T9000 Gamma */
#define T9000GAMMA_INSTR      ( ARCH_H | H_INSTR_RESERVED_SET \
				       | H_INSTR_T9000_GAMMA )

#define T9000GAMMA_ATTRIB     ( ATTRIB_WORD_32 | ATTRIB_FPU_CALLING \
					| H_ATTRIB_RESERVED_SET )

/*}}}  */

/*{{{  T9000 Gamma E */
#define T9000GAMMAE_INSTR      ( ARCH_H | H_INSTR_RESERVED_SET \
				       | H_INSTR_T9000_GAMMAE )

#define T9000GAMMAE_ATTRIB     ( ATTRIB_WORD_32 | ATTRIB_FPU_CALLING \
					| H_ATTRIB_RESERVED_SET )

/*}}}  */

/*}}}  */
/*}}}  */
