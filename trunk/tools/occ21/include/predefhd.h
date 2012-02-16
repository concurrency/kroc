/* $Id: predefhd.h,v 1.1 1996/04/15 10:52:18 djb1 Exp $ */

/*
 *	predefined routine numbers
 *	Copyright (C) 1987 Inmos Limited
 *	Modified by Fred Barnes
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

/*{{{  Predefined routines defns. */

/********************
*   The order of these matters because there are tables built up
*   in the same order
*********************/

#define PD_LONGADD                    0
#define PD_LONGSUM                    1
#define PD_LONGSUB                    2
#define PD_LONGDIFF                   3
#define PD_LONGPROD                   4
#define PD_LONGDIV                    5
#define PD_SHIFTRIGHT                 6
#define PD_SHIFTLEFT                  7
#define PD_NORMALISE                  8
#define PD_FRACMUL                    9
#define PD_ASHIFTRIGHT                10
#define PD_ASHIFTLEFT                 11
#define PD_ROTATERIGHT                12
#define PD_ROTATELEFT                 13
#define PD_CAUSEERROR                 14
#if 0
#define PD_KERNELRUN                  15
#define PD_LOADINPUTCHANNEL           16
#define PD_LOADOUTPUTCHANNEL          17
#define PD_LOADINPUTCHANNELVECTOR     18
#define PD_LOADOUTPUTCHANNELVECTOR    19
#define PD_LOADBYTEVECTOR             20
#endif
#define PD_UNPACKSN                   21
#define PD_ROUNDSN                    22
#define PD_DRAW2D                     23
#define PD_CLIP2D                     24
#define PD_MOVE2D                     25
#define PD_CRCWORD                    26
#define PD_CRCBYTE                    27
#define PD_BITCOUNT                   28
#define PD_BITREVWORD                 29
#define PD_BITREVNBITS                30
#define PD_ABS                        31
#define PD_ISNAN                      32
#define PD_NOTFINITE                  33
#define PD_ORDERED                    34
#define PD_MINUSX                     35
#define PD_MULBY2                     36
#define PD_DIVBY2                     37
#define PD_SQRT                       38
#define PD_FPINT                      39
#define PD_DABS                       40
#define PD_DISNAN                     41
#define PD_DNOTFINITE                 42
#define PD_DORDERED                   43
#define PD_DMINUSX                    44
#define PD_DMULBY2                    45
#define PD_DDIVBY2                    46
#define PD_DSQRT                      47
#define PD_DFPINT                     48
#define PD_SCALEB                     49
#define PD_DSCALEB                    50
#define PD_COPYSIGN                   51
#define PD_DCOPYSIGN                  52
#define PD_NEXTAFTER                  53
#define PD_DNEXTAFTER                 54
#define PD_LOGB                       55
#define PD_DLOGB                      56
#define PD_FLOATINGUNPACK             57
#define PD_DFLOATINGUNPACK            58
#define PD_ARGUMENTREDUCE             59
#define PD_DARGUMENTREDUCE            60
#define PD_REAL32OP                   61
#define PD_REAL64OP                   62
#define PD_IEEE32OP                   63
#define PD_IEEE64OP                   64
#define PD_REAL32REM                  65
#define PD_REAL64REM                  66
#define PD_REAL32EQ                   67
#define PD_REAL64EQ                   68
#define PD_REAL32GT                   69
#define PD_REAL64GT                   70
#define PD_IEEECOMPARE                71
#define PD_DIEEECOMPARE               72
#define PD_IEEE32REM                  73
#define PD_IEEE64REM                  74
#define PD_RESCHEDULE                 75
#define PD_ASSERT                     76 /* added 25/9/90 */
#define PD_WSSIZEOF                   77 /* bug TS/1797 18/08/92 */
#define PD_VSSIZEOF                   78 /* bug TS/1797 18/08/92 */
#if 0
#define PD_UPDATE_PROFCOUNT           79 /* It is introduced and inlined by the
                                            compiler */
#define PD_IMS_GET_PDATA              80 /* bug TS/1039 05/02/93 */
#endif
/*}}}*/
/*{{{  Added by frmb for priority handling */
#define PD_GETPRI		81	/* get process priority */
#define PD_SETPRI		82	/* set process priority */
#define PD_INCPRI		83	/* raise process priority */
#define PD_DECPRI		84	/* lower process priority */
/*}}}  */
/*{{{  Added by frmb for channel-type allocation handling */
#define PD_ALLOC_CHAN_TYPE	85	/* manufacture a dynamic mobile chan type */
/*}}}  */
/*{{{  added by frmb for encode/decode and friends */
#if 0
#define PD_PROTOCOL_HASH	86	/* INT FUNCTION PROTOCOL.HASH (<name>) */
#define PD_DECODE_CHANNEL	87	/* PROC DECODE.CHANNEL (CHAN * in?, CHAN INT term?, CHAN * out!, CHAN BOOL go?)
					 * note: the output channel must be (INT; INT) or (INT; INT; INT) underneath! -- checked for
					 */
#define PD_DECODE_CHANNEL3	88	/* PROC DECODE.CHANNEL (CHAN * in?, CHAN INT term?, CHAN * out!)
					 * note: the output channel must be (INT; INT) or (INT; INT; INT) underneath! -- checked for
					 */
#define PD_ENCODE_CHANNEL	89	/* PROC ENCODE.CHANNEL (CHAN * in?, CHAN INT term?, CHAN * out!)
					 * note: the input channel must be (INT; INT) or (INT; INT; INT) underneath! -- checked for
					 */
#define PD_DETACH_DYNMOB	90	/* PROC DETACH.DYNMOB (MOBILE []* v, RESULT INT addr, size) */
#define PD_ATTACH_DYNMOB	91	/* PROC ATTACH.DYNMOB (VAL INT addr, size, MOBILE []* v) */
#endif
#define PD_DECODE_DATA		92	/* PROC DECODE.DATA (* item, RESULT INT addr, size) */
/*}}}*/
/*{{{  mobile-process related builtins*/
#define PD_MPBARSYNC		93	/* mobile-process barrier sync */
#define PD_MPBARRESIGN		94	/* mobile-process barrier resign */
#define PD_MPPSERIALISE		95	/* mobile-process serialisation */
#define PD_MPPDESERIALISE	96	/* mobile-process deserialisation */
#define PD_MPPCHECKROUTINE	97	/* mobile-process check routine */
#define PD_MPPLOADLIBRARY	98	/* mobile-process load library */
#define PD_MPPUNLOADLIBRARY	99	/* mobile-process unload library */

/*}}}*/
/*{{{  process-group related builtins*/
#if 0
#define PD_PGRP_NEWGRP		100	/* create a new process group */
#define PD_PGRP_INFO		101	/* process group info */
#endif

/*}}}*/
/*{{{  other occam-pi builtins*/
#if 0
#define PD_LOAD_TYPE_DESC	102	/* load type descriptor: INT FUNCTION LOAD.TYPE.DESC (<name>) */
#endif

/*}}}*/
/*{{{  architecture capability builtins*/
#define PD_REAL32SIN		103	/* REAL32 FUNCTION SIN (VAL REAL32 rad) */
#define PD_REAL64SIN		104	/* REAL64 FUNCTION DSIN (VAL REAL64 rad) */
#define PD_REAL32COS		105	/* REAL32 FUNCTION COS (VAL REAL32 rad) */
#define PD_REAL64COS		106	/* REAL64 FUNCTION DCOS (VAL REAL64 rad) */

/*}}}*/
/*{{{  processor affinity builtins*/
#define PD_GETAFF		107
#define PD_SETAFF		108
/*}}}*/
/*{{{  blocking system calls */
#define PD_KILLCALL		109
/*}}}*/
/*{{{  RMoX interrupt handling */
#define PD_WAIT_FOR_INTERRUPT	110
/*}}}*/
/*{{{  MOBILE manipulation */
#define PD_BIND_MOBILE		111
#define PD_BIND_MOBILE_HW	112
#define PD_DMA_CAPABLE		113
#define PD_MAKE_DMA_CAPABLE	114
/*}}}*/
/*{{{  further floating-point builtins*/
#define PD_REAL32TAN		115	/* REAL32 FUNCTION TAN (VAL REAL32 rad) */
#define PD_REAL64TAN		116	/* REAL64 FUNCTION DTAN (VAL REAL64 rad) */
/*}}}*/
/*{{{  memory barriers */
#define PD_MEMORY_BARRIER	117
#define PD_READ_MEMORY_BARRIER	118
#define PD_WRITE_MEMORY_BARRIER	119
/*}}}*/
/*{{{  further MOBILE manipulation */
#define PD_RESIZE_MOBILE_ARRAY_1D	120
/*}}}*/

/*{{{  Predefines for the configuration stuff */
/*{{{  Predefines for the configurer */
/* Validity flags: T=Txxx, H=T9000, C=C104, P=Partition,
                   U=Unknown, E=ControlPort, R=Route, S=Software
 */
#define PD_ATTR_LINK                 200 /* []EDGE     THCP.... */ /* NETWORK */
#define PD_ATTR_TYPE                 201 /* []BYTE     THCP.... */ /* NETWORK */
#define PD_ATTR_MEMSIZE              202 /* INT        T....... */ /* NETWORK */
#define PD_ATTR_ROMSIZE              203 /* INT        T....... */ /* NETWORK */
#define PD_ATTR_ROOT                 204 /* BOOL       TH...... */ /* NETWORK */

/* I have added 2 new entries for T450 attributes , memstart and num.links at the end of the list - NICK */

#define PD_ATTR_ORDER_CODE           205 /* INT        .......S */ /* MAPPING */
#define PD_ATTR_ORDER_VS             206 /* INT        .......S */ /* MAPPING */
#define PD_ATTR_LOCATION_CODE        207 /* INT        .......S */ /* MAPPING */
#define PD_ATTR_LOCATION_WS          208 /* INT        .......S */ /* MAPPING */
#define PD_ATTR_LOCATION_VS          209 /* INT        .......S */ /* MAPPING */
#define PD_ATTR_RESERVED             210 /* INT        T....... */ /* MAPPING */
#define PD_ATTR_TOLERANCE            211 /* INT        .......S */ /* MAPPING */ /* ONLY applicable to non-NDL configurer */
#define PD_ATTR_LINKQUOTA            212 /* INT        .......S */ /* MAPPING */ /* ONLY applicable to non-NDL configurer */
#define PD_ATTR_ROUTECOST            213 /* INT        .......S */ /* MAPPING */ /* ONLY applicable to non-NDL configurer */
#define PD_ATTR_ORDER_WS             214 /* INT        .......S */ /* MAPPING */
#define PD_ATTR_NODEBUG              215 /* BOOL       .......S */ /* MAPPING */
#define PD_ATTR_NOPROFILE            216 /* BOOL       .......S */ /* MAPPING */

/* The following are ONLY declared by the non-NDL configurer */

#define PD_HOSTEDGE                  218 /* EDGE       ........ */ /* Pre-declared EDGE (HOST) */
#define PD_DEFAULTNODE               219 /* NODE       ........ */ /* Pre-declared NODE (DEFAULT) */
/*}}}*/
/*{{{  Predefines for the NDL reader */
/* Validity flags: T=Txxx, H=T9000, C=C104, P=Partition,
                   U=Unknown, E=ControlPort, R=Route, S=Software, L=C100
 */
#define PD_ATTR_LINK_SPEED_MULTIPLY  220 /* INT        .HC.....L */ /* NETWORK */
#define PD_ATTR_LINK_SPEED_DIVIDE    221 /* []INT      .HC.....L */ /* NETWORK */
#define PD_ATTR_CONTROL_UP           222 /* EDGE       .HC.U...L */ /* NETWORK */
#define PD_ATTR_CONTROL_SPEED_DIVIDE 223 /* [2]INT     .HC.....L */ /* NETWORK */
#define PD_ATTR_MEMCONFIG            224 /* []BYTE     .H....... */ /* NETWORK */
#define PD_ATTR_PREAMBLE             225 /* []BYTE     .H....... */ /* NETWORK */
#define PD_ATTR_BOOTSTRAP            226 /* []BYTE     .H....... */ /* NETWORK */
#define PD_ATTR_MEMORY               227 /* [][3]INT   .H....... */ /* NETWORK */
#define PD_ATTR_CACHESIZE            228 /* INT        .H....... */ /* NETWORK */
#if 0
#define PD_ATTR_LINK_TXMODE          229 /* [4]BOOL    .H....... */ /* NETWORK */
#endif
#define PD_ATTR_LINK_BYTE_MODE       229 /* [4]BOOL    .H....... */ /* NETWORK */
#define PD_ATTR_EVENT_OUT            230 /* [4]BOOL    .H....... */ /* NETWORK */
#define PD_ATTR_LOCAL_ROM            231 /* BOOL       .H....... */ /* NETWORK */
#define PD_ATTR_SYSTEM_ROM           232 /* BOOL       .H....... */ /* NETWORK */
#define PD_ATTR_PMI_CONFIG_INROM     233 /* BOOL       .H....... */ /* NETWORK */
#define PD_ATTR_LINKSET_INROM        234 /* BOOL       .H....... */ /* NETWORK */
#define PD_ATTR_BOOTSTRAP_INROM      235 /* BOOL       .H....... */ /* NETWORK */
#define PD_ATTR_CACHE_CONFIG_INROM   236 /* BOOL       .H....... */ /* NETWORK */
#if 0
#define PD_ATTR_INTERVAL             236 /* [][2]INT   ..CP..... */ /* NETWORK */
#endif
#define PD_ATTR_LINK_SELECT          237 /* []INT      ..CP..... */ /* NETWORK */
#define PD_ATTR_DISCARD              238 /* []BOOL     ..CP..... */ /* NETWORK */
#define PD_ATTR_DELETE               239 /* []BOOL     ..CP..... */ /* NETWORK */
#define PD_ATTR_RANDOM               240 /* []BOOL     ..CP..... */ /* NETWORK */
#if 0
#define PD_ATTR_RANDOMSEED           241 /* INT        ..CP..... */ /* NETWORK */
#endif
#define PD_ATTR_RANDOM_INTERVAL      242 /* [2]INT     ..CP..... */ /* NETWORK */
#define PD_ATTR_NODE                 243 /* NODE       ...P..... */ /* NETWORK */
#define PD_ATTR_LINKS                244 /* [][2]INT   ...P..... */ /* NETWORK */
#define PD_ATTR_START_CODE           245 /* BYTE       ....U.... */ /* NETWORK */
#define PD_ATTR_IDENTIFY_CODE        246 /* BYTE       ....U.... */ /* NETWORK */
#define PD_ATTR_IDENTIFY_RESPONSE    247 /* BYTE       ....U.... */ /* NETWORK */
#define PD_ATTR_EXPECTED_IDENTITY    248 /* INT        ....U.... */ /* NETWORK */
#define PD_ATTR_ERROR_RESPONSE       249 /* BYTE       ....U.... */ /* NETWORK */
#define PD_ATTR_ERROR_HANDSHAKE      250 /* BYTE       ....U.... */ /* NETWORK */
#define PD_ATTR_INITIALISATION       251 /* []INT      ....U.... */ /* NETWORK */
#define PD_ATTR_DATA                 252 /* EDGE       .....E... */ /* NETWORK */
#define PD_ATTR_CONTROL              253 /* EDGE       .....E... */ /* NETWORK */
#define PD_ATTR_CONTROL_DOWN         254 /* EDGE       .HC.U...L */ /* NETWORK */
#define PD_ATTR_OUTWARD              255 /* ARC        ......R.. */ /* NETWORK */
#define PD_ATTR_RETURN               256 /* ARC        ......R.. */ /* NETWORK */
#define PD_ATTR_LINK_GROUPS          257 /* [][2]INT   ..C...... */ /* NETWORK */
#define PD_ATTR_INTERVAL_SEPARATOR   258 /* []INT      ..CP..... */ /* NETWORK */
#define PD_ATTR_ROM_FILE             259 /* []BYTE     TH....... */ /* NETWORK */
#define PD_ATTR_REBOOT_FROM_LINK     260 /* BOOL       .H....... */ /* NETWORK */
#define PD_ATTR_BOOT_FROM_ROM        261 /* BOOL       .H....... */ /* NETWORK */
#define PD_ATTR_MODE                 262 /* INT        ........L */ /* NETWORK */
#define PD_ATTR_DS_LINK              263 /* []EDGE     ........L */ /* NETWORK */
/*}}}*/
/*{{{  Further additions for the configurer and NDL reader added late for the T450 */
#define PD_ATTR_MEMSTART             264 /* INT        T....... */ /* NETWORK */
#define PD_ATTR_NUM_LINKS            265 /* INT        T....... */ /* NETWORK */
/*}}}*/
/*}}}*/

/*{{{  Pragma details */
typedef enum
  {
    pragma_name_translate,
    pragma_name_linkage,
    pragma_name_external,
    pragma_name_shared,
    pragma_name_comment,
    pragma_name_aliased,
    pragma_name_hardware,
    pragma_name_nestedtimer,
    pragma_name_nestedplace,
    pragma_name_nestedport,
    pragma_name_badlybehaved,
    pragma_name_assumeconst,
    pragma_name_defined,
    pragma_name_undefined,
    pragma_name_iospace,
    pragma_name_dyncall,
    pragma_name_dexternal,
    pragma_name_export,
    pragma_name_formalmodel,
    pragma_name_fmtypes
  } pragma_name_tag_t;

/*}}}*/


