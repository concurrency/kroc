#!/usr/bin/env perl
#
#  Perl code for linking and assembling bytecode for the TVM
#  Copyright (C) 2008 Carl Ritson <cgr@kent.ac.uk>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

#
#  Perl code for linking and assembling bytecode for the TVM
#  Copyright (C) 2008 Carl Ritson <cgr@kent.ac.uk>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

#
#  Perl code for linking and assembling bytecode for the TVM
#  Copyright (C) 2008 Carl Ritson <cgr@kent.ac.uk>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

#
#  Perl code for linking and assembling bytecode for the TVM
#  Copyright (C) 2008 Carl Ritson <cgr@kent.ac.uk>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

package PLinker;

use strict;
use vars qw($STORE);

$STORE = {};
$STORE = {
           'tcoff' => {
                        '32' => 'UNINDEXED_USAGE',
                        'LAST_TCOFF_OP' => '13',
                        'INSTR_DEBUG_SUPPORT' => 2048,
                        'ATTRIB_UNIVERSAL' => 0,
                        'LANG_OCCAM' => '3',
                        'INSTR_WSUBDB' => 16,
                        '18' => 'REP_START',
                        'INSTR_RMC_CORE1' => 33554432,
                        '16' => 'KILL_ID',
                        'TIMES_OP' => '8',
                        'ADJUST_POINT' => '5',
                        '27' => 'VERSION',
                        'LOAD_EXPR' => '8',
                        'ATTRIB_FPU_CALLING' => 524288,
                        '512' => 'DATA_USAGE',
                        'WORD_PATCH' => '25',
                        '8388608' => 'INSTR_088_16',
                        'INSTR_NO_DEVICE' => 65536,
                        '20' => 'COMMENT',
                        'LP_VALUE' => '2',
                        'ATTRIB_WORD_64' => 4,
                        'INSTR_CRC' => 64,
                        'UNINDEXED_USAGE' => 32,
                        '31' => 'SPECIFIC_SYMBOL',
                        'WRITE_SECTION' => 1,
                        'MAX_OP' => '11',
                        '11' => 'SECTION',
                        'SYMBOL' => '30',
                        'MIN_OP' => '12',
                        '134217728' => 'INSTR_RMC_CORE3',
                        'SPECIFIC_SYMBOL' => '31',
                        'INSTR_088_16' => 8388608,
                        '4096' => 'INSTR_TIMER_DISABLE',
                        'INSTR_POP' => 16384,
                        'INSTR_DUP' => 8,
                        'INSTR_LDDEVID' => 1024,
                        'KILL_ID' => '16',
                        '15' => 'DEFINE_SYMBOL',
                        'INDEX_ENTRY' => '24',
                        'INSTR_FP_SUPPORT' => 4,
                        'NOTYPE_USAGE' => 1024,
                        'INSTR_BITOPS' => 128,
                        'LANG_ANSI_C' => '4',
                        'CO_VALUE' => '1',
                        'DEFINE_LABEL' => '14',
                        'PFX_8_NUMBER' => '254',
                        '16777216' => 'INSTR_386_16',
                        'SYMBOL_TABLE_MAGIC_TEXT_LENGTH' => '19',
                        '2' => 'START_MODULE',
                        '17' => 'BYTE_PATCH',
                        '262144' => 'ATTRIB_NON_FPU_CALLING',
                        'CONDITIONAL_USAGE' => 16,
                        'REM_OP' => '10',
                        '524288' => 'ATTRIB_FPU_CALLING',
                        'LANG_FORTRAN_77' => '5',
                        'ATTRIB_CALL_IO' => 0,
                        'LANG_ADA' => '8',
                        'REP_START' => '18',
                        '14' => 'DEFINE_LABEL',
                        '65536' => 'INSTR_NO_DEVICE',
                        'INSTR_MOVE2D' => 32,
                        '24' => 'INDEX_ENTRY',
                        'IMPORT_USAGE' => 4,
                        'INSTR_FPU_CORE' => 256,
                        'ARCH_T' => 1048576,
                        'SIGN_INDICATOR' => '255',
                        '100663296' => 'INSTR_387',
                        'READ_SECTION' => 2,
                        '0' => 'BOOL_FALSE',
                        '23' => 'LIB_INDEX_END',
                        'LOCAL_USAGE' => 1,
                        'SYMTAB_SYMBOL' => '2',
                        '1048576' => 'ARCH_T',
                        'BYTE_PATCH' => '17',
                        '251' => 'PFX_1_NUMBER',
                        'INSTR_RMC_CORE2' => 67108864,
                        'INSTR_LDMEMSTARTVAL' => 8192,
                        'ATTRIB_WORD_16' => 1,
                        '253' => 'PFX_4_NUMBER',
                        'MINUS_OP' => '7',
                        'LINKABLE' => '1',
                        'INSTR_RMC_CORE3' => 134217728,
                        'LOAD_ZEROS' => '9',
                        '8' => 'LOAD_EXPR',
                        'SET_LOAD_POINT' => '4',
                        'INSTR_TIMER_DISABLE' => 4096,
                        '256' => 'ROUTINE_USAGE',
                        'WEAK_USAGE' => 8,
                        'AP_VALUE' => '13',
                        '5' => 'ADJUST_POINT',
                        'ATTRIB_INSTR_IO' => 2048,
                        '21' => 'MESSAGE',
                        '32768' => 'INSTR_NO_SEMAPHORE',
                        '16384' => 'INSTR_POP',
                        '7' => 'LOAD_PREFIX',
                        'COMMENT' => '20',
                        '26' => 'DESCRIPTOR',
                        'SECTION' => '11',
                        'ATTRIB_HALT' => 128,
                        'ORIGIN_USAGE' => 128,
                        'LANG_ISO_PASCAL' => '6',
                        'LOAD_PREFIX' => '7',
                        '255' => 'SIGN_INDICATOR',
                        'VIRTUAL_SECTION' => 16,
                        'EXECUTE_SECTION' => 4,
                        'LINKED_UNIT' => '28',
                        'ATTRIB_MEMSTARTLEQ28' => 0,
                        'WL_VALUE' => '5',
                        'START_MODULE' => '2',
                        'INSTR_386_16' => 16777216,
                        'DESCRIPTOR' => '26',
                        'DIVIDE_OP' => '9',
                        'ERROR_MSG' => '3',
                        '10' => 'ALIGN',
                        'LOCAL_SYMBOLS' => '13',
                        'SV_VALUE' => '3',
                        'LANG_MODULA_2' => '7',
                        'ARCH_H' => 2097152,
                        'DEBUG_SECTION' => 8,
                        'ATTRIB_NON_FPU_CALLING' => 262144,
                        'LAST_TCOFF' => '31',
                        'WARNING_MSG' => '2',
                        'INSTR_CORE' => 1,
                        'LANG_ASSEMBLER' => '9',
                        'SYMTAB_SPECIFIC_SYMBOL' => '3',
                        'SYMTAB_HEADER' => '1',
                        'LANG_OCCAM_HARNESS' => '10',
                        'LOAD_TEXT' => '6',
                        '64' => 'PROVISIONAL_USAGE',
                        'NORMAL_MSG' => '1',
                        '12' => 'DEFINE_MAIN',
                        'INSTR_387' => 100663296,
                        'INSTR_NO_SEMAPHORE' => 32768,
                        'ATTRIB_STOP' => 256,
                        'SS_VALUE' => '4',
                        '67108864' => 'INSTR_RMC_CORE2',
                        'LIB_INDEX_END' => '23',
                        'ATTRIB_MEMSTART28' => 16,
                        '19' => 'REP_END',
                        'PFX_1_NUMBER' => '251',
                        'END_MODULE' => '3',
                        '1' => 'LINKABLE',
                        '30' => 'SYMBOL',
                        'REP_END' => '19',
                        'MESSAGE' => '21',
                        'DATA_USAGE' => 512,
                        '25' => 'WORD_PATCH',
                        '128' => 'ORIGIN_USAGE',
                        '252' => 'PFX_2_NUMBER',
                        '28' => 'LINKED_UNIT',
                        'DEFINE_MAIN' => '12',
                        '2048' => 'ATTRIB_INSTR_IO',
                        'PLUS_OP' => '6',
                        'INSTR_FPTESTERR' => 512,
                        'BOOL_TRUE' => '1',
                        'BOOL_FALSE' => '0',
                        '254' => 'PFX_8_NUMBER',
                        '8192' => 'INSTR_LDMEMSTARTVAL',
                        'ATTRIB_WORD_32' => 2,
                        'EXPORT_USAGE' => 2,
                        'INSTR_FMUL' => 2,
                        'DEFINE_SYMBOL' => '15',
                        '1024' => 'NOTYPE_USAGE',
                        'ROUTINE_USAGE' => 256,
                        'LANG_NOT_KNOWN' => '1',
                        '22' => 'LIB_INDEX_START',
                        '33554432' => 'INSTR_RMC_CORE1',
                        'ATTRIB_MEMSTART18' => 8,
                        '13' => 'LOCAL_SYMBOLS',
                        '6' => 'LOAD_TEXT',
                        'PROVISIONAL_USAGE' => 64,
                        'LIB_INDEX_START' => '22',
                        '3' => 'END_MODULE',
                        'ALIGN' => '10',
                        '9' => 'LOAD_ZEROS',
                        'PFX_4_NUMBER' => '253',
                        '4' => 'SET_LOAD_POINT',
                        '2097152' => 'ARCH_H',
                        'INSTR_386_32' => 33554432,
                        'PFX_2_NUMBER' => '252',
                        'VERSION' => '27',
                        'LANG_LINKED' => '2'
                      },
           'ins_sec' => {
                          '' => 'REAL64TAN',
                          '559' => 'PROC_ALLOC',
                          '32' => 'RET',
                          'TRAP' => 252,
                          '118' => 'BITCNT',
                          'TALTWT' => 81,
                          '71' => 'ENBT',
                          'FPUEXPDEC32' => undef,
                          'ROUNDSN' => 109,
                          '560' => 'PROC_PARAM',
                          'ENBS2' => 558,
                          'STARTP' => 13,
                          '580' => 'MT_XOUT',
                          'LEND' => 33,
                          'TESTLDE' => 36,
                          '84' => 'STTIMER',
                          'FPUABS' => undef,
                          '194' => 'LDTH',
                          'VOUT' => -19,
                          'CLOCKDIS' => -66,
                          'DIFF' => 4,
                          'WRITEHDR' => -27,
                          'FPR64TOR32' => 216,
                          '-18' => 'STOPCH',
                          'TESTSTS' => 38,
                          '31' => 'REM',
                          '578' => 'MT_SYNC',
                          'TESTHARDCHAN' => 45,
                          'STLF' => 28,
                          'PROD' => 8,
                          'FPRN' => 208,
                          'STOPCH' => -18,
                          '-65' => 'CLOCKENB',
                          'FPENTRY' => 171,
                          '29' => 'XDBLE',
                          'GTU' => 95,
                          '572' => 'MT_OUT',
                          '-37' => 'IRDSQ',
                          'FPCHKERR' => 131,
                          'CLOCKENB' => -65,
                          '-29' => 'DISG',
                          '226' => 'MALLOC',
                          '58' => 'XWORD',
                          '211' => 'FPSQRT',
                          '153' => 'ENBT3',
                          '15' => 'OUTWORD',
                          '101' => 'SULMUL',
                          '-8' => 'SYSCALL',
                          'TIMERENABLEL' => 125,
                          '76' => 'CSNGL',
                          'WMB' => 586,
                          '62' => 'SAVEH',
                          'IOR' => 548,
                          '571' => 'MT_IN',
                          'MRELEASE' => 227,
                          '139' => 'FPMUL',
                          'NDIST' => 238,
                          '129' => 'WSUBDB',
                          'MT_LOCK' => 574,
                          '548' => 'IOR',
                          'FPNAN' => 145,
                          'FPCHKI32' => 222,
                          'FPLDNLMULSN' => 172,
                          '-12' => 'SIGNAL',
                          'DEVLS' => 242,
                          '236' => 'XEND',
                          'OUTBYTE' => 14,
                          '218' => 'FPEXPINC32',
                          '168' => 'FPLDNLMULDB',
                          '135' => 'FPADD',
                          '14' => 'OUTBYTE',
                          'REAL64SIN' => undef,
                          'CSU' => 251,
                          '-21' => 'STVLCB',
                          '145' => 'FPNAN',
                          '49' => 'LMUL',
                          '178' => 'CLRJ0BREAK',
                          'BITREVNBITS' => 120,
                          'SATSUB' => 105,
                          'REAL32TAN' => undef,
                          '124' => 'TIMERENABLEH',
                          'TALT' => 78,
                          'LDCNT' => 192,
                          'CLRHALTERR' => 87,
                          'SETERR' => 16,
                          '0' => 'REV',
                          '23' => 'STLB',
                          '96' => 'EXTIN',
                          'FPLDNLSNI' => 134,
                          '-47' => 'ICA',
                          'FPDUP' => 163,
                          '160' => 'FPLDZERODB',
                          '569' => 'MT_RELEASE',
                          'CIRU' => 204,
                          '8' => 'PROD',
                          'STVLCB' => -21,
                          'LDCLOCK' => -67,
                          'SULMUL' => 101,
                          '43' => 'TIN',
                          'FPDIVBY2' => 209,
                          '-9' => 'TRAPENB',
                          '21' => 'STOPP',
                          'EXTVRFY' => 20,
                          'MT_BIND' => 583,
                          '193' => 'SSUB',
                          '119' => 'BITREVWORD',
                          'FPUSQRTSTEP' => undef,
                          '586' => 'WMB',
                          '180' => 'TESTJ0BREAK',
                          'SLMUL' => 100,
                          '-26' => 'INITVLCB',
                          '244' => 'DEVLW',
                          '-132' => 'LDPRODID',
                          'LDRESPTR' => -40,
                          '61' => 'SAVEL',
                          'PROC_START' => 563,
                          'RESTART' => -34,
                          '-44' => 'DEVMOVE',
                          'LMUL' => 49,
                          'ENBS3' => 173,
                          'PROC_ALLOC' => 559,
                          '113' => 'LDINF',
                          '152' => 'FPI32TOR64',
                          '189' => 'READBFR',
                          'ENBC3' => 112,
                          '579' => 'MT_XIN',
                          'XSWORD' => 248,
                          '-34' => 'RESTART',
                          'ENBT' => 71,
                          '87' => 'CLRHALTERR',
                          '77' => 'CCNT1',
                          '221' => 'FPADDDBSN',
                          '39' => 'TESTSTE',
                          '64' => 'SHR',
                          'TESTLDS' => 35,
                          'XWORD' => 58,
                          '558' => 'ENBS2',
                          '12' => 'SUB',
                          'RESETCH' => 18,
                          '45' => 'TESTHARDCHAN',
                          'PROC_MT_COPY' => 561,
                          '-11' => 'WAIT',
                          '573' => 'MT_XCHG',
                          '237' => 'NDISC',
                          'CLRJ0BREAK' => 178,
                          'FPLDNLSN' => 142,
                          'IRET' => -17,
                          'FPSTTEST' => 128,
                          'XOR' => 51,
                          'GRANT' => -31,
                          'FPMULBY2' => 210,
                          '567' => 'GETPAS',
                          '1' => 'LB',
                          '136' => 'FPSTNLSN',
                          '116' => 'CRCWORD',
                          '144' => 'FPREMSTEP',
                          '380' => 'LDDEVID',
                          '100' => 'SLMUL',
                          'MT_XXCHG' => 581,
                          '120' => 'BITREVNBITS',
                          'MB' => 584,
                          'DIV' => 44,
                          'TIMERDISABLEL' => 123,
                          'FPUMULBY2' => undef,
                          '581' => 'MT_XXCHG',
                          'REV' => 0,
                          'FPREMFIRST' => 143,
                          'LADD' => 22,
                          '177' => 'BREAK',
                          'ENBS' => 73,
                          'FPADDDBSN' => 221,
                          '22' => 'LADD',
                          '42' => 'TESTPRANAL',
                          'WSUBDB' => 129,
                          'FPR32TOR64' => 215,
                          'GT' => 9,
                          '213' => 'FPRM',
                          '94' => 'MOVE2DZERO',
                          'SATMUL' => 106,
                          '51' => 'XOR',
                          '568' => 'MT_ALLOC',
                          'FPEXPDEC32' => 217,
                          'CWORD' => 86,
                          'EXT_MT_IN' => 587,
                          'MALLOC' => 226,
                          '171' => 'FPENTRY',
                          'FPREM' => 207,
                          'FPGT' => 148,
                          '200' => 'SS',
                          '-35' => 'UNMKRC',
                          '27' => 'LDPI',
                          '161' => 'FPINT',
                          '582' => 'MT_DCLONE',
                          '-67' => 'LDCLOCK',
                          '-4' => 'LDSHADOW',
                          'SWAPQUEUE' => -16,
                          'STMOVE2DINIT' => -32,
                          '20' => 'EXTVRFY',
                          '109' => 'ROUNDSN',
                          'FPUR32TOR64' => undef,
                          '151' => 'FPGE',
                          'FPUCLRERR' => undef,
                          '557' => 'ENBT2',
                          'LDTIMER' => 34,
                          'STHB' => 80,
                          'EXTNDISC' => 103,
                          'FPRP' => 212,
                          'REAL32SIN' => undef,
                          'FPUSETERR' => undef,
                          'STOPP' => 21,
                          'SHL' => 65,
                          '78' => 'TALT',
                          'DIST' => 46,
                          '197' => 'INTENB',
                          'LDTH' => 194,
                          'FPSUB' => 137,
                          'PROC_PARAM' => 560,
                          '138' => 'FPLDNLDB',
                          'TESTPRANAL' => 42,
                          'DISC' => 47,
                          '137' => 'FPSUB',
                          'MT_RELEASE' => 569,
                          'FPRZ' => 214,
                          '60' => 'GAJW',
                          '-10' => 'TRAPDIS',
                          'LDDEVID' => 380,
                          '-2' => 'FPLDALL',
                          'CCNT1' => 77,
                          'MT_ENROLL' => 576,
                          'IRDSQ' => -37,
                          'BREAK' => 177,
                          '17' => 'MRELEASEP',
                          'DEVLW' => 244,
                          '82' => 'SUM',
                          '110' => 'LDTRAPH',
                          '69' => 'ALTEND',
                          '112' => 'ENBC3',
                          '191' => 'STCONF',
                          'MRELEASEP' => 17,
                          'TESTHALTERR' => 89,
                          '-45' => 'ICL',
                          'ENBC2' => 556,
                          'LDTRAPPED' => 198,
                          '187' => 'CBU',
                          'OR' => 75,
                          '588' => 'EXT_MT_OUT',
                          'CSUB0' => 19,
                          'ENBT3' => 153,
                          '79' => 'LDIFF',
                          'MT_XCHG' => 573,
                          '212' => 'FPRP',
                          'LB' => 1,
                          'SWAPBFR' => -23,
                          '126' => 'LDMEMSTARTVAL',
                          'TESTSTE' => 39,
                          '251' => 'CSU',
                          'ICA' => -47,
                          'IOW8' => 551,
                          'FPMUL' => 139,
                          '-23' => 'SWAPBFR',
                          'WCNT' => 63,
                          '176' => 'SETTIMESLICE',
                          'CS' => 250,
                          'MUL' => 83,
                          'FPURN' => undef,
                          '-28' => 'READHDR',
                          'SB' => 59,
                          '574' => 'MT_LOCK',
                          '170' => 'FPLDNLADDSN',
                          '33' => 'LEND',
                          'STFLAGS' => 183,
                          '7' => 'IN',
                          '227' => 'MRELEASE',
                          '26' => 'LDIV',
                          'IOW32' => 555,
                          '99' => 'UNPACKSN',
                          '566' => 'SETAFF',
                          '72' => 'ENBC',
                          'IOR16' => 552,
                          'DISS' => 48,
                          '182' => 'LDFLAGS',
                          'SETTIMESLICE' => 176,
                          'LSUB' => 56,
                          '108' => 'POSTNORMSN',
                          '-66' => 'CLOCKDIS',
                          '-20' => 'VIN',
                          '556' => 'ENBC2',
                          '232' => 'XABLE',
                          'LSHL' => 54,
                          'SWAPTIMER' => -15,
                          '207' => 'FPREM',
                          '142' => 'FPLDNLSN',
                          '167' => 'FPENTRY3',
                          '48' => 'DISS',
                          'LDPI' => 27,
                          'SAVEL' => 61,
                          'DEVLB' => 240,
                          '50' => 'NOT',
                          'GETAFF' => 565,
                          '-32' => 'STMOVE2DINIT',
                          'RUNP' => 57,
                          'RET' => 32,
                          'SETHALTERR' => 88,
                          '549' => 'IOW',
                          'UNMKRC' => -35,
                          'NOT' => 50,
                          'TESTSTD' => 40,
                          'GETPRI' => 162,
                          'ENBC' => 72,
                          '575' => 'MT_UNLOCK',
                          'START' => 511,
                          'INSERTQUEUE' => -14,
                          'LDIFF' => 79,
                          'FPLG' => 155,
                          'FPENTRY3' => 167,
                          'STOPERR' => 85,
                          '128' => 'FPSTTEST',
                          'FPUEXPINC32' => undef,
                          '28' => 'STLF',
                          '40' => 'TESTSTD',
                          '589' => 'MT_RESIZE',
                          '250' => 'CS',
                          '192' => 'LDCNT',
                          'LDPRODID' => -132,
                          '215' => 'FPR32TOR64',
                          'SYSCALL' => -8,
                          '150' => 'FPI32TOR32',
                          '155' => 'FPLG',
                          '130' => 'FPLDNLDBI',
                          'NULL' => 253,
                          '53' => 'LSHR',
                          '245' => 'DEVSW',
                          'ALTWT' => 68,
                          'FPURM' => undef,
                          '583' => 'MT_BIND',
                          'ENBT2' => 557,
                          '-5' => 'TRET',
                          '85' => 'STOPERR',
                          'EXTENBC' => 102,
                          '9' => 'GT',
                          'MT_OUT' => 572,
                          'FPSQRT' => 211,
                          'SETPRI' => 165,
                          '34' => 'LDTIMER',
                          'NDISS' => 239,
                          '90' => 'DUP',
                          '-17' => 'IRET',
                          '102' => 'EXTENBC',
                          '565' => 'GETAFF',
                          'BCNT' => 52,
                          'SETHDR' => -24,
                          '16' => 'SETERR',
                          'MT_RESIGN' => 577,
                          '55' => 'LSUM',
                          'FPURP' => undef,
                          '233' => 'XIN',
                          'SETJ0BREAK' => 179,
                          '57' => 'RUNP',
                          '163' => 'FPDUP',
                          '89' => 'TESTHALTERR',
                          'EXTOUT' => 97,
                          'LBX' => 185,
                          '584' => 'MB',
                          '35' => 'TESTLDS',
                          'POSTNORMSN' => 108,
                          '11' => 'OUT',
                          '208' => 'FPRN',
                          '511' => 'START',
                          'NOP' => -64,
                          '93' => 'MOVE2DNONZERO',
                          'CSNGL' => 76,
                          'FPLDTEST' => 133,
                          'FPEXPINC32' => 218,
                          'XBWORD' => 184,
                          '114' => 'FMUL',
                          '199' => 'CIR',
                          'TESTJ0BREAK' => 180,
                          'FPORDERED' => 146,
                          'FPURZ' => undef,
                          '73' => 'ENBS',
                          'MOVE2DINIT' => 91,
                          'STLB' => 23,
                          '-25' => 'SETCHMODE',
                          '67' => 'ALT',
                          '241' => 'DEVSB',
                          '198' => 'LDTRAPPED',
                          '585' => 'RMB',
                          'MINT' => 66,
                          'LDTRAPH' => 110,
                          'TIMERENABLEH' => 124,
                          'TRAPENB' => -9,
                          'MT_ALLOC' => 568,
                          'LDFLAGS' => 182,
                          'ICL' => -45,
                          'POP' => 121,
                          '202' => 'LS',
                          '249' => 'LSX',
                          'STHF' => 24,
                          '184' => 'XBWORD',
                          'VIN' => -20,
                          '24' => 'STHF',
                          '140' => 'FPDIV',
                          '104' => 'SATADD',
                          '131' => 'FPCHKERR',
                          'EXTIN' => 96,
                          '-30' => 'ENBG',
                          '154' => 'FPB32TOR64',
                          'DUP' => 90,
                          'REAL64COS' => undef,
                          'SATADD' => 104,
                          '159' => 'FPLDZEROSN',
                          '553' => 'IOW16',
                          'FPENTRY2' => 169,
                          '555' => 'IOW32',
                          '47' => 'DISC',
                          'MT_XOUT' => 580,
                          '37' => 'TESTLDD',
                          '5' => 'ADD',
                          '195' => 'LDCHSTATUS',
                          '554' => 'IOR32',
                          'WSMAP' => -145,
                          '-22' => 'LDVLCB',
                          'XABLE' => 232,
                          '552' => 'IOR16',
                          'MT_SYNC' => 578,
                          'TRAPDIS' => -10,
                          'FPNOTFINITE' => 147,
                          '162' => 'GETPRI',
                          'FDCL' => -46,
                          'BITCNT' => 118,
                          'CBU' => 187,
                          '-48' => 'FDCA',
                          '-3' => 'STSHADOW',
                          'GCALL' => 6,
                          '74' => 'MOVE',
                          'STCONF' => 191,
                          '240' => 'DEVLB',
                          '-38' => 'ERDSQ',
                          'STTIMER' => 84,
                          'OUT' => 11,
                          '115' => 'CFLERR',
                          '103' => 'EXTNDISC',
                          '201' => 'CHANTYPE',
                          'FPDIV' => 140,
                          'FPUNOROUND' => undef,
                          'REAL64TAN' => undef,
                          '91' => 'MOVE2DINIT',
                          'SELTH' => -7,
                          '214' => 'FPRZ',
                          'PROC_MT_MOVE' => 562,
                          'FPABS' => 219,
                          '564' => 'PROC_END',
                          'LDSHADOW' => -4,
                          '563' => 'PROC_START',
                          'ADD' => 5,
                          'WSUB' => 10,
                          '97' => 'EXTOUT',
                          '41' => 'TESTERR',
                          'DEVMOVE' => -44,
                          '52' => 'BCNT',
                          'LSUM' => 55,
                          'NORM' => 25,
                          'ENDP' => 3,
                          'FDCA' => -48,
                          'FPLDNLDBI' => 130,
                          'FPUR64TOR32' => undef,
                          'UNPACKSN' => 99,
                          'INITVLCB' => -26,
                          'INSPHDR' => 188,
                          'XEND' => 236,
                          'RMB' => 585,
                          'XDBLE' => 29,
                          '-145' => 'WSMAP',
                          '68' => 'ALTWT',
                          '188' => 'INSPHDR',
                          'LSX' => 249,
                          '-33' => 'CAUSEERROR',
                          '576' => 'MT_ENROLL',
                          'FPSTNLDB' => 132,
                          '222' => 'FPCHKI32',
                          '25' => 'NORM',
                          'TIMERDISABLEH' => 122,
                          'FPREV' => 164,
                          'FPUCHKI64' => undef,
                          '83' => 'MUL',
                          'REAL32COS' => undef,
                          'LDVLCB' => -22,
                          'SHR' => 64,
                          '217' => 'FPEXPDEC32',
                          '-6' => 'GOPROT',
                          '239' => 'NDISS',
                          '122' => 'TIMERDISABLEH',
                          'CAUSEERROR' => -33,
                          '143' => 'FPREMFIRST',
                          '158' => 'FPSTNLI32',
                          'MT_IN' => 571,
                          'BITREVWORD' => 119,
                          '46' => 'DIST',
                          'DEVSS' => 243,
                          '-68' => 'STCLOCK',
                          'MOVE2DALL' => 92,
                          '6' => 'GCALL',
                          'FPRTOI32' => 157,
                          '562' => 'PROC_MT_MOVE',
                          '36' => 'TESTLDE',
                          '183' => 'STFLAGS',
                          'XIN' => 233,
                          'SSUB' => 193,
                          '132' => 'FPSTNLDB',
                          '169' => 'FPENTRY2',
                          '-16' => 'SWAPQUEUE',
                          'FPI32TOR64' => 152,
                          '-39' => 'STRESPTR',
                          'INTDIS' => 196,
                          'BSUB' => 2,
                          'CIR' => 199,
                          'ERDSQ' => -38,
                          '18' => 'RESETCH',
                          '125' => 'TIMERENABLEL',
                          'FPLDZEROSN' => 159,
                          '-27' => 'WRITEHDR',
                          '44' => 'DIV',
                          '587' => 'EXT_MT_IN',
                          'FPLDNLMULDB' => 168,
                          '190' => 'LDCONF',
                          'SUB' => 12,
                          '95' => 'GTU',
                          'SUM' => 82,
                          'NDISC' => 237,
                          '-146' => 'CODEMAP',
                          '243' => 'DEVSS',
                          'ENBG' => -30,
                          'FPCHKI64' => 223,
                          '551' => 'IOW8',
                          'MT_DCLONE' => 582,
                          '148' => 'FPGT',
                          'SETCHMODE' => -25,
                          'FPSTALL' => -1,
                          'LS' => 202,
                          'SETAFF' => 566,
                          'LDMEMSTARTVAL' => 126,
                          'IOW16' => 553,
                          '106' => 'SATMUL',
                          'FPREMSTEP' => 144,
                          '157' => 'FPRTOI32',
                          '65' => 'SHL',
                          'DISG' => -29,
                          'GETPAS' => 567,
                          'DEVSB' => 241,
                          'MOVE' => 74,
                          '203' => 'STTRAPPED',
                          'IOR8' => 550,
                          'DEVSW' => 245,
                          '81' => 'TALTWT',
                          'SAVEH' => 62,
                          '86' => 'CWORD',
                          'FPB32TOR64' => 154,
                          '204' => 'CIRU',
                          'MKRC' => -36,
                          '165' => 'SETPRI',
                          '-15' => 'SWAPTIMER',
                          'CB' => 186,
                          '2' => 'BSUB',
                          'FPLDNLADDSN' => 170,
                          'STRESPTR' => -39,
                          'TIMESLICE' => -13,
                          '186' => 'CB',
                          'MT_XIN' => 579,
                          '147' => 'FPNOTFINITE',
                          'IOR32' => 554,
                          'FPEQ' => 149,
                          'LDPRI' => 30,
                          'AND' => 70,
                          'GOPROT' => -6,
                          '172' => 'FPLDNLMULSN',
                          '-36' => 'MKRC',
                          '223' => 'FPCHKI64',
                          'FPTESTERR' => 156,
                          'FPSTNLSN' => 136,
                          '-64' => 'NOP',
                          '121' => 'POP',
                          'PROC_END' => 564,
                          'FPRM' => 213,
                          'IOW' => 549,
                          '238' => 'NDIST',
                          '577' => 'MT_RESIGN',
                          'FPLDNLDB' => 138,
                          '253' => 'NULL',
                          '561' => 'PROC_MT_COPY',
                          'STTRAPPED' => 203,
                          'IN' => 7,
                          '209' => 'FPDIVBY2',
                          'TESTERR' => 41,
                          '216' => 'FPR64TOR32',
                          '117' => 'CRCBYTE',
                          'ALT' => 67,
                          '63' => 'WCNT',
                          'CODEMAP' => -146,
                          'FPLDNLADDDB' => 166,
                          '80' => 'STHB',
                          'FPADD' => 135,
                          'INTENB' => 197,
                          'REM' => 31,
                          '179' => 'SETJ0BREAK',
                          'FPINT' => 161,
                          'SS' => 200,
                          'CHANTYPE' => 201,
                          'ALTEND' => 69,
                          '92' => 'MOVE2DALL',
                          '-19' => 'VOUT',
                          '10' => 'WSUB',
                          'GAJW' => 60,
                          '550' => 'IOR8',
                          '-46' => 'FDCL',
                          '-7' => 'SELTH',
                          'FPUSQRTLAST' => undef,
                          'FPRANGE' => 141,
                          'TRET' => -5,
                          'STCLOCK' => -68,
                          '-24' => 'SETHDR',
                          'CFLERR' => 115,
                          '133' => 'FPLDTEST',
                          '123' => 'TIMERDISABLEL',
                          '149' => 'FPEQ',
                          'MOVE2DNONZERO' => 93,
                          '210' => 'FPMULBY2',
                          'LDINF' => 113,
                          'FPUDIVBY2' => undef,
                          'FPLDZERODB' => 160,
                          'FPSTNLI32' => 158,
                          'EXT_MT_OUT' => 588,
                          'FMUL' => 114,
                          '173' => 'ENBS3',
                          '56' => 'LSUB',
                          '66' => 'MINT',
                          'FPUSQRTFIRST' => undef,
                          '19' => 'CSUB0',
                          'LDIV' => 26,
                          '54' => 'LSHL',
                          '-13' => 'TIMESLICE',
                          'MT_UNLOCK' => 575,
                          '-40' => 'LDRESPTR',
                          '70' => 'AND',
                          'CRCBYTE' => 117,
                          'OUTWORD' => 15,
                          '-14' => 'INSERTQUEUE',
                          'SIGNAL' => -12,
                          'LSHR' => 53,
                          '166' => 'FPLDNLADDDB',
                          'LDCONF' => 190,
                          'FPUCHKI32' => undef,
                          '88' => 'SETHALTERR',
                          '30' => 'LDPRI',
                          '141' => 'FPRANGE',
                          '570' => 'MT_CLONE',
                          'FPI32TOR32' => 150,
                          'LDCHSTATUS' => 195,
                          'STSHADOW' => -3,
                          '252' => 'TRAP',
                          '75' => 'OR',
                          '134' => 'FPLDNLSNI',
                          '156' => 'FPTESTERR',
                          '59' => 'SB',
                          'MT_RESIZE' => 589,
                          'READBFR' => 189,
                          'FPLDALL' => -2,
                          'MOVE2DZERO' => 94,
                          'TIN' => 43,
                          'FPGE' => 151,
                          '219' => 'FPABS',
                          'MT_CLONE' => 570,
                          '13' => 'STARTP',
                          '105' => 'SATSUB',
                          'TESTLDD' => 37,
                          'READHDR' => -28,
                          '3' => 'ENDP',
                          '185' => 'LBX',
                          '248' => 'XSWORD',
                          '111' => 'STTRAPH',
                          '146' => 'FPORDERED',
                          '-31' => 'GRANT',
                          'STTRAPH' => 111,
                          '38' => 'TESTSTS',
                          'CRCWORD' => 116,
                          '4' => 'DIFF',
                          'WAIT' => -11,
                          '164' => 'FPREV',
                          '196' => 'INTDIS',
                          '-1' => 'FPSTALL',
                          '242' => 'DEVLS'
                        },
           'tvm_ins' => [
                          [
                            'PROC_ALLOC',
                            559
                          ],
                          [
                            'PROC_PARAM',
                            560
                          ],
                          [
                            'PROC_MT_COPY',
                            561
                          ],
                          [
                            'PROC_MT_MOVE',
                            562
                          ],
                          [
                            'PROC_START',
                            563
                          ],
                          [
                            'PROC_END',
                            564
                          ],
                          [
                            'GETAFF',
                            565
                          ],
                          [
                            'SETAFF',
                            566
                          ],
                          [
                            'GETPAS',
                            567
                          ],
                          [
                            'DISC',
                            47
                          ],
                          [
                            'DIST',
                            46
                          ],
                          [
                            'DISS',
                            48
                          ],
                          [
                            'ALT',
                            67
                          ],
                          [
                            'ALTWT',
                            68
                          ],
                          [
                            'ALTEND',
                            69
                          ],
                          [
                            'ENBT',
                            71
                          ],
                          [
                            'ENBC',
                            72
                          ],
                          [
                            'ENBS',
                            73
                          ],
                          [
                            'TALT',
                            78
                          ],
                          [
                            'TALTWT',
                            81
                          ],
                          [
                            'MB',
                            584
                          ],
                          [
                            'RMB',
                            585
                          ],
                          [
                            'WMB',
                            586
                          ],
                          [
                            'LDTIMER',
                            34
                          ],
                          [
                            'TIN',
                            43
                          ],
                          [
                            'UNPACKSN',
                            99
                          ],
                          [
                            'POSTNORMSN',
                            108
                          ],
                          [
                            'ROUNDSN',
                            109
                          ],
                          [
                            'LDINF',
                            113
                          ],
                          [
                            'FMUL',
                            114
                          ],
                          [
                            'FPLDNLDBI',
                            130
                          ],
                          [
                            'FPCHECKERR',
                            131
                          ],
                          [
                            'FPSTNLDB',
                            132
                          ],
                          [
                            'FPLDNLSNI',
                            134
                          ],
                          [
                            'FPADD',
                            135
                          ],
                          [
                            'FPSTNLSN',
                            136
                          ],
                          [
                            'FPSUB',
                            137
                          ],
                          [
                            'FPLDNLDB',
                            138
                          ],
                          [
                            'FPMUL',
                            139
                          ],
                          [
                            'FPDIV',
                            140
                          ],
                          [
                            'FPLDNLSN',
                            142
                          ],
                          [
                            'FPNAN',
                            145
                          ],
                          [
                            'FPORDERED',
                            146
                          ],
                          [
                            'FPNOTFINITE',
                            147
                          ],
                          [
                            'FPGT',
                            148
                          ],
                          [
                            'FPEQ',
                            149
                          ],
                          [
                            'FPI32TOR32',
                            150
                          ],
                          [
                            'FPI32TOR64',
                            152
                          ],
                          [
                            'FPB32TOR64',
                            154
                          ],
                          [
                            'FPRTOI32',
                            157
                          ],
                          [
                            'FPSTNLI32',
                            158
                          ],
                          [
                            'FPLDZERODB',
                            160
                          ],
                          [
                            'FPINT',
                            161
                          ],
                          [
                            'FPDUP',
                            163
                          ],
                          [
                            'FPREV',
                            164
                          ],
                          [
                            'FPLDNLADDDB',
                            166
                          ],
                          [
                            'FPLDNLMULDB',
                            168
                          ],
                          [
                            'FPLDNLADDSN',
                            170
                          ],
                          [
                            'FPLDNLMULSN',
                            172
                          ],
                          [
                            'FPREM',
                            207
                          ],
                          [
                            'I64TOREAL',
                            208
                          ],
                          [
                            'FPDIVBY2',
                            209
                          ],
                          [
                            'FPMULBY2',
                            210
                          ],
                          [
                            'FPSQRT',
                            211
                          ],
                          [
                            'FPRZ',
                            214
                          ],
                          [
                            'FPR32TOR64',
                            7
                          ],
                          [
                            'FPR64TOR32',
                            8
                          ],
                          [
                            'FPEXPDEC32',
                            217
                          ],
                          [
                            'FPABS',
                            219
                          ],
                          [
                            'FPCHKI64',
                            223
                          ],
                          [
                            'XBWORD',
                            184
                          ],
                          [
                            'LBX',
                            185
                          ],
                          [
                            'CB',
                            186
                          ],
                          [
                            'CBU',
                            187
                          ],
                          [
                            'SSUB',
                            193
                          ],
                          [
                            'CIR',
                            199
                          ],
                          [
                            'SS',
                            200
                          ],
                          [
                            'LS',
                            202
                          ],
                          [
                            'CIRU',
                            204
                          ],
                          [
                            'XSWORD',
                            248
                          ],
                          [
                            'LSX',
                            249
                          ],
                          [
                            'CS',
                            250
                          ],
                          [
                            'CSU',
                            251
                          ],
                          [
                            'REV',
                            0
                          ],
                          [
                            'LB',
                            1
                          ],
                          [
                            'BSUB',
                            2
                          ],
                          [
                            'ENDP',
                            3
                          ],
                          [
                            'DIFF',
                            4
                          ],
                          [
                            'ADD',
                            5
                          ],
                          [
                            'GCALL',
                            6
                          ],
                          [
                            'PROD',
                            8
                          ],
                          [
                            'GT',
                            9
                          ],
                          [
                            'WSUB',
                            10
                          ],
                          [
                            'SUB',
                            12
                          ],
                          [
                            'STARTP',
                            13
                          ],
                          [
                            'SETERR',
                            16
                          ],
                          [
                            'CSUB0',
                            19
                          ],
                          [
                            'STOPP',
                            21
                          ],
                          [
                            'LADD',
                            22
                          ],
                          [
                            'NORM',
                            25
                          ],
                          [
                            'LDPI',
                            27
                          ],
                          [
                            'XDBLE',
                            29
                          ],
                          [
                            'REM',
                            31
                          ],
                          [
                            'RET',
                            32
                          ],
                          [
                            'LEND',
                            33
                          ],
                          [
                            'DIV',
                            44
                          ],
                          [
                            'LMUL',
                            49
                          ],
                          [
                            'NOT',
                            50
                          ],
                          [
                            'XOR',
                            51
                          ],
                          [
                            'LSHR',
                            53
                          ],
                          [
                            'LSHL',
                            54
                          ],
                          [
                            'LSUM',
                            55
                          ],
                          [
                            'LSUB',
                            56
                          ],
                          [
                            'RUNP',
                            57
                          ],
                          [
                            'SB',
                            59
                          ],
                          [
                            'GAJW',
                            60
                          ],
                          [
                            'SHR',
                            64
                          ],
                          [
                            'SHL',
                            65
                          ],
                          [
                            'MINT',
                            66
                          ],
                          [
                            'AND',
                            70
                          ],
                          [
                            'MOVE',
                            74
                          ],
                          [
                            'OR',
                            75
                          ],
                          [
                            'CSNGL',
                            76
                          ],
                          [
                            'CCNT1',
                            77
                          ],
                          [
                            'LDIFF',
                            79
                          ],
                          [
                            'SUM',
                            82
                          ],
                          [
                            'MUL',
                            83
                          ],
                          [
                            'STOPERR',
                            85
                          ],
                          [
                            'CWORD',
                            86
                          ],
                          [
                            'POP',
                            121
                          ],
                          [
                            'SHUTDOWN',
                            254
                          ],
                          [
                            'DUP',
                            90
                          ],
                          [
                            'WSUBDB',
                            129
                          ],
                          [
                            'IN',
                            7
                          ],
                          [
                            'OUT',
                            11
                          ],
                          [
                            'OUTBYTE',
                            14
                          ],
                          [
                            'OUTWORD',
                            15
                          ],
                          [
                            'BOOLINVERT',
                            35
                          ],
                          [
                            'WIDENSHORT',
                            36
                          ],
                          [
                            'FFICALL',
                            37
                          ],
                          [
                            'LEND3',
                            38
                          ],
                          [
                            'LENDB',
                            39
                          ],
                          [
                            'RESCHEDULE',
                            40
                          ],
                          [
                            'EXTVRFY',
                            20
                          ],
                          [
                            'EXTIN',
                            96
                          ],
                          [
                            'EXTOUT',
                            97
                          ],
                          [
                            'EXT_MT_IN',
                            587
                          ],
                          [
                            'EXT_MT_OUT',
                            588
                          ],
                          [
                            'GETPRI',
                            162
                          ],
                          [
                            'SETPRI',
                            165
                          ],
                          [
                            'SAVECREG',
                            173
                          ],
                          [
                            'RESTORECREG',
                            174
                          ],
                          [
                            'SEMINIT',
                            122
                          ],
                          [
                            'SEMCLAIM',
                            123
                          ],
                          [
                            'SEMRELEASE',
                            124
                          ],
                          [
                            'CHECKNOTNULL',
                            224
                          ],
                          [
                            'XABLE',
                            232
                          ],
                          [
                            'XIN',
                            233
                          ],
                          [
                            'XEND',
                            236
                          ],
                          [
                            'NULL',
                            253
                          ],
                          [
                            'MRELEASEP',
                            17
                          ],
                          [
                            'MALLOC',
                            226
                          ],
                          [
                            'MRELEASE',
                            227
                          ],
                          [
                            'MT_ALLOC',
                            568
                          ],
                          [
                            'MT_RELEASE',
                            569
                          ],
                          [
                            'MT_CLONE',
                            570
                          ],
                          [
                            'MT_IN',
                            571
                          ],
                          [
                            'MT_OUT',
                            572
                          ],
                          [
                            'MT_XCHG',
                            573
                          ],
                          [
                            'MT_LOCK',
                            574
                          ],
                          [
                            'MT_UNLOCK',
                            575
                          ],
                          [
                            'MT_ENROLL',
                            576
                          ],
                          [
                            'MT_RESIGN',
                            577
                          ],
                          [
                            'MT_SYNC',
                            578
                          ],
                          [
                            'MT_XIN',
                            579
                          ],
                          [
                            'MT_XOUT',
                            580
                          ],
                          [
                            'MT_XXCHG',
                            581
                          ],
                          [
                            'MT_DCLONE',
                            582
                          ],
                          [
                            'MT_BIND',
                            583
                          ]
                        ],
           'ins_pri' => {
                          '32' => 'PFIX',
                          '80' => 'LDNLP',
                          '144' => 'CALL',
                          'CALL' => 144,
                          '16' => 'LDLP',
                          'LDLP' => 16,
                          '240' => 'OPR',
                          '128' => 'ADC',
                          'EQC' => 192,
                          'J' => 0,
                          '192' => 'EQC',
                          'PFIX' => 32,
                          '112' => 'LDL',
                          '224' => 'STNL',
                          'LDNL' => 48,
                          'STNL' => 224,
                          '208' => 'STL',
                          'STL' => 208,
                          '48' => 'LDNL',
                          'CJ' => 160,
                          '0' => 'J',
                          '96' => 'NFIX',
                          'OPR' => 240,
                          '64' => 'LDC',
                          'ADC' => 128,
                          '160' => 'CJ',
                          'AJW' => 176,
                          'NFIX' => 96,
                          '176' => 'AJW',
                          'LDC' => 64,
                          'LDL' => 112,
                          'LDNLP' => 80
                        }
         };

sub get ($) {
	my $key = shift;
	return $STORE->{$key};
}


package Transputer::Instructions;

use strict;

sub new ($) {
	my ($class) = @_;

	my $self = {
		'pri'	=> PLinker::get ('ins_pri'),
		'sec'	=> PLinker::get ('ins_sec')
	};

	$self = bless $self, $class;

	return $self;
}

sub compile ($$) {
	my ($store, $fn) = @_;
	my $pri = {};
	my $sec = {};
	my $ins;
	my $fh;

	open ($fh, $fn) || die $!;
	while (my $line = <$fh>) {
		$ins = $pri if $line =~ /primary instructions/;
		$ins = $sec if $line =~ /secondary instructions/;
		next if $line !~ /^#define I_(.+?)\s+(.*)/;
		
		my ($name, $value) = ($1, $2);
		my $v;
		
		$value =~ s/\/\/.*//;
		$value =~ s/\/\*.*\*\///;
		$value =~ s/\s+$//;
		
		if ($value =~ /^0x[0-9a-f]+$/i) {
			$v = hex ($value);
		} elsif ($value =~ /\(\s*0x([0-9a-f]+)\s*\|\s*I_([0-9a-z]+)\s*\)/i) {
			my ($value, $shift)	= ($1, $2);
			$v = hex ($value);
			if ($shift eq 'NEGATIVE') {
				$v = -$v;
			} else {
				$v |= $ins->{$shift};
			}
		}

		$ins->{$name}	= $v;
		$ins->{$v}	= $name;
	}
	close ($fh);

	$store->{'ins_pri'} = $pri;
	$store->{'ins_sec'} = $sec;
}

sub valid_primary ($$) {
	my ($self, $pri) = @_;
	return 1 if exists ($self->{'pri'}->{$pri});
}

sub valid_secondary ($$) {
	my ($self, $sec) = @_;
	return 1 if exists ($self->{'sec'}->{$sec});
}

sub valid_instruction ($$) {
	my ($self, $n) = @_;
	return ($self->valid_primary ($n) || $self->valid_secondary ($n));
}

sub primary ($$) {
	my ($self, $key) = @_;
	return $self->{'pri'}->{$key};
}

sub secondary ($$) {
	my ($self, $key) = @_;
	return $self->{'sec'}->{$key};
}

sub numeric ($$) {
	my ($self, $name) = @_;
	my $num = $self->{'pri'}->{$name} || $self->{'sec'}->{$name} || undef;
	return $num if $num =~ /^\d+$/;
}

package Transterpreter::Instructions;

use strict;

@Transterpreter::Instructions::ISA = qw(Transputer::Instructions);

sub new ($) {
	my ($class) = @_;
	my $self = new Transputer::Instructions ();
	$self = bless $self, $class;
	$self->load_tvm_instructions ();
	return $self;
}

sub load_tvm_instructions ($) {
	my ($self) = @_;
	my $data = PLinker::get ('tvm_ins');

	$self->{'tvm'} = {};

	foreach my $tuple (@$data) {
		my ($name, $number) = @$tuple;
		if (!$self->valid_instruction ($name)) {
			$self->{'tvm'}->{$number}	= $name;
			$self->{'tvm'}->{$name}		= $number;
		}
	}
}

sub compile ($$) {
	my ($store, $dir) = @_;
	my @data;
	my $dh;

	if (!opendir ($dh, $dir)) {
		print STDERR "Unable to open directory $dir: $!\n";
		return;
	}

	while (my $fn = readdir ($dh)) {
		next if $fn =~ /^\./;
		load_tvm_header (\@data, "$dir/$fn") if $fn =~ /\.h$/;
	}

	closedir ($dh);

	$store->{'tvm_ins'} = \@data;
}

sub load_tvm_header ($$) {
	my ($data, $fn) = @_;
	my $fh;

	open ($fh, $fn) || return;
	while (my $line = <$fh>) {
		next if $line !~ /^\/\*\s+0x([0-9a-f]+)\s+-[x \t0-9a-f]+-\s+(\S+).*\*\//i;
		my ($number, $name) = ($1, $2);
		$number = hex ($number);
		$name =~ tr/a-z/A-Z/;
		$name =~ s/^INS_//;
		push (@$data, [ $name, $number ]);
	}
	close ($fh);
}

sub valid_instruction ($$) {
	my ($self, $n) = @_;
	return (
		$self->valid_primary ($n)	|| 
		$self->valid_secondary ($n)	||
		exists ($self->{'tvm'}->{$n})
	);
}

sub numeric ($$) {
	my ($self, $name) = @_;
	my $num =
		$self->{'pri'}->{$name} || 
		$self->{'sec'}->{$name} || 
		$self->{'tvm'}->{$name} ||
		undef;

	return $num if $num =~ /^\d+$/;
}

#
#  Perl code for linking and assembling bytecode for the TVM
#  Copyright (C) 2008 Carl Ritson <cgr@kent.ac.uk>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#


package Transputer::TCOFF;

use strict;

sub new ($$) {
	my ($class, $tcoff_h) = @_;

	my $self	= {
		'tcoff'	=> PLinker::get ('tcoff')
	};
	
	$self = bless $self, $class;

	return $self;
}

sub read_int ($) {
	my $fh = shift;
	my $b;

	read ($fh, $b, 1) || return;
	$b = unpack ('C', $b);

	if ($b > 250 && $b <= 255) {
		my ($len, $n);

		if ($b == 255) {
			$len = 4;
		} else {
			$len = (1 << ($b - 251));
		}

		die "Invalid length $len" if $len > 4;

		read ($fh, $n, $len) || return;
		
		if ($len == 1) {
			$n = unpack ('C', $n);
		} elsif ($len == 2) {
			$n = unpack ('v', $n);
		} else {
			$n = unpack ('V', $n);
		}
		
		$n ^= $n if $b == 255;

		return $n;
	} else {
		return $b;
	}
}

sub decode_int {
	my ($self, $data);
	
	if (@_ > 1) {
		($self, $data) = @_;
	} else {
		($data) = @_;
	}

	my $b = unpack ('C', substr ($data, 0, 1));

	if ($b > 250 && $b <= 255) {
		my ($len, $n);

		if ($b == 255) {
			$len = 4;
		} else {
			$len = (1 << ($b - 251));
		}

		die "Invalid length $len" if $len > 4;

		my $n = substr ($data, 1, $len);
		
		if ($len == 1) {
			$n = unpack ('C', $n);
		} elsif ($len == 2) {
			$n = unpack ('v', $n);
		} else {
			$n = unpack ('V', $n);
		}
		
		$n ^= $n if $b == 255;

		return ($n, substr ($data, $len + 1));
	} else {
		return ($b, substr ($data, 1));
	}
}

sub read_file ($$) {
	my ($self, $filename)	= @_;
	my $tcoff		= $self->{'tcoff'};
	
	my $fh;
	my %sections;
	my @sections;

	if (!open ($fh, $filename)) {
		print STDERR "Unable to open $filename: $!\n";
		return;
	}

	binmode ($fh);
	while (!eof ($fh)) {
		my $r_tag	= read_int ($fh);
		my $r_len	= read_int ($fh);
		my $tag		= $tcoff->{$r_tag} || $r_tag;
		my $data;
		
		if ($r_len && !read ($fh, $data, $r_len)) {
			return;
		}
		
		my $section	= {
			'tag' 		=> $tag,
			'length'	=> $r_len,
			'data'		=> $data
		};

		$sections{$tag} = [] if !$sections{$tag};
		push (@{$sections{$tag}}, $section);
		push (@sections, $section);
	}
	close ($fh);

	$sections{'sections'} = \@sections;
	
	$self->decode_comments (\%sections);
	$self->decode_symbols (\%sections);

	return \%sections;
}

sub decode_symbols ($$) {
	my ($self, $file) 	= @_;
	my $tcoff		= $self->{'tcoff'};
	my $symbol_id		= 0;
	my %symbols;

	foreach my $section (@{$file->{'sections'}}) {
		my $tag		= $section->{'tag'};
		my $data	= $section->{'data'};

		if ($tag eq 'SECTION' || $tag eq 'SYMBOL' || $tag eq 'SPECIFIC_SYMBOL') {
			my ($type, $scope, $len, $str, $origin_id);

			($type, $data)	= decode_int ($data)
				if $tag eq 'SECTION';

			($scope, $data) = decode_int ($data);
			($len, $data)	= decode_int ($data);

			$str		= substr ($data, 0, $len);
			$data		= substr ($data, $len);
			
			($origin_id, $data) = decode_int ($data)
				if $tag eq 'SPECIFIC_SYMBOL';

			my $sym = {
				'type'		=> $type,
				'scope'		=> $scope, 
				'string'	=> $str,
				'origin_id'	=> $origin_id
			};
			$symbols{'@' . $symbol_id++}	= $sym;
			$symbols{$str}			= $sym;
		} elsif ($tag eq 'DESCRIPTOR') {
			my ($id, $data) = decode_int ($data);

			$id = '@' . $id;
			
			if (exists ($symbols{$id}) && $symbols{$id}->{'scope'} == ($tcoff->{'EXPORT_USAGE'} | $tcoff->{'ROUTINE_USAGE'})) {
				my $symbol	= $symbols{$id};
				my ($lang, $p_len, $ws, $vs, $ms);

				($lang, $data)	= decode_int ($data);
				($p_len, $data)	= decode_int ($data);
				($ws, $data)	= decode_int ($data);
				($vs, $data)	= decode_int ($data);
				($ms, $data)	= decode_int ($data);

				$symbol->{'definition'} = $data;
				$symbol->{'ws'}		= $ws;
				$symbol->{'vs'}		= $vs;
				$symbol->{'ms'}		= $ms;
			}
		}
	}

	$file->{'symbols'} = \%symbols;
}

sub decode_comments ($$) {
	my ($self, $file) 	= @_;
	my $tcoff		= $self->{'tcoff'};
	
	foreach my $comment (@{$file->{'COMMENT'}}) {
		my ($copy, $print, $s_len);
		my $data	= $comment->{'data'};

		($copy, $data)	= decode_int ($data);
		($print, $data)	= decode_int ($data);
		($s_len, $data) = decode_int ($data);

		if ($print && $data =~ /^(\.[A-Z0-9\.]+)\s*(.+)$/) {
			my ($cmd, $data) = ($1, $2);
			$file->{$cmd} = [] if !$file->{$cmd};
			push (@{$file->{$cmd}}, $data);
		}
	}
}

sub compile ($$) {
	my ($store, $fn) = @_;
	my $tcoff = {};
	my $fh;

	open ($fh, $fn) || die $!;
	while (my $line = <$fh>) {
		if ($line =~ /^#define (.+?)_TAG\s+(\d+)/) {
			my ($name, $value) = ($1, $2);
			
			$tcoff->{$name}	 = $value;
			$tcoff->{$value} = $name if !exists ($tcoff->{$value});
		} elsif ($line =~ /^#define (.+?)\s+(\d+|0x[0-9a-f]+)\s*$/) {
			my ($name, $value) = ($1, $2);
			$value = hex ($value) if $value =~ /^0x/;

			$tcoff->{$name}	 = $value;
			$tcoff->{$value} = $name if !exists ($tcoff->{$value});
		}
	}
	close ($fh);

	$store->{'tcoff'} = $tcoff;
}


package Transputer::ETC;

use strict;

sub new ($$) {
	my ($class, $instruct_h) = @_;

	my $self	= {
		'instr'	=> new Transputer::Instructions ($instruct_h),
	};
	
	$self = bless $self, $class;

	$self->define_constants ();

	return $self;
}

sub decode_fn_opd ($$$) {
	my ($self, $data, $pos) = @_;
	my $instr		= $self->{'instr'};
	my $PFIX 		= $instr->primary ('PFIX');
	my $NFIX 		= $instr->primary ('NFIX');
	my ($fn, $opd) 		= (0, 0);
	
	while ($pos < @$data) {
		my $b	= unpack ('C', $data->[$pos++]);
		$fn	= ($b & 0xf0);
		$opd 	= $opd | ($b & 0x0f);
		
		if ($fn == $PFIX) {
			$opd <<= 4;
		} elsif ($fn == $NFIX) {
			$opd = ((~$opd) << 4) & 0xffffffff;
		} else {
			return ($fn, $opd, $pos);
		}
	}

	return (undef, undef, undef);
}

sub fix_sign ($) {
	my $val = shift;
	$val = $val - (2 ** 32) if $val >= (2 ** 31);
	return $val;
}

sub decode ($$) {
	my ($self, $data)	= @_;
	my @data		= split (//, $data);
	my $instr		= $self->{'instr'};
	my $LDC			= $instr->primary ('LDC');
	my $OPR			= $instr->primary ('OPR');
	my $pos			= 0;
	my @text;

	while ($pos < @data) {
		my ($fn, $opd);
		
		($fn, $opd, $pos) = $self->decode_fn_opd (\@data, $pos);
		return if !defined ($pos);

		if ($fn == $OPR) {
			if ($instr->valid_secondary ($opd)) {
				push (@text, { 
					'name' => $instr->secondary ($opd)
				});
			} elsif (exists ($self->{$opd})) {
				my $tag		= $self->{$opd};
				my @params	= split (//, $tag->{'p'});

				for (my $i = 0; $i < @params; ++$i) {
					my $p = $params[$i];
					my ($fn, $opd);
				
					($fn, $opd, $pos) = $self->decode_fn_opd (\@data, $pos);
					return if !defined ($pos);

					if ($p =~ /^[bcdlsx]$/ && $fn != $LDC) {
						die "Invalid operation while loading constant: $fn $opd";
					}

					if ($p =~ /^[bs]$/) {
						return if $pos + $opd >= @data;
						$p	= join ('', @data[$pos..($pos + $opd - 1)]);
						$pos	= $pos + $opd;
					} elsif ($p eq 'l') {
						$p = "L$opd";
					} elsif ($p eq 'c') {
						$p = $opd;
					} elsif ($p eq 'x') {
						die "Unknown special $opd" if !exists ($tag->{'specials'}->{$opd});
						$p = { 'name' => $tag->{'specials'}->{$opd} };
					} else {
						my $name = $instr->primary ($fn);
						if ($tag->{'name'} eq 'LABEL' && (@params == 1)) {
							if ($name eq 'LDC') {
								push (@params, 'i');
							}	
							$opd = "L$opd";
						} else {
							$opd = fix_sign ($opd);
						}
						$p = { 
							'name'	=> $name, 
							'arg'	=> $opd
						};
					}

					$params[$i] = $p;
				}
				
				push (@text, {
					'name' 	=> '.' . $tag->{'name'},
					'arg'	=>
						(@params <= 1	? 
						 $params[0]	:
						 \@params)
				});
			} else {
				die "Unknown operation operand $opd";
			}
		} else {
			push (@text, {
				'name'	=> $instr->primary ($fn),
				'arg'	=> fix_sign ($opd)
			});
		}
	}

	return @text;
}

sub decode_str ($$) {
	my ($self, $data) = @_;
	my ($code, $str_len);
	$code = substr ($data, 0, 2);
	$data = substr ($data, 2);
	($str_len, $data) = Transputer::TCOFF::decode_int ($data);
	return substr ($data, 0, $str_len);
}

sub decode_load_text ($$) {
	my ($self, $data) = @_;
	my $code_len;
	($code_len, $data) = Transputer::TCOFF::decode_int ($data);
	return $self->decode (substr ($data, 0, $code_len));
}

sub define_constants ($) {
	my $self = shift;
	my %tags = (
		# Annotations
		'SPECIAL'	=> { 'i' => 0x00, 'p' => 'x' },
		'TSDEPTH'	=> { 'i' => 0x01 },
		'FUNCRESULTS'	=> { 'i' => 0x02 },
		'FUNCRETURN'	=> { 'i' => 0x03 },
		'ENDWS'		=> { 'i' => 0x04 },
		'REALRESULT'	=> { 'i' => 0x05 },
		'SETLAB'	=> { 'i' => 0x06 },
		'SECTIONLAB'	=> { 'i' => 0x07 },
		'ALIGN'		=> { 'i' => 0x08 },
		'LINE'		=> { 'i' => 0x09 },
		'DEBUGLINE'	=> { 'i' => 0x0a },
		'SETWS'		=> { 'i' => 0x0b },
		'SETVS'		=> { 'i' => 0x0c },
		'SLLIMM'	=> { 'i' => 0x0d, 'p' => 'c' },
		'SLRIMM'	=> { 'i' => 0x0e, 'p' => 'c' },
		'LOOPHEADTOP'	=> { 'i' => 0x0f },
		# Strings
		'STUBNAME'	=> { 'i' => 0x11, 'p' => 's' },
		'GLOBAL'	=> { 'i' => 0x12, 'p' => 's' },
		'JUMPENTRY'	=> { 'i' => 0x13, 'p' => 's' },
		'PROC'		=> { 'i' => 0x14, 'p' => 's' },
		'FILENAME'	=> { 'i' => 0x15, 'p' => 's' },
		'OCCCOMMENT'	=> { 'i' => 0x16, 'p' => 's' },
		'CODEMAP'	=> { 'i' => 0x17, 'p' => 's' },
		'DATABYTES'	=> { 'i' => 0x18, 'p' => 'b' },
		'MESSAGEBYTES'	=> { 'i' => 0x19, 'p' => 'b' },
		'LOADLABELNAME'	=> { 'i' => 0x1a, 'p' => 's' },
		'LOADCODEMAPNAME'=> { 'i' => 0x1b, 'p' => 's' },
		'GLOBALEND'	=> { 'i' => 0x1c, 'p' => 's' },
		# Labels
		'LABEL'		=> { 'i' => 0x20, 'p' => 'i' },
		'LEND'		=> { 'i' => 0x21, 'p' => 'pll' },
		'LEND3'		=> { 'i' => 0x22, 'p' => 'pll' },
		'LENDB'		=> { 'i' => 0x23, 'p' => 'pll' },
		'MS_USAGE'	=> { 'i' => 0x24 },
		'MS_INIT'	=> { 'i' => 0x25 },
		'TCOFF_RECORD'	=> { 'i' => 0x26 },
		'LOADWSMAP'	=> { 'i' => 0x27 },
		'UNLOADWSMAP'	=> { 'i' => 0x28 },
	);
	my %specials = (
		'BOOLINVERT'	=> 1,
		'STARTTABLE'	=> 2,
		'WIDENSHORT'	=> 3,
		'LOOPHEADBOT'	=> 4,
		'CONTRSPLIT'	=> 5,
		'CONTRJOIN'	=> 6,
		'I64TOREAL'	=> 7,
		'NOTPROCESS'	=> 8,
		'FPPOP'		=> 9,
		'CHECKNOTNULL'	=> 10,
		'SEMCLAIM'	=> 11,
		'SEMRELEASE'	=> 12,
		'SEMINIT'	=> 13,
		'RESCHEDULE'	=> 14,
		'INDIRECT_AREG'	=> 15,
		'INDIRECT_BREG'	=> 16,
		'INDIRECT_CREG'	=> 17,
		'RMWSMAP'	=> 18,
		'MPPCLONE'	=> 19,
		'MPPSERIALISE'	=> 20,
		'MPPDESERIALISE'=> 21,
		'LOADCODEMAP'	=> 22,
		'R32SIN'	=> 30,
		'R64SIN'	=> 31,
		'R32COS'	=> 32,
		'R64COS'	=> 33,
		'DTRACE'	=> 34,
		'KILLCALL'	=> 35,
		'WAIT_FOR_INTERRUPT' => 36,
		'R32TAN'	=> 37,
		'R64TAN'	=> 38
	);

	foreach my $key (keys (%specials)) {
		$specials{$specials{$key}} = $key;
	}

	$tags{'SPECIAL'}->{'specials'} = \%specials;

	foreach my $key (keys (%tags)) {
		my $val = $tags{$key};
		my $opd = $val->{'i'} | 0xffffff00;
		$val->{'name'}	= $key;
		$val->{'opd'}	= $opd;
		$val->{'p'}	= 'c' if !exists ($val->{'p'});
		$self->{$key}	= $val;
		$self->{$opd}	= $val;
	}
}

#
#  Perl code for linking and assembling bytecode for the TVM
#  Copyright (C) 2008 Carl Ritson <cgr@kent.ac.uk>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#


package Transterpreter::Linker;

use strict;
use Data::Dumper;

sub new ($) {
	my ($class, $instruct_h) = @_;

	my $self = {
		'instructions' => new Transterpreter::Instructions ()
	};
	
	$self = bless $self, $class;

	return $self;
}

sub link ($$@) {
	my ($self, $entry_point, @etc)	= @_;
	my $instructions		= $self->{'instructions'};

	# State
	my %ffi;
	my %globals;
	my $n = 0;

	# Tranform ETC
	foreach my $texts (@etc) {
		my @data;
		
		foreach my $text (@$texts) {
			my ($current, %labels, @procs);
			my $align	= 0;
			my $etc		= $text->{'etc'};
			my $file	= $text->{'file'};
			my $filename	= undef;
			my $line	= undef;
			my $global	= undef; # present global
			my $n_inst	= 0;     # "real" instructions in present/last global

			# Initial operation translation
			expand_etc_ops ($etc, $instructions);

			# Build ETC stream for each label
			# Identify PROCs and global symbols
			# Carry alignment
			# Carry file names and line numbers
			foreach my $op (@$etc) {
				my $name	= $op->{'name'};
				my $arg		= $op->{'arg'};

				if ($name eq '.ALIGN') {
					$align	= $arg;
				} elsif ($name =~ /^\.(SET|SECTION)LAB$/) {
					my $label = 'L' . $n . ':' . $arg;
					my @inst;

					die "Label collision $label" 
						if exists ($labels{$label});
					
					if ($filename) {
						push (@inst, { 
							'name'	=> '.FILENAME',
							'arg'	=> $filename
						});
					}
					if (defined ($line)) {
						push (@inst, {
							'name'	=> '.LINE',
							'arg'	=> $line
						});
					}
					if ($align) {
						push (@inst, {
							'name'	=> '.ALIGN',
							'arg'	=> $align
						});
					}

					my $new = { 
						'name'		=> $label, 
						'prev'		=> $current, 
						'inst'		=> \@inst,
						'align'		=> $align,
						'source'	=> $etc
					};

					$current->{'next'} 	= $new;
					$current 		= $new;
					$labels{$label}		= $new;

					$align			= 0;
					$n_inst			= 0;
				} elsif ($name eq '.FILENAME') {
					$filename		= $arg;
				} elsif ($name eq '.LINE') {
					$line			= $arg;
				} elsif ($name eq '.PROC') {
					$current->{'symbol'}	= $arg;
					push (@procs, $current);
				} elsif ($name eq '.STUBNAME') {
					$current->{'stub'}	= $arg;
					$current->{'symbol'}	= $arg;
					if ($arg =~ /^(C|BX?)\./) {
						$ffi{$arg} = $current
							if !exists ($ffi{$arg});
					}
				} elsif ($name eq '.GLOBAL') {
					if (exists ($globals{$arg})) {
						my $current 	= $globals{$arg};
						my $c_file	= $current->{'loci'}->{'file'};
						my $c_fn	= $current->{'loci'}->{'filename'};
						my $c_ln	= $current->{'loci'}->{'line'};
						print STDERR 
							"Warning: multiple definitions of global name '$arg'\n",
							"\tOld symbol is from $c_fn($c_file), line $c_ln.";
							"\tNew symbol is from $filename($file), line $line.";
					}
					$n_inst			= 0;
					$global			= $arg;
					$globals{$arg}		= $current;
					$current->{'loci'}	= {
						'file'		=> $file,
						'filename'	=> $filename,
						'line'		=> $line
					};
				} elsif ($name eq '.GLOBALEND') {
					$globals{$arg}->{'end'}	= $current;
					$global			= undef;
				} elsif ($name eq '.JUMPENTRY') {
					if ($global && !$n_inst) {
						$globals{$global} = {
							'jump_entry' => $arg
						}
					};
				} elsif ($name !~ /^\./) {
					$n_inst++;
				}
				
				push (@{$current->{'inst'}}, $op)
					if $current; 
			}

			# Resolve local labels
			foreach_label (\%labels, \&resolve_labels, $n);
			
			# Queue data for other passes
			push (@data, { 
				'file' 		=> $file, 
				'filename' 	=> $filename, 
				'labels' 	=> \%labels, 
				'procs' 	=> \@procs
			});
			$n++;
		}

		foreach my $d (@data) {
			my $labels 	= $d->{'labels'};
			my $procs	= $d->{'procs'};
			
			# Transform Passes
			foreach_label ($labels, \&resolve_globals, \%globals, \%ffi);
			foreach_label ($labels, \&follow_jump_entries, \%globals, \%ffi);
			foreach_label ($labels, \&build_data_blocks);
			foreach_label ($labels, \&add_data_lengths);
			foreach_label ($labels, \&isolate_static_sections);
			tag_and_index_code_blocks ($procs);
			separate_code_blocks ($procs);
			foreach_label ($labels, \&build_proc_dependencies);
		}
	}

	if (!exists ($globals{$entry_point})) {
		print STDERR "Unable to find entry point symbol $entry_point\n";
		return;
	}
	
	# The entry point might just be a jump entry
	while (exists ($globals{$entry_point}->{'jump_entry'})) {
		$entry_point = $globals{$entry_point}->{'jump_entry'};
	}
	
	# Code from entry point down through dependencies
	my @coding_order;
	build_coding_order ($globals{$entry_point}, \@coding_order);
	generate_code (\@coding_order, $instructions);
	
	# Add jump entry and shift labels
	my $jump = $self->jump_entry ($globals{$entry_point});
	@coding_order = expand_coding_order (@coding_order);
	shift_labels ($jump->{'length'}, @coding_order);

	return ($jump, @coding_order);
}

sub count_nybbles ($) {
	my $value = shift;
	my $count = 1;
	while ($value >>= 4) {
		$count++;
	}
	return $count;
}

sub foreach_label ($$@) {
	my ($labels, $func, @param) = @_;
	my @labels = keys (%$labels);
	foreach my $label (@labels) {
		&$func ($labels, $labels->{$label}, @param);
	}
}

sub foreach_inst ($$$@) {
	my ($labels, $label, $func, @param) = @_;
	my $inst = $label->{'inst'};
	foreach my $inst (@$inst) {
		&$func ($labels, $label, $inst, @param);
	}
}

sub expand_etc_ops ($$) {
	my ($etc, $instructions) = @_;
	my %IGNORE_SPECIAL = (
		'CONTRJOIN'	=> 1,
		'CONTRSPLIT'	=> 1,
		'FPPOP'		=> 1
	);
	my ($labn, $labo) = (0, 0);
	
	for (my $i = 0; $i < @$etc; ++$i) {
		my $op		= $etc->[$i];
		my $name	= $op->{'name'};
		my $arg		= $op->{'arg'};
		if ($name =~ /^\.(SET|SECTION)LAB$/) {
			$labn = $arg;
			$labo = 0;
		} elsif ($name eq '.LABEL') {
			if (ref ($arg) =~ /^ARRAY/) {
				my $l1 = $arg->[0];
				my $l2 = $arg->[1];
				if ($l2->{'arg'} >= 0) {
					$l1->{'arg'}	= [ $l1->{'arg'}, 'L' . $l2->{'arg'} ];
					$etc->[$i]	= $l1;
				} else {
					$l1->{'arg'} 	= [ $l1->{'arg'}, 'LDPI' ];
					splice (@$etc, $i, 1, 
						$l1,
						{ 'name' => 'LDPI' }
					);
				}
			} else {
				$etc->[$i] = $arg;
			}
		} elsif ($name eq '.SPECIAL') {
			my $name	= $arg->{'name'};

			if ($name eq 'NOTPROCESS') {
				$op = { 
					'name'	=> 'LDC',
					'arg'	=> 0
				};
			} elsif ($name eq 'STARTTABLE') {
				my (@arg, $done, @table);
				for (my $j = ($i+1); !$done && $j < @$etc; ++$j) {
					my $op = $etc->[$j];
					if ($op->{'name'} eq '.LABEL') {
						my $op = $op->{'arg'};
						if ($op->{'name'} eq 'J') {
							push (@arg, $op->{'arg'});
							push (@table, $op);
							next;
						}
					}
					$done = 1;
				}
				my $size_op		= {
					'name'		=> 'LDC',
					'arg'		=> 0
				};
				$op->{'name'}		= 'TABLE';
				$op->{'arg'}		= \@arg;
				$op->{'label_arg'}	= 1;
				$op->{'table'}		= \@table;
				$op->{'size_op'}	= $size_op;
				my $mlab = $labn + (++$labo / 10.0);
				my $jlab = $labn + (++$labo / 10.0);
				splice (@$etc, $i, 1,
					{},
					$size_op,
					{ 'name' => 'PROD'	},
					{ 'name' => 'LDC', 'arg' => [ "L$jlab", "L$mlab" ] },
					{ 'name' => 'LDPI'	},
					{ 'name' => '.SETLAB', 'arg' => $mlab		},
					{ 'name' => 'BSUB'	},
					{ 'name' => 'GCALL'	},
					{ 'name' => '.SETLAB', 'arg' => $jlab		},
					$op
				);
			} elsif (!exists ($IGNORE_SPECIAL{$name})) {
				$op = $arg;
			}
			
			$etc->[$i] = $op;
		} elsif ($name =~ /^\.(LEND.?)$/) {
			my $name	= $1;
			my @arg		= @$arg;
			my $start	= ($arg[2] =~ /^L(\d+)$/)[0];
			my $end		= ($arg[1] =~ /^L(\d+)$/)[0];
			splice (@$etc, $i, 1, 
				$arg[0],
				{ 'name' => 'LDC', 'arg' => [ "L$end", "L$start" ]	},
				{ 'name' => $name					},
				{ 'name' => '.SETLAB', 'arg' => $end			}
			);
		} elsif ($name =~ /^\.SL([RL])IMM$/) {
			$op->{'name'} = "SH$1";
			splice (@$etc, $i, 1,
				{ 'name' => 'SAVECREG'				},
				{ 'name' => 'LDC', 'arg' => $op->{'arg'}	},
				$op,
				{ 'name' => 'RESTORECREG'			}
			);
			delete ($op->{'arg'});
		}
	}
}

sub resolve_inst_label ($$$$) {
	my ($labels, $label, $inst, $fn) = @_;
	
	return if $inst->{'name'} =~ /^\..*BYTES$/;
	
	my $arg = $inst->{'arg'};
	foreach my $arg (ref ($arg) =~ /^ARRAY/ ? @$arg : $arg) {
		if ($arg =~ /^L([0-9_\.]+)$/) {
			my $num	= $1;
			my $n	= 'L' . $fn . ':' . $num;
			if (!exists ($labels->{$n})) {
				die "Undefined label $n";
			} else {
				if ($arg eq $inst->{'arg'}) {
					$inst->{'arg'} = $labels->{$n};
				} else {
					$arg = $labels->{$n};
				}
				$labels->{$n}->{'refs'}++;
			}
			$inst->{'label_arg'} = 1;
		}
	}
}

sub resolve_labels ($$$) {
	my ($labels, $label, $fn) = @_;
	foreach_inst ($labels, $label, \&resolve_inst_label, $fn);
}

sub resolve_inst_globals ($$$$$) {
	my ($labels, $label, $inst, $globals, $ffi) = @_;
	if ($inst->{'label_arg'}) {
		my $arg = $inst->{'arg'};
		if ((ref ($arg) =~ /^HASH/) && $arg->{'stub'}) {
			my $n = $arg->{'stub'};
			if (exists ($globals->{$n})) {
				$inst->{'arg'} = $globals->{$n};
			} elsif (exists ($ffi->{$n})) {
				$inst->{'arg'} = $ffi->{$n};
			} else {
				die "Undefined global reference $n";
			}
			$inst->{'arg'}->{'refs'}++;
		}
	}
}

sub resolve_globals ($$$$) {
	my ($labels, $label, $globals, $ffi) = @_;
	foreach_inst ($labels, $label, \&resolve_inst_globals, $globals, $ffi);
}

sub follow_jump_entry_inst ($$$$$) {
	my ($labels, $label, $inst, $globals, $ffi) = @_;
	if (ref ($inst->{'arg'}) =~ /^HASH/ && $inst->{'arg'}->{'jump_entry'}) {
		my $arg = $inst->{'arg'};
		while ($arg->{'jump_entry'}) {
			my $n = $arg->{'jump_entry'};
			if (exists ($globals->{$n})) {
				$arg = $globals->{$n};
			} elsif (exists ($ffi->{$n})) {
				$arg = $ffi->{$n};
			} else {
				die "Undefined global reference $n (in jump entry)";
			}
		}
		$inst->{'arg'} = $arg;
		$arg->{'refs'}++;
	}
}

sub follow_jump_entries ($$$$) {
	my ($labels, $label, $globals, $ffi) = @_;
	foreach_inst ($labels, $label, \&follow_jump_entry_inst, $globals, $ffi);
}

sub build_data_blocks ($$) {
	my ($labels, $label) = @_;
	my ($data, $inst) = (undef, 0);
	foreach my $op (@{$label->{'inst'}}) {
		my $name = $op->{'name'};
		if ($name =~ /^[^\.]/) {
			$inst++;
		} elsif ($name eq '.DATABYTES') {
			$data .= $op->{'arg'};
			$op->{'bytes'}	= [ split (//, $op->{'arg'}) ];
			$op->{'length'}	= length ($op->{'arg'});
		}
	}
	if (!$inst && $data) {
		$label->{'data'} = $data;
		if ($label->{'prev'}) {
			$label->{'prev'}->{'next'} = $label->{'next'};
		}
		if ($label->{'next'}) {
			$label->{'next'}->{'prev'} = $label->{'prev'};
		}
		delete ($label->{'prev'});
		delete ($label->{'next'});
	}
}

sub add_data_lengths ($$) {
	my ($labels, $label) = @_;
	if ($label->{'data'}) {
		$label->{'length'} = length ($label->{'data'});
	}
}

sub new_sub_label ($$$$) {
	my ($labels, $label, $current, $sub_idx) = @_;
	my $name		= sprintf ('%s_%d', $label->{'name'}, ($$sub_idx)++);
	my $new 		= {
		'name' => $name, 
		'prev' => $current,
		'next' => $current->{'next'},
		'inst' => undef
	};
	$current->{'next'}	= $new;
	if ($new->{'next'}) {
		$new->{'next'}->{'prev'} = $new;
	}
	$labels->{$name}	= $new;
	return $new;
}

sub isolate_static_sections ($$) {
	my ($labels, $label) = @_;
	
	return if $label->{'data'};

	my @inst	= @{$label->{'inst'}};
	my $sub_idx	= 0;
	my $current	= $label;
	my $cinst	= [];
	for (my $i = 0; $i < @inst; ++$i) {
		my $inst = $inst[$i];
		if ($inst->{'label_arg'}) {
			if (@$cinst > 0) {
				$current->{'inst'}	= $cinst;
				$current		= new_sub_label (
					$labels, $label, $current, \$sub_idx
				);
				$cinst			= [];
			}
			$current->{'inst'}		= [ $inst ];
			$current 			= new_sub_label (
				$labels, $label, $current, \$sub_idx
			) if $i < (@inst - 1);
		} else {
			push (@$cinst, $inst);
		}
	}
	if (@$cinst > 0) {
		$current->{'inst'} = $cinst;
	}
}

sub tag_and_index_code_blocks ($) {
	my ($procs) = @_;
	foreach my $proc (@$procs) {
		my @labels	= ($proc);
		my $label	= $proc->{'next'};
		my $idx		= 0;
		$proc->{'proc'}	= $proc;
		$proc->{'idx'}	= $idx++;
		while ($label && !$label->{'symbol'}) {
			$label->{'proc'}	= $proc;
			$label->{'idx'}		= $idx++;
			push (@labels, $label);
			$label = $label->{'next'};
		}
		$proc->{'labels'} = \@labels;
	}
}

sub separate_code_blocks ($) {
	my ($procs) = @_;
	foreach my $proc (@$procs) {
		my $labels	= $proc->{'labels'};
		my $first	= $proc;
		my $last	= $labels->[-1];

		if ($first->{'prev'}) {
			delete ($first->{'prev'}->{'next'});
		}
		delete ($first->{'prev'});

		if ($last->{'next'}) {
			delete ($last->{'next'}->{'prev'});
		}
		delete ($last->{'next'});
	}
}

sub instruction_proc_dependencies ($$$$) {
	my (undef, $label, $inst, $depends) = @_;
	if ($inst->{'label_arg'}) {
		my $arg = $inst->{'arg'};
		foreach my $arg (ref ($arg) =~ /^ARRAY/ ? @$arg : $arg) {
			next if !ref ($arg);
			if ($label->{'proc'} ne $arg->{'proc'}) {
				$depends->{$arg} = $arg;
			}
		}
	}
}

sub build_proc_dependencies ($$) {
	my ($labels, $label) = @_;
	my $proc = $label->{'proc'};
	if ($proc) {
		my $depends = $proc->{'proc_depends'} || {};
		foreach_inst (undef, $label, \&instruction_proc_dependencies, $depends);
		delete ($depends->{$proc}); # no self-dependencies
		$proc->{'proc_depends'} = $depends;
	}
}

sub build_coding_order ($$) {
	my ($label, $order) = @_;
	my $depends = $label->{'proc_depends'};
	
	if ($depends) {
		$label->{'ordering'} = 1;
		
		my @suborder = sort {
			my $da = $depends->{$a};
			my $db = $depends->{$b};
			my $rc = $da->{'refs'} <=> $db->{'refs'};
			return $rc if $rc != 0;
			return ($a cmp $b);
		} (keys (%$depends));

		foreach my $dep (@suborder) {
			my $label = $depends->{$dep};
			$label = $label->{'proc'}
				if $label->{'proc'};
			if ($label->{'ordering'}) {
				my $name = $label->{'name'};
				my $proc = $label->{'proc'} || "data";
				die "Recursive label dependencies $name ($proc)";
			}
			build_coding_order ($label, $order)
				if !$label->{'ordered'};
		}
		
		delete ($label->{'ordering'});
	}

	$label->{'ordered'} = 1;
	push (@{$order}, $label);
}

sub code_instruction ($$$$) {
	my ($num, $arg, $length, $instructions) = @_;
	my @bytes;
	
	my $negate = 0;
	if ($arg < 0) {
		$negate = 1;
		$arg = ~$arg;
	}

	my $nybbles = count_nybbles ($arg);
	if ($nybbles == 1 && $negate) {
		$nybbles = 2;
	}

	for (my $i = ($nybbles - 1); $i >= 0; --$i) {
		my $byte = ($arg >> ($i * 4)) & 0xf;
		if ($i == 0) {
			$byte 	|= $num;
		} elsif ($i == 1 && $negate) {
			$byte	|= $instructions->primary ('NFIX');
			$arg 	= ~$arg;
		} else {
			$byte 	|= $instructions->primary ('PFIX');
		}
		push (@bytes, pack ('C', $byte));
	}

	if (defined ($length)) {
		if ($arg < 0) {
			while (@bytes < $length) {
				push (@bytes, code_instruction (
					$instructions->primary ('J'),
					0,
					undef,
					$instructions
				));
			}
		} else {
			while (@bytes < $length) {
				unshift (@bytes, code_instruction (
					$instructions->primary ('PFIX'),
					0,
					undef,
					$instructions
				));
			}
		}	
	}
	
	return @bytes;
}

sub code_static_instruction ($$$$) {
	my (undef, $label, $inst, $instructions) = @_;
	
	return if $inst->{'label_arg'};
	return if exists ($inst->{'length'});
	
	my $name	= $inst->{'name'};
	my $arg		= $inst->{'arg'};
	my $length	= $inst->{'target_length'};
	my @bytes;

	if ($name =~ /^\./) {
		# Special
	} else {
		my $num;

		if (defined ($arg) && $instructions->valid_primary ($name)) {
			# Primary
			$num = $instructions->primary ($name);
		} elsif ($instructions->valid_instruction ($name)) {
			# Other
			$arg = $instructions->numeric ($name);
			$num = $instructions->primary ('OPR');
		} else {
			die "Unknown instruction $name";
		}

		@bytes = code_instruction ($num, $arg, $length, $instructions);
	}

	if (@bytes) {
		$inst->{'bytes'} = \@bytes;
	}
	
	$inst->{'length'} = scalar (@bytes);
}

sub code_static_instructions ($$) {
	my ($labels, $instructions) = @_;
	foreach my $label (@$labels) {
		foreach_inst (undef, $label, \&code_static_instruction, $instructions);
	}
}

sub tag_if_complete ($) {
	my $label = shift;
	my $inst = $label->{'inst'};
	my $static = 1;
	my $length = 0;
	for (my $i = 0; $static && $i < @$inst; ++$i) {
		my $inst = $inst->[$i];
		if (exists ($inst->{'length'})) {
			$length += $inst->{'length'};
		} else {
			$static = 0;
		}
	}
	if ($static) {
		$label->{'length'} = $length;
	}
}

sub tag_complete_labels ($) {
	my $labels = shift;
	foreach my $label (@$labels) {
		die if exists ($label->{'data'});
		tag_if_complete ($label);
	}
}

sub merge_offsets ($$$) {
	my ($length, $offsets, $instructions) = @_;
	my $diff;
	my @d;

	foreach my $o (@$offsets) {
		if (ref ($o)) {
			push (@d, $o);
		} else {
			my @diff = code_instruction (
				$instructions->primary ('OPR'),
				$instructions->numeric ($o),
				undef,
				$instructions
			);

			$diff += scalar (@diff);
		}
	}

	die if !@d;
	my $ret = 0;

	if ($d[0]->{'d'} eq '+') {
		$ret = $d[0]->{'v'} - $diff;
	} elsif ($d[0]->{'d'} eq '-') {
		$ret = -($d[0]->{'v'} + $length + $diff);
	} else {
		die;
	}

	for (my $i = 1; $i < @d; ++$i) {
		my $o = $d[$i];
		if ($o->{'d'} eq '+') {
			$ret -= $o->{'v'} - $diff;
		} elsif ($o->{'d'} eq '-') {
			$ret -= -($o->{'v'} + $length + $diff);
		} else {
			die;
		}
	}

	return $ret;
}

sub code_offset_instruction ($$$$) {
	my ($name, $length, $offsets, $instructions) = @_;
	my $num	= $instructions->primary ($name);

	die "Unknown instruction $name" 
		if !defined ($num);

	# Estimate
	my @bytes = code_instruction (
		$num,
		merge_offsets (1, $offsets, $instructions),
		undef,
		$instructions
	);

	# Code until stable
	my $stable;
	do {
		my @new_bytes = code_instruction (
			$num,
			merge_offsets (scalar (@bytes), $offsets, $instructions),
			undef,
			$instructions
		);
		$stable = scalar (@bytes) == scalar (@new_bytes);
		@bytes	= @new_bytes;
	} until ($stable);

	# Add padding (if required)
	if (defined ($length)) {
		@bytes = code_instruction (
			$num,
			merge_offsets (scalar (@bytes), $offsets, $instructions),
			$length,
			$instructions
		);
	}

	return @bytes;
}

sub code_table ($$$) {
	my ($label, $inst, $instructions) = @_;
	my $max_length = 0;

	die if scalar (@{$inst->{'table'}}) == 0;

	foreach my $op (@{$inst->{'table'}}) {
		return 0 if !exists ($op->{'length'});
		my $length = $op->{'length'};
		$max_length = $length if $length > $max_length;
	}
	
	die if $max_length == 0;

	foreach my $op (@{$inst->{'table'}}) {
		$op->{'target_length'} = $max_length;
	}

	my $size_op	= $inst->{'size_op'};
	my @bytes	= code_instruction (
		$instructions->primary ($size_op->{'name'}),
		$max_length,
		undef,
		$instructions
	);
	
	$size_op->{'arg'}	= $max_length;
	$size_op->{'bytes'}	= \@bytes;
	$size_op->{'length'}	= scalar (@bytes);

	$inst->{'length'}	= 0;
	$label->{'length'}	= 0;

	return 1;
}

sub code_offset ($$$) {
	my ($label, $inst, $instructions) = @_;
	my $arg	= $inst->{'arg'};
	my @offsets;

	return code_table ($label, $inst, $instructions)
		if $inst->{'name'} eq 'TABLE';

	foreach my $arg (ref ($arg) =~ /^ARRAY/ ? @$arg : $arg) {
		if (!ref ($arg)) {
			push (@offsets, $arg);
			next;
		}

		my $this	= $label->{'idx'};
		my $backward;
		my $distance;
		my $target;

		if ($arg->{'proc'} eq $label->{'proc'}) {
			$target = $arg->{'idx'};
			$distance = 0;
		} else {
			$target = $label->{'proc'}->{'idx'};
			$distance = $label->{'proc'}->{'pos'} - $arg->{'pos'};
		}

		if ($target > $this) {
			# Forward Jump
			my $p = $label->{'next'};
			while ($p->{'idx'} != $target) {
				return 0 if !exists ($p->{'length'});
				$distance 	+= $p->{'length'};
				$p		= $p->{'next'};
			}
			push (@offsets, { 'd' => '+', 'v' => $distance });
		} elsif ($target < $this) {
			# Backward Jump
			my $p = $label->{'prev'};
			while ($p->{'idx'} != $target) {
				return 0 if !exists ($p->{'length'});
				$distance 	+= $p->{'length'};
				$p		= $p->{'prev'};
			}
			return 0 if !exists ($p->{'length'});
			$distance += $p->{'length'};
			push (@offsets, { 'd' => '-', 'v' => $distance });
		} else {
			# Self Reference
			push (@offsets, { 'd' => '-', 'v' => 0 });
		}
	}

	my $name	= $inst->{'name'};
	my @bytes	= code_offset_instruction (
		$name, $inst->{'target_length'}, \@offsets, $instructions
	);

	$inst->{'bytes'}	= \@bytes;
	$inst->{'length'}	= scalar (@bytes);
	$label->{'length'}	= $inst->{'length'};

	return 1;
}

sub code_offset_if_known ($$) {
	my ($label, $instructions) = @_;
	my $inst	= $label->{'inst'}->[0];
	my $arg		= $inst->{'arg'};
	
	foreach my $arg (ref ($arg) =~ /^ARRAY/ ? @$arg : $arg) {
		next if !ref ($arg);
		return 0 if $arg->{'proc'} ne $label->{'proc'};
	}

	return code_offset ($label, $inst, $instructions);
}

sub code_known_offsets ($$) {
	my ($labels, $instructions) = @_;
	my $coded = 0;
	foreach my $label (@$labels) {
		next if exists ($label->{'length'});
		$coded += code_offset_if_known ($label, $instructions);
	}
	return $coded;
}

sub code_static_external_offsets ($$) {
	my ($labels, $instructions) = @_;
	my $pos = $labels->[0]->{'pos'};

	foreach my $label (@$labels) {
		if (exists ($label->{'length'})) {
			# Computed label, move on to next
			$pos += $label->{'length'};
			next;
		}

		my $inst	= $label->{'inst'}->[0];
		my $arg		= $inst->{'arg'};
		my @offsets;
	
		# Can't compute tables
		return if $inst->{'name'} eq 'TABLE';

		foreach my $arg (ref ($arg) =~ /^ARRAY/ ? @$arg : $arg) {
			if (!ref ($arg)) {
				push (@offsets, $arg);
			} elsif ($arg->{'proc'} eq $label->{'proc'}) {
				# Uncomputed internal reference can't continue
				return;
			} else {
				push (@offsets, { 'd' => '-', 'v' => $pos - $arg->{'pos'} });
			}
		}

		my $name	= $inst->{'name'};
		my @bytes	= code_offset_instruction (
			$name, $inst->{'target_length'}, 
			\@offsets, $instructions
		);

		$inst->{'bytes'}	= \@bytes;
		$inst->{'length'}	= scalar (@bytes);
		$label->{'length'}	= $inst->{'length'};
		$pos 			+= $label->{'length'};
	}
}

sub build_dynamic_label_map ($$) {
	my ($proc, $labels) = @_;
	my @map = scalar (@$labels) * 2;
	my $i	= 0;

	foreach my $label (@$labels) {
		if ($label->{'length'}) {
			$map[$i++] = '';
			$map[$i++] = undef;
		} else {
			$map[$i++] = $label;
			$map[$i++] = $label;
		}
	}
	
	$proc->{'dynamic_label_map'} = \@map;
}

sub build_label_dependencies ($) {
	my ($label)	= @_;
	my $idx		= $label->{'idx'};
	my $map		= $label->{'proc'}->{'dynamic_label_map'};
	my $arg		= $label->{'inst'}->[0]->{'arg'};
	my %deps;

	foreach my $arg (ref ($arg) =~ /^ARRAY/ ? @$arg : $arg) {
		next if !ref ($arg);
		my $arg_idx = $arg->{'proc'} == $label->{'proc'} ?
				$arg->{'idx'} : 0;

		# Slice bounds (lower, upper)
		my $lb = ($arg_idx <= $idx ? $arg_idx : $idx) * 2;
		my $ub = (($arg_idx >= $idx ? $arg_idx : $idx) * 2) + 1;

		# Slice map, hash and merge
		my %arg_deps = @{$map}[$lb..$ub];
		merge_hashes (\%deps, \%arg_deps);
	}
	
	# Remove empty and self references
	delete ($deps{''});
	delete ($deps{$label});

	# If we have no dynamic dependencies something is wrong
	if (keys (%deps) == 0) {
		my $name = $label->{'name'};
		die "No dynamic dependencies $name";
	}

	# Record ourselves in dependent hashes of dependencies
	foreach my $k (keys (%deps)) {
		my $dependents = $deps{$k}->{'dependents'};
		if (!$dependents) {
			$dependents			= {};
			$deps{$k}->{'dependents'}	= $dependents;
		}
		$dependents->{$label} = $label;
	}
}

sub build_label_dependents ($) {
	my ($labels) = @_;
	foreach my $label (@$labels) {
		next if exists ($label->{'length'});
		build_label_dependencies ($label);
	}
}

sub add_initial_estimate_lengths ($) {
	my ($labels) = @_;
	foreach my $label (@$labels) {
		if (!exists ($label->{'length'})) {
			my $inst = $label->{'inst'}->[0];
			if (!$inst || ($inst->{'name'} eq 'TABLE')) {
				$label->{'length'}	= 0;
			} else {
				$inst->{'length'}	= 1
					if !exists ($inst->{'length'});
				$label->{'length'}	= 1;
			}
			$label->{'estimate'}		= 1;
		}
	}
}

sub merge_hashes ($$) {
	my ($dst, $src) = @_;
	foreach my $key (keys (%$src)) {
		$dst->{$key} = $src->{$key}
			if !exists ($dst->{$key});
	}
}

sub first_inst ($) {
	my $inst = shift;
	foreach my $inst (@$inst) {
		return $inst if $inst->{'name'} !~ /^\./;
	}
	die "Unable to find instruction in label";
}

sub code_dynamic_offsets ($$) {
	my ($labels, $instructions) = @_;
	my $to_code = {};
	
	# Initial pass list is all estimated labels
	foreach my $label (@$labels) {
		$to_code->{$label} = $label if (exists ($label->{'estimate'}));
	}

	# Keep interating until all labels are stable
	while (keys (%$to_code)) {
		my $pending = {};
		foreach my $k (keys (%$to_code)) {
			my $label = $to_code->{$k};
			my $length = $label->{'length'};
			die if !code_offset ($label, first_inst ($label->{'inst'}), $instructions);
			if ($label->{'length'} != $length) {
				merge_hashes ($pending, $label->{'dependents'});
			}
		}
		$to_code = $pending;
	}

	# Code all the labels again now they are stable
	foreach my $label (@$labels) {
		next if !$label->{'estimate'};
		my $length = $label->{'length'};

		die if !code_offset ($label, first_inst ($label->{'inst'}), $instructions);
		die if $length != $label->{'length'};
		
		delete ($label->{'estimate'});
	}
}

sub tag_positions ($$) {
	my ($labels, $pos) = @_;
	foreach my $label (@$labels) {
		$label->{'pos'} = $pos;
		$pos += $label->{'length'};
	}
	return $pos;
}

sub build_ffi_call ($$) {
	my ($label, $ffi_idx)	= @_;
	my $inst		= $label->{'inst'};
	my $stub		= $label->{'stub'};
	my $idx			= $$ffi_idx;
	
	if ($stub =~ /^C\.tvmspecial\.(\d+)/) {
		$idx = -($1 + 1);
	} else {
		my ($name) = ($stub =~ /^.*?\.(.*)$/);
		$name =~ s/\./_/g;

		$label->{'ffi_index'} 	= $idx;
		$label->{'ffi_symbol'}	= $name;

		${$ffi_idx}++;
	}

	push (@$inst, 
		{ 'name' => 'LDC', 'arg' => $idx	},
		{ 'name' => 'FFICALL'			}
	);

	delete ($label->{'prev'});
	delete ($label->{'next'});
}

sub generate_code ($$) {
	my ($labels, $instructions) = @_;
	my $ffi_idx	= 0;
	my $pos		= 0;
	foreach my $label (@$labels) {
		if ($label->{'align'}) {
			my %alignment = ( 1 => 0x1, 2 => 0x3, 3 => 0x7, 4 => 0xf );
			my $mask = $alignment{$label->{'align'}};
			if ($pos & $mask) {
				$pos &= ~$mask;
				$pos += $mask + 1;
			}
		}

		$label->{'pos'} = $pos;

		if ($label->{'proc'}) {
			my $proc	= $label->{'proc'};
			my $labels	= $proc->{'labels'};
			code_static_instructions ($labels, $instructions);
			tag_complete_labels ($labels);
			code_static_external_offsets ($labels, $instructions);
			while (code_known_offsets ($labels, $instructions) > 0) {
				code_static_external_offsets ($labels, $instructions);
			}
			build_dynamic_label_map ($proc, $labels);
			build_label_dependents ($labels);
			add_initial_estimate_lengths ($labels);
			code_dynamic_offsets ($labels, $instructions);
			$pos = tag_positions ($labels, $pos);
		} elsif ($label->{'stub'}) {
			my $labels = [ $label ];
			build_ffi_call ($label, \$ffi_idx);
			code_static_instructions ($labels, $instructions);
			tag_complete_labels ($labels);
			$pos = tag_positions ($labels, $pos);
		} else {
			$pos += $label->{'length'};
		}
	}
}

sub jump_entry ($$) {
	my ($self, $entry) 	= @_;
	my $instructions	= $self->{'instructions'};

	my @bytes		= code_instruction (
		$instructions->numeric ('J'),
		$entry->{'pos'},
		undef,
		$instructions
	);

	while (@bytes < 4) {
		@bytes = (
			code_instruction (
				$instructions->numeric ('PFIX'),
				0,
				undef,
				$instructions
			),
			@bytes
		);
	}

	return {
		'name'	=> 'Jump',
		'pos'	=> 0,
		'inst'	=> [
			{
				'name'	=> 'J',
				'arg'	=> $entry->{'pos'},
				'bytes'	=> \@bytes
			}
		],
		'length' => scalar (@bytes)
	};
}

sub expand_coding_order (@) {
	my @coding_order = @_;
	my @ret;
	foreach my $ent (@coding_order) {
		my $label = $ent;
		do {
			push (@ret, $label);
			$label = $label->{'next'};
		} while ($label);
	}
	return @ret;
}

sub shift_labels ($@) {
	my ($shift, @labels) = @_;
	foreach my $label (@labels) {
		$label->{'pos'} += $shift;
	}
}

#
#  Perl code for linking and assembling bytecode for the TVM
#  Copyright (C) 2008 Carl Ritson <cgr@kent.ac.uk>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

package Transterpreter::TEncode;

use strict;

sub new ($@) {
	my ($class, @elements) = @_;

	my $self = bless {
		'elements' => []
	}, $class;

	for (my $i = 0; $i < @elements; $i += 2) {
		$self->add ($elements[$i + 0], $elements[$i + 1]);
	}

	return $self;
}

sub add ($$$) {
	my ($self, $id, $data) = @_;

	die "Invalid element identifier: $id"
		if $id !~ /^.{3}[BILSU]$/;
	
	die "Data for '$id' must be a TEncode object"
		if $id =~ /L$/ && (!ref ($data));

	push (@{$self->{'elements'}}, {
		'id'	=> $id,
		'data'	=> $data
	});
}

sub _encode ($$) {
	my ($self, $ib)	= @_;
	my $ifmt	= $ib == 2 ? 'n' : 'N';
	my $elements	= $self->{'elements'};
	my $bytes;

	foreach my $element (@$elements) {
		my $id		= $element->{'id'};
		my $data 	= $element->{'data'};
		my $length;
		
		$element->{'pad'}	= 0;
		
		if ($id =~ /L$/) {
			$element->{'bytes'} 	= $data->_encode ($ib);
			$length			= length ($element->{'bytes'});
		} elsif ($id =~ /[IU]$/) {
			# Unsign data
			$data += 2 ** ((8 * $ib) - 1) 
				if $id =~ /I$/ && $data < 0;
			
			my $coded	= pack ($ifmt, $data);
			my $decoded	= unpack ($ifmt, $coded);
			
			die "In consistent data coding for $id = $data"
				if $decoded != $data;

			$element->{'bytes'}	= '';
			$length			= $data;
		} else {
			$element->{'bytes'} 	= $data;
			$element->{'bytes'}	.= "\0" if $id =~ /S$/;
			$length			= length ($element->{'bytes'});
			$element->{'pad'}	= $ib - ($length & ($ib - 1))
				if ($length & ($ib - 1));
		}

		$bytes .= $id;
		$bytes .= pack ($ifmt, $length);
		$bytes .= $element->{'bytes'};

		for (my $i = 0; $i < $element->{'pad'}; ++$i) {
			$bytes .= "\0";
		}
	}

	$self->{'bytes'} = $bytes;

	return $bytes;
}

sub encode ($$) {
	my ($self, $header) = @_;

	die "Invalid TEncode header $header"
		if $header !~ /^(TE|te)nc$/;
	
	my $ib 		= $header eq 'TEnc' ? 4 : 2;
	my $bytes 	= $self->_encode ($ib);
	my $length	= length ($bytes);

	return $header . pack ($ib == 4 ? 'N' : 'n', $length) . $bytes;
}

#
#  Perl code for linking and assembling bytecode for the TVM
#  Copyright (C) 2008 Carl Ritson <cgr@kent.ac.uk>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

package Transterpreter::VM;

use strict;

sub new ($$) {
	my ($class) = @_;
	my $self = bless {}, $class;
	return $self;
}

sub assemble ($@) {
	my ($self, @labels) = @_;
	my $verbose = $self->{'verbose'};
	my @bytecode;
	my %debug;
	my $pos = 0;

	foreach my $label (@labels) {
		printf ("0x%05x %-6s  % 3d  % 3d  %s\n",
			$label->{'pos'},
			$label->{'name'},
			$pos,
			$label->{'length'},
			$label->{'symbol'}
		) if $verbose;

		while ($pos < $label->{'pos'}) {
			push (@bytecode, "\0");
			$pos++;
		}

		if ($pos != $label->{'pos'}) {
			die sprintf (
				"Label positioning error %s at %d (pos:%d)",
				$label->{'name'}, $label->{'pos'}, $pos
			);
		}
		
		foreach my $op (@{$label->{'inst'}}) {
			my $name = $op->{'name'};
			if ($op->{'bytes'}) {
				my $bytes = $op->{'bytes'};
				push (@bytecode, @$bytes);
				$pos += scalar (@$bytes);
				
				if ($verbose) {
					for (my $i = 0; $i < 23; ++$i) { print " "; }
					printf ('%-16s', $name);
					for (my $i = 0; $i < @$bytes; ++$i) {
						printf ('%02x ', unpack ('C', $bytes->[$i]));
					}
					print "\n";
				}
			} elsif ($name =~ /^\.FILENAME$/) {
				$debug{$pos} = {} if !$debug{$pos};
				$debug{$pos}->{'file'} = $op->{'arg'};
			} elsif ($name =~ /^\.LINE$/) {
				$debug{$pos} = {} if !$debug{$pos};
				$debug{$pos}->{'line'} = $op->{'arg'};
			} elsif ($name =~ /^\.DATABYTES/) {
				my @bytes = split (//, $op->{'arg'});
				push (@bytecode, @bytes);
				$pos += scalar (@bytes);
				
				if ($verbose) {
					for (my $i = 0; $i < 23; ++$i) { print " "; }
					for (my $i = 0; $i < @bytes; ++$i) {
						printf ('%02x ', unpack ('C', $bytes[$i]));
					}
					print "\n";
				}
			}
		}
	}

	# Debug info: fill in missing file entries and delete duplicates
	my $last;
	foreach my $key (sort { $a <=> $b } (keys (%debug))) {
		my $entry = $debug{$key};
		
		$entry->{'pos'} = $key;
		
		if (!exists ($entry->{'file'})) {
			next if !$last;
			$entry->{'file'} = $last->{'file'};
		}

		if ($last->{'file'} eq $entry->{'file'} 
				&& $last->{'line'} == $entry->{'line'}) {
			delete ($debug{$key});
		} else {
			$last = $entry;
		}
	}

	# Debug info: build return array
	my @debug;
	foreach my $key (sort { $a <=> $b } (keys (%debug))) {
		push (@debug, $debug{$key});
	}

	return (\@bytecode, \@debug);
}


package main;

use strict;
use Data::Dumper;

# ETC Decoder
my $etc		= new Transputer::ETC ();

# Linker
my $linker	= new Transterpreter::Linker ();

# TCOFF Decoder
my $tcoff	= new Transputer::TCOFF ();

# TVM helper
my $tvm		= new Transterpreter::VM ();

# Options
my $bits	= 32;
my $output;
my %sections	= (
	'debug'		=> 1,
	'ffi'		=> 1,
	'symbols'	=> 1,
	'tlp'		=> 1
);
my $verbose;
my @files;

# Command Line Parsing
my @args	= @ARGV;
my $options 	= 1;
while (my $arg = shift @args) {
	if ($options && $arg eq '--') {
		$options 		= 0;
	} elsif ($options && $arg eq '-v') {
		$verbose		= 1;
		$etc->{'verbose'} 	= 1;
		$linker->{'verbose'} 	= 1;
		$tcoff->{'verbose'}	= 1;
		$tvm->{'verbose'} 	= 1;
	} elsif ($options && $arg eq '-o') {
		$output 		= shift @args;
	} elsif ($options && $arg eq '-s') {
		$bits			= 16;
	} elsif ($options && $arg =~ /^-[ed]$/) {
		my $section		= shift @args;
		$sections{$section}	= $arg eq '-e';
	} else {
		push (@files, $arg);
	}
}

if (!$output) {
	$output = $files[-1];
	$output =~ s/\.tce$/.tbc/i;
}

if (!$output || !@files) {
	print <<END;
Usage:
  plinker [OPTIONS] <file> [<file> ...]
    Link one or more ETC input files as a TEncode bytecode.

Options:
  -d <section>   Disable output of a section
  -e <section>   Enable output of a section
  -o <name>      Specific output file name
  -s             Small 16-bit output (default is 32-bit)
  -v             Verbose mode

Valid sections are:
  debug          Line numbering information   (default: enabled)
  ffi            Foreign Function Interface   (default: enabled)
  symbols        Symbol information           (default: enabled)
  tlp            Top Level Process descriptor (default: enabled)

Report bugs to <kroc-bugs\@kent.ac.uk>.
END
	exit 1;
}

# Load ETC
my $last_file;
my $endian;
my %etc_file;
my @etc;
my $objects = [];

foreach my $file (@files) {
	my $data = $tcoff->read_file ($file);
	die "Failed to read $file" if !$data;

	# Check endian
	if ($data->{'.ENDIAN'}) {
		my $file_endian = $data->{'.ENDIAN'}->[0];
		die "Inconsistent endian settings, $file is $file_endian"
			if defined ($endian) && $file_endian ne $endian;
		$endian = $file_endian;
	}

	# Decode text sections
	my @texts;
	foreach my $section (@{$data->{'LOAD_TEXT'}}) {
		my @text = $etc->decode_load_text ($section->{'data'});

		if (!@text) {
			print STDERR "Failed to decode a text section in $file...\n";
		} else {
			my $ref = { 'file' => $file, 'etc' => \@text };
			push (@texts, $ref);
			$etc_file{\@text} = $data;
		}
	}

	# Separate objects and libraries
	if ($file =~ /\.tce$/i) {
		push (@$objects, @texts);
	} elsif (@texts) {
		if (@$objects) {
			push (@etc, $objects);
			$objects = [];
		}
		push (@etc, \@texts);
	}

	$last_file = $data;
}

push (@etc, $objects) if @$objects;
$objects = [];

# Check we have some ETC to work with
die "No valid data loaded (invalid ETC files?)" if !@etc;

# Pick Entry Point
my $symbols 	= $last_file->{'symbols'};
my $jentry;
my $last_texts	= $etc[@etc - 1];
my $last_text	= $last_texts->[@$last_texts - 1]->{'etc'};
foreach my $op (@$last_text) {
	if (!$jentry && ($op->{'name'} eq '.JUMPENTRY')) {
		$jentry = $op->{'arg'};
	}
}

die "No jump entry in the final text section: don't know which process to link"
	if !defined ($jentry);

my $entry_point = $symbols->{$jentry};

die "Unable to find symbol definition for entry point $jentry"
	if !defined ($entry_point) || !exists ($entry_point->{'definition'});

if ($verbose) {
	print 	"Target:\n",
		format_symbol_definition ($entry_point->{'definition'}, "  ");
}

# Link
my @labels = $linker->link ($entry_point->{'string'}, @etc);

# TEncode
my ($bytecode, $debug)	= $tvm->assemble (@labels);
my $tlp			= new Transterpreter::TEncode ();
my $ffi			= new Transterpreter::TEncode ();
my $stb			= new Transterpreter::TEncode ();
my $dbg			= new Transterpreter::TEncode ();
my $tbc			= new Transterpreter::TEncode (
	'endU'	=> $endian eq 'big' ? 1 : 0,
	'ws U'	=> $entry_point->{'ws'},
	'vs U'	=> $entry_point->{'vs'},
	'padB'	=> "\0\0\0\0",
	'ms U'	=> $entry_point->{'ms'},
	'bc B'	=> join ('', @$bytecode)
);
$tbc->add ('tlpL', $tlp) if $sections{'tlp'};
$tbc->add ('ffiL', $ffi) if $sections{'ffi'};
$tbc->add ('stbL', $stb) if $sections{'symbols'};
$tbc->add ('dbgL', $dbg) if $sections{'debug'};

my $tenc		= new Transterpreter::TEncode (
	'tbcL'	=> $tbc
);

# Top-Level-Process
$tlp->add ('fmtS', build_format_string ($entry_point->{'definition'}));
$tlp->add ('symS', $entry_point->{'string'});

# FFI
my %ffi_libs;
my @ffi_symbols;
foreach my $label (@labels) {
	if (exists ($label->{'ffi_index'})) {
		$ffi_symbols[$label->{'ffi_index'}] = '_' . $label->{'ffi_symbol'};
	}
	if (exists ($label->{'source'})) {
		my $comments = $etc_file{$label->{'source'}}->{'COMMENT'};

		# Search comments
		foreach my $comment (@$comments) {
			my $data = $comment->{'data'};
			if ($data =~ /\(spragma\s+\(dynlib\s+(.*)\)\)/) {
				my $library = $1;
				$ffi_libs{$library}++;
			}
		}
	}
}

if (@ffi_symbols) {
	my $libs = new Transterpreter::TEncode ();
	my $syms = new Transterpreter::TEncode ();
	my $map	= new Transterpreter::TEncode ();

	$ffi->add ('libL' => $libs);
	$ffi->add ('symL' => $syms);
	$ffi->add ('mapL' => $map);

	foreach my $lib (sort (keys (%ffi_libs))) {
		$libs->add ('libS' => $lib);
	}
	foreach my $sym (@ffi_symbols) {
		$syms->add ('symS' => $sym);
	}
	foreach my $sym (@ffi_symbols) {
		$map->add ('idxI' => -1);
	}
}

# Symbol Table
foreach my $label (@labels) {
	next if !exists ($label->{'symbol'});
	die if !$label->{'source'};
	my $name	= $label->{'symbol'};
	my $symbols 	= $etc_file{$label->{'source'}}->{'symbols'};
	my $symbol	= $symbols->{$name};
	my $sym		= new Transterpreter::TEncode (
		'offU'	=> $label->{'pos'},
		'symS'	=> $name
	);

	if (exists ($symbol->{'definition'})) {
		$sym->add ('defS' => $symbol->{'definition'});
		$sym->add ('ws U' => $symbol->{'ws'});
		$sym->add ('vs U' => $symbol->{'vs'});
	}

	$stb->add ('symL' => $sym);
}

# Line numbering information
my $fns 	= new Transterpreter::TEncode ();
my %files;

foreach my $entry (@$debug) {
	$files{$entry->{'file'}}++;
}

my @files = sort { $files{$a} <=> $files{$b} } (keys (%files));

for (my $i = 0; $i < @files; ++$i) {
	my $file	= $files[$i];
	$files{$file}	= $i;
	$fns->add ('fn S' => $file );
}
$dbg->add ('fn L' => $fns);

my $lnd;
foreach my $entry (@$debug) {
	$lnd .= pack (($bits == 32 ? 'NNN' : 'nnn'), 
		$entry->{'pos'},
		$files{$entry->{'file'}},
		$entry->{'line'}
	);
}
$dbg->add ('lndB' => $lnd);

# Output 
my $fh;
open ($fh, ">$output") || die $!;
binmode ($fh);
print $fh $tenc->encode ($bits == 32 ? 'TEnc' : 'tenc');
close ($fh);

# Debug labels
if (0) {
	foreach my $label (@labels) {
		print $label->{'name'}, " ";
		if ($label->{'data'}) {
			print "data";
		} elsif ($label->{'stub'}) {
			print "stub ", $label->{'stub'};
		} else {
			print $label->{'symbol'};
		}
		print "\n";
	}
}

exit 0;

sub build_format_string {
	my ($def)	= @_;
	my ($params)	= ($def =~ m/PROC\s+\S+\(([^\)]+)\)/s);
	my @params	= split (/,/, $params);
	my @fmt;

	foreach my $param (@params) {
		$param =~ s/^\s*//s;
		$param =~ s/\s*$//s;

		if ($param =~ /^FIXED/) {
			if ($param =~ /\?/) {
				push (@fmt, 'S');
			} elsif ($param =~ /!/) {
				push (@fmt, 'C');
			} else {
				die;
			}
		} elsif ($param =~ m/^CHAN\s+OF\s+(\S+)\s+(\S+)/s) {
			my ($type, $name)	= ($1, $2);
			my ($dir)		= ($name =~ m/([\?!])$/);
			if (!$dir) {
				($dir) = ($def =~ m/$name([\?!])/gs);
				$dir = '.' if !$dir;
			}
			push (@fmt, $dir);
		} elsif ($param =~ /^VAL/) {
			push (@fmt, 'V');
		} else {
			push (@fmt, '_');
		}
	}

	push (@fmt, 'F') if $def =~ m/PROC.*\(.*\).*FORK/s;

	return join ('', @fmt);
}

sub format_symbol_definition {
	my ($def, $prefix) 	= @_;
	my @lines		= split (/\r?\n/, $def);
	my $indent		= "";

	foreach my $line (@lines) {
		$line =~ s/PROC\s+(\S+)\s*\(/PROC \1 (/;
		$line =~ s/(--|,)(\S)/\1 \2/g;
		$line =~ s/(\S)--/\1 --/g;

		if ($line =~ /^(PROC|SEQ|PAR)/) {
			$line = $indent . $line;
			$indent = "$indent  ";
		} elsif ($line =~ /^:/) {
			$indent = "";
		} else {
			$line = $indent . $line;
		}
		
		$line = $prefix . $line . "\n";
	}

	return join ('', @lines);
}
