#
#  Perl code for generating LLVM assembly from ETC assembly
#  Copyright (C) 2009 Carl Ritson <cgr@kent.ac.uk>
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

package Transputer::LLVM;

use strict;
use vars qw($GRAPH);
use Data::Dumper;

$GRAPH = {
	# Branching
	'CALL'		=> { 'branching' => 1, 'in' => 3, 'fin' => 3, 
			'generator' => \&gen_call }, # actually out is 3
	'CJ'		=> { 'branching' => 1, 'in' => 3, 'out' => 2, 'fin' => 3,
			'generator' => \&gen_cj },
	'GCALL'		=> { 'branching' => 1, 'in' => 3, 'fin' => 3,
			'generator' => \&gen_call }, # check
	'J' 		=> { 'branching' => 1, 'in' => 3, 'fin' => 3,
			'generator' => \&gen_j },
	'LEND'		=> { 'branching' => 1, 'in' => 3,
			'generator' => \&gen_lend },
	'LEND3'		=> { 'branching' => 1, 'in' => 3,
			'generator' => \&gen_lend },
	'LENDB'		=> { 'branching' => 1, 'in' => 3,
			'generator' => \&gen_lend },
	'RET'		=> { # Intentionally not 'branching' => 1,
			'fin' => 3, # Eats floating point stack
			'generator' => \&gen_ret },
	'TABLE'		=> { 'branching' => 1, 'in' => 1,
			'generator' => \&gen_table },
	# Workspace/Operand Stack
	'AJW'		=> { 'wptr' => 1, 
			'generator' => \&gen_ajw },
	'REV'		=> { 'in' => 2, 'out' => 2,
			'generator' => \&gen_rev },
	'POP'		=> { 'in' => 1,
			'generator' => \&gen_nop },
	'GAJW'		=> { 'in' => 1, 'out' => 1, 'wptr' => 1,
			'generator' => \&gen_gajw },
	'DUP'		=> { 'in' => 1, 'out' => 2, 
			'generator' => \&gen_dup },
	# Load/Store
	'LDLP'		=> { 'out' => 1, 
			'generator' => \&gen_ldlp },
	'LDNL'		=> { 'in' => 1, 'out' => 1, 
			'generator' => \&gen_ldnl },
	'LDNLP'		=> { 'in' => 1, 'out' => 1, 
			'generator' => \&gen_ldnlp },
	'LDL'		=> { 'out' => 1, 
			'generator' => \&gen_ldl },
	'STL'		=> { 'in' => 1, 
			'generator' => \&gen_stl },
	'STNL'		=> { 'in' => 2, 
			'generator' => \&gen_stnl },
	'LB'		=> { 'in' => 1, 'out' => 1,
			'generator' => \&gen_lb },
	'BSUB'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_bsub },
	'LDPI'		=> { 
			'generator' => \&gen_nop },
	'WSUB'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_wsub },
	'WSUBDB'	=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_wsub },
	'CSUB0'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_csub0 },
	'MOVE'		=> { 'in' => 3,
			'generator' => \&gen_move },
	# Constants
	'LDC'		=> { 'out' => 1, 
			'generator' => \&gen_ldc },
	'LDINF'		=> { 'out' => 1,
			'generator' => \&gen_ldinf },
	'NULL'		=> { 'out' => 1,
			'generator' => \&gen_null },
	'MINT'		=> { 'out' => 1,
			'generator' => \&gen_mint },
	# Arithmetic
	'ADC'		=> { 'in' => 1, 'out' => 1, 
			'generator' => \&gen_adc },
	'EQC'		=> { 'in' => 1, 'out' => 1,
			'generator' => \&gen_eqc },
	'DIFF'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_diff },
	'ADD'		=> { 'in' => 2, 'out' => 1, 
			'generator' => \&gen_add },
	'PROD'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_prod },
	'GT'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_gt },
	'SUB'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_sub },
	'REM'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_divrem },
	'DIV'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_divrem },
	'NOT'		=> { 'in' => 1, 'out' => 1,
			'generator' => \&gen_not },
	'XOR'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_xor },
	'SB'		=> { 'in' => 2,
			'generator' => \&gen_sb },
	'SHR'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_shift },
	'SHL'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_shift },
	'XSHR'		=> { 'in' => 1, 'out' => 1,
			'generator' => \&gen_shift },
	'XSHL'		=> { 'in' => 1, 'out' => 1,
			'generator' => \&gen_shift },
	'AND'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_and },
	'OR'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_or },
	'SUM'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_sum },
	'MUL'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_mul },
	'BOOLINVERT'	=> { 'in' => 1, 'out' => 1,
			'generator' => \&gen_boolinvert },
	'NORM'		=> { 'in' => 3, 'out' => 3,
			'generator' => \&gen_norm },
	'FMUL'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_fmul },
	'POSTNORMSN'	=> { 'in' => 3, 'out' => 3 },
	'ROUNDSN'	=> { 'in' => 3, 'out' => 1 },
	# Shorts and Bytes
	'WIDENSHORT'	=> { 'in' => 1, 'out' => 1,
			'generator' => \&gen_widenshort },
	# Long Arithmetic
	'XDBLE'		=> { 'in' => 1, 'out' => 2,
			'generator' => \&gen_xdble },
	'LADD'		=> { 'in' => 3, 'out' => 1,
			'generator' => \&gen_laddsub },
	'LDIFF'		=> { 'in' => 3, 'out' => 2,
			'generator' => \&gen_ldiffsum },
	'LDIV'		=> { 'in' => 3, 'out' => 2,
			'generator' => \&gen_ldiv },
	'LMUL'		=> { 'in' => 3, 'out' => 2,
			'generator' => \&gen_lmul },
	'LSHR'		=> { 'in' => 3, 'out' => 2,
			'generator' => \&gen_lshift },
	'LSHL'		=> { 'in' => 3, 'out' => 2,
			'generator' => \&gen_lshift },
	'LSUM'		=> { 'in' => 3, 'out' => 2,
			'generator' => \&gen_ldiffsum },
	'LSUB'		=> { 'in' => 3, 'out' => 1,
			'generator' => \&gen_laddsub },
	'I64TOREAL'	=> { 'in' => 1, 'fout' => 1,
			'generator' => \&gen_i64toreal },
	# Errors
	'SETERR'	=> {
			'generator' => \&gen_seterr },
	'FPCHKERR'	=> { 
			'generator' => \&gen_nop },
	'CCNT1'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_ccnt1 },
	'CHECKNOTNULL'	=> { 'in' => 1, 'out' => 1,
			'generator' => \&gen_checknotnull },
	'CSNGL'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_csngl },
	'CWORD'		=> { 'in' => 2, 'out' => 1,
			'generator' => \&gen_cword },
	'FPCHKI32'	=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpchki },
	'FPCHKI64'	=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpchki },
	# Floating Point
	'FPLDNLDBI'	=> { 'in' => 2, 'fout' => 1,
			'generator' => \&gen_fpldnl },
	'FPLDNLSNI'	=> { 'in' => 2, 'fout' => 1,
			'generator' => \&gen_fpldnl },
	'FPLDNLDB'	=> { 'in' => 1, 'fout' => 1,
			'generator' => \&gen_fpldnl },
	'FPLDNLSN'	=> { 'in' => 1, 'fout' => 1,
			'generator' => \&gen_fpldnl },
	'FPSTNLDB'	=> { 'in' => 1, 'fin' => 1,
			'generator' => \&gen_fpstnl },
	'FPSTNLSN'	=> { 'in' => 1, 'fin' => 1,
			'generator' => \&gen_fpstnl },
	'FPSTNLI32'	=> { 'in' => 1, 'fin' => 1,
			'generator' => \&gen_fpstnl },
	'FPI32TOR32'	=> { 'in' => 1, 'fout' => 1,
			'generator' => \&gen_fpi32to },
	'FPI32TOR64'	=> { 'in' => 1, 'fout' => 1,
			'generator' => \&gen_fpi32to },
	'FPB32TOR64'	=> { 'in' => 1, 'fout' => 1,
			'generator' => \&gen_fpi32to },
	'FPRTOI32'	=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fprtoi32 },
	'FPR32TOR64'	=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpnop },
	'FPR64TOR32'	=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpnop },
	'FPLDZEROSN'	=> { 'fout' => 1,
			'generator' => \&gen_fpldzero },
	'FPLDZERODB'	=> { 'fout' => 1,
			'generator' => \&gen_fpldzero },
	'FPADD'		=> { 'fin' => 2, 'fout' => 1,
			'generator' => \&gen_fparithop },
	'FPSUB'		=> { 'fin' => 2, 'fout' => 1,
			'generator' => \&gen_fparithop },
	'FPMUL'		=> { 'fin' => 2, 'fout' => 1,
			'generator' => \&gen_fparithop },
	'FPDIV'		=> { 'fin' => 2, 'fout' => 1,
			'generator' => \&gen_fparithop },
	'FPREM'		=> { 'fin' => 2, 'fout' => 1,
			'generator' => \&gen_fprem },
	'FPNAN'		=> { 'fin' => 1, 'fout' => 1, 'out' => 1,
			'generator' => \&gen_fpnan },
	'FPNOTFINITE'	=> { 'fin' => 1, 'fout' => 1, 'out' => 1,
			'generator' => \&gen_fpnotfinite },
	'FPORDERED'	=> { 'fin' => 2, 'fout' => 2, 'out' => 1,
			'generator' => \&gen_fpordered },
	'FPGT'		=> { 'fin' => 2, 'out' => 1,
			'generator' => \&gen_fpcmp },
	'FPEQ'		=> { 'fin' => 2, 'out' => 1,
			'generator' => \&gen_fpcmp },
	'FPINT'		=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpint },
	'FPDUP'		=> { 'fin' => 1, 'fout' => 2,
			'generator' => \&gen_dup },
	'FPPOP'		=> { 'fin' => 1,
			'generator' => \&gen_nop },
	'FPREV'		=> { 'fin' => 2, 'fout' => 2,
			'generator' => \&gen_rev },
	'FPDIVBY2'	=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpxby2 },
	'FPMULBY2'	=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpxby2 },
	'FPSQRT'	=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpsqrt },
	'FPRZ'		=> { 
			'generator' => \&gen_nop },
	'FPEXPDEC32'	=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpexpdec32 },
	'FPABS'		=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpabs },
	'FPLDNLADDDB'	=> { 'in' => 1, 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpldnlop },
	'FPLDNLMULDB'	=> { 'in' => 1, 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpldnlop },
	'FPLDNLADDSN'	=> { 'in' => 1, 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpldnlop },
	'FPLDNLMULSN'	=> { 'in' => 1, 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fpldnlop },
	'R32COS'	=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fcossin },
	'R32SIN'	=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fcossin },
	'R64COS'	=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fcossin },
	'R64SIN'	=> { 'fin' => 1, 'fout' => 1,
			'generator' => \&gen_fcossin },
	# Helpers
	'INDIRECT_AREG'	=> { 'in' => 3, 'out' => 3,
			'generator' => \&gen_indirect_reg },
	'INDIRECT_BREG'	=> { 'in' => 3, 'out' => 3,
			'generator' => \&gen_indirect_reg },
	'INDIRECT_CREG'	=> { 'in' => 3, 'out' => 3,
			'generator' => \&gen_indirect_reg },
	# Kernel
	'RESCHEDULE'	=> { 'kcall' => 1,
		'symbol' => 'Y_pause' },
	'ENDP'		=> { 'kcall' => 1, 'in' => 1,
		'symbol' => 'Y_endp' },
	'IN'		=> { 'kcall' => 1, 'in' => 3,
		'symbol' => 'Y_in' },
	'OUT'		=> { 'kcall' => 1, 'in' => 3,
		'symbol' => 'Y_out' },
	'STARTP'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'Y_startp' },
	'OUTBYTE'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'Y_outbyte' },
	'OUTWORD'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'Y_outword' },
	'MRELEASEP'	=> { 'kcall' => 1, 'in' => 1,
		'symbol' => 'Y_mreleasep' },
	'RUNP'		=> { 'kcall' => 1, 'in' => 1,
		'symbol' => 'X_runp' },
	'STOPP'		=> { 'kcall' => 1,
		'symbol' => 'Y_stopp' },
	'LDTIMER'	=> { 'kcall' => 1, 'out' => 1,
		'symbol' => 'X_ldtimer' },
	'TIN'		=> { 'kcall' => 1, 'in' => 1,
		'symbol' => 'Y_tin' },
	'MALLOC'	=> { 'kcall' => 1, 'in' => 1, 'out' => 1,
		'symbol' => 'X_malloc' },
	'MRELEASE'	=> { 'kcall' => 1, 'in' => 1,
		'symbol' => 'X_mrelease' },
	'XABLE'		=> { 'kcall' => 1, 'in' => 1,
		'symbol' => 'Y_xable' },
	'XIN'		=> { 'kcall' => 1, 'in' => 3,
		'symbol' => 'Y_xin' },
	'XEND'		=> { 'kcall' => 1, 'in' => 1,
		'symbol' => 'X_xend' },
	'PROC_ALLOC'	=> { 'kcall' => 1, 'in' => 2, 'out' => 1,
		'symbol' => 'X_proc_alloc' },
	'PROC_PARAM'	=> { 'kcall' => 1, 'in' => 3,
		'symbol' => 'X_proc_param' },
	'PROC_MT_COPY'	=> { 'kcall' => 1, 'in' => 3,
		'symbol' => 'X_proc_mt_copy' },
	'PROC_MT_MOVE'	=> { 'kcall' => 1, 'in' => 3,
		'symbol' => 'X_proc_mt_move' },
	'PROC_START'	=> { 'kcall' => 1, 'in' => 3,
		'symbol' => 'Y_proc_start' },
	'PROC_END'	=> { 'kcall' => 1, 'in' => 1,
		'symbol' => 'Y_proc_end' },
	'GETAFF'	=> { 'kcall' => 1, 'out' => 1,
		'symbol' => 'X_getaff' },
	'SETAFF'	=> { 'kcall' => 1, 'in' => 1,
		'symbol' => 'Y_setaff' },
	'GETPAS'	=> { 'kcall' => 1, 'out' => 1, 
		'symbol' => 'X_getpas' },
	'GETPRI'	=> { 'kcall' => 1, 'out' => 1,
		'symbol' => 'X_getpri' },
	'SETPRI'	=> { 'kcall' => 1, 'in' => 1,
		'symbol' => 'Y_setpri' },
	'MT_ALLOC'	=> { 'kcall' => 1, 'in' => 2, 'out' => 1,
		'symbol' => 'X_mt_alloc' },
	'MT_RELEASE'	=> { 'kcall' => 1, 'in' => 1,
		'symbol' => 'X_mt_release' },
	'MT_CLONE'	=> { 'kcall' => 1, 'in' => 1, 'out' => 1,
		'symbol' => 'X_mt_clone' },
	'MT_IN'		=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'Y_mt_in' },
	'MT_OUT'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'Y_mt_out' },
	'MT_XCHG'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'Y_mt_xchg' },
	'MT_LOCK'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'Y_mt_lock' },
	'MT_UNLOCK'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'X_mt_unlock' },
	'MT_ENROLL'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'X_mt_enroll' },
	'MT_RESIGN'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'X_mt_resign' },
	'MT_SYNC'	=> { 'kcall' => 1, 'in' => 1,
		'symbol' => 'Y_mt_sync' },
	'MT_XIN'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'Y_mt_xin' },
	'MT_XOUT'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'Y_mt_xout' },
	'MT_XXCHG'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'Y_mt_xxchg' },
	'MT_DCLONE'	=> { 'kcall' => 1, 'in' => 3, 'out' => 1,
		'symbol' => 'X_mt_dclone' },
	'MT_BIND'	=> { 'kcall' => 1, 'in' => 3, 'out' => 1,
		'symbol' => 'X_mt_bind' },
	'MT_RESIZE'	=> { 'kcall' => 1, 'in' => 3, 'out' => 1,
		'symbol' => 'X_mt_resize' },
	# ALTing
	'ALT'		=> { 'kcall' => 1, 
		'symbol' => 'X_alt' },
	'TALT'		=> { 'kcall' => 1, 
		'symbol' => 'X_talt' },
	'ALTWT'		=> { 'kcall' => 1, 
		'symbol' => 'Y_altwt' },
	'TALTWT'	=> { 'kcall' => 1, 
		'symbol' => 'Y_taltwt' },
	'ALTEND'	=> { 'kcall' => 1, 
		'symbol' => 'Y_altend' },
	'_ENBT'		=> { 'kcall' => 1, 'in' => 2, 'out' => 1,
		'symbol' => 'X_enbt' },
	'_ENBC'		=> { 'kcall' => 1, 'in' => 2, 'out' => 1,
		'symbol' => 'X_enbc' },
	'_ENBS'		=> { 'kcall' => 1, 'in' => 1, 'out' => 1,
		'symbol' => 'X_enbs' },
	'ENBT'		=> { 'in' => 2, 'out' => 1,
		'generator' => \&gen_enb1 },
	'ENBC'		=> { 'in' => 2, 'out' => 1,
		'generator' => \&gen_enb1 },
	'ENBS'		=> { 'in' => 1, 'out' => 1,
		'generator' => \&gen_enb1 },
	'ENBT3'		=> { 'in' => 3, 'out' => 1,
		'generator' => \&gen_enb3 },
	'ENBC3'		=> { 'in' => 3, 'out' => 1,
		'generator' => \&gen_enb3 },
	'ENBS3'		=> { 'in' => 2, 'out' => 1,
		'generator' => \&gen_enb3 },
	'DISC'		=> { 'kcall' => 1, 'in' => 3, 'out' => 1,
		'symbol' => 'X_disc' },
	'NDISC'		=> { 'kcall' => 1, 'in' => 3, 'out' => 1,
		'symbol' => 'X_ndisc' },
	'DIST'		=> { 'kcall' => 1, 'in' => 3, 'out' => 1,
		'symbol' => 'X_dist' },
	'NDIST'		=> { 'kcall' => 1, 'in' => 3, 'out' => 1,
		'symbol' => 'X_ndist' },
	'DISS'		=> { 'kcall' => 1, 'in' => 2, 'out' => 1,
		'symbol' => 'X_diss' },
	'NDISS'		=> { 'kcall' => 1, 'in' => 2, 'out' => 1,
		'symbol' => 'X_ndiss' },
	# Mobile Processes
	'LDWSMAP'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'X_ldwsmap' },
	'ULWSMAP'	=> { 'kcall' => 1, 'in' => 2,
		'symbol' => 'X_ulwsmap' },
	'RMWSMAP'	=> { 'kcall' => 1, 'in' => 1,
		'symbol' => 'X_rmwsmap' },
	# External Channels
	'EXTVRFY' 	=> { 'kcall' => 1, 'in' => 2 },
	'EXTIN'		=> { 'kcall' => 1, 'in' => 3 },
	'EXTOUT'	=> { 'kcall' => 1, 'in' => 3 },
	'EXT_MT_IN'	=> { 'kcall' => 1, 'in' => 2 },
	'EXT_MT_OUT'	=> { 'kcall' => 1, 'in' => 2 },
};


sub new ($$) {
	my ($class) = @_;
	my $self = bless {}, $class;
	$self->{'aliases'}	= {};
	$self->{'constants'}	= {};
	$self->{'globals'}	= {};
	$self->{'header'}	= {};
	$self->{'source_files'}	= {};
	$self->{'source_file'}	= undef;
	$self->{'source_line'} 	= 0;
	return $self;
}

sub message ($$$) {
	my ($self, $warning, $msg) = @_;
	my $file = $self->{'constants'}->{$self->source_file}->{'str'};
	my $line = $self->source_line;

	if ($warning) {
		print STDERR "$file:$line $msg\n";
		die if $warning > 1;
	} else {
		print "$file:$line $msg\n";
	}
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

sub resolve_inst_label ($$$$) {
	my ($labels, $label, $inst, $fn) = @_;
	
	return if $inst->{'name'} =~ /^\..*BYTES$/;
	
	my $arg = $inst->{'arg'};
	foreach my $arg (ref ($arg) =~ /^ARRAY/ ? @$arg : $arg) {
		if ($arg =~ /^L([0-9_\.]+)$/) {
			my $num	= $1;
			my $n	= 'L' . $fn . '.' . $num;
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
				$globals->{$n}->{'refs'}++;
			} elsif (exists ($ffi->{$n})) {
				$inst->{'arg'} = $ffi->{$n};
				$ffi->{$n}->{'refs'}++;
			} else {
				#die "Undefined global reference $n";
			}
		}
	}
}

sub resolve_globals ($$$$) {
	my ($labels, $label, $globals, $ffi) = @_;
	foreach_inst ($labels, $label, \&resolve_inst_globals, $globals, $ffi);
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

sub isolate_branches ($$) {
	my ($labels, $label) = @_;
	
	return if $label->{'data'};

	my @inst	= @{$label->{'inst'}};
	my $sub_idx	= 0;
	my $current	= $label;
	my $cinst	= [];
	for (my $i = 0; $i < @inst; ++$i) {
		my $inst = $inst[$i];
		my $data = $GRAPH->{$inst->{'name'}};
		if ($data && ($data->{'branching'} || $data->{'kcall'})) {
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

sub expand_etc_ops ($) {
	my ($etc) = @_;
	my %IGNORE_SPECIAL = (
		'CONTRJOIN'	=> 1,
		'CONTRSPLIT'	=> 1
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
					#$l1->{'arg'} 	= [ $l1->{'arg'}, 'LDPI' ];
					#splice (@$etc, $i, 1, 
					#	$l1,
					#	{ 'name' => 'LDPI' }
					#);
					splice (@$etc, $i, 1, $l1);
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
				$op->{'name'}		= 'TABLE';
				$op->{'arg'}		= \@arg;
				$op->{'label_arg'}	= 1;
				$op->{'table'}		= \@table;
				splice (@$etc, $i, @arg + 1,
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
				{ 'name' => $name, 'arg' => "L$start"			},
				{ 'name' => '.SETLAB', 'arg' => $end			}
			);
		} elsif ($name =~ /^\.SL([RL])IMM$/) {
			$op->{'name'} = "XSH$1";
		}
	}
}

sub preprocess_etc ($$$) {
	my ($self, $file, $etc) = @_;
	my ($current, %labels, @procs);
	my $globals	= $self->{'globals'};
	
	my $fn		= 0;
	my $align	= 0;
	my $filename	= undef;
	my $line	= undef;
	my $global	= undef; # present global
	my $n_inst	= 0;     # "real" instructions in present/last global

	# Initial operation translation
	expand_etc_ops ($etc);

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
			my $label = 'L' . $fn . '.' . $arg;
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
			my $symbol = $arg;
			if ($current->{'global'}) {
				$symbol = $current->{'global'};
			} else {
				$symbol = $current->{'name'} . '__' . $symbol;
			}
			$current->{'symbol'} = $symbol;
			push (@procs, $current);
		} elsif ($name eq '.STUBNAME') {
			$current->{'stub'}	= $arg;
			$current->{'symbol'}	= $arg;
			#if ($arg =~ /^(C|BX?)\./) {
			#	$ffi{$arg} = $current
			#		if !exists ($ffi{$arg});
			#}
		} elsif ($name eq '.GLOBAL') {
			if (exists ($globals->{$arg})) {
				my $current 	= $globals->{$arg};
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
			$globals->{$arg}	= $current;
			$current->{'global'}	= $arg;
			$current->{'loci'}	= {
				'file'		=> $file,
				'filename'	=> $filename,
				'line'		=> $line
			};
		} elsif ($name eq '.GLOBALEND') {
			$globals->{$arg}->{'end'} 	= $current;
			$global				= undef;
		} elsif ($name eq '.JUMPENTRY') {
			if ($global && !$n_inst) {
				$self->{'aliases'}->{$global} = $arg;
			}
		} elsif ($name !~ /^\./) {
			$n_inst++;
		}
		
		push (@{$current->{'inst'}}, $op)
			if $current; 
	}

	$self->{'file'}		= $file;
	$self->{'filename'}	= $filename;
	$self->{'labels'}	= \%labels;
	$self->{'procs'}	= \@procs;

	foreach_label (\%labels, \&resolve_labels, 0);
	foreach_label (\%labels, \&resolve_globals, $globals, {});
	foreach_label (\%labels, \&build_data_blocks);
	foreach_label (\%labels, \&add_data_lengths);
	foreach_label (\%labels, \&isolate_branches);
	tag_and_index_code_blocks (\@procs);
	separate_code_blocks (\@procs);
}

sub define_registers ($$) {
	my ($self, $labels) = @_;
	my ($reg_n, $freg_n, $wptr_n) 	= (0, 0, 0);
	my $wptr			= sprintf ('wptr_%d', $wptr_n++);
	my (@stack, @fstack);

	foreach my $label (@$labels) {
		#print $label->{'name'}, " ", join (', ', @stack, @fstack), " ($wptr)\n";
		my $carry 	= 0;
		my $generated 	= 0;

		$label->{'in'} = [ @stack ];
		$label->{'fin'} = [ @fstack ];
		$label->{'wptr'} = $wptr;

		for (my $i = 0; $i < @{$label->{'inst'}}; ++$i) {
			my $inst = $label->{'inst'}->[$i];
			my $name = $inst->{'name'};
			
			if ($name =~ /^\./) {
				# Special Operations
				my $arg = $inst->{'arg'};
				if ($name eq '.FUNCRETURN') {
					$carry = $arg;
				} elsif ($name eq '.FUNCRESULTS') {
					my @regs;
					for (my $i = 0; $i < $arg; ++$i) {
						push (@regs, sprintf ('reg_%d', $reg_n++));
					}
					$label->{'in'} = [ @regs ];
					@stack = @regs;
				} elsif (!$generated && $name eq '.REALRESULT') {
					# $arg = 1 is single, $arg = 2 is double
					my $reg = sprintf ('freg_%d', $freg_n++);
					$label->{'fin'} = [ $reg ];
					@fstack 	= ( $reg );
				} elsif ($name eq '.TSDEPTH') {
					while (@stack > $arg) {
						pop (@stack);
					}
				}
				next;
			}
			
			$generated++;

			splice (@{$label->{'inst'}}, $i, 1, 
				{ 'name' => '.STACKS', 'stack' => [ @stack ], 'fstack' => [ @fstack ] },
				$inst
			);
			$i += 1;
			
			my $data	= $GRAPH->{$name};
			my $in 		= $name eq 'RET' ? $carry : $data->{'in'};
			my $out 	= $data->{'out'};
			my (@in, @out, @fin, @fout);

			for (my $i = 0; $i < $in; ++$i) {
				my $reg = shift (@stack);
				push (@in, $reg) if $reg;
			}
			for (my $i = 0; $i < $data->{'fin'}; ++$i) {
				my $reg = shift (@fstack);
				push (@fin, $reg) if $reg;
			}
			
			if ($name eq 'CJ') {
				$out = @in - ($in - $out);
			}
			
			for (my $i = 0; $i < $out; ++$i) {
				my $reg = sprintf ('reg_%d', $reg_n++);
				unshift (@out, $reg);
				unshift (@stack, $reg);
			}
			for (my $i = 0; $i < $data->{'fout'}; ++$i) {
				my $reg = sprintf ('freg_%d', $freg_n++);
				unshift (@fout, $reg);
				unshift (@fstack, $reg);
			}
			
			$inst->{'in'} = \@in;
			$inst->{'out'} = \@out;
			$inst->{'fin'} = \@fin;
			$inst->{'fout'} = \@fout;
			$inst->{'wptr'} = $wptr;
			
			if ($data->{'wptr'}) {
				$wptr = sprintf ('wptr_%d', $wptr_n++);
				$inst->{'_wptr'} = $wptr;
			}
			if (0) {
				print "\t";
				print join (', ', @in, @fin), " => " if @in || @fin;
				print $name;
				if ($inst->{'label_arg'}) {
					print ' ', $inst->{'arg'}->{'name'};
				}
				print " => ", join (', ', @out, @fout) if @out || @fout;
				if ($data->{'wptr'}) {
					print " (", $inst->{'wptr'}, ' => ', $inst->{'_wptr'}, ")";
				}
				print "\n";
			}
			@stack = @stack[0..2] if @stack > 3;
			@fstack = @fstack[0..2] if @fstack > 3;
		}
		
		$label->{'out'} = [ @stack ];
		$label->{'fout'} = [ @fstack ];
		$label->{'_wptr'} = $wptr;
	}
}	

sub build_phi_nodes ($$) {
	my ($self, $labels) = @_;

	foreach my $label (@$labels) {
		my $lname = $label->{'name'};

		foreach my $inst (@{$label->{'inst'}}) {
			next if $inst->{'name'} ne 'CJ';
			my $tlabel = $inst->{'arg'};
			$tlabel->{'phi'} = {} if !$tlabel->{'phi'};
			$tlabel->{'phi'}->{$lname} = [ $label->{'wptr'}, @{$inst->{'in'}} ];
		}
	}
}

sub build_label_types ($$) {
	my ($self, $labels) = @_;

	foreach my $label (@$labels) {
		my $name = $label->{'name'};
		
		my @param;
		foreach my $reg (@{$label->{'in'}}) {
			push (@param, $self->int_type);
		}
		foreach my $reg (@{$label->{'fin'}}) {
			push (@param, $self->float_type);
		}
		
		$label->{'type'} = sprintf ('void (%s, %s%s%s)',
			$self->sched_type, $self->workspace_type,
			@param ? ', ' : '',
			join (', ', @param)
		);
	}
}

sub define_rounding ($$) {
	my ($self, $labels) = @_;
	my $mode = 'nearest';

	foreach my $label (@$labels) {
		foreach my $inst (@{$label->{'inst'}}) {
			my $name = $inst->{'name'};
			if ($name eq 'FPRZ') {
				$mode = 'zero';
			} elsif ($name eq 'FPRM') {
				$mode = 'nearest';
			} elsif ($name eq 'FPRP') {
				$mode = '+infinity';
			} elsif ($name eq 'FPRN') {
				$mode = '-infinity';
			} elsif ($name =~ /^FP/) {
				$inst->{'rounding'} = $mode;
				$mode = 'nearest';
			}
		}
	}
}

sub output_regs (@) {
	my (@regs) = @_;
	return if !@regs;
	my @out;
	foreach my $regs (@regs) {
		foreach my $reg (@$regs) {
			push (@out, '%' . $reg);
		}
	}
	return join (', ', @out);
}

sub proc_prefix {
	my $self = shift;
	return 'O_';
}

sub symbol_to_proc_name ($$) {
	my ($self, $symbol) = @_;
	$symbol =~ s/\./_/gs;
	$symbol =~ s/[\^\*\&\@\$]//gs;
	$symbol =~ s/\%.*$//;
	return $self->proc_prefix . $symbol;
}

sub int_type {
	my $self = shift;
	return 'i32'; # FIXME:
}

sub short_type {
	my $self = shift;
	return 'i16';
}

# virtual type
sub uint_type {
	my $self = shift;
	return 'uint';
}

sub long_type {
	my $self = shift;
	my $type = $self->int_type;
	if ($type eq 'i32') {
		return 'i64';
	} elsif ($type eq 'i16') {
		return 'i32';
	} elsif ($type eq 'i64') {
		return 'i128'; # FIXME: err...
	}
	die "unknown integer type $type";
}

sub int_length {
	my $self = shift;
	my $type = shift || $self->int_type;
	if ($type eq 'i16') {
		return 2;
	} elsif ($type eq 'i32') {
		return 4;
	} elsif ($type eq 'i64') {
		return 8;
	}
	die "unknown integer type $type";
}

sub int_bits {
	my $self = shift;
	return $self->int_length * 8;
}

sub index_type {
	my $self = shift;
	if ($self->int_type eq 'i64') {
		return 'i64';
	} else {
		return 'i32';
	}
}

sub fp_single_type {
	my $self = shift;
	return 'float';
}

sub fp_double_type {
	my $self = shift;
	return 'double';
}

sub float_type {
	my $self = shift;
	return 'double';
}

sub is_float_type {
	my ($self, $type) = @_;
	return ($type =~ /^(float|double|fp128)$/) ? 1 : 0;
}

sub intrinsic_type ($$) {
	my ($self, $type) = @_;
	if ($type eq 'float') {
		return 'f32';
	} elsif ($type eq 'double') {
		return 'f64';
	} else {
		return $type;
	}
}

sub float_length {
	my $self = shift;
	my $type = shift || $self->int_type;
	if ($type eq 'float') {
		return 4;
	} elsif ($type eq 'double') {
		return 8;
	} elsif ($type eq 'fp128') {
		return 16;
	}
	die "unknown float type $type";
}

sub sched_type {
	my $self = shift;
	return 'i8*';
}

sub func_type {
	my $self = shift;
	return sprintf ('void (%s, %s)', $self->sched_type, $self->workspace_type)
}

sub workspace_type {
	my $self = shift;
	return $self->int_type . '*';
}

sub null {
	my $self = shift;
	return 0;
}

sub infinity {
	my $self = shift;
	my $type = shift || $self->float_type;
	if ($type eq 'float') {
		return ('0xff800000', '0x7f800000');
	} elsif ($type eq 'double') {
		return ('0xfff0000000000000', '0x7ff0000000000000');
	} else {
		die "infinity not defined for type $type";
	}
}

sub reset_tmp ($) {
	my $self = shift;
	$self->{'tmp_n'} = 0;
}

sub tmp_label ($) {
	my $self = shift;
	my $n = $self->{'tmp_n'}++;
	return "tmp_$n";
}

sub tmp_reg ($) {
	my $self = shift;
	my $n = $self->{'tmp_n'}++;
	return "tmp_$n";
}

sub new_constant ($$$) {
	my ($self, $name, $value) = @_;
	die "Trying to create duplicate constant" if exists ($self->{'constants'}->{$name});
	$self->{'constants'}->{$name} = $value;
}

sub new_generated_constant ($$) {
	my ($self, $value) = @_;
	my $name = sprintf ('C_%d', $self->{'constant_n'}++);
	$self->new_constant ($name, $value);
	return $name;
}

sub source_file {
	my ($self, $file) = @_;
	if (defined ($file)) {
		$self->{'source_file'} = $self->{'source_files'}->{$file};
		if (!$self->{'source_file'}) {
			my $constant = $self->new_generated_constant (
				{ 'str' => $file }
			);
			$self->{'source_files'}->{$file} = $constant;
			$self->{'source_file'} = $constant;
		}
	}
	return $self->{'source_file'};
}

sub source_line {
	my ($self, $line) = @_;
	if (defined ($line)) {
		$self->{'source_line'} = $line;
	}
	return $self->{'source_line'};
}

sub int_constant ($$) {
	my ($self, $int) = @_;
	my $int_type = $self->int_type;
	return sprintf ('bitcast %s %d to %s', $int_type, $int, $int_type)
}

sub single_assignment ($$$$) {
	my ($self, $type, $src, $dst) = @_;
	return sprintf ('%%%s = bitcast %s %%%s to %s', $dst, $type, $src, $type);
}

sub global_ptr_value ($$$) {
	my ($self, $type, $global) = @_;
	my $tmp_reg = $self->tmp_reg ();
	return ($tmp_reg, 
		sprintf ('%%%s = load %s* @%s', $tmp_reg, $type, $global)
	);
}

sub declare_proc ($$) {
	my ($self, $name) = @_;
	my $symbol = $name;

	$symbol =~ s/^[@%]//;
	$name 	= '@' . $name if $name !~ /^[@%]/;

	$self->{'header'}->{$symbol} = [
		sprintf ('declare fastcc void %s (%s, %s)', 
			$name, $self->sched_type, $self->workspace_type
		)
	] if !exists ($self->{'header'}->{$symbol});
}

sub _gen_error ($$$$$) {
	my ($self, $proc, $label, $inst, $type) = @_;
	my ($source_reg, $source_asm) = $self->global_ptr_value ('i8*', $self->source_file);
	my $symbol = 'etc_error_' . $type;
	$self->{'header'}->{$symbol} = [ 
		sprintf ('declare void @%s (%s, %s, i8*, %s)',
			$symbol, $self->sched_type, $self->workspace_type, $self->int_type
	)];
	return (
		$source_asm,
		sprintf ('call void @%s (%s %%sched, %s %%%s, i8* %%%s, %s %s)',
			$symbol,
			$self->sched_type,
			$self->workspace_type, $inst->{'wptr'},
			$source_reg,
			$self->int_type, $self->source_line
		)
	);
}

sub gen_j ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $arg 	= $inst->{'arg'};
	my @params 	= (
		sprintf ('%s %%sched', $self->sched_type),
		sprintf ('%s %%%s', $self->workspace_type, $inst->{'wptr'})
	);

	if ($arg->{'in'}) {
		for (my $i = 0; $i < @{$arg->{'in'}}; ++$i) {
			push (@params, sprintf ('%s %%%s', $self->int_type, $inst->{'in'}->[$i]));
		}
	}
	if ($arg->{'out'}) {
		for (my $i = 0; $i < @{$arg->{'fin'}}; ++$i) {
			push (@params, sprintf ('%s %%%s', $self->float_type, $inst->{'fin'}->[$i]));
		}
	}
	
	return (
		sprintf ('tail call fastcc void %s (%s) noreturn',
			$proc->{'call_prefix'} . $arg->{'name'},
			join (', ', @params)
		)
	);
}

sub gen_cj ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $next_label 	= $label->{'next'};
	my $arg		= $inst->{'arg'};
	my $tmp_reg 	= $self->tmp_reg ();
	my $jump_label 	= $self->tmp_label ();
	my $cont_label 	= $self->tmp_label ();
	my $in		= $inst->{'in'};
	my $fin		= $inst->{'fin'};
	my $a_in	= $arg->{'in'}; 
	my $a_fin	= $arg->{'fin'};
	my (@asm, @cont_in);
	
	if (@$in < @$a_in || @$fin < @$a_fin) {
		my $msg = sprintf (
			'ERROR: not enough registers to fill conditional jump target stack (%d,%d < %d,%d)',
			scalar (@$in), scalar (@$fin),  
			scalar (@$a_in), scalar (@$a_fin) 
		);
		$self->message (1, $msg);
		push (@asm, "; $msg");
	}

	@cont_in = ( @$in );
	shift (@cont_in);

	return (
		@asm,
		sprintf ('%%%s = icmp eq %s %%%s, %d',
			$tmp_reg,
			$self->int_type, $in->[0],
			0
		),
		sprintf ('br i1 %%%s, label %%%s, label %%%s',
			$tmp_reg,
			$jump_label, $cont_label
		),
		$jump_label . ':',
		$self->gen_j ($proc, $label, {
			'in' 	=> $in,
			'fin'	=> $fin,
			'wptr'	=> $inst->{'wptr'},
			'arg'	=> $arg
		}),
		'ret void',
		$cont_label . ':',
		$self->gen_j ($proc, $label, {
			'in'	=> \@cont_in,
			'fin'	=> $next_label->{'fin'},
			'wptr' 	=> $inst->{'wptr'},
			'arg'	=> $next_label
		})
	);
}

sub gen_call ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	my $name 	= $inst->{'name'};
	my $wptr 	= $inst->{'wptr'};
	my $in		= $inst->{'in'};
	my $symbol 	= ref ($inst->{'arg'}) =~ /HASH/ ? $inst->{'arg'}->{'symbol'} : '';
	my $ffi		= ($symbol =~ m/^(C|BX?)\./)[0];
	my $reschedule	= $inst->{'reschedule'};
	my $tail_call	= 1;
	my (@asm, @params, $new_wptr);
	
	if ($inst->{'fin'} && @{$inst->{'fin'}}) {
		my $msg = "WARNING: floating point stack is not empty at point of call";
		$self->message (1, $msg);
		push (@asm, "; $msg");
	}

	if ($name =~ /^G?CALL$/ || $reschedule) {
		my $iptr_offset = 0;

		$new_wptr = $self->tmp_reg ();

		if ($reschedule) {
			$iptr_offset = -1;
		} elsif ($name eq 'CALL') {
			$iptr_offset = -4;
		}

		push (@asm, sprintf ('%%%s = getelementptr %s %%%s, %s %d',
			$new_wptr,
			$self->workspace_type, $wptr,
			$self->index_type,
			$iptr_offset
		));

		if ($ffi ne 'C') {
			my $next_label 	= $label->{'next'};
			my $ret_ptr 	= $self->tmp_reg ();
			my $ret_val 	= $self->tmp_reg ();
			push (@asm, sprintf ('%%%s = bitcast %s %s to i8*',
				$ret_ptr,
				$next_label->{'type'} . '*' ,
				$proc->{'call_prefix'} . $next_label->{'name'}
			));
			push (@asm, sprintf ('%%%s = ptrtoint i8* %%%s to %s',
				$ret_val, 
				$ret_ptr,
				$self->int_type
			));
			push (@asm, sprintf ('store %s %%%s, %s %%%s',
				$self->int_type, $ret_val,
				$self->workspace_type, $new_wptr
			));
		}

		if ($name eq 'CALL') {
			for (my $i = 0; $i < @$in; ++$i) {
				my $tmp_reg = $self->tmp_reg ();
				push (@asm, sprintf ('%%%s = getelementptr %s %%%s, %s %d',
					$tmp_reg,
					$self->workspace_type, $new_wptr,
					$self->index_type, ($i + 1)
				));
				push (@asm, sprintf ('store %s %%%s, %s %%%s',
					$self->int_type, $in->[$i],
					$self->workspace_type, $tmp_reg
				));
			}
		} elsif (($name eq 'GCALL') && @$in > 1) {
			my $msg = "WARNING: stack has excess operands at point of general call";
			$self->message (1, $msg);
			push (@asm, "; $msg");
		}
	}
	
	if ($ffi) {
		$symbol =~ s/^(C|BX?)//;
		$symbol =~ s/\./_/g;

		$self->{'header'}->{$symbol} = [
			sprintf ('declare void @%s (%s)', 
				$symbol, $self->workspace_type
			)
		] if !exists ($self->{'header'}->{$symbol});

		my $args_ptr = $self->tmp_reg ();
		push (@asm, sprintf ('%%%s = getelementptr %s %%%s, %s %d',
			$args_ptr,
			$self->workspace_type, $new_wptr,
			$self->index_type, 1
		));
		$in = [ $args_ptr ];
		
		if ($ffi =~ /B/) {
			my $args_val = $self->tmp_reg ();
			my $func_ptr = $self->tmp_reg ();
			my $func_val = $self->tmp_reg ();
			
			push (@asm, sprintf ('%%%s = ptrtoint %s %%%s to %s',
				$args_val,
				$self->workspace_type, $in->[0],
				$self->int_type
			));
			push (@asm, sprintf ('%%%s = bitcast void (%s)* @%s to i8*',
				$func_ptr, 
				$self->workspace_type, $symbol
			));
			push (@asm, sprintf ('%%%s = ptrtoint i8* %%%s to %s',
				$func_val,
				$func_ptr,
				$self->int_type
			));

			$in = [ $func_val, $args_val ];

			# Transform CALL into kernel call

			if ($ffi eq 'B') {
				$name = 'Y_b_dispatch';
			} elsif ($ffi eq 'BX') {
				$name = 'Y_bx_dispatch';
			}
			
			$name 		= $self->def_kcall ($name, { 'in' => $in });
			$ffi 		= undef;
			$reschedule	= 1;
		} else {
			$tail_call = 0;
		}
	}

	if ($name !~ /^G?CALL$/) {
		foreach my $param (@$in) {
			push (@params, sprintf ('%s %%%s',
				$self->int_type, $param
			));
		}
	}

	if ($name eq 'GCALL') {
		my $jump_ptr = $self->tmp_reg ();
		push (@asm, sprintf ('%%%s = inttoptr %s %%%s to %s',
			$jump_ptr,
			$self->int_type, $in->[0],
			$self->func_type . '*'
		));
		push (@asm, sprintf ('tail call fastcc void %%%s (%s %%sched, %s %%%s) noreturn',
			$jump_ptr,
			$self->sched_type,
			$self->workspace_type,
			$wptr
		));
	} elsif ($name eq 'CALL') {
		if (!$ffi) {
			my $symbol = $self->symbol_to_proc_name ($symbol);
			
			$self->declare_proc ($symbol);
			
			push (@asm, sprintf ('tail call fastcc void @%s (%s %%sched, %s %%%s) noreturn',
				$symbol,
				$self->sched_type,
				$self->workspace_type,
				$new_wptr
			));
		} else {
			push (@asm, sprintf ('call void @%s (%s %%%s)',
				$symbol,
				$self->workspace_type,
				$in->[0]
			));
		}	
	} else {
		my $ret = $inst->{'out'} && @{$inst->{'out'}} ? $inst->{'out'}->[0] : $self->tmp_reg ();

		push (@asm, sprintf ('%%%s = call %s @%s (%s %%sched, %s %%%s%s%s)',
			$ret,
			$reschedule ? $self->workspace_type : $self->int_type,
			$name,
			$self->sched_type,
			$self->workspace_type,
			$ffi eq 'C' ? $new_wptr : $wptr,
			@params ? ', ' : '',
			join (', ', @params)
		));
		
		if ($reschedule) {
			my $jump_ptr_ptr = $self->tmp_reg ();
			my $jump_ptr 	= $self->tmp_reg ();
			my $jump_val 	= $self->tmp_reg ();
			my $new_wptr	= $ret;
			push (@asm, sprintf ('%%%s = getelementptr %s %%%s, %s %d',
				$jump_ptr_ptr,
				$self->workspace_type, $new_wptr,
				$self->index_type, -1
			));
			push (@asm, sprintf ('%%%s = load %s %%%s',
				$jump_val, 
				$self->workspace_type, $jump_ptr_ptr
			));
			push (@asm, sprintf ('%%%s = inttoptr %s %%%s to void (%s, %s)*',
				$jump_ptr, 
				$self->int_type, $jump_val, 
				$self->sched_type, $self->workspace_type
			));
			push (@asm, sprintf ('tail call fastcc void %%%s (%s %%sched, %s %%%s) noreturn',
				$jump_ptr,
				$self->sched_type,
				$self->workspace_type, $new_wptr
			));
		} else {
			$tail_call = $inst->{'no_tail'};
		}
	}

	if (!$tail_call) {
		my $next_label = $label->{'next'};
		push (@asm, $self->gen_j ($proc, $label, {
			'in'	=> $next_label->{'in'},
			'fin'	=> $next_label->{'fin'},
			'wptr' 	=> $next_label->{'wptr'},
			'arg'	=> $next_label
		}));
	}

	return @asm;
}	

sub gen_table ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $prefix	= $self->tmp_label () . '_';
	my $otherwise 	= $prefix . 'otherwise';
	my $targets 	= $inst->{'arg'};
	my (@asm, @jump_asm, @jump_labels);
	
	for (my $i = 0; $i < @$targets; ++$i) {
		my $target 	= $targets->[$i];
		my $lab 	= $prefix . $i;
		push (@jump_labels, $lab);
		push (@jump_asm, $lab . ':');
		push (@jump_asm, $self->gen_j ($proc, $label, 
			{ 'arg' => $target, 'wptr' => $inst->{'wptr'} }
		));
		push (@jump_asm, 'ret void');
	}

	push (@asm, sprintf ('switch %s %%%s, label %%%s [ %s %d, label %%%s',
		$self->int_type,
		$inst->{'in'}->[0],
		$otherwise,
		$self->int_type, 0, $jump_labels[0]
	));
	for (my $i = 1; $i < @jump_labels; ++$i) {
		push (@asm, sprintf ('  %s %d, label %%%s%s',
			$self->int_type, $i, $jump_labels[$i],
			$i == (@jump_labels - 1) ? ' ]' : ''
		));
	}
	push (@asm, @jump_asm);
	push (@asm, $otherwise . ':');
	push (@asm, 'unreachable');
	push (@asm, $self->gen_j ($proc, $label, 
		{ 'arg' => $label->{'next'}, 'wptr' => $inst->{'wptr'} }
	));

	return @asm;
}

sub gen_ajw ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return sprintf ('%%%s = getelementptr %s %%%s, %s %d',
		$inst->{'_wptr'},
		$self->workspace_type, $inst->{'wptr'},
		$self->index_type, $inst->{'arg'}
	);
}

sub gen_gajw ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return (
		sprintf ('%%%s = ptrtoint %s %%%s to %s',
			$inst->{'out'}->[0], 
			$self->workspace_type, $inst->{'wptr'},
			$self->int_type
		),
		sprintf ('%%%s = inttoptr %s %%%s to %s',
			$inst->{'_wptr'}, 
			$self->int_type, $inst->{'in'}->[0],
			$self->workspace_type
		)
	);
}

sub gen_ldc ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $arg = $inst->{'arg'};
	if (ref ($arg)) {
		my $name 	= $arg->{'name'};
		my (@load, $tmp_reg);

		if ($arg->{'data'}) {
			$self->new_constant ($name, $arg)
				if !exists ($self->{'constants'}->{$name});
			($tmp_reg, @load) = $self->global_ptr_value ('i8*', $name);
		} else {
			my $symbol;
			
			if ($arg->{'proc'} eq $proc) {
				$symbol = $proc->{'call_prefix'} . $name;
			} elsif (($arg->{'proc'} eq $arg) || $arg->{'stub'}) {
				$symbol = '@' . $self->symbol_to_proc_name ($arg->{'symbol'});
				$self->declare_proc ($symbol);
			} else {
				$self->message (2, 
					"attempting to reference a label in a different process..."
				);
			}

			$tmp_reg = $self->tmp_reg ();
			@load = ( sprintf ('%%%s = bitcast %s %s to i8*',
				$tmp_reg, 
				($arg->{'type'} ? $arg->{'type'} : $self->func_type) . '*',
				$symbol
			));
		}

		return (
			@load,
			sprintf ('%%%s = ptrtoint i8* %%%s to %s',
				$inst->{'out'}->[0], 
				$tmp_reg,
				$self->int_type
			)
		);
	} else {
		return sprintf ('%%%s = %s',
			$inst->{'out'}->[0],
			$self->int_constant ($arg)
		);
	}
}

sub gen_ldinf ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return $self->int_constant (0x7f800000);
}

sub gen_mint ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $type = $self->int_type;
	my $bits = ($type =~ /i(\d+)/)[0];
	my $mint = -(1 << ($bits - 1));
	return $self->gen_ldc ($proc, $label, {
		'out' => $inst->{'out'},
		'arg' => $mint
	});
}

sub gen_null ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return $self->gen_ldc ($proc, $label, {
		'out' => $inst->{'out'},
		'arg' => $self->null
	});
}

sub _gen_ldlp ($$) {
	my ($self, $inst) = @_;
	my ($reg, @asm);
	if ($inst->{'arg'} != 0) {
		$reg = $self->tmp_reg ();
		push (@asm, 
			sprintf ('%%%s = getelementptr %s %%%s, %s %d',
				$reg, 
				$self->workspace_type, $inst->{'wptr'},
				$self->index_type, $inst->{'arg'}
		));
	} else {
		$reg = $inst->{'wptr'};
	}
	return ($reg, @asm);
}

sub gen_ldlp ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my ($tmp_reg, @asm) = $self->_gen_ldlp ($inst);
	my $conv = sprintf ('%%%s = ptrtoint %s %%%s to %s',
		$inst->{'out'}->[0],
		$self->workspace_type,
		$tmp_reg,
		$self->int_type
	);
	return (@asm, $conv);
}

sub gen_ldl ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my ($tmp_reg, @asm) = $self->_gen_ldlp ($inst);
	my $load = sprintf ('%%%s = load %s %%%s',
		$inst->{'out'}->[0],
		$self->workspace_type, $tmp_reg
	);
	return (@asm, $load);
}	

sub gen_stl ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	my ($tmp_reg, @asm) = $self->_gen_ldlp ($inst);
	my $store = sprintf ('store %s %%%s, %s %%%s',
		$self->int_type, $inst->{'in'}->[0],
		$self->workspace_type, $tmp_reg
	);
	return (@asm, $store);
}

sub _gen_ldnlp ($@) {
	my ($self, $inst, $out_reg) = @_;
	my (@asm, $ptr_reg);
	
	$out_reg = $self->tmp_reg () if !$out_reg;
	
	if ($inst->{'arg'} != 0) {
		$ptr_reg = $self->tmp_reg ();
	} else {
		$ptr_reg = $out_reg;
	}

	push (@asm,
		sprintf ('%%%s = inttoptr %s %%%s to %s',
			$ptr_reg, 
			$self->int_type, $inst->{'in'}->[0], 
			$self->int_type . '*'
		)
	);
	if ($inst->{'arg'} != 0) {
		push (@asm, 
			sprintf ('%%%s = getelementptr %s %%%s, %s %d',
				$out_reg, 
				$self->int_type . '*', $ptr_reg,
				$self->index_type, $inst->{'arg'}
		));
	}
	
	return ($out_reg, @asm);
}

sub gen_ldnlp ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my ($reg, @asm) = $self->_gen_ldnlp ($inst);
	my $to_int = sprintf ('%%%s = ptrtoint %s* %%%s to %s',
		$inst->{'out'}->[0],
		$self->int_type, $reg,
		$self->int_type
	);
	return (@asm, $to_int);
}

sub gen_ldnl ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my ($tmp_reg, @asm) = $self->_gen_ldnlp ($inst);
	my $load = sprintf ('%%%s = load %s %%%s',
		$inst->{'out'}->[0],
		$self->workspace_type, $tmp_reg
	);
	return (@asm, $load);
}	

sub gen_stnl ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	my ($tmp_reg, @asm) = $self->_gen_ldnlp ($inst);
	my $store = sprintf ('store %s %%%s, %s %%%s',
		$self->int_type, $inst->{'in'}->[1],
		$self->workspace_type, $tmp_reg
	);
	return (@asm, $store);
}

sub gen_dup ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $f = $inst->{'name'} =~ /^F/ ? 'f' : '';
	my @asm;
	my $in = $inst->{$f . 'in'}->[0];
	foreach my $reg (@{$inst->{$f . 'out'}}) {
		push (@asm, $self->single_assignment (
			$f ? $self->float_type : $self->int_type, $in, $reg
		)); 
	}
	return @asm;
}

sub _gen_checked_arithmetic ($$$$$) {
	my ($self, $proc, $label, $inst, $func) = @_;
	my @asm;
	my $in		= $inst->{'in'};
	my $res 	= $self->tmp_reg ();
	my $overflow 	= $self->tmp_reg ();
	my $tmp		= $self->tmp_label ();
	my $error_lab	= $tmp . '_overflow_error';
	my $ok_lab	= $tmp . '_ok';

	push (@asm, sprintf ('%%%s = call {%s, i1} %s (%s %%%s, %s %%%s)',
		$res, 
		$self->int_type,
		$func,
		$self->int_type, $in->[1],
		$self->int_type, $in->[0]
	));
	push (@asm, sprintf ('%%%s = extractvalue {%s, i1} %%%s, 0',
		$inst->{'out'}->[0],
		$self->int_type,
		$res
	));
	push (@asm, sprintf ('%%%s = extractvalue {%s, i1} %%%s, 1',
		$overflow,
		$self->int_type,
		$res
	));
	push (@asm, sprintf ('br i1 %%%s, label %%%s, label %%%s',
		$overflow,
		$error_lab,
		$ok_lab
	));
	push (@asm, $error_lab . ':');
	push (@asm, $self->_gen_error ($proc, $label, $inst, 'overflow'));
	push (@asm, sprintf ('br label %%%s', $ok_lab));
	push (@asm, $ok_lab . ':');

	return @asm;
}

sub gen_add ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return $self->_gen_checked_arithmetic (
		$proc, $label, $inst,
		'@llvm.sadd.with.overflow.' . $self->int_type
	);
}

sub gen_adc ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $tmp_reg = $self->tmp_reg ();
	my @ldc = $self->gen_ldc ($proc, $label, { 
		'arg' 	=> $inst->{'arg'},
		'out' 	=> [ $tmp_reg ]
	});
	my @add = $self->gen_add ($proc, $label, {
		'wptr'	=> $inst->{'wptr'},
		'in' 	=> [ @{$inst->{'in'}}, $tmp_reg ],
		'out'	=> $inst->{'out'}
	});
	return (@ldc, @add);
}

sub gen_sub ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return $self->_gen_checked_arithmetic (
		$proc, $label, $inst,
		'@llvm.ssub.with.overflow.' . $self->int_type
	);
}

sub gen_mul ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return $self->_gen_checked_arithmetic (
		$proc, $label, $inst,
		'@llvm.smul.with.overflow.' . $self->int_type
	);
}

sub gen_sum ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return sprintf ('%%%s = add %s %%%s, %%%s',
		$inst->{'out'}->[0],
		$self->int_type,
		$inst->{'in'}->[0], $inst->{'in'}->[1]
	);
}	

sub gen_diff ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return sprintf ('%%%s = sub %s %%%s, %%%s',
		$inst->{'out'}->[0],
		$self->int_type,
		$inst->{'in'}->[1], $inst->{'in'}->[0]
	);
}	

sub gen_prod ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return sprintf ('%%%s = mul %s %%%s, %%%s',
		$inst->{'out'}->[0],
		$self->int_type,
		$inst->{'in'}->[0], $inst->{'in'}->[1]
	);
}	

sub gen_divrem ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $mint 	= $self->tmp_reg ();
	my $cond_a_0	= $self->tmp_reg ();
	my $cond_a_m1	= $self->tmp_reg ();
	my $cond_b_mint	= $self->tmp_reg ();
	my $cond_error0	= $self->tmp_reg ();
	my $cond_error1	= $self->tmp_reg ();
	my $tmp		= $self->tmp_label ();
	my $error_lab	= $tmp . '_div_error';
	my $ok_lab	= $tmp . '_ok';
	my @asm;

	push (@asm, $self->gen_mint ($proc, $label, { 'out' => [ $mint ] }));
	push (@asm, sprintf ('%%%s = icmp eq %s %%%s, -1',
		$cond_a_m1, 
		$self->int_type, $inst->{'in'}->[0]
	));
	push (@asm, sprintf ('%%%s = icmp eq %s %%%s, %%%s',
		$cond_b_mint, 
		$self->int_type, $inst->{'in'}->[1],
		$mint
	));
	push (@asm, sprintf ('%%%s = and i1 %%%s, %%%s',
		$cond_error0, 
		$cond_a_m1, $cond_b_mint
	));
	push (@asm, sprintf ('%%%s = icmp eq %s %%%s, 0',
		$cond_a_0, 
		$self->int_type, $inst->{'in'}->[0]
	));
	push (@asm, sprintf ('%%%s = or i1 %%%s, %%%s',
		$cond_error1, 
		$cond_error0, $cond_a_0
	));
	push (@asm, sprintf ('br i1 %%%s, label %%%s, label %%%s',
		$cond_error1,
		$error_lab, $ok_lab
	));
	push (@asm, $error_lab . ':');
	push (@asm, $self->_gen_error ($proc, $label, $inst, 'div'));
	push (@asm, sprintf ('br label %%%s', $ok_lab));
	push (@asm, $ok_lab . ':');
	push (@asm, sprintf ('%%%s = %s %s %%%s, %%%s',
		$inst->{'out'}->[0],
		($inst->{'name'} eq 'REM' ? 'srem' : 'sdiv'),
		$self->int_type,
		$inst->{'in'}->[1], $inst->{'in'}->[0]
	));

	return @asm;
}

sub gen_rev ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	my $f 		= $inst->{'name'} =~ /^F/ ? 'f' : '';
	my $type 	= $f ? $self->float_type : $self->int_type;
	return (
		$self->single_assignment ($type, $inst->{$f . 'in'}->[0], $inst->{$f . 'out'}->[1]),
		$self->single_assignment ($type, $inst->{$f . 'in'}->[1], $inst->{$f . 'out'}->[0])
	);
}

sub gen_eqc ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	my $tmp_reg = $self->tmp_reg ();
	return (
		sprintf ('%%%s = icmp eq %s %%%s, %d', 
			$tmp_reg, 
			$self->int_type,
			$inst->{'in'}->[0], $inst->{'arg'}
		),
		sprintf ('%%%s = zext i1 %%%s to %s', 
			$inst->{'out'}->[0], 
			$tmp_reg,
			$self->int_type
		)
	);
}

sub gen_gt ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	my $tmp_reg = $self->tmp_reg ();
	return (
		sprintf ('%%%s = icmp sgt %s %%%s, %%%s', 
			$tmp_reg,
			$self->int_type,
			$inst->{'in'}->[1], $inst->{'in'}->[0]
		),
		sprintf ('%%%s = zext i1 %%%s to %s', 
			$inst->{'out'}->[0], 
			$tmp_reg,
			$self->int_type
		)
	);
}

sub _gen_bitop ($$$@) { 
	my ($self, $inst, $op, $a, $b, undef) = @_;
	if ($b =~ /[^\d-]/) {
		return sprintf ('%%%s = %s %s %%%s, %%%s',
			$inst->{'out'}->[0],
			$op,
			$self->int_type, $a,
			$b
		);
	} else {
		return sprintf ('%%%s = %s %s %%%s, %d',
			$inst->{'out'}->[0],
			$op,
			$self->int_type, $a,
			$b
		);
	}
}

sub gen_not ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	return $self->_gen_bitop ($inst, 'xor', @{$inst->{'in'}}, -1);
}

sub gen_xor ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	return $self->_gen_bitop ($inst, 'xor', @{$inst->{'in'}});
}

sub gen_or ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	return $self->_gen_bitop ($inst, 'or', @{$inst->{'in'}});
}

sub gen_and ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	return $self->_gen_bitop ($inst, 'and', @{$inst->{'in'}});
}

sub gen_shift ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	my $op 		= $inst->{'name'} =~ /L$/ ? 'shl' : 'lshr';
	my $val 	= @{$inst->{'in'}} == 2 ? $inst->{'in'}->[1] : $inst->{'in'}->[0];
	my $shift 	= @{$inst->{'in'}} == 2 ? $inst->{'in'}->[0] : $inst->{'arg'};
	
	if ($shift =~ /^[A-Z]/i) {
		# Dynamic shift, tests required to check it is valid at runtime
		my $valid_shift	= $self->tmp_reg ();
		my $res		= $self->tmp_reg ();
		my $tmp		= $self->tmp_label ();
		my $set_lab	= $tmp . '_set';
		my $shift_lab	= $tmp . '_shift';
		my $cont_lab	= $tmp . '_continue';
		return (
			sprintf ('%%%s = icmp ult %s %%%s, %d',
				$valid_shift, $self->int_type, $shift, $self->int_bits
			),
			sprintf ('br i1 %%%s, label %%%s, label %%%s',
				$valid_shift, $shift_lab, $set_lab
			),

			$shift_lab . ':',
			$self->_gen_bitop ({ 'out' => [ $res ] }, $op, $val, $shift),
			sprintf ('br label %%%s', $cont_lab),
			
			$set_lab . ':',
			sprintf ('br label %%%s', $cont_lab),

			$cont_lab . ':',
			sprintf ('%%%s = phi %s [ 0, %%%s ], [ %%%s, %%%s ]',
				$inst->{'out'}->[0],
				$self->int_type,
				$set_lab,
				$res, $shift_lab
			)
		);
	} elsif ($shift >= 0 && $shift < $self->int_bits) {
		# Valid static shift
		return $self->_gen_bitop ($inst, $op, $val, $shift);
	} else {
		# Value shifted out of range
		return $self->gen_ldc ($proc, $label, {
			'arg' => 0, 
			'out' => $inst->{'out'} 
		});
	}
}

sub gen_boolinvert ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $tmp = $self->tmp_reg ();
	return (
		$self->_gen_bitop ({ 'out' => [ $tmp ] }, 'and', @{$inst->{'in'}}, 1),
		$self->_gen_bitop ($inst, 'xor', $tmp, 1)
	);
}

sub gen_nop ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	return ();
}

sub gen_ret ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $jump_val = $self->tmp_reg ();
	my $jump_ptr = $self->tmp_reg ();
	my $new_wptr = $self->tmp_reg ();
	my @types = ( $self->sched_type, $self->workspace_type );
	my @param = ( 
		sprintf ('%s %%sched', $self->sched_type) ,
		sprintf ('%s %%%s', $self->workspace_type, $new_wptr)
	);
	my @asm;	

	foreach my $key ([ 'in', $self->int_type ], [ 'fin', $self->float_type ]) {
		my ($key, $type) = @$key;
		next if !$inst->{$key};
		foreach my $reg (@{$inst->{$key}}) {
			push (@types, $type);
			push (@param, sprintf ('%s %%%s', $type, $reg));
		}
	}

	push (@asm, sprintf ('%%%s = load %s %%%s',
		$jump_val, 
		$self->workspace_type, $inst->{'wptr'}
	));
	push (@asm, sprintf ('%%%s = inttoptr %s %%%s to void (%s)*',
		$jump_ptr, 
		$self->int_type, $jump_val,
		join (', ', @types)
	));
	push (@asm, sprintf ('%%%s = getelementptr %s %%%s, %s %d',
		$new_wptr,
		$self->workspace_type, $inst->{'wptr'},
		$self->index_type, 4
	));
	push (@asm, sprintf ('tail call fastcc void %%%s (%s) noreturn',
		$jump_ptr,
		join (', ', @param)
	));
	
	return @asm;
}

sub gen_csub0 ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $error 	= $self->tmp_reg ();
	my $tmp		= $self->tmp_label ();
	my $error_lab	= $tmp . '_bounds_error';
	my $ok_lab	= $tmp . '_ok';
	my @asm;
	push (@asm, $self->single_assignment (
		$self->int_type, $inst->{'in'}->[1], $inst->{'out'}->[0]
	));
	push (@asm, sprintf ('%%%s = icmp uge %s %%%s, %%%s',
		$error, $self->int_type, $inst->{'out'}->[0], $inst->{'in'}->[0]
	));
	push (@asm, sprintf ('br i1 %%%s, label %%%s, label %%%s',
		$error, $error_lab, $ok_lab
	));
	push (@asm, $error_lab . ':');
	push (@asm, $self->_gen_error ($proc, $label, $inst, 'bounds'));
	push (@asm, sprintf ('br label %%%s', $ok_lab));
	push (@asm, $ok_lab . ':');

	return @asm;
}

sub gen_bsub ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	return $self->gen_sum ($proc, $label, $inst);
}

sub gen_wsub ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	my $tmp_reg = $self->tmp_reg ();
	return (
		sprintf ('%%%s = mul %s %%%s, %d',
			$tmp_reg,
			$self->int_type, $inst->{'in'}->[1],
			$self->int_length * ($inst->{'name'} eq 'WSUBDB' ? 2 : 1)
		),
		$self->gen_sum ($proc, $label, {
			'wptr' 	=> $inst->{'wptr'},
			'in'	=> [ $inst->{'in'}->[0], $tmp_reg ],
			'out'	=> $inst->{'out'}
		})
	);
}

sub gen_lb ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	my $byte_ptr = $self->tmp_reg ();
	my $byte_val = $self->tmp_reg ();
	return (
		sprintf ('%%%s = inttoptr %s %%%s to i8*',
			$byte_ptr, 
			$self->int_type, $inst->{'in'}->[0], 
		),
		sprintf ('%%%s = load i8* %%%s',
			$byte_val, $byte_ptr
		),
		sprintf ('%%%s = zext i8 %%%s to %s',
			$inst->{'out'}->[0],
			$byte_val, $self->int_type
		)
	);
}

sub gen_sb ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	my $byte_ptr = $self->tmp_reg ();
	my $byte_val = $self->tmp_reg ();
	return (
		sprintf ('%%%s = inttoptr %s %%%s to i8*',
			$byte_ptr, 
			$self->int_type, $inst->{'in'}->[0], 
		),
		sprintf ('%%%s = trunc %s %%%s to i8',
			$byte_val, 
			$self->int_type, $inst->{'in'}->[1]
		),
		sprintf ('store i8 %%%s, i8* %%%s',
			$byte_val, $byte_ptr
		)
	);
}

sub gen_seterr ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return $self->_gen_error ($proc, $label, $inst, 'set');
}

sub gen_lend ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $block_ptr		= $self->tmp_reg ();
	my $index_ptr		= $self->tmp_reg ();
	my $block_cnt		= $self->tmp_reg ();
	my $new_block_cnt	= $self->tmp_reg ();
	my $index_cnt		= $self->tmp_reg ();
	my $new_index_cnt	= $self->tmp_reg ();
	my $loop_cond		= $self->tmp_reg ();
	my $loop_lab		= $self->tmp_label ();
	my $next_lab		= $self->tmp_label ();
	my @asm;
	
	# Generate pointer to index block
	push (@asm, sprintf ('%%%s = inttoptr %s %%%s to %s',
		$index_ptr, $self->int_type, $inst->{'in'}->[0], $self->int_type . '*'
	));
	# Decrement index
	push (@asm, sprintf ('%%%s = getelementptr %s %%%s, %s %d',
		$block_ptr,
		$self->int_type . '*', $index_ptr,
		$self->index_type, 1
	));
	push (@asm, sprintf ('%%%s = load %s %%%s',
		$block_cnt,
		$self->int_type . '*', $block_ptr
	));
	push (@asm, sprintf ('%%%s = sub %s %%%s, %d',
		$new_block_cnt,
		$self->int_type, $block_cnt, 1
	));
	push (@asm, sprintf ('store %s %%%s, %s %%%s',
		$self->int_type, $new_block_cnt,
		$self->int_type . '*', $block_ptr
	));

	# Compare index to zero
	push (@asm, sprintf ('%%%s = icmp ugt %s %%%s, %d',
		$loop_cond,
		$self->int_type, $new_block_cnt, 0
	));
	push (@asm, sprintf ('br i1 %%%s, label %%%s, label %%%s',
		$loop_cond, $loop_lab, $next_lab
	));

	# index > 0, Keep looping
	push (@asm, $loop_lab . ':');
	
	# Update count
	push (@asm, sprintf ('%%%s = load %s %%%s',
		$index_cnt,
		$self->int_type . '*', $index_ptr
	));

	if ($inst->{'name'} eq 'LEND') {
		push (@asm, sprintf ('%%%s = add %s %%%s, %d',
			$new_index_cnt,
			$self->int_type, $index_cnt, 1
		));
	} elsif ($inst->{'name'} eq 'LENDB') {
		push (@asm, sprintf ('%%%s = sub %s %%%s, %d',
			$new_index_cnt,
			$self->int_type, $index_cnt, 1
		));
	} else {
		my $step_ptr = $self->tmp_reg ();
		my $step_val = $self->tmp_reg ();
		push (@asm, sprintf ('%%%s = getelementptr %s %%%s, %s %d',
			$step_ptr,
			$self->int_type . '*', $index_ptr,
			$self->index_type, 2
		));
		push (@asm, sprintf ('%%%s = load %s %%%s',
			$step_val,
			$self->int_type . '*', $step_ptr
		));
		push (@asm, sprintf ('%%%s = add %s %%%s, %%%s',
			$new_index_cnt,
			$self->int_type, $index_cnt, $step_val
		));
	}

	push (@asm, sprintf ('store %s %%%s, %s %%%s',
		$self->int_type, $new_index_cnt,
		$self->int_type . '*', $index_ptr
	));

	# Loop
	push (@asm, $self->gen_j ($proc, $label, 
		{ 'wptr' => $inst->{'wptr'}, 'arg' => $inst->{'arg'} }
	));
	push (@asm, 'ret void');
	
	# index = 0, Exit loop
	push (@asm, $next_lab . ':');
	push (@asm, $self->gen_j ($proc, $label, 
		{ 'wptr' => $inst->{'wptr'}, 'arg' => $label->{'next'} }
	));

	return @asm;
}

sub def_kcall ($$$) {
	my ($self, $symbol, $inst) = @_;
	my $name 	= 'kernel_' . $symbol;
	my $reschedule	= $symbol =~ /^Y_/ ? 1 : 0;

	if (!exists ($self->{'header'}->{$name})) {
		my @param = ($self->sched_type, $self->workspace_type);
		
		if ($inst->{'in'}) {
			for (my $i = 0; $i < @{$inst->{'in'}}; ++$i) {
				push (@param, $self->int_type);
			}
		}
		
		$self->{'header'}->{$name} = [ sprintf (
			'declare %s @%s (%s)',
			$reschedule ? $self->workspace_type : $self->int_type, 
			$name, join (', ', @param)
		) ];
	}

	return $name;
}

sub _gen_kcall ($$$$$) {
	my ($self, $proc, $label, $inst, $symbol) = @_;
	my $reschedule 	= $symbol =~ /^Y_/ ? 1 : 0;
	
	die "Unknown kernel call " . $inst->{'name'} . " ($symbol)" if $symbol !~ /^[XY]_/;

	my $name = $self->def_kcall ($symbol, $inst);

	return $self->gen_call ($proc, $label, {
		'name' 		=> $name,
		'in'		=> $inst->{'in'},
		'out'		=> $inst->{'out'},
		'wptr'		=> $inst->{'wptr'},
		'reschedule' 	=> $reschedule,
		'no_tail'	=> $inst->{'no_tail'}
	});
}

sub gen_kcall ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $data = $GRAPH->{$inst->{'name'}};
	return $self->_gen_kcall ($proc, $label, $inst, $data->{'symbol'});
}

sub _gen_long ($$$$) {
	my ($self, $lo, $hi, $out) = @_;
	my $long_lo = $self->tmp_reg ();
	my $long_hi = $self->tmp_reg ();
	my $long_tmp = $self->tmp_reg ();
	return (
		sprintf ('%%%s = zext %s %%%s to %s',
			$long_lo, $self->int_type, $lo, $self->long_type
		),
		sprintf ('%%%s = zext %s %%%s to %s',
			$long_hi, $self->int_type, $hi, $self->long_type
		),
		sprintf ('%%%s = shl %s %%%s, %d',
			$long_tmp, $self->long_type, $long_hi, $self->int_bits
		),
		sprintf ('%%%s = or %s %%%s, %%%s',
			$out, $self->long_type, $long_lo, $long_tmp
		)
	);
}

sub gen_xdble ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return (
		$self->single_assignment (
			$self->int_type, $inst->{'in'}->[0], $inst->{'out'}->[0]
		),
		sprintf ('%%%s = ashr %s %%%s, %d',
			$inst->{'out'}->[1],
			$self->int_type, $inst->{'in'}->[0],
			$self->int_bits - 1
		)
	);
}

sub _gen_csngl {
	my ($self, $proc, $label, $inst, $val, $val_trunc) = @_;

	$val_trunc = $self->tmp_reg () if !$val_trunc;

	my $val_ext	= $self->tmp_reg ();
	my $error_cond	= $self->tmp_reg ();
	my $tmp		= $self->tmp_label ();
	my $error_lab	= $tmp . '_overflow_error';
	my $ok_lab	= $tmp . '_ok';

	return (
		sprintf ('%%%s = trunc %s %%%s to %s',
			$val_trunc, $self->long_type, $val, $self->int_type
		),
		sprintf ('%%%s = sext %s %%%s to %s',
			$val_ext, $self->int_type, $val_trunc, $self->long_type
		),
		sprintf ('%%%s = icmp ne %s %%%s, %%%s',
			$error_cond, $self->long_type, $val, $val_ext
		),
		sprintf ('br i1 %%%s, label %%%s, label %%%s',
			$error_cond, $error_lab, $ok_lab
		),
		
		$error_lab . ':',
		$self->_gen_error ($proc, $label, $inst, 'overflow'),
		sprintf ('br label %%%s', $ok_lab),
		
		$ok_lab . ':'
	);
}

sub gen_csngl ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $long_val = $self->tmp_reg ();
	return (
		$self->_gen_long ($inst->{'in'}->[0], $inst->{'in'}->[1], $long_val),
		$self->_gen_csngl ($proc, $label, $inst, $long_val, $inst->{'out'}->[0])
	);
}	

sub gen_laddsub ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $long_val_a 	= $self->tmp_reg ();
	my $long_val_b 	= $self->tmp_reg ();
	my $long_val_c 	= $self->tmp_reg ();
	my $carry	= $self->tmp_reg ();
	my $res_tmp	= $self->tmp_reg ();
	my $res		= $self->tmp_reg ();
	my $out		= $inst->{'out'}->[0];
	my $op		= $inst->{'name'} eq 'LADD' ? 'add' : 'sub';
	return (
		sprintf ('%%%s = sext %s %%%s to %s',
			$long_val_a, $self->int_type, $inst->{'in'}->[0], $self->long_type
		),
		sprintf ('%%%s = sext %s %%%s to %s',
			$long_val_b, $self->int_type, $inst->{'in'}->[1], $self->long_type
		),	
		sprintf ('%%%s = trunc %s %%%s to i1',
			$carry, $self->int_type, $inst->{'in'}->[2]
		),
		sprintf ('%%%s = zext i1 %%%s to %s',
			$long_val_c, $carry, $self->long_type
		),
		sprintf ('%%%s = %s %s %%%s, %%%s',
			$res_tmp, $op, $self->long_type, $long_val_b, $long_val_a
		),
		sprintf ('%%%s = %s %s %%%s, %%%s',
			$res, $op, $self->long_type, $res_tmp, $long_val_c
		),
		$self->_gen_csngl ($proc, $label, $inst, $res, $out)
	);
}

sub gen_ldiffsum ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	
	my $res_1	= $self->tmp_reg ();
	my $b_op_a	= $self->tmp_reg ();
	my $carry_1	= $self->tmp_reg ();
	my $c_tmp	= $self->tmp_reg ();
	my $res_2	= $self->tmp_reg ();
	my $carry_2	= $self->tmp_reg ();
	my $carry	= $self->tmp_reg ();
	my $op		= $inst->{'name'} eq 'LDIFF' ? 'usub' : 'uadd';
	return (
		sprintf ('%%%s = call { %s, i1 } @llvm.%s.with.overflow.%s (%s %%%s, %s %%%s)',
			$res_1, 
			$self->int_type, $op, $self->int_type, 
			$self->int_type, $inst->{'in'}->[1],
			$self->int_type, $inst->{'in'}->[0]
		),
		sprintf ('%%%s = extractvalue { %s, i1 } %%%s, 0',
			$b_op_a, $self->int_type, $res_1
		),
		sprintf ('%%%s = extractvalue { %s, i1 } %%%s, 1',
			$carry_1, $self->int_type, $res_1
		),
		sprintf ('%%%s = and %s %%%s, 1',
			$c_tmp, $self->int_type, $inst->{'in'}->[2]
		),
		sprintf ('%%%s = call { %s, i1 } @llvm.%s.with.overflow.%s (%s %%%s, %s %%%s)',
			$res_2, 
			$self->int_type, $op, $self->int_type, 
			$self->int_type, $b_op_a,
			$self->int_type, $c_tmp
		),
		sprintf ('%%%s = extractvalue { %s, i1 } %%%s, 0',
			$inst->{'out'}->[0], $self->int_type, $res_2
		),
		sprintf ('%%%s = extractvalue { %s, i1 } %%%s, 1',
			$carry_2, $self->int_type, $res_2
		),
		sprintf ('%%%s = or i1 %%%s, %%%s',
			$carry, $carry_1, $carry_2
		),
		sprintf ('%%%s = zext i1 %%%s to %s',
			$inst->{'out'}->[1], $carry, $self->int_type
		)
	);
}

sub gen_ldiv ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $overflow	= $self->tmp_reg ();
	my $long_val 	= $self->tmp_reg ();
	my $long_a 	= $self->tmp_reg ();
	my $div_res	= $self->tmp_reg ();
	my $rem_res	= $self->tmp_reg ();
	my $tmp		= $self->tmp_label ();
	my $error_lab	= $tmp . '_overflow_error';
	my $ok_lab	= $tmp . '_ok';
	return (
		sprintf ('%%%s = icmp uge %s %%%s, %%%s',
			$overflow, $self->int_type, $inst->{'in'}->[2], $inst->{'in'}->[0]
		),
		sprintf ('br i1 %%%s, label %%%s, label %%%s',
			$overflow, $error_lab, $ok_lab
		),

		$error_lab . ':',
		$self->_gen_error ($proc, $label, $inst, 'overflow'),
		sprintf ('br label %%%s', $ok_lab),
		
		$ok_lab . ':',
		$self->_gen_long ($inst->{'in'}->[1], $inst->{'in'}->[2], $long_val),
		sprintf ('%%%s = zext %s %%%s to %s',
			$long_a, $self->int_type, $inst->{'in'}->[0], $self->long_type
		),
		sprintf ('%%%s = udiv %s %%%s, %%%s',
			$div_res, $self->long_type, $long_val, $long_a
		),
		sprintf ('%%%s = urem %s %%%s, %%%s',
			$rem_res, $self->long_type, $long_val, $long_a
		),
		sprintf ('%%%s = trunc %s %%%s to %s',
			$inst->{'out'}->[0], $self->long_type, $div_res, $self->int_type
		),
		sprintf ('%%%s = trunc %s %%%s to %s',
			$inst->{'out'}->[1], $self->long_type, $rem_res, $self->int_type
		)
	);
}

sub _gen_long_split ($$$$) {
	my ($self, $in, $lo, $hi) = @_;
	my $long_tmp = $self->tmp_reg ();
	return (
		sprintf ('%%%s = trunc %s %%%s to %s',
			$lo, $self->long_type, $in, $self->int_type
		),
		sprintf ('%%%s = lshr %s %%%s, %d',
			$long_tmp, $self->long_type, $in, $self->int_bits
		),
		sprintf ('%%%s = trunc %s %%%s to %s',
			$hi, $self->long_type, $long_tmp, $self->int_type
		)
	);
}

sub gen_lmul ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $long_val_a 	= $self->tmp_reg ();
	my $long_val_b 	= $self->tmp_reg ();
	my $long_val_c 	= $self->tmp_reg ();
	my $mul_res	= $self->tmp_reg ();
	my $sum_res	= $self->tmp_reg ();
	return (
		sprintf ('%%%s = zext %s %%%s to %s',
			$long_val_a, $self->int_type, $inst->{'in'}->[0], $self->long_type
		),
		sprintf ('%%%s = zext %s %%%s to %s',
			$long_val_b, $self->int_type, $inst->{'in'}->[1], $self->long_type
		),	
		sprintf ('%%%s = zext %s %%%s to %s',
			$long_val_c, $self->int_type, $inst->{'in'}->[2], $self->long_type
		),
		sprintf ('%%%s = mul %s %%%s, %%%s',
			$mul_res, $self->long_type, $long_val_a, $long_val_b
		),
		sprintf ('%%%s = add %s %%%s, %%%s',
			$sum_res, $self->long_type, $mul_res, $long_val_c
		),
		$self->_gen_long_split ($sum_res, $inst->{'out'}->[0], $inst->{'out'}->[1])
	);
}

sub gen_lshift ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $long_val 	= $self->tmp_reg ();
	my $shift	= $self->tmp_reg ();
	my $long_res 	= $self->tmp_reg ();
	my $long_tmp 	= $self->tmp_reg ();
	return (
		$self->_gen_long ($inst->{'in'}->[1], $inst->{'in'}->[2], $long_val),
		sprintf ('%%%s = zext %s %%%s to %s',
			$shift, $self->int_type, $inst->{'in'}->[0], $self->long_type
		),
		sprintf ('%%%s = %s %s %%%s, %%%s',
			$long_res, 
			$inst->{'name'} eq 'LSHR' ? 'lshr' : 'shl',
			$self->long_type, $long_val, $shift
		),
		$self->_gen_long_split ($long_res, $inst->{'out'}->[0], $inst->{'out'}->[1])
	);
}

sub gen_i64toreal ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $ptr		= $inst->{'in'}->[0];
	my $lo		= $self->tmp_reg ();
	my $hi		= $self->tmp_reg ();
	my $long_val 	= $self->tmp_reg ();
	return (
		$self->gen_ldnl ($proc, $label, {
			'in'	=> [ $ptr ],
			'out'	=> [ $lo ],
			'arg'	=> 0
		}),
		$self->gen_ldnl ($proc, $label, {
			'in'	=> [ $ptr ],
			'out'	=> [ $hi ],
			'arg'	=> 1
		}),
		$self->_gen_long ($lo, $hi, $long_val),
		$self->numeric_conversion (
			$self->long_type, $long_val,
			$self->float_type, $inst->{'fout'}->[0],
			$inst->{'rounding'}
		)
	);
}

sub numeric_conversion {
	my ($self, $src_type, $src, $dst_type, $dst, $rounding) = @_;

	if ($src_type ne $dst_type) {
		my $src_float = $self->is_float_type ($src_type);
		my $dst_float = $self->is_float_type ($dst_type);
		my (@asm, $op);
		
		if ($src_float && $dst_float) {
			$op = 	$self->float_length ($src_type) < $self->float_length ($dst_type) 
				? 'fpext' : 'fptrunc'
		} elsif ($src_float) {
			$rounding = 'nearest' if !$rounding;
			if ($rounding eq 'nearest') {
				my $neg_cond	= $self->tmp_reg ();
				my $neg_val	= $self->tmp_reg ();
				my $add_val	= $self->tmp_reg ();
				my $n_src 	= $self->tmp_reg ();
				push (@asm, 
					sprintf ('%%%s = fcmp ult %s %%%s, 0.0',
						$neg_cond, $src_type, $src
					),
					sprintf ('%%%s = uitofp i1 %%%s to %s',
						$neg_val, $neg_cond, $src_type
					),
					sprintf ('%%%s = fsub %s 0.5, %%%s',
						$add_val, $src_type, $neg_val
					),
					sprintf ('%%%s = fadd %s %%%s, %%%s',
						$n_src, $src_type, $src, $add_val
					)
				);
				$src = $n_src;
			}
			$op = 'fptosi';
		} elsif ($dst_float) {
			if ($src_type eq $self->uint_type) {
				$src_type 	= $self->int_type;
				$op		= 'uitofp';
			} else {
				$op 		= 'sitofp';
			}
		} else {
			$op = 	$self->int_length ($src_type) < $self->int_length ($dst_type) 
				? 'sext' : 'trunc'
		}
		
		return (
			@asm,
			sprintf ('%%%s = %s %s %%%s to %s',
				$dst, $op, $src_type, $src, $dst_type
			)
		);
	} else {
		return $self->single_assignment ($src_type, $src, $dst);
	}
}

sub gen_fpldnl ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $type	= $inst->{'name'} =~ /SN/ ? 
		$self->fp_single_type : $self->fp_double_type;
	my $d_ptr 	= $self->tmp_reg ();
	my $d_val	= $self->tmp_reg ();
	my @asm;

	push (@asm, sprintf ('%%%s = inttoptr %s %%%s to %s*',
		$d_ptr, 
		$self->int_type, $inst->{'in'}->[0],
		$type
	));
	if ($inst->{'name'} =~ /I$/) {
		my $d_ptr_tmp 	= $d_ptr;
		$d_ptr 		= $self->tmp_reg ();
		push (@asm, sprintf ('%%%s = getelementptr %s* %%%s, %s %%%s',
			$d_ptr,
			$type, $d_ptr_tmp,
			$self->int_type, $inst->{'in'}->[1]
		));
	};

	return (
		@asm,
		sprintf ('%%%s = load %s* %%%s',
			$d_val,
			$type, $d_ptr
		),
		$self->numeric_conversion (
			$type, $d_val, 
			$self->float_type, $inst->{'fout'}->[0],
			$inst->{'rounding'}
		)
	);
}

sub gen_fpstnl ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $d_ptr 	= $self->tmp_reg ();
	my $d_val	= $self->tmp_reg ();
	my $type;

	if ($inst->{'name'} =~ /SN/) {
		$type = $self->fp_single_type;
	} elsif ($inst->{'name'} =~ /I32/) {
		$type = $self->int_type;
	} else {
		$type = $self->fp_double_type;
	}

	return (
		$self->numeric_conversion (
			$self->float_type, $inst->{'fin'}->[0], 
			$type, $d_val,
			$inst->{'rounding'}
		),
		sprintf ('%%%s = inttoptr %s %%%s to %s*',
			$d_ptr, 
			$self->int_type, $inst->{'in'}->[0],
			$type
		),
		sprintf ('store %s %%%s, %s* %%%s',
			$type, $d_val, 
			$type, $d_ptr
		),
	);
}

sub gen_fpi32to ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $name 	= $inst->{'name'};
	my $src_type 	= $name =~ /b32/ ? 
		$self->uint_type : $self->int_type;
	my $dst_type 	= $name =~ /r32/ ? 
		$self->fp_single_type : $self->fp_double_type;
	my $int_val 	= $self->tmp_reg ();
	my $fp_val	= $self->tmp_reg ();
	return (
		$self->gen_ldnl ($proc, $label, {
			'in' 	=> $inst->{'in'},
			'out' 	=> [ $int_val ],
			'arg'	=> 0
		}),
		$self->numeric_conversion (
			$src_type, $int_val,
			$dst_type, $fp_val,
			$inst->{'rounding'}
		),
		$self->numeric_conversion (
			$dst_type, $fp_val,
			$self->float_type, $inst->{'fout'}->[0],
			$inst->{'rounding'}
		)
	);
}

sub gen_fpchki ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $val = $inst->{'fin'}->[0];
	my @asm;

	if ($inst->{'fout'}) {
		my $out = $inst->{'fout'}->[0];
		push (@asm, $self->single_assignment (
			$self->float_type, $val, $out
		));
		$val = $out;
	}

	my $cmp_lower 	= $self->tmp_reg ();
	my $cmp_upper 	= $self->tmp_reg ();
	my $error_cond	= $self->tmp_reg ();
	my $tmp		= $self->tmp_label ();
	my $error_lab	= $tmp . '_overflow';
	my $ok_lab	= $tmp . '_ok';
	my ($bits)	= ($inst->{'name'} =~ m/(\d+)$/);

	return (
		@asm,
		sprintf ('%%%s = fcmp olt %s %%%s, %.1f',
			$cmp_lower, $self->float_type, $val, -(2 ** ($bits - 1))
		),
		sprintf ('%%%s = fcmp oge %s %%%s, %.1f',
			$cmp_upper, $self->float_type, $val, (2 ** ($bits - 1))
		),
		sprintf ('%%%s = or i1 %%%s, %%%s',
			$error_cond, $cmp_lower, $cmp_upper
		),
		sprintf ('br i1 %%%s, label %%%s, label %%%s',
			$error_cond, $error_lab, $ok_lab
		),

		$error_lab . ':',
		$self->_gen_error ($proc, $label, $inst, 'overflow'),
		sprintf ('br label %%%s', $ok_lab),

		$ok_lab . ':'
	);
}

sub gen_fprtoi32 ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $int_tmp = $self->tmp_reg ();
	return (
		$self->gen_fpint ($proc, $label, $inst),
		$self->gen_fpchki ($proc, $label, {
			'name'	=> 'FPCHKI32',
			'wptr'	=> $inst->{'wptr'},
			'fin'	=> $inst->{'fout'}
		})
	);
}

sub gen_fpnop ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my @asm;
	for (my $i = 0; $i < @{$inst->{'fin'}}; ++$i) {
		push (@asm, $self->single_assignment (
			$self->float_type, $inst->{'fin'}->[$i], $inst->{'fout'}->[$i]
		));
	}
	return @asm;
}

sub gen_fpldzero ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return sprintf ('%%%s = bitcast %s 0.0 to %s',
		$inst->{'fout'}->[0], $self->float_type, $self->float_type
	);
}

sub gen_fparithop ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my ($op) = ($inst->{'name'} =~ m/FP(.*)/);
	$op =~ tr/A-Z/a-z/;
	$op = 'f' . $op;
	return sprintf ('%%%s = %s %s %%%s, %%%s',
		$inst->{'fout'}->[0],
		$op,
		$self->float_type,
		$inst->{'fin'}->[1],
		$inst->{'fin'}->[0]
	);
}

sub gen_fprem ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $div_res	= $self->tmp_reg ();
	my $div_int	= $self->tmp_reg ();
	my $div_float	= $self->tmp_reg ();
	my $mul_res	= $self->tmp_reg ();
	return (
		$self->gen_fparithop ($proc, $label, {
			'name'	=> 'FPDIV',
			'fin'	=> $inst->{'fin'},
			'fout'	=> [ $div_res ]
		}),
		$self->numeric_conversion (
			$self->float_type, $div_res,
			$self->long_type, $div_int,
			'nearest'
		),
		$self->numeric_conversion (
			$self->long_type, $div_int,
			$self->float_type, $div_float,
			'nearest'
		),
		sprintf ('%%%s = fmul %s %%%s, %%%s',
			$mul_res,
			$self->float_type,
			$inst->{'fin'}->[0], $div_float
		),
		sprintf ('%%%s = fsub %s %%%s, %%%s',
			$inst->{'fout'}->[0],
			$self->float_type,
			$inst->{'fin'}->[1], $mul_res
		)
	);
}

sub gen_fpxby2 ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my ($op) = ($inst->{'name'} =~ /FP(.*)BY2/);
	$op =~ tr/A-Z/a-z/;
	$op = 'f' . $op;
	return sprintf ('%%%s = %s %s %%%s, 2.0',
		$inst->{'fout'}->[0],
		$op,
		$self->float_type,
		$inst->{'fin'}->[0]
	);
}

sub gen_fpsqrt ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $i_type 	= $self->intrinsic_type ($self->float_type);
	my $func 	= '@llvm.sqrt.' . $i_type;
	
	$self->{'header'}->{$func} = [
		sprintf ('declare %s %s (%s)',
			$self->float_type, $func, $self->float_type
		)
	];
	
	return sprintf ('%%%s = call %s %s (%s %%%s)',
		$inst->{'fout'}->[0],
		$self->float_type,
		$func,
		$self->float_type, $inst->{'fin'}->[0]
	);
}

sub gen_fpint ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $tmp = $self->tmp_reg ();
	return (
		$self->numeric_conversion (
			$self->float_type, $inst->{'fin'}->[0],
			$self->int_type, $tmp,
			$inst->{'rounding'}
		),
		$self->numeric_conversion (
			$self->int_type, $tmp,
			$self->float_type, $inst->{'fout'}->[0],
			$inst->{'rounding'}
		)
	);
}

sub gen_fpexpdec32 ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	return sprintf ('%%%s = fdiv %s %%%s, %.1f',
		$inst->{'fout'}->[0],
		$self->float_type,
		$inst->{'fin'}->[0],
		2.0 ** 32
	);
}

sub gen_fpabs ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $cmp		= $self->tmp_reg ();
	my $tmp 	= $self->tmp_label ();
	my $inv_label 	= $tmp . '_invert';
	my $nop_label 	= $tmp . '_do_nothing';
	my $cont_label 	= $tmp . '_continue';
	my $res1	= $self->tmp_reg ();
	my $res2	= $self->tmp_reg ();
	return (
		sprintf ('%%%s = fcmp oge %s %%%s, 0.0',
			$cmp,
			$self->float_type, $inst->{'fin'}->[0]
		),
		sprintf ('br i1 %%%s, label %%%s, label %%%s',
			$cmp,
			$nop_label, $inv_label
		),

		$nop_label . ':',
		$self->single_assignment ($self->float_type, $inst->{'fin'}->[0], $res1),
		sprintf ('br label %%%s', $cont_label),

		$inv_label . ':',
		sprintf ('%%%s = fsub %s 0.0, %%%s',
			$res2,
			$self->float_type, $inst->{'fin'}->[0]
		),
		sprintf ('br label %%%s', $cont_label),

		$cont_label . ':',
		sprintf ('%%%s = phi %s [ %%%s, %%%s ], [ %%%s, %%%s ]',
			$inst->{'fout'}->[0],
			$self->float_type,
			$res1, $nop_label,
			$res2, $inv_label
		)
	);
}

sub gen_fpnan ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $cmp = $self->tmp_reg ();
	# FIXME: this is probably not complete
	return (
		$self->single_assignment (
			$self->float_type, 
			$inst->{'fin'}->[0], $inst->{'fout'}->[0]
		),
		sprintf ('%%%s = fcmp uno %s %%%s, 0.0',
			$cmp,
			$self->float_type, $inst->{'fout'}->[0]
		),
		sprintf ('%%%s = zext i1 %%%s to %s',
			$inst->{'out'}->[0],
			$cmp,
			$self->int_type
		)
	);
}

sub gen_fpnotfinite ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my @infinity 	= $self->infinity;
	my $val 	= $inst->{'fout'}->[0];
	my (@asm, $cmp);
	push (@asm, $self->single_assignment (
		$self->float_type,
		$inst->{'fin'}->[0],
		$inst->{'fout'}->[0]
	));
	foreach my $infinity (@infinity) {
		my $this_cmp = $self->tmp_reg ();
		push (@asm, sprintf ('%%%s = fcmp ueq %s %%%s, %s',
			$this_cmp, $self->float_type, $val, $infinity
		));
		if ($cmp) {
			my $new_cmp = $self->tmp_reg ();
			push (@asm, sprintf ('%%%s = or i1 %%%s, %%%s',
				$new_cmp, $cmp, $this_cmp
			));
			$cmp = $new_cmp;
		} else {
			$cmp = $this_cmp;
		}
	}
	push (@asm, sprintf ('%%%s = zext i1 %%%s to %s',
		$inst->{'out'}->[0],
		$cmp,
		$self->int_type
	));
	return @asm;
}

sub gen_fpordered ($$$$) { 
	my ($self, $proc, $label, $inst) = @_;
	my $cmp = $self->tmp_reg ();
	return (
		$self->single_assignment (
			$self->float_type, 
			$inst->{'fin'}->[0], $inst->{'fout'}->[0]
		),
		$self->single_assignment (
			$self->float_type, 
			$inst->{'fin'}->[1], $inst->{'fout'}->[1]
		),
		sprintf ('%%%s = fcmp ord %s %%%s, %%%s',
			$cmp,
			$self->float_type, @{$inst->{'fout'}}
		),
		sprintf ('%%%s = zext i1 %%%s to %s',
			$inst->{'out'}->[0],
			$cmp,
			$self->int_type
		)
	);
}

sub gen_fpcmp ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $op = $inst->{'name'} =~ /GT/ ? 'ogt' : 'oeq';
	my $cmp = $self->tmp_reg ();
	return (
		sprintf ('%%%s = fcmp %s %s %%%s, %%%s',
			$cmp,
			$op, $self->float_type, 
			$inst->{'fin'}->[1], $inst->{'fin'}->[0]
		),
		sprintf ('%%%s = zext i1 %%%s to %s',
			$inst->{'out'}->[0],
			$cmp,
			$self->int_type
		)
	);
}

sub gen_fpldnlop ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $val = $self->tmp_reg ();
	my ($op) = ($inst->{'name'} =~ m/LDNL(.{3})/);
	$op =~ tr/A-Z/a-z/;
	$op = 'f' . $op;
	return (
		$self->gen_fpldnl ($proc, $label, {
			'name' 		=> $inst->{'name'},
			'in'		=> $inst->{'in'},
			'fout'		=> [ $val ],
			'rounding'	=> $inst->{'rounding'}
		}),
		sprintf ('%%%s = %s %s %%%s, %%%s',
			$inst->{'fout'}->[0],
			$op, $self->float_type,
			$inst->{'fin'}->[0], 
			$val
		)
	);
}

sub gen_fcossin ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $i_type 	= $self->intrinsic_type ($self->float_type);
	my $op = ($inst->{'name'} =~ /\d+([A-Z]{3})/)[0];
	$op =~ tr/A-Z/a-z/;
	my $func 	= '@llvm.' . $op . '.' . $i_type;
	
	$self->{'header'}->{$func} = [
		sprintf ('declare %s %s (%s)',
			$self->float_type, $func, $self->float_type
		)
	];
	
	return sprintf ('%%%s = call %s %s (%s %%%s)',
		$inst->{'fout'}->[0],
		$self->float_type,
		$func,
		$self->float_type, $inst->{'fin'}->[0]
	);
}

sub gen_norm ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	
	my $a		= $inst->{'in'}->[0];
	my $b		= $inst->{'in'}->[1];

	my $tmp 	= $self->tmp_label ();
	my $b0_lab 	= $tmp . '_b0';
	my $a_shift_lab = $tmp . '_a_shift';
	my $b_shift_lab = $tmp . '_b_shift';
	my $no_shift_lab= $tmp . '_no_shift';
	my $cont_lab 	= $tmp . '_continue';

	my $cmp_shift	= $self->tmp_reg ();
	my $cmp_b 	= $self->tmp_reg ();
	my $cmp_a 	= $self->tmp_reg ();

	my $a_shift_a_out	= $self->tmp_reg ();
	my $a_shift_b_out	= $self->tmp_reg ();
	my $a_shift_c_out	= $self->tmp_reg ();
	my $b_shift_a_out	= $self->tmp_reg ();
	my $b_shift_b_out	= $self->tmp_reg ();
	my $b_shift_c_out	= $self->tmp_reg ();
	my $a_shift_tmp		= $self->tmp_reg ();
	my $b_shift_tmp		= $self->tmp_reg ();
	my $b_shift_a_shift	= $self->tmp_reg ();
	my $b_shift_b_tmp0	= $self->tmp_reg ();
	my $b_shift_b_tmp1	= $self->tmp_reg ();

	return (
		sprintf ('%%%s = icmp eq %s %%%s, 0', 
			$cmp_b, $self->int_type, $b),
		sprintf ('br i1 %%%s, label %%%s, label %%%s', 
			$cmp_b, $b0_lab, $no_shift_lab),
		

		$b0_lab . ':',
		sprintf ('%%%s = icmp eq %s %%%s, 0',
			$cmp_a, $self->int_type, $a),
		sprintf ('br i1 %%%s, label %%%s, label %%%s',
			$cmp_a, $cont_lab, $a_shift_lab),
		

		$a_shift_lab . ':',
		sprintf ('%%%s = call %s @llvm.ctlz.%s (%s %%%s)',
			$a_shift_tmp,
			$self->int_type, $self->int_type, $self->int_type,
			$a
		),
		sprintf ('%%%s = bitcast %s 0 to %s',
			$a_shift_a_out,
			$self->int_type, $self->int_type
		),
		sprintf ('%%%s = shl %s %%%s, %%%s',
			$a_shift_b_out,
			$self->int_type, $a, $a_shift_tmp
		),
		sprintf ('%%%s = add %s %%%s, %d',
			$a_shift_c_out,
			$self->int_type, $a_shift_tmp, $self->int_bits
		),
		sprintf ('br label %%%s', $cont_lab), 
	
		$no_shift_lab . ':',
		sprintf ('%%%s = call %s @llvm.ctlz.%s (%s %%%s)',
			$b_shift_tmp,
			$self->int_type, $self->int_type, $self->int_type,
			$b
		),
		sprintf ('%%%s = icmp ne %s %%%s, 0',
			$cmp_shift, $self->int_type, $b_shift_tmp),
		sprintf ('br i1 %%%s, label %%%s, label %%%s',
			$cmp_shift, $b_shift_lab, $cont_lab),


		$b_shift_lab . ':',
		sprintf ('%%%s = shl %s %%%s, %%%s',
			$b_shift_a_out,
			$self->int_type, $a, $b_shift_tmp
		),
		sprintf ('%%%s = shl %s %%%s, %%%s',
			$b_shift_b_tmp0,
			$self->int_type, $b, $b_shift_tmp
		),
		sprintf ('%%%s = sub %s %d, %%%s',
			$b_shift_a_shift,
			$self->int_type, $self->int_bits, $b_shift_tmp
		),
		sprintf ('%%%s = lshr %s %%%s, %%%s',
			$b_shift_b_tmp1,
			$self->int_type, $a, $b_shift_a_shift
		),
		sprintf ('%%%s = or %s %%%s, %%%s',
			$b_shift_b_out,
			$self->int_type, $b_shift_b_tmp0, $b_shift_b_tmp1
		),
		sprintf ('%%%s = bitcast %s %%%s to %s',
			$b_shift_c_out,
			$self->int_type, $b_shift_tmp, $self->int_type
		),
		sprintf ('br label %%%s', $cont_lab), 


		$cont_lab . ':',
		sprintf ('%%%s = phi %s [ 0, %%%s ], [ %%%s, %%%s ], [ %%%s, %%%s ], [ %%%s, %%%s ]',
			$inst->{'out'}->[0],
			$self->int_type, 
			$b0_lab,
			$a, $no_shift_lab,
			$a_shift_a_out, $a_shift_lab,
			$b_shift_a_out, $b_shift_lab
		),
		sprintf ('%%%s = phi %s [ 0, %%%s ], [ %%%s, %%%s ], [ %%%s, %%%s ], [ %%%s, %%%s ]',
			$inst->{'out'}->[1],
			$self->int_type, 
			$b0_lab,
			$b, $no_shift_lab,
			$a_shift_b_out, $a_shift_lab,
			$b_shift_b_out, $b_shift_lab
		),
		sprintf ('%%%s = phi %s [ %d, %%%s ], [ %d, %%%s ], [ %%%s, %%%s ], [ %%%s, %%%s ]',
			$inst->{'out'}->[2],
			$self->int_type, 
			$self->int_bits * 2, $b0_lab,
			0, $no_shift_lab,
			$a_shift_c_out, $a_shift_lab,
			$b_shift_c_out, $b_shift_lab
		)
	);
}

sub gen_fmul ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $long_a	= $self->tmp_reg ();
	my $long_b 	= $self->tmp_reg ();
	my $long_mul 	= $self->tmp_reg ();
	my $long_res 	= $self->tmp_reg ();
	my $lo_val	= $self->tmp_reg ();
	my $hi_val	= $self->tmp_reg ();
	my $lo_lt_0_tmp	= $self->tmp_reg ();
	my $lo_lt_0	= $self->tmp_reg ();
	my $lo_mint	= $self->tmp_reg ();
	my $hi_bit0	= $self->tmp_reg ();
	my $plus_tmp	= $self->tmp_reg ();
	my $plus_val	= $self->tmp_reg ();
	return (
		sprintf ('%%%s = sext %s %%%s to %s',
			$long_a, $self->int_type, $inst->{'in'}->[0], $self->long_type
		),
		sprintf ('%%%s = sext %s %%%s to %s',
			$long_b, $self->int_type, $inst->{'in'}->[1], $self->long_type
		),
		sprintf ('%%%s = mul %s %%%s, %%%s',
			$long_mul, $self->long_type, $long_a, $long_b
		),
		sprintf ('%%%s = shl %s %%%s, 1',
			$long_res, $self->long_type, $long_mul
		),
		$self->_gen_long_split ($long_res, $lo_val, $hi_val),
		sprintf ('%%%s = icmp slt %s %%%s, 0',
			$lo_lt_0_tmp, $self->int_type, $lo_val,
		),
		sprintf ('%%%s = zext i1 %%%s to %s',
			$lo_lt_0, $lo_lt_0_tmp, $self->int_type
		),
		sprintf ('%%%s = lshr %s %%%s, %d',
			$lo_mint, $self->int_type, $lo_val, $self->int_bits - 1
		),
		sprintf ('%%%s = and %s %%%s, 1',
			$hi_bit0, $self->int_type, $hi_val
		),
		sprintf ('%%%s = or %s %%%s, %%%s',
			$plus_tmp, $self->int_type, $lo_mint, $hi_bit0
		),
		sprintf ('%%%s = and %s %%%s, %%%s',
			$plus_val, $self->int_type, $plus_tmp, $lo_lt_0
		),
		sprintf ('%%%s = add %s %%%s, %%%s',
			$inst->{'out'}->[0], $self->int_type, $hi_val, $plus_val
		)
	);
}

sub gen_move ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $src_ptr = $self->tmp_reg ();
	my $dst_ptr = $self->tmp_reg ();
	return (
		sprintf ('%%%s = inttoptr %s %%%s to i8*',
			$src_ptr, $self->int_type, $inst->{'in'}->[2]
		),
		sprintf ('%%%s = inttoptr %s %%%s to i8*',
			$dst_ptr, $self->int_type, $inst->{'in'}->[1]
		),
		sprintf ('call void @llvm.memcpy.%s (i8* %%%s, i8* %%%s, %s %%%s, i32 0)',
			$self->int_type, $dst_ptr, $src_ptr, $self->int_type, $inst->{'in'}->[0]
		)
	);
}

sub gen_widenshort ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $s_val = $self->tmp_reg ();
	return (
		sprintf ('%%%s = trunc %s %%%s to %s',
			$s_val, $self->int_type, $inst->{'in'}->[0], $self->short_type
		),
		sprintf ('%%%s = sext %s %%%s to %s',
			$inst->{'out'}->[0], $self->short_type, $s_val, $self->int_type
		)
	);
}

# FIXME: CCNT1 and CWORD are very similar
sub gen_ccnt1 ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $a		= $inst->{'in'}->[0];
	my $b		= $inst->{'out'}->[0];
	my $cmp_0	= $self->tmp_reg ();
	my $cmp_1	= $self->tmp_reg ();
	my $error_cond	= $self->tmp_reg ();
	my $tmp		= $self->tmp_label ();
	my $error_lab	= $tmp . '_error';
	my $ok_lab	= $tmp . '_ok';
	return (
		$self->single_assignment ($self->int_type, $inst->{'in'}->[1], $b),
		sprintf ('%%%s = icmp eq %s %%%s, 0',
			$cmp_0, $self->int_type, $b
		),
		sprintf ('%%%s = icmp ugt %s %%%s, %%%s',
			$cmp_1, $self->int_type, $b, $a
		),
		sprintf ('%%%s = or i1 %%%s, %%%s',
			$error_cond, $cmp_0, $cmp_1
		),
		sprintf ('br i1 %%%s, label %%%s, label %%%s',
			$error_cond, $error_lab, $ok_lab
		),

		$error_lab . ':',
		$self->_gen_error ($proc, $label, $inst, 'set'),
		sprintf ('br label %%%s', $ok_lab),

		$ok_lab . ':'
	);
}

sub gen_cword ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $a		= $inst->{'in'}->[0];
	my $b		= $inst->{'out'}->[0];
	my $neg_a	= $self->tmp_reg ();
	my $cmp_0	= $self->tmp_reg ();
	my $cmp_1	= $self->tmp_reg ();
	my $error_cond	= $self->tmp_reg ();
	my $tmp		= $self->tmp_label ();
	my $error_lab	= $tmp . '_error';
	my $ok_lab	= $tmp . '_ok';
	return (
		$self->single_assignment ($self->int_type, $inst->{'in'}->[1], $b),
		sprintf ('%%%s = sub %s 0, %%%s',
			$neg_a, $self->int_type, $a
		),
		sprintf ('%%%s = icmp sge %s %%%s, %%%s',
			$cmp_0, $self->int_type, $b, $a
		),
		sprintf ('%%%s = icmp slt %s %%%s, %%%s',
			$cmp_1, $self->int_type, $b, $neg_a
		),
		sprintf ('%%%s = or i1 %%%s, %%%s',
			$error_cond, $cmp_0, $cmp_1
		),
		sprintf ('br i1 %%%s, label %%%s, label %%%s',
			$error_cond, $error_lab, $ok_lab
		),

		$error_lab . ':',
		$self->_gen_error ($proc, $label, $inst, 'overflow'),
		sprintf ('br label %%%s', $ok_lab),

		$ok_lab . ':'
	);
}

sub gen_checknotnull ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $in		= $inst->{'in'}->[0];
	my $out		= $inst->{'out'}->[0];
	my $error_cond	= $self->tmp_reg ();
	my $tmp		= $self->tmp_label ();
	my $error_lab	= $tmp . '_null';
	my $ok_lab	= $tmp . '_ok';
	return (
		$self->single_assignment ($self->int_type, $in, $out),
		sprintf ('%%%s = icmp eq %s %%%s, %d',
			$error_cond, $self->int_type, $in, $self->null
		),
		sprintf ('br i1 %%%s, label %%%s, label %%%s',
			$error_cond, $error_lab, $ok_lab
		),

		$error_lab . ':',
		$self->_gen_error ($proc, $label, $inst, 'null'),
		sprintf ('br label %%%s', $ok_lab),

		$ok_lab . ':'
	);
}

sub gen_enb1 ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $name	= '_' . $inst->{'name'};
	my $guard	= $inst->{'in'}->[0];
	my $opt		= $inst->{'in'}->[1];
	my $ret		= $inst->{'out'}->[0];
	my $ret_tmp	= $self->tmp_reg ();
	
	return (
		$self->gen_kcall ($proc, $label, {
			'wptr'		=> $inst->{'wptr'},
			'name' 		=> $name,
			'in'		=> $opt ? [ $guard, $opt ] : [ $guard ],
			'out'		=> [ $ret_tmp ],
			'no_tail'	=> 1
		}),
		sprintf ('%%%s = and %s %%%s, 1',
			$ret, 
			$self->int_type, $ret_tmp
		)
	);
}

sub gen_enb3 ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my $name	= '_' . ($inst->{'name'} =~ m/^([A-Z]+)\d+$/)[0];
	
	my $jump	= $inst->{'in'}->[0];
	my $guard	= $inst->{'in'}->[1];
	my $opt		= $inst->{'in'}->[2];
	
	my $ret		= $inst->{'out'}->[0];
	my $n_guard	= $self->tmp_reg ();
	my $jump_ptr	= $self->tmp_reg ();
	
	my $tmp		= $self->tmp_label ();
	my $jump_lab	= $tmp . '_ready';
	my $cont_lab	= $tmp . '_continue';
	my $next_label	= $label->{'next'};
	
	return (
		$self->gen_kcall ($proc, $label, {
			'wptr'		=> $inst->{'wptr'},
			'name' 		=> $name,
			'in'		=> $opt ? [ $guard, $opt ] : [ $guard ],
			'out'		=> [ $ret ],
			'no_tail'	=> 1
		}),
		sprintf ('%%%s = icmp eq %s %%%s, -1',
			$n_guard, 
			$self->int_type, $ret
		),
		sprintf ('br i1 %%%s, label %%%s, label %%%s',
			$n_guard,
			$jump_lab, $cont_lab
		),

		$jump_lab . ':',
		sprintf ('%%%s = inttoptr %s %%%s to %s',
			$jump_ptr,
			$self->int_type, $jump,
			$self->func_type . '*'
		),
		sprintf ('tail call fastcc void %%%s (%s %%sched, %s %%%s) noreturn',
			$jump_ptr,
			$self->sched_type,
			$self->workspace_type,
			$inst->{'wptr'}
		),
		'ret void',

		$cont_lab . ':'
	);
}

sub gen_indirect_reg ($$$$) {
	my ($self, $proc, $label, $inst) = @_;
	my @regs = ( 'A', 'B', 'C' );
	my ($reg) = ($inst->{'name'} =~ m/INDIRECT_(.)REG/);
	my @asm;
	for (my $i = 0; $i < @{$inst->{'in'}}; ++$i) {
		if ($regs[$i] eq $reg) {
			push (@asm, $self->gen_ldnl ($proc, $label, {
				'in'	=> [ $inst->{'in'}->[$i] ],
				'out'	=> [ $inst->{'out'}->[$i] ],
				'arg'	=> 0
			}));
		} else {
			push (@asm, $self->single_assignment (
				$self->int_type, 
				$inst->{'in'}->[$i],
				$inst->{'out'}->[$i]
			));
		}
	}
	return @asm;
}

sub format_lines (@) {
	my @lines = @_;
	my @asm;
	foreach my $line (@lines) {
		if ($line =~ /^(\@|[a-zA-Z0-9\$\._]+:|de(clare|fine))/ || $line =~ /[{}]\s*$/) {
			push (@asm, $line);
		} else {
			push (@asm, "\t" . $line);
		}
		push (@asm, '') if $line =~ /^([@}]|declare)/;
	}
	return @asm;
}

sub generate_proc ($$) {
	my ($self, $proc) = @_;
	my $symbol 	= '@' . $self->symbol_to_proc_name ($proc->{'symbol'});
	my $call_pfix 	= $symbol . '_';
	my @asm;

	$proc->{'call_prefix'} = $call_pfix;
	
	push (@asm, format_lines (
		sprintf ('define %sfastcc void %s (%s %%sched, %s %%wptr) {',
			$proc->{'global'} ? '' : 'private ',
			$symbol,
			$self->sched_type, $self->workspace_type
		),
		'entry:',
		sprintf ('tail call fastcc void %s (%s %%sched, %s %%wptr) noreturn',
			$call_pfix . $proc->{'labels'}->[0]->{'name'},
			$self->sched_type, $self->workspace_type
		),
		'ret void',
		'}'
	));

	foreach my $label (@{$proc->{'labels'}}) {
		my ($name, $insts) = ($label->{'name'}, $label->{'inst'});
		my $last_inst;

		if (1) {
			my $sym_name = sprintf ('%s_%s', $symbol, $name);
			my @types = ( $self->sched_type, $self->workspace_type );
			my @params = (
				sprintf ('%s %%sched', $self->sched_type),
				sprintf ('%s %%%s', $self->workspace_type, $label->{'wptr'})
			);
			for (my $i = 0; $i < @{$label->{'in'}}; ++$i) {
				push (@types, $self->int_type);
				push (@params, sprintf ('%s %%%s', $self->int_type, $label->{'in'}->[$i]));
			}
			for (my $i = 0; $i < @{$label->{'fin'}}; ++$i) {
				push (@types, $self->float_type);
				push (@params, sprintf ('%s %%%s', $self->float_type, $label->{'fin'}->[$i]));
			}
			
			my $tmp_sym = $sym_name;
			$tmp_sym =~ s/^[@%]//;
			$self->{'header'}->{$tmp_sym} = [
				sprintf ('declare fastcc void %s (%s)',
					$sym_name,
					join (', ', @types)
				)
			];
			
			push (@asm,
				sprintf ('define private fastcc void %s (%s) {',
					$sym_name,
					join (', ', @params)
			));
			push (@asm, $name . ':');

		} elsif ($label->{'phi'}) {
			my $wptr	= $label->{'wptr'};
			my $in 		= $label->{'in'} || [];
			my $fin 	= $label->{'fin'} || [];
			my @vars 	= ( $wptr, @$in, @$fin );
			my %var_map 	= ( map { $_ => [] } @vars ); # build hash of arrays for each var
			my %type 	= ( $wptr => $self->workspace_type );
			foreach my $var (@$in) {
				$type{$var} = $self->int_type;
			}
			foreach my $var (@$fin) {
				$type{$var} = $self->float_type;
			}
			my $wptr_same = 1;
			foreach my $slabel (keys (%{$label->{'phi'}})) {
				my $svars = $label->{'phi'}->{$slabel};
				for (my $i = 0; $i < @vars; ++$i) {
					$wptr_same = $wptr_same && $vars[$i] eq $svars->[$i]
						if $i == 0;
					push (@{$var_map{$vars[$i]}},
						'[ %' . $svars->[$i] . ', %' . $slabel . ' ]'
					);
				}
			}
			shift @vars if $wptr_same;
			foreach my $var (@vars) {
				push (@asm, join ('', 
					"\t", '%', $var, 
					' = phi ', $type{$var}, ' ', join (' ', @{$var_map{$var}})
				));
			}
		}
		
		for (my $inst_idx = 0; $inst_idx < @$insts; ++$inst_idx) {
			my $inst	= $insts->[$inst_idx];
			my $name 	= $inst->{'name'};
			
			if ($name =~ /^\./) {
				if ($name eq '.LINE') {
					$self->source_line ($inst->{'arg'});
				} elsif ($name eq '.FILENAME') {
					$self->source_file ($inst->{'arg'});
				} elsif ($name eq '.STACKS') {
					my @stack = @{$inst->{'stack'}};
					my @fstack = @{$inst->{'fstack'}};
					my $text;
					$text .= "STACK = " . join (', ', @stack) if @stack;
					$text .= (@stack ? ", " : '') . 
						" FSTACK = " . join (', ', @fstack) if @fstack;
					push (@asm, format_lines ("; $text")) if $text;
				} elsif ($name ne '.OCCCOMMENT') {
					my $arg = $inst->{'arg'};
					if (!ref ($arg)) {
						$arg =~ s/\n//gs;
						push (@asm, format_lines ("; $name $arg"));
					}
				}
				next;
			}
			
			my $line = "\t; $name";
			if (exists ($inst->{'arg'})) {
				$line .= " ";
				if (exists ($inst->{'label_arg'})) {
					if (ref ($inst->{'arg'}) =~ /ARRAY/) {
						if ($name eq 'LDC' && @{$inst->{'arg'}} == 2) {
							$inst->{'arg'} = $inst->{'arg'}->[0];
							$line .= $inst->{'arg'}->{'name'};
						} elsif ($name eq 'TABLE') {
							my @names = map { $_->{'name'} } @{$inst->{'arg'}};
							$line .= join (', ', @names);
						} else {
							my @names = map { $_->{'name'} } @{$inst->{'arg'}};
							print STDERR "Unknown label array for $name ",
								join (', ', @names), "\n";
							exit 0;
						}
					} else {
						$line .= $inst->{'arg'}->{'name'};
					}
				} else {
					$line .= $inst->{'arg'};
				}
			}
			$line .= "\t{ ";
			$line .= '(' . join (', ', @{$inst->{'in'}}, @{$inst->{'fin'}}) . ')';
			$line .= ' => ';
			$line .= '(' . join (', ', @{$inst->{'out'}}, @{$inst->{'fout'}}) . ')';
			$line .= ' }';
			push (@asm, $line);
			
			my $data	= $GRAPH->{$name};
			my $in 		= $inst->{'in'} || [];
			my $fin 	= $inst->{'fin'} || [];
			my $out 	= $inst->{'out'} || [];
			my $fout 	= $inst->{'fout'} || [];
			
			if ($data->{'generator'}) {
				my @lines = &{$data->{'generator'}}($self, $proc, $label, $inst);
				push (@asm, format_lines (@lines));
			} elsif ($data->{'kcall'}) {
				my @lines = $self->gen_kcall ($proc, $label, $inst);
				push (@asm, format_lines (@lines));
			} else {
				# NOP unknown instructions for debugging
				$self->message (1, "Generating NOP for $name");
				push (@asm, '; NOP');
				foreach my $reg (@$out) {
					push (@asm, sprintf ('%%%s = bitcast %s 0 to %s',
						$reg, $self->int_type, $self->int_type
					));
				}
				foreach my $reg (@$fout) {
					push (@asm, sprintf ('%%%s = bitcast %s 0.0 to %s',
						$reg, $self->float_type, $self->float_type
					));
				}
			}
			
			$last_inst = $inst;
		}

		my $last_data = $GRAPH->{$last_inst->{'name'}};
		if ($last_inst->{'name'} ne 'RET' && !($last_data->{'branching'} || $last_data->{'kcall'})) {
			my $next_label = $label->{'next'};
			push (@asm, format_lines (
				$self->gen_j ($proc, $label, {
					'in'	=> $next_label->{'in'},
					'fin'	=> $next_label->{'fin'},
					'wptr' 	=> $next_label->{'wptr'},
					'arg'	=> $next_label
				})
			));
		}
		push (@asm, format_lines ('ret void', '}'));
	}
	#push (@asm, '}') if !$prev_call;

	return @asm;
}

sub code_aliases ($) {
	my $self = shift;
	my $aliases = $self->{'aliases'};
	my @asm;

	foreach my $alias (sort (keys (%$aliases))) {
		my $target 	= $aliases->{$alias};
		my $alias	= $self->symbol_to_proc_name ($alias);
		my $symbol 	= $self->symbol_to_proc_name ($target);

		$self->declare_proc ($alias);
		$self->declare_proc ($symbol);
	
		push (@asm, format_lines (
			sprintf ('define fastcc void @%s (%s %%sched, %s %%wptr) {',
				$alias,
				$self->sched_type, $self->workspace_type
			),
			'entry:',
			sprintf ('tail call fastcc void @%s (%s %%sched, %s %%wptr) noreturn',
				$symbol,
				$self->sched_type, $self->workspace_type
			),
			'ret void',
			'}'
		));
	}

	return @asm;
}

sub code_constants ($) {
	my $self = shift;
	my $constants = $self->{'constants'};
	my @asm;

	foreach my $name (sort (keys (%$constants))) {
		my $value = $constants->{$name};
		my $align = undef;
		my @data;
		if (exists ($value->{'str'})) {
			my $str = $value->{'str'};
			@data = split (//, $str);
			foreach my $chr (@data) {
				$chr = ord ($chr);
			}
			push (@data, 0);
			push (@asm, "; \@$name = \"$str\"");
		} elsif (exists ($value->{'data'})) {
			$align = $value->{'align'};
			@data = unpack ('C*', $value->{'data'}); 
		}
		
		foreach my $val (@data) {
			$val = sprintf ('i8 %d', $val);
		}

		push (@asm, sprintf ('@%s_value = internal constant [ %d x i8 ] [ %s ]%s',
			$name, scalar (@data), join (', ', @data),
			($align ? ', align ' . (2 ** $align) : '')
		));
		push (@asm, sprintf ('@%s = internal constant i8* getelementptr ([ %d x i8 ]* @%s_value, i32 0, i32 0)',
			$name, scalar (@data), $name
		));
	}

	return @asm;
}

sub code_proc ($$) {
	my ($self, $proc) = @_;
	
	$self->reset_tmp ();

	$self->define_registers ($proc->{'labels'});
	#$self->build_phi_nodes ($proc->{'labels'});
	$self->build_label_types ($proc->{'labels'});
	$self->define_rounding ($proc->{'labels'});
	
	my $comments = "; " . $proc->{'symbol'};
	return ($comments, $self->generate_proc ($proc));
}

sub intrinsics ($) {
	my $self = shift;
	my $int = $self->int_type;
	return (
		"declare $int \@llvm.ctlz.$int ($int)",
		"declare void \@llvm.memcpy.$int (i8*, i8*, $int, i32)",
		"declare { $int, i1 } \@llvm.sadd.with.overflow.$int ($int, $int)",
		"declare { $int, i1 } \@llvm.uadd.with.overflow.$int ($int, $int)",
		"declare { $int, i1 } \@llvm.smul.with.overflow.$int ($int, $int)",
		"declare { $int, i1 } \@llvm.ssub.with.overflow.$int ($int, $int)",
		"declare { $int, i1 } \@llvm.usub.with.overflow.$int ($int, $int)"
	);
}

sub generate ($@) {
	my ($self, @texts) = @_;
	my $verbose = $self->{'verbose'};
	my @proc_asm;
	
	foreach my $text (@texts) {
		my $file	= $text->{'file'};
		my $etc		= $text->{'etc'};

		$self->preprocess_etc ($file, $etc);

		foreach my $proc (@{$self->{'procs'}}) {
			push (@proc_asm, $self->code_proc ($proc));
		}
	}
	
	my @header 	= $self->intrinsics;
	my @alias_asm	= $self->code_aliases ();
	my @const_asm 	= $self->code_constants ();
	foreach my $elem (sort (keys (%{$self->{'header'}}))) {
		push (@header, @{$self->{'header'}->{$elem}});
	}

	return (@header, @alias_asm, @const_asm, @proc_asm);
}

sub build_tlp_desc {
	my ($self, $symbol)	= @_;
	my $def			= $symbol->{'definition'};
	my ($params)		= ($def =~ m/PROC\s+\S+\(([^\)]+)\)/s);
	$params			=~ s/\r?\n//gs;
	my @params		= split (/,/, $params);
	my @tlp;

	foreach my $param (@params) {
		my $shared = 0;
		
		if ($param =~ /^FIXED SHARED/) {
			$param =~ s/^FIXED SHARED\s*//;
			$shared = 1;
		}
		
		if ($param =~ m/^CHAN\s+OF\s+(\S+)\s+(\S+)/) {
			my ($type, $name)	= ($1, $2);
			my ($dir)		= ($name =~ m/([\?!])$/);
			if (!$dir) {
				($dir) = ($def =~ m/$name([\?!])/gs);
				$dir = '.' if !$dir;
			}
			$name = 'keyboard?' 	if $name =~ /(kyb|key(board)?|in(put)?)/ && $dir =~ /[.?]/;
			$name = 'screen!' 	if $name =~ /(scr(een)?|out(put)?)/ && $dir =~ /[.!]/;
			$name = 'error!' 	if $name =~ /(err(or)?)/ && $dir =~ /[.!]/;
			push (@tlp, $name);
		} else {
			push (@tlp, 'unknown');
		}
	}

	push (@tlp, 'VSPTR') 	if $symbol->{'vs'};
	push (@tlp, 'MSPTR')	if $symbol->{'ms'};
	push (@tlp, 'FBARPTR') 	if $def =~ /PROC.*\(.*\).*FORK/;

	return @tlp;
}

sub entry_point ($$) {
	my ($self, $symbol) = @_;
	my $name	= $symbol->{'string'};
	my $ws		= $symbol->{'ws'};
	my $vs		= $symbol->{'vs'};
	my $ms		= $symbol->{'ms'};
	my @tlp_desc	= $self->build_tlp_desc ($symbol); 
	my @asm;

	my @tlp_elem;
	for (my $i = 0; $i < @tlp_desc; ++$i) {
		my $elem = $tlp_desc[$i];
		push (@asm, sprintf ('@tlp_%d = internal constant [ %d x i8 ] c"%s\00"',
			$i, length ($tlp_desc[$i]) + 1, $tlp_desc[$i]
		));
		push (@tlp_elem, sprintf ('i8* getelementptr ([ %d x i8 ]* @tlp_%d, i32 0, i32 0)',
			length ($tlp_desc[$i]) + 1, $i
		));
	}

	push (@asm, sprintf ('@tlp_desc = internal constant [ %d x i8* ] [ %s%si8* null ]',
		@tlp_elem + 1, 
		join (', ', @tlp_elem), 
		(@tlp_elem ? ', ' : '')
	));
	push (@asm,
		sprintf ('declare %s @occam_start (%s, i8**, i8*, i8*, i8**, i8*, %s, %s, %s)',
			$self->int_type, 
			$self->int_type, 
			$self->int_type, $self->int_type, $self->int_type
		)
	);

	push (@asm,
		sprintf ('define private fastcc void @code_exit (%s %%sched, %s %%wptr) {',
			$self->sched_type,
			$self->workspace_type
		),
		# FIXME: handle fork barrier
		'ret void',
		'}'
	);
	
	push (@asm,
		sprintf ('define void @code_entry (%s %%sched, %s %%wptr) {',
			$self->sched_type, $self->workspace_type
		),
		sprintf ('%%iptr_ptr = getelementptr %s %%wptr, %s %d',
			$self->workspace_type,
			$self->index_type, -1
		),
		sprintf ('%%iptr_val = load %s %%iptr_ptr',
			$self->workspace_type
		),
		sprintf ('%%iptr = inttoptr %s %%iptr_val to %s',
			$self->int_type,
			$self->func_type . '*'
		),
		sprintf ('tail call fastcc void %%iptr (%s %%sched, %s %%wptr) noreturn',
			$self->sched_type,
			$self->workspace_type
		),
		'ret void',
		'}',
	);

	push (@asm, 
		sprintf ('define %s @main (%s %%argc, i8** %%argv) {',
			$self->int_type, $self->int_type
		),
		'entry:',
		"; ENTRY POINT: $name, WS = $ws, VS = $vs, MS = $ms",
		sprintf ('%%code_entry = bitcast %s @code_entry to i8*',
			$self->func_type . '*'
		),
		sprintf ('%%code_exit = bitcast %s @code_exit to i8*',
			$self->func_type . '*'
		),
		sprintf ('%%start_proc = bitcast %s @%s to i8*',
			$self->func_type . '*',
			$self->symbol_to_proc_name ($name)
		),
		sprintf ('%%ret = call %s @occam_start (%s %%argc, i8** %%argv'
			 	. ', i8* %%code_entry, i8* %%code_exit'
				. ', i8** getelementptr ([ %d x i8* ]* @tlp_desc, i32 0, i32 0)'
				. ', i8* %%start_proc, %s %d, %s %d, %s %d)',
			$self->int_type,
			$self->int_type, 
			@tlp_elem + 1,
			$self->int_type, $ws,
			$self->int_type, $vs,
			$self->int_type, $ms
		),
		sprintf ('ret %s %%ret',
			$self->int_type
		),
		'}'
	);

	return format_lines (@asm); 
}

1;
