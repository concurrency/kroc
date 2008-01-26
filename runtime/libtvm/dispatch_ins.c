/* Generated automatically by make-dispatch.py; do not modify! */

static inline void dispatch_instruction(BYTE instr)
{
	switch (instr >> 4) {
	case 0x00: ins_j(); break;
	case 0x01: ins_ldlp(); break;
	case 0x02: ins_pfix(); break;
	case 0x03: ins_ldnl(); break;
	case 0x04: ins_ldc(); break;
	case 0x05: ins_ldnlp(); break;
	case 0x06: ins_nfix(); break;
	case 0x07: ins_ldl(); break;
	case 0x08: ins_adc(); break;
	case 0x09: ins_call(); break;
	case 0x0A: ins_cj(); break;
	case 0x0B: ins_ajw(); break;
	case 0x0C: ins_eqc(); break;
	case 0x0D: ins_stl(); break;
	case 0x0E: ins_stnl(); break;

	case 0x0F:
		switch (oreg) {
		case 0x00: ins_rev(); break;
		case 0x01: ins_lb(); break;
		case 0x02: ins_bsub(); break;
		case 0x03: ins_endp(); break;
		case 0x04: ins_diff(); break;
		case 0x05: ins_add(); break;
		case 0x06: ins_gcall(); break;
		case 0x07: ins_in(); break;
		case 0x08: ins_prod(); break;
		case 0x09: ins_gt(); break;
		case 0x0A: ins_wsub(); break;
		case 0x0B: ins_out(); break;
		case 0x0C: ins_sub(); break;
		case 0x0D: ins_startp(); break;
		case 0x0E: ins_outbyte(); break;
		case 0x0F: ins_outword(); break;
		case 0x10: ins_seterr(); break;
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
		case 0x11: ins_mreleasep(); break;
#endif
		case 0x13: ins_csub0(); break;
#ifdef TVM_OCCAM_PI
		case 0x14: ins_extvrfy(); break;
#endif
		case 0x15: ins_stopp(); break;
		case 0x16: ins_ladd(); break;
		case 0x19: ins_norm(); break;
		case 0x1A: ins_ldiv(); break;
		case 0x1B: ins_ldpi(); break;
		case 0x1D: ins_xdble(); break;
		case 0x1F: ins_rem(); break;
		case 0x20: ins_ret(); break;
		case 0x21: ins_lend(); break;
		case 0x22: ins_ldtimer(); break;
		case 0x23: ins_boolinvert(); break;
		case 0x24: ins_widenshort(); break;
		case 0x25: ins_fficall(); break;
		case 0x26: ins_lend3(); break;
		case 0x27: ins_lendbw(); break;
		case 0x28: ins_reschedule(); break;
		case 0x2B: ins_tin(); break;
		case 0x2C: ins_div(); break;
		case 0x2E: ins_dist(); break;
		case 0x2F: ins_disc(); break;
		case 0x30: ins_diss(); break;
		case 0x31: ins_lmul(); break;
		case 0x32: ins_not(); break;
		case 0x33: ins_xor(); break;
		case 0x35: ins_lshr(); break;
		case 0x36: ins_lshl(); break;
		case 0x37: ins_lsum(); break;
		case 0x38: ins_lsub(); break;
		case 0x39: ins_runp(); break;
		case 0x3B: ins_sb(); break;
		case 0x3C: ins_gajw(); break;
		case 0x3E: ins_saveh(); break;
		case 0x40: ins_shr(); break;
		case 0x41: ins_shl(); break;
		case 0x42: ins_mint(); break;
		case 0x43: ins_alt(); break;
		case 0x44: ins_altwt(); break;
		case 0x45: ins_altend(); break;
		case 0x46: ins_and(); break;
		case 0x47: ins_enbt(); break;
		case 0x48: ins_enbc(); break;
		case 0x49: ins_enbs(); break;
		case 0x4A: ins_move(); break;
		case 0x4B: ins_or(); break;
		case 0x4C: ins_csngl(); break;
		case 0x4D: ins_ccnt1(); break;
		case 0x4E: ins_talt(); break;
		case 0x4F: ins_ldiff(); break;
		case 0x51: ins_taltwt(); break;
		case 0x52: ins_sum(); break;
		case 0x53: ins_mul(); break;
		case 0x55: ins_stoperr(); break;
		case 0x56: ins_cword(); break;
		case 0x5A: ins_dup(); break;
#ifdef TVM_OCCAM_PI
		case 0x60: ins_extin(); break;
		case 0x61: ins_extout(); break;
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
		case 0x62: ins_minn(); break;
#endif
#if defined(TVM_EMULATE_T4) || defined(TVM_EMULATE_T8)
		case 0x63: ins_unpacksn(); break;
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
		case 0x64: ins_moutn(); break;
		case 0x65: ins_xminn(); break;
#endif
#if defined(TVM_EMULATE_T4) || defined(TVM_EMULATE_T8)
		case 0x6C: ins_postnormsn(); break;
		case 0x6D: ins_roundsn(); break;
		case 0x71: ins_ldinf(); break;
		case 0x72: ins_fmul(); break;
#endif
		case 0x79: ins_pop(); break;
#ifdef TVM_OCCAM_PI
		case 0x7A: ins_seminit(); break;
		case 0x7B: ins_semclaim(); break;
		case 0x7C: ins_semrelease(); break;
#endif
		case 0x81: ins_wsubdb(); break;
#ifdef TVM_EMULATE_T8
		case 0x82: ins_fpldnldbi(); break;
		case 0x83: ins_fpchkerr(); break;
		case 0x84: ins_fpstnldb(); break;
		case 0x86: ins_fpldnlsni(); break;
		case 0x87: ins_fpadd(); break;
		case 0x88: ins_fpstnlsn(); break;
		case 0x89: ins_fpsub(); break;
		case 0x8A: ins_fpldnldb(); break;
		case 0x8B: ins_fpmul(); break;
		case 0x8C: ins_fpdiv(); break;
		case 0x8E: ins_fpldnlsn(); break;
		case 0x91: ins_fpnan (); break;
		case 0x92: ins_fpordered (); break;
		case 0x93: ins_fpnotfinite (); break;
		case 0x94: ins_fpgt (); break;
		case 0x95: ins_fpeq (); break;
		case 0x96: ins_fpi32tor32(); break;
		case 0x98: ins_fpi32tor64 (); break;
		case 0x9A: ins_fpb32tor64(); break;
		case 0x9D: ins_fprtoi32(); break;
		case 0x9E: ins_fpstnli32(); break;
		case 0x9F: ins_fpldzerosn(); break;
		case 0xA0: ins_fpldzerodb(); break;
		case 0xA1: ins_fpint(); break;
#endif
		case 0xA2: ins_getpri(); break;
#ifdef TVM_EMULATE_T8
		case 0xA3: ins_fpdup(); break;
		case 0xA4: ins_fprev(); break;
#endif
		case 0xA5: ins_setpri(); break;
#ifdef TVM_EMULATE_T8
		case 0xA6: ins_fpldnladddb(); break;
		case 0xA8: ins_fpldnlmuldb(); break;
		case 0xAA: ins_fpldnladdsn(); break;
		case 0xAC: ins_fpldnlmulsn(); break;
#endif
		case 0xAD: ins_savecreg(); break;
		case 0xAE: ins_restorecreg(); break;
#ifdef TVM_OCCAM_PI
		case 0xB0: ins_barinit(); break;
		case 0xB1: ins_barsync(); break;
		case 0xB2: ins_barresign(); break;
		case 0xB3: ins_barenroll(); break;
#endif
#ifdef TVM_EMULATE_T8
		case 0xCF: ins_fprem(); break;
		case 0xD0: ins_i64toreal(); break;
		case 0xD1: ins_fpdivby2(); break;
		case 0xD2: ins_fpmulby2(); break;
		case 0xD3: ins_fpsqrt(); break;
		case 0xD6: ins_fprz(); break;
		case 0xD7: ins_fpr32to64(); break;
		case 0xD8: ins_fpr64to32(); break;
		case 0xD9: ins_fpexpdec32(); break;
		case 0xDB: ins_fpabs(); break;
		case 0xDF: ins_fpchki64(); break;
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
		case 0xE0: ins_mnew(); break;
		case 0xE1: ins_mfree(); break;
		case 0xE2: ins_malloc(); break;
		case 0xE3: ins_mrelease(); break;
		case 0xE4: ins_min(); break;
		case 0xE5: ins_mout(); break;
		case 0xE6: ins_min64(); break;
		case 0xE7: ins_mout64(); break;
#endif
#ifdef TVM_OCCAM_PI
		case 0xE8: ins_xable(); break;
		case 0xE9: ins_xin(); break;
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
		case 0xEA: ins_xmin(); break;
		case 0xEB: ins_xmin64(); break;
#endif
#ifdef TVM_OCCAM_PI
		case 0xEC: ins_xend(); break;
#endif
		case 0xFD: ins_null(); break;
		case 0xFE: ins_shutdown(); break;
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
		case 0x22F: ins_proc_alloc(); break;
		case 0x230: ins_proc_param(); break;
		case 0x231: ins_proc_mt_copy(); break;
		case 0x232: ins_proc_mt_move(); break;
		case 0x233: ins_proc_start(); break;
		case 0x234: ins_proc_end(); break;
#endif
#ifdef TVM_OCCAM_PI
		case 0x235: ins_getaff(); break;
		case 0x236: ins_setaff(); break;
#endif
		case 0x237: ins_getpas(); break;
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
		case 0x238: ins_mt_alloc(); break;
		case 0x239: ins_mt_release(); break;
		case 0x23A: ins_mt_clone(); break;
		case 0x23B: ins_mt_in(); break;
		case 0x23C: ins_mt_out(); break;
		case 0x23D: ins_mt_xchg(); break;
		case 0x23E: ins_mt_lock(); break;
		case 0x23F: ins_mt_unlock(); break;
		case 0x240: ins_mt_enroll(); break;
		case 0x241: ins_mt_resign(); break;
		case 0x242: ins_mt_sync(); break;
		case 0x243: ins_mt_xin(); break;
		case 0x244: ins_mt_xout(); break;
		case 0x245: ins_mt_xxchg(); break;
		case 0x246: ins_mt_dclone(); break;
		case 0x247: ins_mt_bind(); break;
#endif
#ifdef __RMOX_PI_SUPPORT__
		case 0x248: ins_mb(); break;
		case 0x249: ins_rmb(); break;
		case 0x24A: ins_wmb(); break;
#endif

		/* FIXME: This handles unimplemented and invalid instructions the same way. */
		default: ins_not_implemented(); break;
		}
		CLEAR(oreg);
		break;
	}
}
