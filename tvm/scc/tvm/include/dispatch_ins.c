/* Generated automatically by make-dispatch.py; do not modify! */

static inline int dispatch_instruction (ECTX ectx, BYTE instr)
{
	#ifdef TVM_PROFILING
	ectx->profile.pri[instr >> 4]++;
	#endif

	switch (instr >> 4) {
	case 0x00: return ins_j(ectx);
	case 0x01: return ins_ldlp(ectx);
	case 0x02: return ins_pfix(ectx);
	case 0x03: return ins_ldnl(ectx);
	case 0x04: return ins_ldc(ectx);
	case 0x05: return ins_ldnlp(ectx);
	case 0x06: return ins_nfix(ectx);
	case 0x07: return ins_ldl(ectx);
	case 0x08: return ins_adc(ectx);
	case 0x09: return ins_call(ectx);
	case 0x0A: return ins_cj(ectx);
	case 0x0B: return ins_ajw(ectx);
	case 0x0C: return ins_eqc(ectx);
	case 0x0D: return ins_stl(ectx);
	case 0x0E: return ins_stnl(ectx);

	case 0x0F:
		{
			WORD ins = OREG;
			CLEAR(OREG);
			
			#ifdef TVM_PROFILING
			ectx->profile.sec[ins]++;
			#endif

			switch (ins) {
			case 0x00: return ins_rev(ectx);
			case 0x01: return ins_lb(ectx);
			case 0x02: return ins_bsub(ectx);
			case 0x03: return ins_endp(ectx);
			case 0x04: return ins_diff(ectx);
			case 0x05: return ins_add(ectx);
			case 0x06: return ins_gcall(ectx);
			case 0x07: return ins_in(ectx);
			case 0x08: return ins_prod(ectx);
			case 0x09: return ins_gt(ectx);
			case 0x0A: return ins_wsub(ectx);
			case 0x0B: return ins_out(ectx);
			case 0x0C: return ins_sub(ectx);
			case 0x0D: return ins_startp(ectx);
			case 0x0E: return ins_outbyte(ectx);
			case 0x0F: return ins_outword(ectx);
			case 0x10: return ins_seterr(ectx);
#ifdef TVM_DYNAMIC_OCCAM_PI
			case 0x11: return ins_mreleasep(ectx);
#endif
			case 0x13: return ins_csub0(ectx);
#ifdef TVM_OCCAM_PI
			case 0x14: return ins_extvrfy(ectx);
#endif
			case 0x15: return ins_stopp(ectx);
			case 0x16: return ins_ladd(ectx);
			case 0x19: return ins_norm(ectx);
#if defined(TVM_HAVE_TWOWORD) || TVM_WORD_LENGTH == 4
			case 0x1A: return ins_ldiv(ectx);
#endif
			case 0x1B: return ins_ldpi(ectx);
			case 0x1D: return ins_xdble(ectx);
			case 0x1F: return ins_rem(ectx);
			case 0x20: return ins_ret(ectx);
			case 0x21: return ins_lend(ectx);
			case 0x22: return ins_ldtimer(ectx);
			case 0x23: return ins_boolinvert(ectx);
			case 0x24: return ins_widenshort(ectx);
			case 0x25: return ins_fficall(ectx);
			case 0x26: return ins_lend3(ectx);
			case 0x27: return ins_lendb(ectx);
			case 0x28: return ins_reschedule(ectx);
			case 0x2B: return ins_tin(ectx);
			case 0x2C: return ins_div(ectx);
			case 0x2E: return ins_dist(ectx);
			case 0x2F: return ins_disc(ectx);
			case 0x30: return ins_diss(ectx);
#if defined(TVM_HAVE_TWOWORD) || TVM_WORD_LENGTH == 4
			case 0x31: return ins_lmul(ectx);
#endif
			case 0x32: return ins_not(ectx);
			case 0x33: return ins_xor(ectx);
#if defined(TVM_HAVE_TWOWORD) || TVM_WORD_LENGTH == 4
			case 0x35: return ins_lshr(ectx);
			case 0x36: return ins_lshl(ectx);
#endif
			case 0x37: return ins_lsum(ectx);
			case 0x38: return ins_lsub(ectx);
			case 0x39: return ins_runp(ectx);
			case 0x3B: return ins_sb(ectx);
			case 0x3C: return ins_gajw(ectx);
			case 0x40: return ins_shr(ectx);
			case 0x41: return ins_shl(ectx);
			case 0x42: return ins_mint(ectx);
			case 0x43: return ins_alt(ectx);
			case 0x44: return ins_altwt(ectx);
			case 0x45: return ins_altend(ectx);
			case 0x46: return ins_and(ectx);
			case 0x47: return ins_enbt(ectx);
			case 0x48: return ins_enbc(ectx);
			case 0x49: return ins_enbs(ectx);
			case 0x4A: return ins_move(ectx);
			case 0x4B: return ins_or(ectx);
			case 0x4C: return ins_csngl(ectx);
			case 0x4D: return ins_ccnt1(ectx);
			case 0x4E: return ins_talt(ectx);
			case 0x4F: return ins_ldiff(ectx);
			case 0x51: return ins_taltwt(ectx);
			case 0x52: return ins_sum(ectx);
			case 0x53: return ins_mul(ectx);
			case 0x55: return ins_stoperr(ectx);
			case 0x56: return ins_cword(ectx);
			case 0x5A: return ins_dup(ectx);
#ifdef TVM_OCCAM_PI
			case 0x60: return ins_extin(ectx);
			case 0x61: return ins_extout(ectx);
#endif
#if defined(TVM_EMULATE_T4) || defined(TVM_EMULATE_T8)
			case 0x63: return ins_unpacksn(ectx);
			case 0x6C: return ins_postnormsn(ectx);
			case 0x6D: return ins_roundsn(ectx);
			case 0x71: return ins_ldinf(ectx);
			case 0x72: return ins_fmul(ectx);
#endif
			case 0x79: return ins_pop(ectx);
#ifdef TVM_OCCAM_PI
			case 0x7A: return ins_sem_init(ectx);
			case 0x7B: return ins_sem_claim(ectx);
			case 0x7C: return ins_sem_release(ectx);
#endif
			case 0x81: return ins_wsubdb(ectx);
#ifdef TVM_EMULATE_T8
			case 0x82: return ins_fpldnldbi(ectx);
			case 0x83: return ins_fpchkerr(ectx);
			case 0x84: return ins_fpstnldb(ectx);
			case 0x86: return ins_fpldnlsni(ectx);
			case 0x87: return ins_fpadd(ectx);
			case 0x88: return ins_fpstnlsn(ectx);
			case 0x89: return ins_fpsub(ectx);
			case 0x8A: return ins_fpldnldb(ectx);
			case 0x8B: return ins_fpmul(ectx);
			case 0x8C: return ins_fpdiv(ectx);
			case 0x8E: return ins_fpldnlsn(ectx);
			case 0x91: return ins_fpnan(ectx);
			case 0x92: return ins_fpordered(ectx);
			case 0x93: return ins_fpnotfinite(ectx);
			case 0x94: return ins_fpgt(ectx);
			case 0x95: return ins_fpeq(ectx);
			case 0x96: return ins_fpi32tor32(ectx);
			case 0x98: return ins_fpi32tor64(ectx);
			case 0x9A: return ins_fpb32tor64(ectx);
			case 0x9D: return ins_fprtoi32(ectx);
			case 0x9E: return ins_fpstnli32(ectx);
			case 0x9F: return ins_fpldzerosn(ectx);
			case 0xA0: return ins_fpldzerodb(ectx);
			case 0xA1: return ins_fpint(ectx);
#endif
			case 0xA2: return ins_getpri(ectx);
#ifdef TVM_EMULATE_T8
			case 0xA3: return ins_fpdup(ectx);
			case 0xA4: return ins_fprev(ectx);
#endif
			case 0xA5: return ins_setpri(ectx);
#ifdef TVM_EMULATE_T8
			case 0xA6: return ins_fpldnladddb(ectx);
			case 0xA8: return ins_fpldnlmuldb(ectx);
			case 0xAA: return ins_fpldnladdsn(ectx);
			case 0xAC: return ins_fpldnlmulsn(ectx);
#endif
			case 0xAD: return ins_savecreg(ectx);
			case 0xAE: return ins_restorecreg(ectx);
#ifdef TVM_SHORT_OPS
			case 0xB8: return ins_xbword(ectx);
			case 0xB9: return ins_lbx(ectx);
			case 0xBA: return ins_cb(ectx);
			case 0xBB: return ins_cbu(ectx);
			case 0xC1: return ins_ssub(ectx);
			case 0xC7: return ins_cir(ectx);
			case 0xC8: return ins_ss(ectx);
			case 0xCA: return ins_ls(ectx);
			case 0xCC: return ins_ciru(ectx);
#endif
#ifdef TVM_EMULATE_T8
			case 0xCF: return ins_fprem(ectx);
			case 0xD0: return ins_i64toreal(ectx);
			case 0xD1: return ins_fpdivby2(ectx);
			case 0xD2: return ins_fpmulby2(ectx);
			case 0xD3: return ins_fpsqrt(ectx);
			case 0xD6: return ins_fprz(ectx);
			case 0xD7: return ins_fpr32to64(ectx);
			case 0xD8: return ins_fpr64to32(ectx);
			case 0xD9: return ins_fpexpdec32(ectx);
			case 0xDB: return ins_fpabs(ectx);
			case 0xDF: return ins_fpchki64(ectx);
#endif
#ifdef TVM_OCCAM_PI
			case 0xE0: return ins_checknotnull(ectx);
#endif
#ifdef TVM_DYNAMIC_OCCAM_PI
			case 0xE2: return ins_malloc(ectx);
			case 0xE3: return ins_mrelease(ectx);
#endif
#ifdef TVM_OCCAM_PI
			case 0xE8: return ins_xable(ectx);
			case 0xE9: return ins_xin(ectx);
			case 0xEC: return ins_xend(ectx);
#endif
#ifdef TVM_SHORT_OPS
			case 0xF8: return ins_xsword(ectx);
			case 0xF9: return ins_lsx(ectx);
			case 0xFA: return ins_cs(ectx);
			case 0xFB: return ins_csu(ectx);
#endif
			case 0xFD: return ins_null(ectx);
			case 0xFE: return ins_shutdown(ectx);
#ifdef TVM_DYNAMIC_OCCAM_PI
			case 0x22F: return ins_proc_alloc(ectx);
			case 0x230: return ins_proc_param(ectx);
			case 0x231: return ins_proc_mt_copy(ectx);
			case 0x232: return ins_proc_mt_move(ectx);
			case 0x233: return ins_proc_start(ectx);
			case 0x234: return ins_proc_end(ectx);
#endif
#ifdef TVM_OCCAM_PI
			case 0x235: return ins_getaff(ectx);
			case 0x236: return ins_setaff(ectx);
#endif
			case 0x237: return ins_getpas(ectx);
#ifdef TVM_DYNAMIC_OCCAM_PI
			case 0x238: return ins_mt_alloc(ectx);
			case 0x239: return ins_mt_release(ectx);
			case 0x23A: return ins_mt_clone(ectx);
			case 0x23B: return ins_mt_in(ectx);
			case 0x23C: return ins_mt_out(ectx);
			case 0x23D: return ins_mt_xchg(ectx);
			case 0x23E: return ins_mt_lock(ectx);
			case 0x23F: return ins_mt_unlock(ectx);
			case 0x240: return ins_mt_enroll(ectx);
			case 0x241: return ins_mt_resign(ectx);
			case 0x242: return ins_mt_sync(ectx);
			case 0x243: return ins_mt_xin(ectx);
			case 0x244: return ins_mt_xout(ectx);
			case 0x245: return ins_mt_xxchg(ectx);
			case 0x246: return ins_mt_dclone(ectx);
			case 0x247: return ins_mt_bind(ectx);
#endif
#ifdef __RMOX_PI_SUPPORT__
			case 0x248: return ins_mb(ectx);
			case 0x249: return ins_rmb(ectx);
			case 0x24A: return ins_wmb(ectx);
#endif
#ifdef TVM_OCCAM_PI
#ifdef TVM_DYNAMIC_OCCAM_PI
			case 0x24B: return ins_ext_mt_in(ectx);
			case 0x24C: return ins_ext_mt_out(ectx);
#endif
#endif
#ifdef TVM_DYNAMIC_OCCAM_PI
			case 0x24D: return ins_mt_resize(ectx);
#endif

			}
		}
	}
	/* FIXME: This handles unimplemented and invalid instructions the same way. */
	return ECTX_INS_INVALID;
}
