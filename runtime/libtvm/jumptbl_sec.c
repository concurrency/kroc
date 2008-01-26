/* Generated automatically by make-dispatch.py; do not modify! */

#include "transputer.h"
#include "instructions.h"
#include "dmem.h"
#include "ins_alt.h"
#include "ins_barrier.h"
#include "ins_chan.h"
#include "ins_float.h"
#include "ins_fred.h"
#include "ins_sec.h"
#include "ins_t800.h"
#include "ins_timer.h"

const unsigned int secondaries_min = 0;
const unsigned int secondaries_max = 254;

void (*const secondaries[255])(void) =
{
	ins_rev,                 /* 00 */
	ins_lb,                  /* 01 */
	ins_bsub,                /* 02 */
	ins_endp,                /* 03 */
	ins_diff,                /* 04 */
	ins_add,                 /* 05 */
	ins_gcall,               /* 06 */
	ins_in,                  /* 07 */
	ins_prod,                /* 08 */
	ins_gt,                  /* 09 */
	ins_wsub,                /* 0A */
	ins_out,                 /* 0B */
	ins_sub,                 /* 0C */
	ins_startp,              /* 0D */
	ins_outbyte,             /* 0E */
	ins_outword,             /* 0F */
	ins_seterr,              /* 10 */
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mreleasep,           /* 11 */
#else
	ins_not_implemented,     /* 11 */
#endif
	ins_not_implemented,     /* 12 */
	ins_csub0,               /* 13 */
#ifdef TVM_OCCAM_PI
	ins_extvrfy,             /* 14 */
#else
	ins_not_implemented,     /* 14 */
#endif
	ins_stopp,               /* 15 */
	ins_ladd,                /* 16 */
	ins_not_implemented,     /* 17 */
	ins_not_implemented,     /* 18 */
	ins_norm,                /* 19 */
	ins_ldiv,                /* 1A */
	ins_ldpi,                /* 1B */
	ins_not_implemented,     /* 1C */
	ins_xdble,               /* 1D */
	ins_not_implemented,     /* 1E */
	ins_rem,                 /* 1F */
	ins_ret,                 /* 20 */
	ins_lend,                /* 21 */
	ins_ldtimer,             /* 22 */
	ins_boolinvert,          /* 23 */
	ins_widenshort,          /* 24 */
	ins_fficall,             /* 25 */
	ins_lend3,               /* 26 */
	ins_lendbw,              /* 27 */
	ins_reschedule,          /* 28 */
	ins_not_implemented,     /* 29 */
	ins_not_implemented,     /* 2A */
	ins_tin,                 /* 2B */
	ins_div,                 /* 2C */
	ins_not_implemented,     /* 2D */
	ins_dist,                /* 2E */
	ins_disc,                /* 2F */
	ins_diss,                /* 30 */
	ins_lmul,                /* 31 */
	ins_not,                 /* 32 */
	ins_xor,                 /* 33 */
	ins_not_implemented,     /* 34 */
	ins_lshr,                /* 35 */
	ins_lshl,                /* 36 */
	ins_lsum,                /* 37 */
	ins_lsub,                /* 38 */
	ins_runp,                /* 39 */
	ins_not_implemented,     /* 3A */
	ins_sb,                  /* 3B */
	ins_gajw,                /* 3C */
	ins_not_implemented,     /* 3D */
	ins_saveh,               /* 3E */
	ins_not_implemented,     /* 3F */
	ins_shr,                 /* 40 */
	ins_shl,                 /* 41 */
	ins_mint,                /* 42 */
	ins_alt,                 /* 43 */
	ins_altwt,               /* 44 */
	ins_altend,              /* 45 */
	ins_and,                 /* 46 */
	ins_enbt,                /* 47 */
	ins_enbc,                /* 48 */
	ins_enbs,                /* 49 */
	ins_move,                /* 4A */
	ins_or,                  /* 4B */
	ins_csngl,               /* 4C */
	ins_ccnt1,               /* 4D */
	ins_talt,                /* 4E */
	ins_ldiff,               /* 4F */
	ins_not_implemented,     /* 50 */
	ins_taltwt,              /* 51 */
	ins_sum,                 /* 52 */
	ins_mul,                 /* 53 */
	ins_not_implemented,     /* 54 */
	ins_stoperr,             /* 55 */
	ins_cword,               /* 56 */
	ins_not_implemented,     /* 57 */
	ins_not_implemented,     /* 58 */
	ins_not_implemented,     /* 59 */
	ins_dup,                 /* 5A */
	ins_not_implemented,     /* 5B */
	ins_not_implemented,     /* 5C */
	ins_not_implemented,     /* 5D */
	ins_not_implemented,     /* 5E */
	ins_not_implemented,     /* 5F */
#ifdef TVM_OCCAM_PI
	ins_extin,               /* 60 */
#else
	ins_not_implemented,     /* 60 */
#endif
#ifdef TVM_OCCAM_PI
	ins_extout,              /* 61 */
#else
	ins_not_implemented,     /* 61 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_minn,                /* 62 */
#else
	ins_not_implemented,     /* 62 */
#endif
#if defined(TVM_EMULATE_T4) || defined(TVM_EMULATE_T8)
	ins_unpacksn,            /* 63 */
#else
	ins_not_implemented,     /* 63 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_moutn,               /* 64 */
#else
	ins_not_implemented,     /* 64 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_xminn,               /* 65 */
#else
	ins_not_implemented,     /* 65 */
#endif
	ins_not_implemented,     /* 66 */
	ins_not_implemented,     /* 67 */
	ins_not_implemented,     /* 68 */
	ins_not_implemented,     /* 69 */
	ins_not_implemented,     /* 6A */
	ins_not_implemented,     /* 6B */
#if defined(TVM_EMULATE_T4) || defined(TVM_EMULATE_T8)
	ins_postnormsn,          /* 6C */
#else
	ins_not_implemented,     /* 6C */
#endif
#if defined(TVM_EMULATE_T4) || defined(TVM_EMULATE_T8)
	ins_roundsn,             /* 6D */
#else
	ins_not_implemented,     /* 6D */
#endif
	ins_not_implemented,     /* 6E */
	ins_not_implemented,     /* 6F */
	ins_not_implemented,     /* 70 */
#if defined(TVM_EMULATE_T4) || defined(TVM_EMULATE_T8)
	ins_ldinf,               /* 71 */
#else
	ins_not_implemented,     /* 71 */
#endif
#if defined(TVM_EMULATE_T4) || defined(TVM_EMULATE_T8)
	ins_fmul,                /* 72 */
#else
	ins_not_implemented,     /* 72 */
#endif
	ins_not_implemented,     /* 73 */
	ins_not_implemented,     /* 74 */
	ins_not_implemented,     /* 75 */
	ins_not_implemented,     /* 76 */
	ins_not_implemented,     /* 77 */
	ins_not_implemented,     /* 78 */
	ins_pop,                 /* 79 */
#ifdef TVM_OCCAM_PI
	ins_seminit,             /* 7A */
#else
	ins_not_implemented,     /* 7A */
#endif
#ifdef TVM_OCCAM_PI
	ins_semclaim,            /* 7B */
#else
	ins_not_implemented,     /* 7B */
#endif
#ifdef TVM_OCCAM_PI
	ins_semrelease,          /* 7C */
#else
	ins_not_implemented,     /* 7C */
#endif
	ins_not_implemented,     /* 7D */
	ins_not_implemented,     /* 7E */
	ins_not_implemented,     /* 7F */
	ins_not_implemented,     /* 80 */
	ins_wsubdb,              /* 81 */
#ifdef TVM_EMULATE_T8
	ins_fpldnldbi,           /* 82 */
#else
	ins_not_implemented,     /* 82 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpchkerr,            /* 83 */
#else
	ins_not_implemented,     /* 83 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpstnldb,            /* 84 */
#else
	ins_not_implemented,     /* 84 */
#endif
	ins_not_implemented,     /* 85 */
#ifdef TVM_EMULATE_T8
	ins_fpldnlsni,           /* 86 */
#else
	ins_not_implemented,     /* 86 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpadd,               /* 87 */
#else
	ins_not_implemented,     /* 87 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpstnlsn,            /* 88 */
#else
	ins_not_implemented,     /* 88 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpsub,               /* 89 */
#else
	ins_not_implemented,     /* 89 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpldnldb,            /* 8A */
#else
	ins_not_implemented,     /* 8A */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpmul,               /* 8B */
#else
	ins_not_implemented,     /* 8B */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpdiv,               /* 8C */
#else
	ins_not_implemented,     /* 8C */
#endif
	ins_not_implemented,     /* 8D */
#ifdef TVM_EMULATE_T8
	ins_fpldnlsn,            /* 8E */
#else
	ins_not_implemented,     /* 8E */
#endif
	ins_not_implemented,     /* 8F */
	ins_not_implemented,     /* 90 */
#ifdef TVM_EMULATE_T8
	ins_fpnan ,              /* 91 */
#else
	ins_not_implemented,     /* 91 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpordered ,          /* 92 */
#else
	ins_not_implemented,     /* 92 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpnotfinite ,        /* 93 */
#else
	ins_not_implemented,     /* 93 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpgt ,               /* 94 */
#else
	ins_not_implemented,     /* 94 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpeq ,               /* 95 */
#else
	ins_not_implemented,     /* 95 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpi32tor32,          /* 96 */
#else
	ins_not_implemented,     /* 96 */
#endif
	ins_not_implemented,     /* 97 */
#ifdef TVM_EMULATE_T8
	ins_fpi32tor64 ,         /* 98 */
#else
	ins_not_implemented,     /* 98 */
#endif
	ins_not_implemented,     /* 99 */
#ifdef TVM_EMULATE_T8
	ins_fpb32tor64,          /* 9A */
#else
	ins_not_implemented,     /* 9A */
#endif
	ins_not_implemented,     /* 9B */
	ins_not_implemented,     /* 9C */
#ifdef TVM_EMULATE_T8
	ins_fprtoi32,            /* 9D */
#else
	ins_not_implemented,     /* 9D */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpstnli32,           /* 9E */
#else
	ins_not_implemented,     /* 9E */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpldzerosn,          /* 9F */
#else
	ins_not_implemented,     /* 9F */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpldzerodb,          /* A0 */
#else
	ins_not_implemented,     /* A0 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpint,               /* A1 */
#else
	ins_not_implemented,     /* A1 */
#endif
	ins_getpri,              /* A2 */
#ifdef TVM_EMULATE_T8
	ins_fpdup,               /* A3 */
#else
	ins_not_implemented,     /* A3 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fprev,               /* A4 */
#else
	ins_not_implemented,     /* A4 */
#endif
	ins_setpri,              /* A5 */
#ifdef TVM_EMULATE_T8
	ins_fpldnladddb,         /* A6 */
#else
	ins_not_implemented,     /* A6 */
#endif
	ins_not_implemented,     /* A7 */
#ifdef TVM_EMULATE_T8
	ins_fpldnlmuldb,         /* A8 */
#else
	ins_not_implemented,     /* A8 */
#endif
	ins_not_implemented,     /* A9 */
#ifdef TVM_EMULATE_T8
	ins_fpldnladdsn,         /* AA */
#else
	ins_not_implemented,     /* AA */
#endif
	ins_not_implemented,     /* AB */
#ifdef TVM_EMULATE_T8
	ins_fpldnlmulsn,         /* AC */
#else
	ins_not_implemented,     /* AC */
#endif
	ins_savecreg,            /* AD */
	ins_restorecreg,         /* AE */
	ins_not_implemented,     /* AF */
#ifdef TVM_OCCAM_PI
	ins_barinit,             /* B0 */
#else
	ins_not_implemented,     /* B0 */
#endif
#ifdef TVM_OCCAM_PI
	ins_barsync,             /* B1 */
#else
	ins_not_implemented,     /* B1 */
#endif
#ifdef TVM_OCCAM_PI
	ins_barresign,           /* B2 */
#else
	ins_not_implemented,     /* B2 */
#endif
#ifdef TVM_OCCAM_PI
	ins_barenroll,           /* B3 */
#else
	ins_not_implemented,     /* B3 */
#endif
	ins_not_implemented,     /* B4 */
	ins_not_implemented,     /* B5 */
	ins_not_implemented,     /* B6 */
	ins_not_implemented,     /* B7 */
	ins_not_implemented,     /* B8 */
	ins_not_implemented,     /* B9 */
	ins_not_implemented,     /* BA */
	ins_not_implemented,     /* BB */
	ins_not_implemented,     /* BC */
	ins_not_implemented,     /* BD */
	ins_not_implemented,     /* BE */
	ins_not_implemented,     /* BF */
	ins_not_implemented,     /* C0 */
	ins_not_implemented,     /* C1 */
	ins_not_implemented,     /* C2 */
	ins_not_implemented,     /* C3 */
	ins_not_implemented,     /* C4 */
	ins_not_implemented,     /* C5 */
	ins_not_implemented,     /* C6 */
	ins_not_implemented,     /* C7 */
	ins_not_implemented,     /* C8 */
	ins_not_implemented,     /* C9 */
	ins_not_implemented,     /* CA */
	ins_not_implemented,     /* CB */
	ins_not_implemented,     /* CC */
	ins_not_implemented,     /* CD */
	ins_not_implemented,     /* CE */
#ifdef TVM_EMULATE_T8
	ins_fprem,               /* CF */
#else
	ins_not_implemented,     /* CF */
#endif
#ifdef TVM_EMULATE_T8
	ins_i64toreal,           /* D0 */
#else
	ins_not_implemented,     /* D0 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpdivby2,            /* D1 */
#else
	ins_not_implemented,     /* D1 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpmulby2,            /* D2 */
#else
	ins_not_implemented,     /* D2 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpsqrt,              /* D3 */
#else
	ins_not_implemented,     /* D3 */
#endif
	ins_not_implemented,     /* D4 */
	ins_not_implemented,     /* D5 */
#ifdef TVM_EMULATE_T8
	ins_fprz,                /* D6 */
#else
	ins_not_implemented,     /* D6 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpr32to64,           /* D7 */
#else
	ins_not_implemented,     /* D7 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpr64to32,           /* D8 */
#else
	ins_not_implemented,     /* D8 */
#endif
#ifdef TVM_EMULATE_T8
	ins_fpexpdec32,          /* D9 */
#else
	ins_not_implemented,     /* D9 */
#endif
	ins_not_implemented,     /* DA */
#ifdef TVM_EMULATE_T8
	ins_fpabs,               /* DB */
#else
	ins_not_implemented,     /* DB */
#endif
	ins_not_implemented,     /* DC */
	ins_not_implemented,     /* DD */
	ins_not_implemented,     /* DE */
#ifdef TVM_EMULATE_T8
	ins_fpchki64,            /* DF */
#else
	ins_not_implemented,     /* DF */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mnew,                /* E0 */
#else
	ins_not_implemented,     /* E0 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mfree,               /* E1 */
#else
	ins_not_implemented,     /* E1 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_malloc,              /* E2 */
#else
	ins_not_implemented,     /* E2 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mrelease,            /* E3 */
#else
	ins_not_implemented,     /* E3 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_min,                 /* E4 */
#else
	ins_not_implemented,     /* E4 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mout,                /* E5 */
#else
	ins_not_implemented,     /* E5 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_min64,               /* E6 */
#else
	ins_not_implemented,     /* E6 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mout64,              /* E7 */
#else
	ins_not_implemented,     /* E7 */
#endif
#ifdef TVM_OCCAM_PI
	ins_xable,               /* E8 */
#else
	ins_not_implemented,     /* E8 */
#endif
#ifdef TVM_OCCAM_PI
	ins_xin,                 /* E9 */
#else
	ins_not_implemented,     /* E9 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_xmin,                /* EA */
#else
	ins_not_implemented,     /* EA */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_xmin64,              /* EB */
#else
	ins_not_implemented,     /* EB */
#endif
#ifdef TVM_OCCAM_PI
	ins_xend,                /* EC */
#else
	ins_not_implemented,     /* EC */
#endif
	ins_not_implemented,     /* ED */
	ins_not_implemented,     /* EE */
	ins_not_implemented,     /* EF */
	ins_not_implemented,     /* F0 */
	ins_not_implemented,     /* F1 */
	ins_not_implemented,     /* F2 */
	ins_not_implemented,     /* F3 */
	ins_not_implemented,     /* F4 */
	ins_not_implemented,     /* F5 */
	ins_not_implemented,     /* F6 */
	ins_not_implemented,     /* F7 */
	ins_not_implemented,     /* F8 */
	ins_not_implemented,     /* F9 */
	ins_not_implemented,     /* FA */
	ins_not_implemented,     /* FB */
	ins_not_implemented,     /* FC */
	ins_null,                /* FD */
	ins_shutdown,            /* FE */

};
