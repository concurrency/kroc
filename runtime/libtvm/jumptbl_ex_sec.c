/* Generated automatically by make-dispatch.py; do not modify! */

#include "transputer.h"
#include "instructions.h"
#include "ins_mt.h"
#include "ins_proc.h"
#include "ins_rmox.h"

const unsigned int extended_secondaries_min = 559;
const unsigned int extended_secondaries_max = 586;

void (*const extended_secondaries[28])(void) =
{
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_proc_alloc,          /* 2F */
#else
	ins_not_implemented,     /* 2F */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_proc_param,          /* 30 */
#else
	ins_not_implemented,     /* 30 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_proc_mt_copy,        /* 31 */
#else
	ins_not_implemented,     /* 31 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_proc_mt_move,        /* 32 */
#else
	ins_not_implemented,     /* 32 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_proc_start,          /* 33 */
#else
	ins_not_implemented,     /* 33 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_proc_end,            /* 34 */
#else
	ins_not_implemented,     /* 34 */
#endif
#ifdef TVM_OCCAM_PI
	ins_getaff,              /* 35 */
#else
	ins_not_implemented,     /* 35 */
#endif
#ifdef TVM_OCCAM_PI
	ins_setaff,              /* 36 */
#else
	ins_not_implemented,     /* 36 */
#endif
	ins_getpas,              /* 37 */
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_alloc,            /* 38 */
#else
	ins_not_implemented,     /* 38 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_release,          /* 39 */
#else
	ins_not_implemented,     /* 39 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_clone,            /* 3A */
#else
	ins_not_implemented,     /* 3A */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_in,               /* 3B */
#else
	ins_not_implemented,     /* 3B */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_out,              /* 3C */
#else
	ins_not_implemented,     /* 3C */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_xchg,             /* 3D */
#else
	ins_not_implemented,     /* 3D */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_lock,             /* 3E */
#else
	ins_not_implemented,     /* 3E */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_unlock,           /* 3F */
#else
	ins_not_implemented,     /* 3F */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_enroll,           /* 40 */
#else
	ins_not_implemented,     /* 40 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_resign,           /* 41 */
#else
	ins_not_implemented,     /* 41 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_sync,             /* 42 */
#else
	ins_not_implemented,     /* 42 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_xin,              /* 43 */
#else
	ins_not_implemented,     /* 43 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_xout,             /* 44 */
#else
	ins_not_implemented,     /* 44 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_xxchg,            /* 45 */
#else
	ins_not_implemented,     /* 45 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_dclone,           /* 46 */
#else
	ins_not_implemented,     /* 46 */
#endif
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	ins_mt_bind,             /* 47 */
#else
	ins_not_implemented,     /* 47 */
#endif
#ifdef __RMOX_PI_SUPPORT__
	ins_mb,                  /* 48 */
#else
	ins_not_implemented,     /* 48 */
#endif
#ifdef __RMOX_PI_SUPPORT__
	ins_rmb,                 /* 49 */
#else
	ins_not_implemented,     /* 49 */
#endif
#ifdef __RMOX_PI_SUPPORT__
	ins_wmb,                 /* 4A */
#else
	ins_not_implemented,     /* 4A */
#endif

};
