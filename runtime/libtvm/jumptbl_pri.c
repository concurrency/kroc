/* Generated automatically by make-dispatch.py; do not modify! */

#include "transputer.h"
#include "instructions.h"
#include "ins_pri.h"

const unsigned int primaries_min = 0;
const unsigned int primaries_max = 15;

void (*const primaries[16])(void) =
{
	ins_j,                   /* 00 */
	ins_ldlp,                /* 01 */
	ins_pfix,                /* 02 */
	ins_ldnl,                /* 03 */
	ins_ldc,                 /* 04 */
	ins_ldnlp,               /* 05 */
	ins_nfix,                /* 06 */
	ins_ldl,                 /* 07 */
	ins_adc,                 /* 08 */
	ins_call,                /* 09 */
	ins_cj,                  /* 0A */
	ins_ajw,                 /* 0B */
	ins_eqc,                 /* 0C */
	ins_stl,                 /* 0D */
	ins_stnl,                /* 0E */
#ifndef TVM_DISPATCH_SWITCH
	ins_opr                  /* 0F */
#else
	ins_not_implemented      /* 0F */
#endif

};
