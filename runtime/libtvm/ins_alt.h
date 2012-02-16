/*
tvm - ins_alt.h
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2004-2008 Christian L. Jacobsen, Matthew C. Jadud

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef INS_ALT_H
#define INS_ALT_H

#include "instructions.h"

/****************************************************************************
 *              0x22 0xF_         0x22 0xF_         0x22 0xF_               *
 ****************************************************************************/

/* 0x2F - 0x22 0xFF - disc - disable channel */
TVM_INSTRUCTION_PROTO (ins_disc);
/* 0x2E - 0x22 0xFE - dist - disable timer */
TVM_INSTRUCTION_PROTO (ins_dist);





/****************************************************************************
 *              0x23 0xF_         0x23 0xF_         0x23 0xF_               *
 ****************************************************************************/

/* 0x30 - 0x23 0xF0 - diss - disable skip */
TVM_INSTRUCTION_PROTO (ins_diss);





/****************************************************************************
 *              0x24 0xF_         0x24 0xF_         0x24 0xF_               *
 ****************************************************************************/

/* 0x43 - 0x24 0xF3 - alt - alt start */
TVM_INSTRUCTION_PROTO (ins_alt);
/* 0x44 - 0x24 0xF4 - altwt - alt wait */
TVM_INSTRUCTION_PROTO (ins_altwt);
/* 0x45 - 0x24 0xF5 - altend - alt end */
TVM_INSTRUCTION_PROTO (ins_altend);
/* 0x47 - 0x24 0xF7 - enbt - enable timer */
TVM_INSTRUCTION_PROTO (ins_enbt);
/* 0x48 - 0x24 0xF8 - enbc - enable channel */
TVM_INSTRUCTION_PROTO (ins_enbc);
/* 0x49 - 0x24 0xF9 - enbs - enable skip */
TVM_INSTRUCTION_PROTO (ins_enbs);
/* 0x4E - 0x24 0xFE - talt - timer alt start */
TVM_INSTRUCTION_PROTO (ins_talt);





/****************************************************************************
 *              0x25 0xF_         0x25 0xF_         0x25 0xF_               *
 ****************************************************************************/

/* 0x51 - 0x25 0xF1 - taltwt - timer alt wait */
TVM_INSTRUCTION_PROTO (ins_taltwt);

#endif /* INS_ALT_H */
