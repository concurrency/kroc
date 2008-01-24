/*
tvm - ins_barrier.h
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

#ifndef BARRIER_H
#define BARRIER_H

#ifdef __PI_SUPPORT__

TVM_HELPER_PROTO void tvm_bar_init(POOTER bar, UWORD initial_count);
TVM_HELPER_PROTO void tvm_bar_sync(POOTER bar);
TVM_HELPER_PROTO void tvm_bar_enroll(POOTER bar, UWORD enroll_count);
TVM_HELPER_PROTO void tvm_bar_resign(POOTER bar, UWORD resign_count);

/* 0xB0 - 0x2B 0xF0 - barrier intialisation */
TVM_INSTRUCTION_PROTO void ins_barinit();
/* 0xB1 - 0x2B 0xF1 - barrier synchronisation */
TVM_INSTRUCTION_PROTO void ins_barsync();
/* 0xB2 - 0x2B 0xF2 - barrier resignation */
TVM_INSTRUCTION_PROTO void ins_barresign();
/* 0xB3 - 0x2B 0xF3 - barrier enroll */
TVM_INSTRUCTION_PROTO void ins_barenroll();

#endif /*__PI_SUPPORT__ */

#endif /*BARRIER_H*/
