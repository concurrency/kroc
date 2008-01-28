/*
tvm - ins_t800.c
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

#ifndef INS_T800_H
#define INS_T800_H

/****************************************************************************
 *              0x25 0xF_         0x25 0xF_         0x25 0xF_               *
 ****************************************************************************/
/* 0x5A - 0x25 0xFA - dup - duplicate top of stack */
TVM_INSTRUCTION_PROTO (ins_dup);

/****************************************************************************
 *              0x28 0xF_         0x28 0xF_         0x28 0xF_               *
 ****************************************************************************/
/* 0x81 - 0x28 0xF1 - wsubdb - word subscript (INT64?) */
TVM_INSTRUCTION_PROTO (ins_wsubdb);

#endif /* INS_T800_H */
