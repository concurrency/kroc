/*
tvm - ins_rmox.c
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2004-2008 Christian L. Jacobsen, Matthew C. Jadud, Carl G. Ritson

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

#include "tvm.h"
#include "ins_rmox.h"

#ifdef __RMOX_PI_SUPPORT__

/* 0x248 - 0x22 0x24 0xF8 - mb - memory barrier */
TVM_INSTRUCTION (ins_mb)
{
	/* currently a no-op */
	UNDEFINE_STACK ();
}

/* 0x249 - 0x22 0x24 0xF9 - rmb - read memory barrier */
TVM_INSTRUCTION (ins_rmb)
{
	/* currently a no-op */
	UNDEFINE_STACK ();
}

/* 0x24A - 0x22 0x24 0xFA - wmb - write memory barrier */
TVM_INSTRUCTION (ins_wmb)
{
	/* currently a no-op */
	UNDEFINE_STACK ();
}

#endif /* __RMOX_PI_SUPPORT__ */

