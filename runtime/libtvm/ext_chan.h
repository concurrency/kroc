/*
tvm - ext_chan.h
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

#ifndef EXT_CHAN_H
#define EXT_CHAN_H

#include "transputer.h"

#define EXT_CHAN_ENTRIES 19
#define EXT_CHAN_KYB 0
#define EXT_CHAN_SCR 1
#define EXT_CHAN_ERR 2

typedef void (*EXT_CHAN_FUNCTION)(WORD count, BYTEPTR address);

extern EXT_CHAN_FUNCTION ext_chan_table[];

#endif
