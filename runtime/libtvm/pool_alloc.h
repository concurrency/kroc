/*
tvm - pool_alloc.h
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

#ifndef POOL_ALLOC_H
#define POOL_ALLOC_H

#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
void dmem_init(void);
void* palloc(int size);
void* palloc_pool(int index);
void* palloc_pool_size(int index, int size);
void pfree(void* address);
void pfree_pool(int index, void* address);
#endif /* TVM_DYNAMIC_MEMORY && TVM_OCCAM_PI */

#endif
