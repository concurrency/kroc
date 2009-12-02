/*
tvm - tvm_compiler.h
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
#ifndef TVM_COMPILER_H
#define TVM_COMPILER_H

#include "tvm_config.h"

/* stddef to get NULL */
#include <stddef.h>

/* alignment */
#if defined(__GNUC__) || defined(__SUNPRO_C)
#define TVM_WORD_ALIGN __attribute__ ((aligned (TVM_WORD_LENGTH)))
#else
#define TVM_WORD_ALIGN
#endif

/* inlining */
#ifdef TVM_USE_INLINE
#define TVM_INLINE inline
#else
#define TVM_INLINE
#endif

/* structure packing */
#if defined(__GNUC__)
#define TVM_PACK	__attribute__ ((packed))
#else
#define TVM_PACK
#endif

/* unused */
#if defined(__GNUC__) || defined(__SUNPRO_C)
#define TVM_UNUSED_OK	__attribute__ ((unused))
#else
#define TVM_UNUSED_OK
#endif

#endif /* !TVM_COMPILER_H */
