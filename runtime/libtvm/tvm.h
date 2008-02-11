/*
tvm - tvm.h
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

#ifndef TVM_H
#define TVM_H

/*{{{  Pre-declare tvm data structure type */
typedef struct _tvm_t tvm_t;
/*}}}*/

#include "tvm_config.h"
#include "tvm_compiler.h"
#include "tvm_types.h"
#include "transputer.h"
#include "tvm_mem.h"
#include "tvm_time.h"

/*{{{  Memory allocator selection */
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_USE_MALLOC)
#define TVM_MALLOC	malloc
#define TVM_FREE	free
#else
#define TVM_MALLOC	...ERROR...
#define TVM_FREE	...ERROR...
#endif /* !(TVM_DYNAMIC_MEMORY && TVM_USE_MALLOC) */
/*}}}*/

/*{{{  TVM instance data structure */
#ifndef TVM_PRIVATE_DATA
#define TVM_PRIVATE_DATA	WORD
typedef TVM_PRIVATE_DATA	tvm_priv_t;
#endif /* !TVM_ECTX_PRIVATE_DATA */

struct _tvm_t {
	/* Execution context list */
	tvm_ectx_t	*head;
	tvm_ectx_t	*tail;
	/* Private data */
	tvm_priv_t	priv;
};
/*}}}*/

/*{{{  Interpreter API */
extern int tvm_init (tvm_t *tvm);
extern void tvm_ectx_reset (ECTX ectx);
extern void tvm_ectx_init (tvm_t *tvm, ECTX ectx);
extern void tvm_ectx_layout (ECTX ectx, WORDPTR base,
				const char *tlp_fmt, const int tlp_argc,
				WORD ws_size, WORD vs_size, WORD ms_size,
				WORD *size, WORDPTR *ws, WORDPTR *vs, WORDPTR *ms);
extern int tvm_ectx_install_tlp (ECTX ectx, BYTEPTR code,
				WORDPTR ws, WORDPTR vs, WORDPTR ms,
				const char *fmt, int argc, const WORD argv[]);
extern void tvm_ectx_disconnect (ECTX ectx);
extern int tvm_ectx_waiting_on (ECTX ectx, WORDPTR ws_base, WORD ws_len);
extern int tvm_dispatch (ECTX ectx);
extern int tvm_run (ECTX ectx);
extern int tvm_run_count (ECTX ectx, UWORD count);
/*}}}*/

#endif /* TVM_H */
