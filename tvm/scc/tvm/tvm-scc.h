#ifndef TVM_SCC_H
#define TVM_SCC_H

#include <stddef.h>
#include <stdio.h>
#include <scc.h>
#include <apic.h>
#include <interrupt.h>
#include <clock.h>

#include <tvm.h>

enum {
	TVM_INTR_VIRTUAL = 1 << (SFLAG_USER_P + 0)
};
#define TVM_INTR_SFLAGS \
	(SFLAG_INTR | \
	TVM_INTR_VIRTUAL)

/*{{{  ffi.c */
extern SFFI_FUNCTION sffi_table[];
extern const int sffi_table_length;
/*}}}*/
/*{{{  tbc.c */
extern int init_context_from_tbc (ECTX context, BYTE *data, WORDPTR memory, UWORD memory_size);
/*}}}*/
/*{{{  tvm.c */
extern tvm_ectx_t context;
extern void terminate (const BYTE *message, const int *status);
extern int main (void);
/*}}}*/

#endif //TVM_SCC_H
