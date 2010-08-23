#ifndef TVM_NXT_H
#define TVM_NXT_H

/* Platform type definitions */
typedef unsigned char uint8_t; 
typedef signed char int8_t;
typedef unsigned short uint16_t;
typedef signed short int16_t;
typedef unsigned long uint32_t;
typedef signed long int32_t;

typedef uint32_t size_t;

typedef uint8_t bool;
#define FALSE (0)
#define TRUE (!FALSE)


/* Define for lots of useful printed-out stuff. */
#undef DEBUG


#define TVM_ECTX_PRIVATE_DATA 	tvm_ectx_priv_t
typedef struct _tvm_ectx_priv_t {
	void            *memory;
	int             memory_length;
} tvm_ectx_priv_t;

#include <tvm.h>
#include <tvm_tbc.h>


enum {
	TVM_INTR_VIRTUAL = 1 << (SFLAG_USER_P + 0)
};
#define TVM_INTR_SFLAGS \
	(SFLAG_INTR | \
	TVM_INTR_VIRTUAL)

/*{{{  sffi.c */
extern SFFI_FUNCTION sffi_table[];
extern const int sffi_table_length;
/*}}}*/
#if 0
/*{{{  interrupts.c */
extern void init_interrupts (void);
extern void clear_pending_interrupts (void);
extern int waiting_on_interrupts (void);
extern int ffi_wait_for_interrupt (ECTX ectx, WORD args[]);
/*}}}*/
#endif
/*{{{  tbc.c */
extern UWORD valid_tbc_header (BYTE *data);
extern tbc_t *load_context_with_tbc (ECTX ectx, tbc_t *tbc, BYTE *data, UWORD length);
/*}}}*/
/*{{{  tvm.c */
extern void main (void);
/*}}}*/

#endif /* !TVM_NXT_H */
