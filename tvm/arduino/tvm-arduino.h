#ifndef TVM_ARDUINO_H
#define TVM_ARDUINO_H

/* Define for lots of useful printed-out stuff. */
#undef DEBUG

#include <stdio.h>
#include <stdint.h>
#include <avr/io.h>
#include <avr/interrupt.h>

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
/*{{{  interrupts.c */
extern void init_interrupts (void);
extern void clear_pending_interrupts (void);
extern int waiting_on_interrupts (void);
extern int ffi_wait_for_interrupt (ECTX ectx, WORD args[]);
/*}}}*/
/*{{{  serial.c */
extern void serial_stdout_init(long speed);
extern int ffi_read_buffer_blocking (ECTX ectx, WORD args[]);
/*}}}*/
/*{{{  tbc.c */
extern int tbc_file_and_line (const prog_char *data, UWORD offset, const prog_char **file, UWORD *line);
extern int init_context_from_tbc (ECTX context, const prog_char *data, WORDPTR memory, UWORD memory_size);
/*}}}*/
/*{{{  time.c */
extern unsigned long time_millis (void);
extern unsigned long time_micros (void);
extern void time_init (void);
/*}}}*/
/*{{{  tvm.c */
extern tvm_ectx_t context;
extern void terminate (const prog_char *message, const int *status);
extern int main (void);
/*}}}*/

#endif
