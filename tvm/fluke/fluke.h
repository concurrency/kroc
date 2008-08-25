#ifndef FLUKE_TVM_H
#define FLUKE_TVM_H

#include "tvm.h"
#include "lpc210x.h"
#include "armVIC.h"

#define MT_TVM
#define MT_DEFINES

#define LED         (1 << 17)

/* 0MiB - 32MiB SDRAM */
#define SDRAM_BOTTOM  0x00000000  /* address of the base of SDRAM */
#define SDRAM_TOP 0x02000000  /* address of the top of SDRAM */

/* >= 128 KiB dynamic memory */
#define DMEM_START  0x00020000  /* address in SDRAM of dynamic memory */

#define ISR_PROTO(name) void name (void) __attribute__((naked));
#define ISR void 

ISR_PROTO(timerISR)

#define TIMER_INT_MODE 1

/*{{{  Assembly Macros */
#define BARRIER	__asm__ __volatile__ ("" : : : "memory")
#define CSYNC	__asm__ __volatile__ ("csync;" : : : "memory")
#define SSYNC	__asm__ __volatile__ ("ssync;" : : : "memory")
#define IDLE	__asm__ __volatile__ ("idle;")
#define NOP	__asm__ __volatile__ ("nop;")
#define DISABLE_INTERRUPTS(mask) \
	__asm__ __volatile__ ("cli %0;" : "=d" (mask))
#define ENABLE_INTERRUPTS(mask) \
	__asm__ __volatile__ ("sti %0;" : : "d" (mask))
#define RAISE_INTERRUPT(n) \
	__asm__ __volatile__ ("raise %0;" : : "i" (n))
/*}}}*/

/*{{{  TVM Interrupts */
enum {
	TVM_INTR_PPI_DMA	= 1 << (SFLAG_USER_P + 0),
	TVM_INTR_TWI		= 1 << (SFLAG_USER_P + 1),
	TVM_INTR_UART0_RX	= 1 << (SFLAG_USER_P + 2),
	TVM_INTR_UART0_TX	= 1 << (SFLAG_USER_P + 3)
};
#define TVM_INTR_SFLAGS \
	(SFLAG_INTR 		| \
	 TVM_INTR_PPI_DMA	| \
	 TVM_INTR_TWI		| \
	 TVM_INTR_UART0_RX	| \
	 TVM_INTR_UART0_TX)
/*}}}*/

/*{{{ UARTs */
int uart0_in (ECTX ectx, WORD count, BYTEPTR pointer);
int uart0_out (ECTX ectx, WORD count, BYTEPTR pointer);
/*{{{*/

/*{{{ Time and Sleep */
WORD arm7tdmi_get_time(ECTX ectx);
void sleep_until(WORD timeout);
void sleep(void);
/*}}}*/

/*{{{ Debug printing */
void debug_print_chr (const unsigned char c);
void debug_print_hex (unsigned int val);
void debug_print_str (const char *str);
/*}}}*/

int run_tvm( void );

#endif /* FLUKE_TVM_H */
