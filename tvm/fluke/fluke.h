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
#define DISABLE_INTERRUPTS(mask) mask = disableIRQ();
#define ENABLE_INTERRUPTS(mask)  restoreIRQ(mask);

/*{{{  TVM Interrupts */
enum {
  TVM_INTR_PPI_DMA     = 1 << (SFLAG_USER_P + 0),
  TVM_INTR_TWI         = 1 << (SFLAG_USER_P + 1),
  TVM_INTR_UART0_RX    = 1 << (SFLAG_USER_P + 2),
  TVM_INTR_UART0_TX    = 1 << (SFLAG_USER_P + 3),
	TVM_INTR_MAGIC_TIMER = 1 << (SFLAG_USER_P + 4)
};
#define TVM_INTR_SFLAGS  \
  (SFLAG_INTR            | \
   TVM_INTR_PPI_DMA      | \
   TVM_INTR_TWI          | \
   TVM_INTR_UART0_RX     | \
   TVM_INTR_UART0_TX     | \
   TVM_INTR_MAGIC_TIMER)
void raise_tvm_interrupt (WORD flag);
/*}}}*/

// This conditionally enables IRQs at the 
// start of our code. For Magic Timer exploration.
// Leaving it in is probably a good idea for now.
#define TIMER_INT_MODE 1

#define WB_CACHE_FLUSH { \
  volatile int dummy_wb_cache_flush; \
  dummy_wb_cache_flush = 0; \
}

/*{{{  Assembly Macros */
#define BARRIER	__asm__ __volatile__ ("" : : : "memory")
#if 0
#define CSYNC	__asm__ __volatile__ ("csync;" : : : "memory")
#define SSYNC	__asm__ __volatile__ ("ssync;" : : : "memory")
#define IDLE	__asm__ __volatile__ ("idle;")
#define NOP	__asm__ __volatile__ ("nop;")
#endif
/*}}}*/

/*{{{ UARTs */
void init_uart0 (uint16_t baud, uint8_t mode, uint8_t fmode);
int uart0_in (ECTX ectx, WORD count, BYTEPTR pointer);
int uart0_out (ECTX ectx, WORD count, BYTEPTR pointer);
void complete_uart0_rx_interrupt (ECTX ectx);
int uart0_is_blocking (void);
void uart0_send_char (const unsigned char ch);
/*}}}*/

/*{{{ Time and Sleep */
void init_timer (void);
WORD arm7tdmi_get_time(ECTX ectx);
void sleep_until(WORD timeout);
void sleep(void);

int  timer_in (ECTX ectx, WORD count, BYTEPTR pointer);
int led_toggle_out (ECTX ectx, WORD count, BYTEPTR pointer);

/*}}}*/

/*{{{ Debug printing */
void debug_print_chr (const unsigned char c);
void debug_print_hex (unsigned int val);
void debug_print_str (const char *str);
/*}}}*/

int run_tvm( void );

#endif /* FLUKE_TVM_H */

extern const char version_string[];
