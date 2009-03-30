/*
 * srv.h - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2007-2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

/* TVM */
#include <tvm.h>
#define MT_TVM
#define MT_DEFINES
#include <mobile_types.h>

/* Blackfin */
#include <cdefBF537.h>

/* Configuration */
#include "bfin_config.h"
#include "memory_map.h"

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

/*{{{  debug.c */
void debug_print_chr (const unsigned char c);
void debug_print_hex (unsigned int val);
void debug_print_str (const char *str);
/*}}}*/

/*{{{  gpio.c */
void init_gpio (void);
/*}}}*/

/*{{{  imagery.c */
int draw_caption_on_frame (ECTX ectx, WORD args[]);
int jpeg_encode_frame (ECTX ectx, WORD args[]);
/*}}}*/

/*{{{  ppi_dma.c */
void init_ppi_dma (void);
void handle_int8 (void);
void complete_ppi_dma_interrupt (ECTX ectx);
int ppi_dma_is_blocking (void);
int ppi_dma_in (ECTX ectx, WORD count, BYTEPTR pointer);
int ppi_dma_out (ECTX ectx, WORD count, BYTEPTR pointer);
int ppi_dma_mt_in (ECTX ectx, WORDPTR pointer);
/*}}}*/

/*{{{  timer.c */
void init_timers (void);
void delay_us (WORD delay);
WORD srv_get_time (ECTX ectx);
void sleep_until (WORD timeout);
void sleep (void);
/*}}}*/

/*{{{  tvm.c */
int tvm_interrupt_pending (void);
void raise_tvm_interrupt (WORD n);
int run_tvm (void);
/*}}}*/

/*{{{  twi.c */
void init_twi (void);
void handle_int13 (void);
void complete_twi_interrupt (ECTX ectx);
int twi_is_blocking (void);
int twi_in (ECTX ectx, WORD count, BYTEPTR pointer);
int twi_out (ECTX ectx, WORD count, BYTEPTR pointer);
int twi_mt_in (ECTX ectx, WORDPTR pointer);
int twi_mt_out (ECTX ectx, WORDPTR pointer);
/*}}}*/

/*{{{  uart.c */
void init_uart (void);
void handle_int10 (void);
void handle_int14 (void);
void complete_uart0_rx_interrupt (ECTX ectx);
void complete_uart0_tx_interrupt (ECTX ectx);
int uart0_is_blocking (void);
int uart0_in (ECTX ectx, WORD count, BYTEPTR pointer);
void uart0_send_char (const unsigned char c);
int uart0_out (ECTX ectx, WORD count, BYTEPTR pointer);
/*}}}*/

/*{{{  version.c */
extern const char version_string[];
/*}}}*/
