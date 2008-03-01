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
#include "camera_const.h"
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
/*}}}*/

/*{{{  gpio.c */
void init_gpio (void);
/*}}}*/

/*{{{  imagery.c */
int draw_caption_on_frame (ECTX ectx, WORD args[]);
int jpeg_encode_frame (ECTX ectx, WORD args[]);
/*}}}*/

/*{{{  ppi_dma.c */
extern WORDPTR camera_channel;

void init_camera (void);
void handle_int8 (void);
void complete_camera_interrupt (ECTX ectx);
int camera_in (ECTX ectx, WORD count, BYTEPTR pointer);
int camera_out (ECTX ectx, WORD count, BYTEPTR pointer);
int camera_mt_in (ECTX ectx, WORDPTR pointer);
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
void raise_tvm_interrupt (void);
int run_tvm (void);
/*}}}*/

/*{{{  uart.c */
extern volatile WORDPTR uart0_rx_channel;

void init_uart (void);
void handle_int10 (void);
void complete_uart0_interrupt (ECTX ectx);
int uart0_in (ECTX ectx, WORD count, BYTEPTR pointer);
void uart0_send_char (const unsigned char c);
void uart0_send_string (const char *str);
int uart0_out (ECTX ectx, WORD count, BYTEPTR pointer);
/*}}}*/

/*{{{  version.c */
extern const char version_string[];
/*}}}*/
