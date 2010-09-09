/*
 * nxt.c - NXT low-level hardware manipulation functions
 *
 * Copyright (C) 2008  the NxOS developers (see NXOS)
 * Copyright (C) 2010  Carl G. Ritson
 *
 * Redistribution of this file is permitted under
 * the terms of the GNU Public License (GPL) version 2.
 */

#include "tvm-nxt.h"
#include "at91sam7s256.h"

/*{{{  System Clock */
/* The main clock is at 48MHz, and the PIT divides that by 16 to get
 * its base timer frequency.
 */
#define PIT_BASE_FREQUENCY (NXT_CLOCK_FREQ/16)

/* We want a timer interrupt 1000 times per second. */
#define SYSIRQ_FREQ 1000

/* The system IRQ processing takes place in two different interrupt
 * handlers: the main PIT interrupt handler runs at a high priority,
 * keeps the system time accurate, and triggers the lower priority
 * IRQ.
 *
 * All other periodic processing (needed eg. by drivers) takes place
 * in this lower priority interrupt handler. This ensures that we
 * don't miss a clock beat.
 *
 * As the PIT only has one interrupt line, we steal the PWM
 * controller's interrupt line and use it as our low-priority system
 * interrupt. The PWM controller is unused on the NXT, so this is no
 * problem.
 */
#define SCHEDULER_SYSIRQ AT91C_ID_PWMC

/* The system timer. Counts the number of milliseconds elapsed since
 * the system's initialization.
 */
static volatile uint32_t systick_time;

/* High priority handler, called 1000 times a second */
static void systick_isr (void)
{
	uint32_t status;
	
	/* Read PIT value register to acknowledge the interrupt.*/
	status = *AT91C_PITC_PIVR;

	/* Do the system timekeeping. */
	systick_time++;

	/* FIXME: do management */
	avr_systick_update ();
}

static void systick_init (void) {
	nxt__interrupts_disable ();

	systick_time = 0;

	aic_install_isr (AT91C_ID_SYS, AIC_PRIO_TICK, 
		AIC_TRIG_EDGE, systick_isr);

	/* Configure and enable the Periodic Interval Timer. The counter
	 * value is 1/16th of the master clock (base frequency), divided by
	 * our desired interrupt period of 1ms.
	 */
	*AT91C_PITC_PIMR = (((PIT_BASE_FREQUENCY / SYSIRQ_FREQ) - 1) |
			AT91C_PITC_PITEN | AT91C_PITC_PITIEN);

	nxt__interrupts_enable ();
}

uint32_t systick_get_ms (void)
{
	return systick_time;
}

void systick_wait_ms (uint32_t ms)
{
	uint32_t final = systick_time + ms;

	while (systick_time < final);
}

void systick_wait_ns (uint32_t ns)
{
	volatile uint32_t x = (ns >> 7) + 1;

	while (x--);
}
/*}}}*/

static uint8_t disp_buf[(NXT_LCD_HEIGHT * NXT_LCD_WIDTH) / 8];

static void display_pattern (uint8_t odd, uint8_t even)
{
	unsigned int i;
	
	for (i = 0; i < sizeof (disp_buf); ++i) {
		if ((i & 1) == 0) {
			disp_buf[i] = even;
		} else {
			disp_buf[i] = odd;
		}
	}

	lcd_dirty_display ();
	lcd_update ();
}

static void blink (void) {
	int i;

	for (i = 0; i < 10; ++i)
		disp_buf[sizeof (disp_buf) - (i + 1)] ^= 0xff;

	lcd_dirty_display ();
	lcd_update ();
}

void debug_msg (unsigned offset, uint8_t code)
{
	uint8_t *buf = disp_buf + (offset * 10);
	int i;
	
	*(buf) = ((*buf) ^ 0xff) | 0x83;
	buf++;

	for (i = 0; i < 8; ++i) {
		uint8_t byte = 0;

		if (code & 0x80)
			byte = 0x78;
		code <<= 1;
		
		*buf = (*buf & (~0x78)) | byte;
		buf++;
	}
	
	*(buf) = ((*buf) ^ 0xff) | 0x83;
}

void nxt_init (void)
{
	uint32_t msd_len;
	uint8_t *msd_mem;
	
	aic_init ();
	avr_data_init ();

	/* Interrupt disable nesting starts at 1, 
	 * so the following line actually enables interrupts.
	 */
	nxt__interrupts_enable ();

	systick_init ();
	avr_init ();

	/* LCD and Frame Buffer */
	lcd_init ();
	memset (disp_buf, 0, sizeof (disp_buf));
	lcd_set_display (disp_buf);
	display_pattern (0x00, 0x01);

	/* USB and Mass Storage */
	msd_mem = NXT_FREE_MEM_START;
	msd_len = NXT_FREE_MEM_LEN;
	if ((((uint32_t) msd_mem) % 256) != 0) {
		uint32_t diff = 256 - (((uint32_t) msd_mem) % 256);
		msd_mem += diff;
		msd_len -= diff;
	}
	msd_len -= (msd_len % 256);
	usb_init (msd_mem, msd_len, 0);

	/* Wait for shutdown button */
	do { 
		systick_wait_ms (500);
		blink ();
	} while (avr_get_button () != BUTTON_CANCEL);

	/* Shutdown */
	lcd_shutdown ();
	avr_power_down ();
}

void nxt_abort (bool data, uint32_t pc, uint32_t cpsr)
{
	return;
}
