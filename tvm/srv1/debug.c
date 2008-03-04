/*
 * debug.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

#include "srv.h"

typedef struct _sys_state_t {
	unsigned int sysstat;
	
	unsigned int rete;
	unsigned int retn;
	unsigned int retx;
	unsigned int reti;
	unsigned int rets;

	unsigned int reta;

	unsigned int lb1;
	unsigned int lb0;
	unsigned int lt1;
	unsigned int lt0;
	unsigned int lc1;
	unsigned int lc0;

	unsigned int a1_w;
	unsigned int a1_x;
	unsigned int a0_w;
	unsigned int a0_x;

	/* stored in reserve order */
	unsigned int b[4];
	unsigned int l[4];
	unsigned int m[4];
	unsigned int i[4];

	unsigned int fp;

	/* stored in reserve order */
	unsigned int p[6];
	unsigned int r[8];

	unsigned int syscfg;
} TVM_PACK sys_state_t;

static const unsigned char digits[] = "0123456789abcdef";

void print_hex (unsigned int val)
{
	int i;

	uart0_send_string ("0x");
	for (i = 7; i >= 0; --i) {
		unsigned int n = (val >> (i << 2)) & 0xf;
		uart0_send_char (digits[n]);
	}
}

static void print_regset (const char *name, int count, unsigned int *p)
{
	int i, j;

	for (j = 0; j < count; j += 4) {
		for (i = j; i < (j + 4) && i < count; ++i) {
			if (i > j)
				uart0_send_string (", ");
			uart0_send_string (name);
			uart0_send_char ('[');
			uart0_send_char (digits[i]);
			uart0_send_string ("] = ");
			print_hex (p[count - (i + 1)]);
		}
		uart0_send_char ('\n');
	}
}

static void print_state (sys_state_t *sys)
{
	uart0_send_string ("syscfg  = ");
	print_hex (sys->syscfg);
	uart0_send_string ("\nsysstat = ");
	print_hex (sys->sysstat);
	uart0_send_string ("\n\n");

	uart0_send_string ("retx = ");
	print_hex (sys->retx);
	uart0_send_string (", reti = ");
	print_hex (sys->reti);
	uart0_send_string (", rets = ");
	print_hex (sys->rets);
	uart0_send_string ("\n\n");

	print_regset ("r", 8, sys->r);
	uart0_send_char ('\n');
	print_regset ("p", 6, sys->p);
	uart0_send_char ('\n');

	print_regset ("i", 4, sys->i);
	print_regset ("m", 4, sys->m);
	print_regset ("b", 4, sys->b);
	print_regset ("l", 4, sys->l);
}

static void hang (void)
{
	for (;;) {
		IDLE;
		SSYNC;
	}
}

void handle_exception (sys_state_t *sys)
{
	const char *text;
	int cause = sys->sysstat & 0x3f;

	uart0_send_string ("## CPU Exception\n");

	switch (cause) {
		case 0x10: text = "single step"; break;
		case 0x11: text = "trace buffer full"; break;
		case 0x21: text = "undefined instruction"; break;
		case 0x22: text = "illegal instruction combination"; break;
		case 0x23: text = "data access CPLB protection violation"; break;
		case 0x24: text = "data access misaligned address violation"; break;
		case 0x25: text = "unrecoverable event (double fault)"; break;
		case 0x26: text = "data access CPLB miss"; break;
		case 0x27: text = "data access multiple CPLB hits"; break;
		case 0x28: text = "watchpoint match"; break;
		case 0x2A: text = "instruction fetch misaligned address violation"; break;
		case 0x2B: text = "instruction fetch CPLB protection violation"; break;
		case 0x2C: text = "instruction fetch CPLB miss"; break;
		case 0x2D: text = "instruction fetch multiple CPLB hits"; break;
		case 0x2E: text = "illegal use of supervisor resource"; break;
		default:
			if (!(cause & 0x30)) {
				text = "EXCPT instruction";
			} else {
				text = "unknown";
			}
			break;
	}

	uart0_send_string ("\nException: ");
	uart0_send_string (text);
	uart0_send_string ("\n\n");

	print_state (sys);
	hang ();
}

void handle_hwerror (sys_state_t *sys)
{
	const char *text;
	int cause = (sys->sysstat >> 14) & 0x1f;

	uart0_send_string ("## Hardware Error\n");

	switch (cause) {
		case 0x02: text = "system MMR error"; break;
		case 0x03: text = "external memory addressing error"; break;
		case 0x12: text = "performance monitoring overflow"; break;
		case 0x18: text = "RAISE5 instruction"; break;
		default:
			text = "unknown";
			break;
	}

	uart0_send_string ("\nError: ");
	uart0_send_string (text);
	uart0_send_string ("\n\n");

	print_state (sys);
	hang ();
}

void unhandled_interrupt (sys_state_t *sys)
{
	uart0_send_string ("## Unhandled Interrupt\n");

	uart0_send_string ("\nInterrupt: ");
	uart0_send_char (digits[sys->r[7] / 10]);
	uart0_send_char (digits[sys->r[7] % 10]);
	uart0_send_string ("\n\n");
	
	print_state (sys);
	hang ();
}

