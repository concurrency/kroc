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

void debug_print_chr (const unsigned char c)
{
	uart0_send_char (c);
}

void debug_print_hex (const unsigned int val)
{
	int i;

	debug_print_str ("0x");
	for (i = 7; i >= 0; --i) {
		unsigned int n = (val >> (i << 2)) & 0xf;
		debug_print_chr (digits[n]);
	}
}

void debug_print_str (const char *str)
{
	const unsigned char *p = (unsigned char *) str;
	unsigned char c;

	while ((c = *p++) != '\0') {
		uart0_send_char (c);
	}
}

static void print_regset (const char *name, int count, unsigned int *p)
{
	int i, j;

	for (j = 0; j < count; j += 4) {
		for (i = j; i < (j + 4) && i < count; ++i) {
			if (i > j)
				debug_print_str (", ");
			debug_print_str (name);
			debug_print_chr ('[');
			debug_print_chr (digits[i]);
			debug_print_str ("] = ");
			debug_print_hex (p[count - (i + 1)]);
		}
		debug_print_chr ('\n');
	}
}

static void print_state (sys_state_t *sys)
{
	debug_print_str ("syscfg  = ");
	debug_print_hex (sys->syscfg);
	debug_print_str ("\nsysstat = ");
	debug_print_hex (sys->sysstat);
	debug_print_str ("\n\n");

	debug_print_str ("retx = ");
	debug_print_hex (sys->retx);
	debug_print_str (", reti = ");
	debug_print_hex (sys->reti);
	debug_print_str (", rets = ");
	debug_print_hex (sys->rets);
	debug_print_str ("\n\n");

	print_regset ("r", 8, sys->r);
	debug_print_chr ('\n');
	print_regset ("p", 6, sys->p);
	debug_print_chr ('\n');

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

	debug_print_str ("## CPU Exception\n");

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

	debug_print_str ("\nException: ");
	debug_print_str (text);
	debug_print_str ("\n\n");

	print_state (sys);
	hang ();
}

void handle_hwerror (sys_state_t *sys)
{
	const char *text;
	int cause = (sys->sysstat >> 14) & 0x1f;

	debug_print_str ("## Hardware Error\n");

	switch (cause) {
		case 0x02: text = "system MMR error"; break;
		case 0x03: text = "external memory addressing error"; break;
		case 0x12: text = "performance monitoring overflow"; break;
		case 0x18: text = "RAISE5 instruction"; break;
		default:
			text = "unknown";
			break;
	}

	debug_print_str ("\nError: ");
	debug_print_str (text);
	debug_print_str ("\n\n");

	print_state (sys);
	hang ();
}

void unhandled_interrupt (sys_state_t *sys)
{
	debug_print_str ("## Unhandled Interrupt\n");

	debug_print_str ("\nInterrupt: ");
	debug_print_chr (digits[sys->r[7] / 10]);
	debug_print_chr (digits[sys->r[7] % 10]);
	debug_print_str ("\n\n");
	
	print_state (sys);
	hang ();
}

