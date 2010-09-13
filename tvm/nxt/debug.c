/*
 * debug.c - NXT TVM Debugging Utilities
 *
 * Copyright (C) 2010  Carl G. Ritson
 *
 * Redistribution of this file is permitted under
 * the terms of the GNU Public License (GPL) version 2.
 */

#include "tvm-nxt.h"
#include "font8x8.h"

#define NXT_LCD_ROWS (NXT_LCD_HEIGHT / 8)
#define NXT_LCD_COLS (NXT_LCD_WIDTH / 8)

static uint8_t disp_buf[(NXT_LCD_HEIGHT * NXT_LCD_WIDTH) / 8];
static uint8_t curr_y;

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

void debug_blink (void) {
	int i;

	for (i = 0; i < 10; ++i)
		disp_buf[sizeof (disp_buf) - (i + 1)] ^= 0xff;

	lcd_dirty_display ();
	lcd_update ();
}

static void draw_char (int x, int y, char c)
{
	int i;
	if (c >= 'A' && c <= 'Z') {
		const uint8_t *src = &(font_8x8_alpha[(c - 'A') * 8]);
		uint8_t *dst = &(disp_buf[(y * NXT_LCD_WIDTH) + (x * 8)]);
		for (i = 0; i < 8; ++i)
			dst[i] = src[i];
	} else if (c >= '0' && c <= '9') {
		const uint8_t *src = &(font_8x8_digit[(c - '0') * 8]);
		uint8_t *dst = &(disp_buf[(y * NXT_LCD_WIDTH) + (x * 8)]);
		for (i = 0; i < 8; ++i)
			dst[i] = src[i];
	} else {
		memset (disp_buf + (y * NXT_LCD_WIDTH) + (x * 8), 0, 8);
	}
}

static int draw_hex (int x, int y, uint32_t value)
{
	int i;
	
	for (i = 7; (i >= 0) && (x < NXT_LCD_COLS); --i, ++x) {
		uint8_t hbyte = (value >> (i * 4)) & 0xf;
		if (hbyte <= 9)
			draw_char (x, y, '0' + hbyte);
		else
			draw_char (x, y, 'A' + (hbyte - 10));
	}

	return x;
}

void debug_msg (const char *msg, uint32_t code)
{
	uint8_t curr_x = 0;
	int pos = 0;

	while (msg[pos] != '\0' && curr_x < NXT_LCD_COLS)
		draw_char (curr_x++, curr_y, msg[pos++]);
	
	curr_x = draw_hex (curr_x, curr_y, code);
	curr_y = (curr_y + 1) % (NXT_LCD_ROWS - 1); 
	
	lcd_dirty_display ();
	lcd_update ();
}

void debug_init (void)
{
	curr_y = 0;
	memset (disp_buf, 0, sizeof (disp_buf));
	lcd_set_display (disp_buf);
	debug_msg ("DBG ", systick_get_ms ());
}
