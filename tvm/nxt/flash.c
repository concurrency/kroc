/*
 * flash.c - NXT Flash Functions
 *
 * Copyright (C) 2010  Carl G. Ritson
 *
 * Redistribution of this file is permitted under
 * the terms of the GNU Public License (GPL) version 2.
 */

#include "tvm-nxt.h"
#include "at91sam7s256.h"

#define PAGE_SIZE 256
#define LOCK_REGION_SIZE ((256 * 1024) / 16)
#define LOCK_REGION_PAGES (LOCK_REGION_SIZE / PAGE_SIZE)
#define FLASH_BASE 0x00100000
#define FLASH_KEY (0x5a << 24)
#define PAGE_NUM(x) ((((uint32_t) (x)) - FLASH_BASE) / PAGE_SIZE)

static void wait_for_fc (void)
{
	while (!(*AT91C_MC_FSR & AT91C_MC_FRDY)) ;
}

static void unlock_region (int area_n)
{
	wait_for_fc ();
	if ((*AT91C_MC_FSR >> 16) & (1 << area_n)) {
		*AT91C_MC_FCR = FLASH_KEY | AT91C_MC_FCMD_UNLOCK | ((area_n * LOCK_REGION_PAGES) << 8); 
		wait_for_fc ();
	}
}

static void write_page (int page_n)
{
	wait_for_fc ();
	*AT91C_MC_FCR = FLASH_KEY | AT91C_MC_FCMD_START_PROG | (page_n << 8);
	wait_for_fc ();
}

/* XXX: src must be word aligned */
void flash_write (uint8_t *dst, const uint8_t *src, size_t len)
{
	volatile uint32_t *dst_p 	= (volatile uint32_t *) dst;
	uint32_t *src_p 		= (uint32_t *) src;
	uint32_t base			= (uint32_t) (dst - FLASH_BASE);
	int dirty			= 0;
	int lock_start			= base / LOCK_REGION_SIZE;
	int lock_end			= (base + len) / LOCK_REGION_SIZE;
	int page_n 			= base / PAGE_SIZE;
	int i;

	/* Unlock regions with pages to be updated */
	for (i = lock_start; i <= lock_end; ++i) {
		unlock_region (i);
	}

	while (len) {
		*(dst_p++) = *(src_p++);
		len -= 4;
		if (((int) PAGE_NUM(src_p)) != page_n) {
			write_page (page_n);
			page_n++;
			dirty = 0;
		} else {
			dirty++;
		}
	}

	if (dirty > 0)
		write_page (page_n);
}
