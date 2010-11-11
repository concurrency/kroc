/*
 * flash.c - NXT Flash Driver
 *
 * Copyright (C) 2010  Carl G. Ritson
 *
 * Redistribution of this file is permitted under
 * the terms of the GNU Public License (GPL) version 2.
 */

#define DRIVER_BUILD
#include "../flash_driver.h"

typedef volatile unsigned int AT91_REG;
#define AT91C_MC_FCR			((AT91_REG *) 0xFFFFFF64)
#define AT91C_MC_FSR			((AT91_REG *) 0xFFFFFF68)
#define AT91C_MC_FRDY			((unsigned int) 0x1)
#define AT91C_MC_FCMD_START_PROG	((unsigned int) 0x1)
#define AT91C_MC_FCMD_UNLOCK		((unsigned int) 0x4)

#define INIT_VAR			((AT91_REG *) INIT_VAR_ADDR)

#define LOCK_REGION_SIZE		((256 * 1024) / 16)
#define LOCK_REGION_PAGES		((LOCK_REGION_SIZE) / PAGE_SIZE)
#define FLASH_BASE			ROM_BASE
#define FLASH_KEY			(0x5a << 24)

#define PAGE_WORDS			(PAGE_SIZE / sizeof (unsigned int))

static void wait_for_flash (void)
{
	while (!(*AT91C_MC_FSR & AT91C_MC_FRDY)) ;
}

static void unlock_region (int area_n)
{
	wait_for_flash ();
	if ((*AT91C_MC_FSR >> 16) & (1 << area_n)) {
		*AT91C_MC_FCR = FLASH_KEY | AT91C_MC_FCMD_UNLOCK | ((area_n * LOCK_REGION_PAGES) << 8); 
		wait_for_flash ();
	}
}

static void write_page (int page_n)
{
	wait_for_flash ();
	*AT91C_MC_FCR = FLASH_KEY | AT91C_MC_FCMD_START_PROG | (page_n << 8);
	wait_for_flash ();
}

static void setup_clock (void)
{
}

static void unlock_regions (void)
{
	int i;
	for (i = 0; i < 16; ++i)
		unlock_region (i);
}

void flash_driver (void)
{
	volatile unsigned int *dst, *src;
	unsigned int page_n;
	unsigned int i;
	
	if (*INIT_VAR) {
		setup_clock ();
		unlock_regions ();
		*INIT_VAR = 0;
	}
	
	page_n = *((volatile unsigned int *) PAGE_N_ADDR);
	dst = (volatile unsigned int *)(FLASH_BASE + (page_n * PAGE_SIZE)); 
	src = (volatile unsigned int *)(PAGE_BUF_ADDR);

	for (i = 0; i < PAGE_WORDS; ++i)
		*(dst++) = *(src++);

	write_page (page_n);
}
