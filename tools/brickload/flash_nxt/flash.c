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
#define AT91C_PMC_MCKR			((AT91_REG *) _AT91C_PMC_MCKR)
#define AT91C_PMC_SR			((AT91_REG *) _AT91C_PMC_SR)

#define AT91C_MC_FCR			((AT91_REG *) _AT91C_MC_FCR)
#define AT91C_MC_FMR			((AT91_REG *) _AT91C_MC_FMR)
#define AT91C_MC_FSR			((AT91_REG *) _AT91C_MC_FSR)

#define FLASH_BASE			ROM_BASE
#define PAGE_WORDS			(PAGE_SIZE / sizeof (unsigned int))

static inline void wait_for_flash (void)
{
	while (!(*AT91C_MC_FSR & AT91C_MC_FRDY)) ;
}

static inline void write_page (int page_n)
{
	*AT91C_MC_FCR = FLASH_KEY | AT91C_MC_FCMD_START_PROG | (page_n << 8);
	wait_for_flash ();
}


void flash_driver (void)
{
	volatile unsigned int *dst, *src;
	unsigned int page_n = 0;
	unsigned int i;
	
	wait_for_flash ();
	//page_n = *((volatile unsigned int *) PAGE_N_ADDR);
	
	dst = (volatile unsigned int *)(FLASH_BASE + (page_n * PAGE_SIZE)); 
	src = (volatile unsigned int *)(PAGE_BUF_ADDR);
	for (i = 0; i < PAGE_WORDS; ++i)
		*(dst++) = *(src++);
	
	//write_page (page_n);
}
