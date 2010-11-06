/*
 * mem.c - NXT memory support functions
 *
 * Copyright (C) 2010  Carl G. Ritson
 *
 * Redistribution of this file is permitted under
 * the terms of the GNU Public License (GPL) version 2.
 */

#include "tvm-nxt.h"

void memcpy (void *dest, const void *source, size_t len)
{
	int8_t *dst = (int8_t *) dest;
	int8_t *src = (int8_t *) source;
	
	while (len--)
		*(dst++) = *(src++);
}

void memset (void *dest, const int8_t val, size_t len)
{
	int8_t *dst = (int8_t *) dest;
	
	while (len--)
		*(dst++) = val;
}
