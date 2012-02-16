/*
 * gpio.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

#include "srv.h"

void init_gpio (void)
{
	/* Port H = GPIO */
	*pPORTH_FER 	= 0;
	*pPORTHIO	= 0;
}

