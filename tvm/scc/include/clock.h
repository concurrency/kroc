/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#ifndef _CLOCK_H_
#define _CLOCK_H_

#include <stddef.h>
#include <conf.h>

extern volatile ulong clkticks;          /**< counts clock interrupts */
extern volatile ulong clktime_ms;           /**< current time in secs since boot */

/* Clock function prototypes */
void init_clock(void);
ulong time_millis(void);
ulong time_micros(void);
interrupt clkhandler(void);

#endif /* _CLOCK_H_ */
