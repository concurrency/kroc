/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#ifndef _CLOCK_H_
#define _CLOCK_H_

#include <stddef.h>
#include <conf.h>

extern ulong clkticks;          /**< counts clock interrupts */
extern ulong clktime;           /**< current time in secs since boot */

/* Clock function prototypes */
void init_clock(void);
interrupt clkhandler(void);
ulong time_millis(void);

#endif /* _CLOCK_H_ */
