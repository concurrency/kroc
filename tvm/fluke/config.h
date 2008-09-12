/******************************************************************************
 *
 * $RCSfile: $
 * $Revision: $
 *
 * This module provides information about the project configuration
 * Copyright 2004, R O SoftWare
 * No guarantees, warrantees, or promises, implied or otherwise.
 * May be used for hobby or commercial purposes provided copyright
 * notice remains intact.
 *
 *****************************************************************************/
#ifndef INC_CONFIG_H
#define INC_CONFIG_H

#include "types.h"
#include "LPC210x.h"

// some handy DEFINES
#ifndef FALSE
#define FALSE               0
#ifndef TRUE
#define TRUE                !FALSE
#endif
#endif

#ifndef BIT
#define BIT(n)              (1 << (n))
#endif

// declare functions and values from crt0.S & the linker control file
extern void reset(void);
// extern void exit(void);
extern void abort(void);
// maybe add interrupt vector addresses


#define WDOG()

/* The commented out values below are now defined in the following file */
#include "config_target.h"
// #define HOST_BAUD           (38400)
// PLL setup values are computed within the LPC include file
// It relies upon the following defines
// #define FOSC                (20000000)      // Master Oscillator Freq.
// #define PLL_MUL             (3)             // PLL Multiplier

// Pheripheral Bus Speed Divider
// #define PBSD                1               // MUST BE 1, 2, or 4
#define PCLK                (CCLK / PBSD)   // Pheripheal Bus Clock Freq.

// Do some value range testing

#if ((FOSC < 10000000) || (FOSC > 25000000))
#error Fosc out of range (10MHz-25MHz)
#error correct and recompile
#endif

#if ((CCLK < 10000000) || (CCLK > 60000000))
#error cclk out of range (10MHz-60MHz)
#error correct PLL_MUL and recompile
#endif

#if ((FCCO < 150000000) || (FCCO > 320000000))
#error Fcco out of range (156MHz-320MHz)
#error internal algorithm error
#endif

#if ((PBSD != 1) && (PBSD != 2) && (PBSD != 4))
#error Pheripheal Bus Speed Divider (PBSD) illegal value (1, 2, or 4)
#endif


#endif
