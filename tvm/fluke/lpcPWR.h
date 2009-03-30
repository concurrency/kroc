/******************************************************************************
 *
 *
 * Header file for Philips LPC ARM Processors.
 * Power Saving Constants
 * Copyright 2008 Christian L. Jacobsen
 *
 *****************************************************************************/
#ifndef LPC_PWR_H
#define LPC_PWR_H

/* Processor powersaving constants, use with PCON */
#define PCON_IDL (1 << 0) /* Idle mode */
#define PCON_PD  (1 << 1) /* Power Down mode */
/* Bits 2-7 are reserved */




/* Peripheral enable constants, use with PCONP
 *
 * All peripherals are enabled by default on the LPC210x. 
 */
/* Bit 0 reserved */
#define PCONP_PCTIM0 (1 << 1) /* Timer 0 */
#define PCONP_PCTIM1 (1 << 2) /* Timer 1 */
#define PCONP_PCURT0 (1 << 3) /* UART 0 */
#define PCONP_PCURT1 (1 << 4) /* UART 1 */
#define PCONP_PCPWM0 (1 << 5) /* Pulse Width Modulator */
/* Bit 6 reserved */
#define PCONP_PCI2C  (1 << 7) /* I2C */
#define PCONP_PCSPI  (1 << 8) /* SPI */
#define PCONP_PCRTC  (1 << 9) /* Real Time Clock */
/* Bits 10-31 reserved */


#endif /* LPC_PWR_H */
