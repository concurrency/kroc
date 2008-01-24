/*****************************************************************************
**																			**
**	 Name: 	UART  Software Interface										**	
**																			**
******************************************************************************

(C) Copyright 2003 - Analog Devices, Inc.  All rights reserved.

File Name:		sicnames.h

Date Modified:	04/03/03		BK		Rev 1.0

Software:       VisualDSP++ 3.1

Purpose:		This file contains some extra defines beyond the 
                defBF53x.h files. Makes handling of SIC controller
                easier.			

******************************************************************************/



#ifndef __SIC_NAMES__
#define __SIC_NAMES__

#define IVG_PLL_WAKEUP	P0_IVG
#define IVG_DMA_ERROR	P1_IVG
#define IVG_PPI_ERROR	P2_IVG
#define IVG_SPT0_ERROR	P3_IVG
#define IVG_SPI_ERROR	P4_IVG
#define IVG_SPT1_ERROR	P5_IVG
#define IVG_UART_ERROR	P6_IVG
#define IVG_RTC			P7_IVG
#define IVG_PPI			P8_IVG
#define IVG_SPT0_RX		P9_IVG
#define IVG_SPT0_TX		P10_IVG
#define IVG_SPT1_RX		P11_IVG
#define IVG_SPT1_TX		P12_IVG
#define IVG_SPI			P13_IVG
#define IVG_UART_RX		P14_IVG
#define IVG_UART_TX		P15_IVG
#define IVG_TIMER0		P16_IVG
#define IVG_TIMER1		P17_IVG
#define IVG_TIMER2		P18_IVG
#define IVG_PFA			P19_IVG
#define IVG_PFB			P20_IVG
#define IVG_MEMDMA0		P21_IVG
#define IVG_MEMDMA1		P22_IVG
#define IVG_SWDT		P23_IVG

#define IRQ_PLL_WAKEUP	0x00000001
#define IRQ_DMA_ERROR	0x00000002
#define IRQ_PPI_ERROR	0x00000004
#define IRQ_SPT0_ERROR	0x00000008
#define IRQ_SPT1_ERROR	0x00000010
#define IRQ_SPI_ERROR	0x00000020
#define IRQ_UART_ERROR	0x00000040
#define IRQ_RTC			0x00000080
#define IRQ_PPI			0x00000100
#define IRQ_SPT0_RX		0x00000200
#define IRQ_SPT0_TX		0x00000400
#define IRQ_SPT1_RX		0x00000800
#define IRQ_SPT1_TX		0x00001000
#define IRQ_SPI			0x00002000
#define IRQ_UART_RX		0x00004000
#define IRQ_UART_TX		0x00008000
#define IRQ_TIMER0		0x00010000
#define IRQ_TIMER1		0x00020000
#define IRQ_TIMER2		0x00040000
#define IRQ_PFA			0x00080000
#define IRQ_PFB			0x00100000
#define IRQ_MEMDMA0		0x00200000
#define IRQ_MEMDMA1		0x00400000
#define IRQ_SWDT		0x00800000

#endif // __SIC_NAMES__
