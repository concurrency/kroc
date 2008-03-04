/*
 * twi.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

#include "srv.h"
#include "twi_const.h"

enum {
	STATE_INIT	= 0,
	STATE_RX_BUF	= 1,
	STATE_TX_BUF	= 2,
	STATE_READY	= 3,
	STATE_RX	= 4,
	STATE_TX	= 5,
	STATE_ERROR	= 6
};

static WORDPTR			channel		= (WORDPTR) NOT_PROCESS_P;
static WORDPTR			ret_ptr		= (WORDPTR) NULL_P;

static BYTEPTR			rx_buffer	= (BYTEPTR) NULL_P;
static WORDPTR			rx_mobile	= (WORDPTR) NULL_P;
static WORD			rx_pending	= 0;

static BYTEPTR			tx_buffer	= (BYTEPTR) NULL_P;
static WORDPTR			tx_mobile	= (WORDPTR) NULL_P;
static WORD			tx_pending	= 0;

static WORD			flags		= 0;
static unsigned short		clock_div	= 0;
static unsigned short		state		= STATE_INIT;

void init_twi (void)
{
	/* Ratio for scaling SCLK to 10MHz */
	*pTWI_CONTROL		= TWI_ENA | (PERIPHERAL_CLOCK / 10000000);

	/* Disable master and slave operation */
	*pTWI_MASTER_CTL	= 0;
	*pTWI_SLAVE_CTL		= 0;

	/* Setup FIFO trigger lengths as 1 byte */
	*pTWI_FIFO_CTL		= 0;

	/* Clear status flags */
	*pTWI_MASTER_STAT	= BUFWRERR | BUFRDERR | DNAK | ANAK | LOSTARB;
	*pTWI_INT_STAT		= RCVSERV | XMTSERV | MERR | MCOMP | SOVF | SERR | SCOMP | SINIT;

	/* Disable interrupt generation */
	*pTWI_INT_MASK		= 0;
	SSYNC;

	/* Enable interrupts on IVG 13 */
	*pSIC_IAR1		= (*pSIC_IAR1 & ~(0xf << 0x4)) | P9_IVG(13);
	*pSIC_IMASK		|= IRQ_TWI;
	SSYNC;
}

static void fill_xmt_fifo (void)
{
	do {
		if ((*pTWI_FIFO_STAT & XMTSTAT) == XMT_FULL)
			return;
		*pTWI_XMT_DATA8		= read_byte (tx_buffer);
		tx_buffer		= byteptr_plus (tx_buffer, 1);
	} while (--tx_pending);
}

void handle_int13 (void)
{
	unsigned short stat = *pTWI_INT_STAT;
	
	if (stat & MERR) {
		*pTWI_INT_MASK	= 0;
		state		= STATE_ERROR;
		raise_tvm_interrupt (TVM_INTR_TWI);
	} else if (state == STATE_RX) {
		if (stat & MCOMP) {
			if (tx_pending) {
				unsigned short ctl = *pTWI_MASTER_CTL;

				ctl &= ~(RSTART | MDIR);
				ctl |= tx_pending << 6;

				*pTWI_MASTER_CTL= ctl;
				state		= STATE_TX;
			} else {
				*pTWI_INT_MASK	= 0;
				raise_tvm_interrupt (TVM_INTR_TWI);
			}
			*pTWI_INT_STAT	= MCOMP;
		} else if (stat & RCVSERV) {
			BYTE b		= *pTWI_RCV_DATA8;
			write_byte (rx_buffer, b);
			rx_buffer	= byteptr_plus (rx_buffer, 1);
			if (!(--rx_pending) && tx_pending) {
				*pTWI_MASTER_CTL = (*pTWI_MASTER_CTL | RSTART) & (~MDIR);
				fill_xmt_fifo ();
			}
			*pTWI_INT_STAT	= RCVSERV;
		}
	} else if (state == STATE_TX) {
		if (stat & MCOMP) {
			if (rx_pending) {
				unsigned short ctl = *pTWI_MASTER_CTL;

				ctl &= ~RSTART;
				ctl |= MDIR | (rx_pending << 6);

				*pTWI_MASTER_CTL= ctl;
				state		= STATE_RX;
			} else {
				*pTWI_INT_MASK	= 0;
				raise_tvm_interrupt (TVM_INTR_TWI);
			}
			*pTWI_INT_STAT	= MCOMP;
		} else if (stat & XMTSERV) {
			fill_xmt_fifo ();
			if (!tx_pending && rx_pending) {
				*pTWI_MASTER_CTL |= (RSTART | MDIR);
			}
			*pTWI_INT_STAT	= XMTSERV;
		}
	}
}

void complete_twi_interrupt (ECTX ectx)
{
	WORD ret = 0;

	if (*pTWI_INT_STAT & MERR) {
		ret = (WORD) *pTWI_MASTER_STAT;
	}
		
	write_word (ret_ptr, ret);
	ectx->add_to_queue (ectx, channel);

	channel = NOT_PROCESS_P;
	state	= STATE_RX_BUF;
}

int twi_is_blocking (void)
{
	/* Return non-zero if channel is not NOT_PROCESS_P. */
	return ((int) channel) ^ NOT_PROCESS_P;
}

static void initiate_transfer (void)
{
	unsigned short ctl = *pTWI_CONTROL;

	if (rx_mobile != NULL_P) {
		WORDPTR ptr	= wordptr_minus (rx_mobile, MT_ARRAY_PTR_OFFSET);
		rx_buffer 	= (BYTEPTR) read_offset (ptr, mt_array_internal_t, array.data);
		rx_pending	= read_offset (ptr, mt_array_internal_t, size);
	}
	
	if (tx_mobile != NULL_P) {
		WORDPTR ptr	= wordptr_minus (tx_mobile, MT_ARRAY_PTR_OFFSET);
		tx_buffer 	= (BYTEPTR) read_offset (ptr, mt_array_internal_t, array.data);
		tx_pending	= read_offset (ptr, mt_array_internal_t, size);
	}

	if (!(flags & TWI_SCCB) && (ctl & SCCB)) {
		*pTWI_CONTROL	= ctl & ~SCCB;
	} else if ((flags & TWI_SCCB) && !(ctl & SCCB)) {
		*pTWI_CONTROL   = ctl | SCCB;
	}	

	/* Clear flags and FIFO */
	*pTWI_MASTER_STAT	= BUFWRERR | BUFRDERR | DNAK | ANAK | LOSTARB;
	*pTWI_INT_STAT		= RCVSERV | XMTSERV | MERR | MCOMP;
	*pTWI_FIFO_CTL		= RCVFLUSH | XMTFLUSH;
	SSYNC;

	/* Enable interrupts and FIFO */
	*pTWI_INT_MASK		= RCVSERV | XMTSERV | MERR | MCOMP;
	*pTWI_FIFO_CTL		= 0;

	/* Setup clock and address */
	*pTWI_CLKDIV		= clock_div;
	*pTWI_MASTER_ADDR	= (flags & TWI_ADDRESS);
	SSYNC;

	/* Build master setup */
	ctl = MEN;

	if (flags & TWI_RECV) {
		ctl		|= MDIR | (rx_pending << 6);
		state		= STATE_RX;
	} else {
		ctl 		|= (tx_pending << 6);
		state		= STATE_TX;
		/* Fill FIFO */
		fill_xmt_fifo ();
	}

	/* Commit configuration; interrupts drive transfer */
	*pTWI_MASTER_CTL	= ctl;
	SSYNC;
}

int twi_in (ECTX ectx, WORD count, BYTEPTR pointer)
{
	if ((state == STATE_READY) && (count == sizeof(WORD))) {
		WORKSPACE_SET (ectx->wptr, WS_IPTR, (WORD) ectx->iptr);
		channel	= ectx->wptr;
		ret_ptr = (WORDPTR) pointer;
		BARRIER;
		initiate_transfer ();
		return ectx->run_next_on_queue (ectx);
	} else {
		return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
	}
}

int twi_out (ECTX ectx, WORD count, BYTEPTR pointer)
{
	if (state == STATE_INIT) {
		if (count == 2) {
			WORD hi		= (WORD) read_byte (byteptr_plus (pointer, 0));
			WORD lo		= (WORD) read_byte (byteptr_plus (pointer, 1));
			clock_div	= CLKHI(hi) | CLKLOW(lo);
			return ECTX_CONTINUE;
		} else if (count == sizeof(WORD)) {
			flags = read_word (pointer);
			state = STATE_TX_BUF;
			return ECTX_CONTINUE;
		}
	}
	return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
}

int twi_mt_in (ECTX ectx, WORDPTR pointer)
{
	switch (state) {
		case STATE_RX_BUF:
			write_word (pointer, (WORD) rx_mobile);
			rx_buffer	= (BYTEPTR) NULL_P;
			rx_mobile	= (WORDPTR) NULL_P;
			state		= STATE_TX_BUF;
			return ECTX_CONTINUE;
		case STATE_TX_BUF:
			write_word (pointer, (WORD) tx_mobile);
			tx_buffer	= (BYTEPTR) NULL_P;
			tx_mobile	= (WORDPTR) NULL_P;
			state		= STATE_INIT;
			return ECTX_CONTINUE;
		default:
			return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
	}
}

int twi_mt_out (ECTX ectx, WORDPTR pointer)
{
	switch (state) {
		case STATE_RX_BUF:
			rx_mobile	= (WORDPTR) read_word (pointer);
			write_word (pointer, NULL_P);
			state		= STATE_READY;
			return ECTX_CONTINUE;
		case STATE_TX_BUF:
			tx_mobile	= (WORDPTR) read_word (pointer);
			write_word (pointer, NULL_P);
			state		= STATE_RX_BUF;
			return ECTX_CONTINUE;
		default:
			break;
	}

	return ectx->set_error_flag (ectx, EFLAG_EXTCHAN);
}

