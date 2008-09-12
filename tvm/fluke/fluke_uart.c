/*
 * uart.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

#include "fluke.h"

#if 0
#define CTS_PIN		6
#define	RTS_PIN		0
#define CTS_MASK	(1 << CTS_PIN)
#define RTS_MASK	(1 << RTS_PIN)
#endif

static void uart0_isr(void);

static volatile unsigned char	rx_buffer	= '\0';
static WORDPTR			rx_channel	= (WORDPTR) NOT_PROCESS_P;
static volatile short		rx_pending	= 0;
static volatile BYTEPTR		rx_ptr		= (BYTEPTR) NULL_P;

static WORDPTR			tx_channel	= (WORDPTR) NOT_PROCESS_P;
static volatile WORD		tx_pending	= 0;
static BYTEPTR			tx_ptr		= (BYTEPTR) NULL_P;

void init_uart0 (uint16_t baud, uint8_t mode, uint8_t fmode)
{

  /* set port pins for UART0 */
  PINSEL0 = (PINSEL0 & ~U0_PINMASK) | U0_PINSEL;

  U0IER = 0x00;                         /* disable all interrupts */
  U0IIR;                                /* clear interrupt ID */
  U0RBR;                                /* clear receive register */
  U0LSR;                                /* clear line status register */

  /* set the baudrate */
  U0LCR = ULCR_DLAB_ENABLE;             /* select divisor latches */
  U0DLL = (uint8_t)baud;                /* set for baud low byte */
  U0DLM = (uint8_t)(baud >> 8);         /* set for baud high byte */

  /* set the number of characters and other */
  /* user specified operating parameters */
  U0LCR = (mode & ~ULCR_DLAB_ENABLE);
  U0FCR = fmode;

  /* initialize the interrupt vector */
  VICIntSelect &= ~VIC_BIT(VIC_UART0);  /* UART0 selected as IRQ */
  VICIntEnable = VIC_BIT(VIC_UART0);    /* UART0 interrupt enabled */
  VICVectCntl1 = VIC_ENABLE | VIC_UART0;
  VICVectAddr1 = (uint32_t)uart0_isr;   /* address of the ISR */

  // enable receiver interrupts
  U0IER = UIER_ERBFI;
  // enable line status interrupts
  U0IER = U0IER | UIER_ELSI;
}

#if 0
static WORD run_output (WORD count, BYTEPTR *pptr)
{
	BYTEPTR ptr = *pptr;

	while (count && (*pPORTHIO & RTS_MASK)) {
		unsigned char c = read_byte (ptr);

		ptr = byteptr_plus (ptr, 1);

		while (!(*pUART0_LSR & THRE)) {
			continue;
		}

		/* Send data */
		*pUART0_THR = c;

		count--;
	}

	*pptr = ptr;

	return count;
}
#endif

static void uart0_isr(void)
{
  /* perform proper ISR entry so thumb-interwork works properly */
  ISR_ENTRY();
 
  {
  uint8_t iid;
  unsigned char buffer;
  /* loop until not more interrupt sources */
  while (((iid = U0IIR) & UIIR_NO_INT) == 0)
    {
    /* identify & process the highest priority interrupt */
    switch (iid & UIIR_ID_MASK)
      {
      case UIIR_RLS_INT:                /* Receive Line Status */
        U0LSR;                          /* read LSR to clear */
        break; /* We ignore errors for now */

      case UIIR_CTI_INT:                /* Character Timeout Indicator */
      case UIIR_RDA_INT:                /* Receive Data Available */
		/* Read character (clears interrupt condition) */
		buffer		= U0RBR;

		//uart0_send_char(buffer);
		//break;
		/* Is anything waiting for the character? */
		if (rx_ptr != (BYTEPTR) NULL_P) {
			/* Yes; give it the data and trigger requeue */
			write_byte (rx_ptr, (BYTE) buffer);
			rx_ptr		= (BYTEPTR) NULL_P;
			raise_tvm_interrupt (TVM_INTR_UART0_RX);
		} else {
			/* No; buffer the character */
			rx_buffer	= buffer;
			rx_pending++;
		}
	
		break;
#if 0	
      case UIIR_THRE_INT:               /* Transmit Holding Register Empty */
        while (U0LSR & ULSR_THRE)
          {
          // check if more data to send
          if (uart0_tx_insert_idx != uart0_tx_extract_idx)
            {
            U0THR = uart0_tx_buffer[uart0_tx_extract_idx++];
            uart0_tx_extract_idx %= UART0_TX_BUFFER_SIZE;
            }
          else
            {
            // no
            uart0_tx_running = 0;       // clear running flag
            break;
            }
          }

        break;
#endif // UART0_TX_INT
      default:                          /* Unknown */
        U0LSR;
        U0RBR;
	IOSET = LED;
        break;
      }
    }

  }
  VICVectAddr = 0x00000000;             /* clear this interrupt from the VIC */

  ISR_EXIT();                           /* recover registers and return */
}

#if 0
void handle_int10 (void)
{
	unsigned char buffer;

	/* Raise (clear) CTS */
	*pPORTHIO_SET	= CTS_MASK;

	/* Read character (clears interrupt condition) */
	buffer		= *pUART0_RBR;

	/* Is anything waiting for the character? */
	if (rx_ptr != (BYTEPTR) NULL_P) {
		/* Yes; give it the data and trigger requeue */
		write_byte (rx_ptr, (BYTE) buffer);
		rx_ptr		= (BYTEPTR) NULL_P;
		raise_tvm_interrupt (TVM_INTR_UART0_RX);
	} else {
		/* No; buffer the character */
		rx_buffer	= buffer;
		rx_pending++;
	}
}

void handle_int14 (void)
{
	if (!(tx_pending = run_output (tx_pending, &tx_ptr))) {
		/* Complete; notify TVM and disable interrupt */
		raise_tvm_interrupt (TVM_INTR_UART0_TX);
		*pPORTHIO_MASKA_CLEAR = RTS_MASK;
		SSYNC;
	}
}
#endif

void complete_uart0_rx_interrupt (ECTX ectx)
{
	ectx->add_to_queue (ectx, rx_channel);
	rx_channel = NOT_PROCESS_P;
}

#if 0
void complete_uart0_tx_interrupt (ECTX ectx)
{
	ectx->add_to_queue (ectx, tx_channel);
	tx_channel = NOT_PROCESS_P;
}
#endif

int uart0_is_blocking (void)
{
	/* Return non-zero if rx_channel or tx_channel is not NOT_PROCESS_P. */
	return ((int) rx_channel | (int) tx_channel) ^ NOT_PROCESS_P;
}

int uart0_in (ECTX ectx, WORD count, BYTEPTR pointer)
{
	unsigned short imask;
	int reschedule;
	
	DISABLE_INTERRUPTS (imask);

	if (rx_pending) {
		write_byte (pointer, (BYTE) rx_buffer);
		rx_pending	= 0;
		BARRIER;
		reschedule	= 0;
	} else {
		rx_channel	= ectx->wptr;
		rx_ptr		= pointer;
		BARRIER;
		reschedule	= 1;
	}

	ENABLE_INTERRUPTS (imask);

	if (reschedule) {
		/* Save instruction pointer */
		WORKSPACE_SET (ectx->wptr, WS_IPTR, (WORD) ectx->iptr);
		/* Reschedule */
		return ectx->run_next_on_queue (ectx);
	} else {
		return ECTX_CONTINUE;
	}
}

void uart0_send_char (const unsigned char ch)
{
  int imask;

  DISABLE_INTERRUPTS (imask);
  while (!(U0LSR & ULSR_THRE))          /* wait for TX buffer to empty */
    continue;                           /* also either WDOG() or swap() */

  U0THR = (uint8_t)ch;
  ENABLE_INTERRUPTS (imask);
}

#if 0
int uart0_out (ECTX ectx, WORD count, BYTEPTR pointer)
{
	if (count == sizeof(WORD)) {
		/* If count is the size of a word then 
		 * throw away data as it will be the count 
		 * from a counted array output.
		 */
	} else if ((count = run_output (count, &pointer))) {
		/* Couldn't ship all the output, hand the
		 * rest to the interrupt handler.
		 */
		tx_channel	= ectx->wptr;
		tx_ptr		= pointer;
		tx_pending	= count;
		
		BARRIER;

		/* Enable interrupt */
		*pPORTHIO_MASKA_SET = RTS_MASK;

		/* Reschedule */
		WORKSPACE_SET (ectx->wptr, WS_IPTR, (WORD) ectx->iptr);
		return ectx->run_next_on_queue (ectx);
	}
	
	return ECTX_CONTINUE;
}
#endif

