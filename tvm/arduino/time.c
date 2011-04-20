/*
  Copyright 2009 Adam Sampson <ats@offog.org>

  Based on wiring.c/wiring.h from the Arduino project, which are:
  Copyright (c) 2005-2006 David A. Mellis

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General
  Public License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place, Suite 330,
  Boston, MA  02111-1307  USA
*/

#include "tvm-arduino.h"

#define clockCyclesPerMicrosecond() ( F_CPU / 1000000L )
#define clockCyclesToMicroseconds(a) ( (a) / clockCyclesPerMicrosecond() )

/* the prescaler is set so that timer0 ticks every 64 clock cycles, and the
   the overflow handler is called every 256 ticks. */
#define MICROSECONDS_PER_TIMER0_OVERFLOW (clockCyclesToMicroseconds(64 * 256))
#define MICROSECONDS_PER_TIMER2_OVERFLOW (clockCyclesToMicroseconds(1024 * 256))

/* the whole number of milliseconds per timer0 overflow */
#define MILLIS_INC (MICROSECONDS_PER_TIMER0_OVERFLOW / 1000)
#define MILLIS2_INC (MICROSECONDS_PER_TIMER2_OVERFLOW / 1000)

/* the fractional number of milliseconds per timer0 overflow. we shift right
   by three to fit these numbers into a byte. (for the clock speeds we care
   about - 8 and 16 MHz - this doesn't lose precision.) */
#define FRACT_INC ((MICROSECONDS_PER_TIMER0_OVERFLOW % 1000) >> 3)
#define FRACT2_INC ((MICROSECONDS_PER_TIMER2_OVERFLOW % 1000) >> 3)
#define FRACT_MAX (1000 >> 3)

volatile unsigned long timer0_overflow_count = 0;
volatile unsigned long timer0_millis = 0;
static unsigned char timer0_fract = 0;

SIGNAL(TIMER0_OVF_vect)
{
	/* copy these to local variables so they can be stored in registers
	   (volatile variables must be read from memory on every access) */
	unsigned long m = timer0_millis;
	unsigned char f = timer0_fract;

	//m += MILLIS_INC;
	//f += FRACT_INC;
	m += 1;
	f += 3;
	if (f >= FRACT_MAX) {
		f -= FRACT_MAX;
		m += 1;
	}

	timer0_fract = f;
	timer0_millis = m;
	timer0_overflow_count++;
}

unsigned long time_millis ()
{
	unsigned long m;
	uint8_t oldSREG = SREG;

	/* disable interrupts while we read timer0_millis or we might get an
	   inconsistent value (e.g. in the middle of a write to timer0_millis)
	   */
	cli();
	m = timer0_millis;
	SREG = oldSREG;

	return m;
}

unsigned long time_micros ()
{
	unsigned long m, t;
	uint8_t oldSREG = SREG;
	
	cli();
	t = TCNT0;

	if ((TIFR0 & _BV(TOV0)) && (t == 0))
		t = 256;

	m = timer0_overflow_count;
	SREG = oldSREG;
	
	return ((m << 8) + t) * (64 / clockCyclesPerMicrosecond());
}

void time_init ()
{
	/* set timer 0 prescale factor to 64 */
	TCCR0B |= 3 << CS00;

	/* enable timer 0 overflow interrupt */
	TIMSK0 |= _BV (TOIE0);
}

SIGNAL(TIMER2_OVF_vect)
{
	/* Stop the timer */
	TCNT2 = 0x00;
	//TCCR2B &= ~(_BV(CS22) | _BV(CS21) | _BV(CS20));
	TCCR2B = 0;

	//printf_P (PSTR ("MILLIS2_INC X%dX%dX%dX\n"), 69, MILLIS2_INC, MILLIS_INC);

	unsigned long m = timer0_millis;
	unsigned char f = timer0_fract;

	//m += MILLIS2_INC;
	// FIXME: We seem to loose 1ms every TIMER2 overflow, figure out where
	// we loose it...
	m += 16 + 1;
	//f += FRACT2_INC;
	f += 48;
	if (f >= FRACT_MAX) {
		f -= FRACT_MAX;
		m += 1;
	}

	timer0_fract = f;
	timer0_millis = m;
	timer0_overflow_count += 16;

}

void inline sleep_timer_start()
{
	/* Reset the timer register */
	TCNT2 = 0x00;

	/* Start the timer */
	TCCR2B |= _BV(CS22) & _BV(CS21) & _BV(CS20);

	TIMSK0 |= _BV (TOIE0);
}
