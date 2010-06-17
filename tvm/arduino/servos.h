/*

 --  This library is free software; you can redistribute it and/or
 --  modify it under the terms of the GNU Lesser General Public
 --  License as published by the Free Software Foundation; either
 --  version 2.1 of the License, or (at your option) any later version.
 
 --  This library is distributed in the hope that it will be useful,
 --  but WITHOUT ANY WARRANTY; without even the implied warranty of
 --  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 --  Lesser General Public License for more details.
 
 --  You should have received a copy of the GNU Lesser General Public
 --  License along with this library; if not, write to the Free 
 --  Software Foundation, Inc., 51 Franklin St, Fifth Floor,
 --  Boston, MA  02110-1301  USA
 
 --  **********************************************************************
 
*/

#ifndef ARDUINO_SERVOS_H
#define ARDUINO_SERVOS_H

#include <inttypes.h>

#include <avr/interrupt.h>

#include "pins_arduino.h"
#include "wiring_short.h"

//#define clockCyclesPerMicrosecond() ( F_CPU / 1000000L )
//#define clockCyclesToMicroseconds(a) ( (a) / clockCyclesPerMicrosecond() )

// from wiring.h

//#define HIGH 0x1
//#define LOW  0x0

//#define INPUT 0x0
//#define OUTPUT 0x1

typedef enum {false, true} bool;

//ISR (TIMER1_COMPA_vect) __attribute__ ((always_inline));

bool isTimerActive();
 
void initialize(void);

int attach(int pin);

int attachWithLimits(int pin, int minPulse, int maxPulse);

void detach(int pin);  

void write(int pin, int value);

void writeMicroseconds(int pin, int value);

int read(int pin);

int readMicroseconds(int pin);

bool attached(int pin);

#endif






