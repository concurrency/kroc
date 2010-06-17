/*


 --  wiring.h - Partial implementation of the Wiring API for the ATmega8.
 --  Part of Arduino - http://www.arduino.cc/

 --  Copyright (c) 2005-2006 David A. Mellis

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

#ifndef Wiring_Short_h
#define Wiring_Short_h

#include <inttypes.h>

#ifdef __cplusplus
extern "C"{
#endif

#ifndef cbi
#define cbi(sfr, bit) (_SFR_BYTE(sfr) &= ~_BV(bit))
#endif
#ifndef sbi
#define sbi(sfr, bit) (_SFR_BYTE(sfr) |= _BV(bit))
#endif

#define HIGH 0x1
#define LOW  0x0

#define INPUT 0x0
#define OUTPUT 0x1

#define PI 3.1415926535897932384626433832795
#define HALF_PI 1.5707963267948966192313216916398
#define TWO_PI 6.283185307179586476925286766559
#define DEG_TO_RAD 0.017453292519943295769236907684886
#define RAD_TO_DEG 57.295779513082320876798154814105

//#define interrupts() sei()
//#define noInterrupts() cli()

#define clockCyclesPerMicrosecond() ( F_CPU / 1000000L )
#define clockCyclesToMicroseconds(a) ( (a) / clockCyclesPerMicrosecond() )
#define microsecondsToClockCycles(a) ( (a) * clockCyclesPerMicrosecond() )

void pinMode( uint8_t, uint8_t );
void digitalWrite( uint8_t, uint8_t );
int digitalRead( uint8_t );

#ifdef __cplusplus
} // extern "C"
#endif

#endif

