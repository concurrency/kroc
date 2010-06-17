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
 
 --  servos.c - A simple servo control scheme based on the Servo
 --  class in the Arduino libraries directory. It's been written to 
 --  adapt it to the occam-pi environment using FFI ( Foreign Function 
 --  Interface ).

 --  This code allows an Arduino to control up to 12 servos. A pulse 
 --  of a determined length is sent to a servo by setting an associated
 --  pin to HIGH. This sets the servo's position. The pin is then
 --  set to LOW until a defined interval has passed. The pulse plus
 --  the following interval are always of the same length which is
 --  determined by the design of the servo. This pulse plus the wait
 --  period is repeated for as long as the servo is to remain in the
 --  same position.
 --
 
*/


#include <stdio.h>

#include "servos.h"


#include "pins_arduino.h"

#define MyServo_VERSION           2      // software version of this library

#define MIN_PULSE_WIDTH       544  // the shortest pulse sent to a servo  
#define MAX_PULSE_WIDTH      2400  // the longest pulse sent to a servo 
#define DEFAULT_PULSE_WIDTH  1500  // default pulse width when servo is attached
#define REFRESH_INTERVAL     20000 // minumim time to refresh servos in microseconds 

 // converts microseconds to ticks (assumes prescale of 8)  // 12 Aug 2009

#define usToTicks(_us)    (( clockCyclesPerMicrosecond()* _us) / 8)

 // converts from ticks back to microseconds

#define ticksToUs(_ticks) (( (unsigned)_ticks * 8)/ clockCyclesPerMicrosecond() )

 // minimum value in uS for a servo

#define SERVO_MIN(_servoMin) (MIN_PULSE_WIDTH - _servoMin * 4)

 // maximum value in uS for this servo

#define SERVO_MAX(_servoMax) (MAX_PULSE_WIDTH - _servoMax * 4) 

 // compensating ticks to adjust for digitalWrite delays

#define TRIM_DURATION       2

 // flag indicating an invalid servo index
   
#define INVALID_SERVO 255  

#define MAX_SERVOS 12

typedef struct {
  int   pin;       // pin associated with this servo
  int   ticks;     // number of ticks to position servo
  int   minTicks;  // minimum is this value times 4 added to MIN_PULSE_WIDTH    
  int   maxTicks;  // maximum is this value times 4 added to MAX_PULSE_WIDTH
  bool  active;    // true if this pin is enabled, pin not pulsed if false    
} Servo;

typedef struct {
  int   servoTableIndex;   
} ActiveServoMap;

Servo servos[ MAX_SERVOS ];
ActiveServoMap servoTableMap[ MAX_SERVOS ];

int nextServo;
int servoCount;
int nextTableSlot;

    //  States of the FSM in the interrupt handler

#define START_FRAME     0
#define SET_PIN_HIGH    1
#define SET_PIN_LOW     2

int state;

bool firstAttach;

inline int getServo( int );

int map( int x, int in_min, int in_max, int out_min, int out_max);

/**********************************************************************/
/*                                                                    */
/*   The following ISR, once it has started, generates the pulses     */
/*   which control the servo(s).                                      */
/*                                                                    */
/**********************************************************************/

ISR (TIMER1_COMPA_vect) 
{
   
  repeat:
   
  switch ( state ) {

    case START_FRAME:

      //Serial.println( "START_FRAME" );
      TCNT1 = 0;
      nextServo = -1; 

    case SET_PIN_HIGH:
  
      //Serial.println( "SET_PIN_HIGH" );

      if ( nextServo < servoCount ) {
        
        //Serial.println( "HIGH" );
  
        nextServo++;
        OCR1A = TCNT1 + servos[getServo(nextServo)].ticks;
        digitalWrite( servos[getServo(nextServo)].pin, HIGH ); // its an active channel so pulse it high   
        
        state = SET_PIN_LOW;      
      } else { 
        
        //Serial.println( "FRAME" );
  
        if( (unsigned)TCNT1 <  (usToTicks(REFRESH_INTERVAL) + 4) )
          OCR1A = (unsigned int)usToTicks(REFRESH_INTERVAL);  
        else 
          OCR1A = TCNT1 + 4;  // at least REFRESH_INTERVAL has elapsed
  
        state = START_FRAME;
      }
  
    break;

    case SET_PIN_LOW:
       
      //Serial.println( "LOW" );
  
      digitalWrite( servos[getServo(nextServo)].pin, LOW );
      
      state = SET_PIN_HIGH;
      goto repeat; 
  
      break;
  
    default:
  
      //Serial.println("Error in ISR (TIMER1_COMPA_vect)" );

      break;
  
    }

}

void initISR()
{  
  TCCR1A = 0;              // normal counting mode 
  TCCR1B = _BV(CS11);      // set prescaler of 8 
  TCNT1 = 0;               // clear the timer count 
  TIFR1 |= _BV(OCF1A);     // clear any pending interrupts; 
  TIMSK1 |=  _BV(OCIE1A) ; // enable the output compare interrupt 
} 

void finISR()
{
  //disable use of the given timer  ??
}

bool isTimerActive()
{

   int i;
   bool response = false;

  // returns true if any servo is active on the 16 bit timer

  for ( i = 0; i < MAX_SERVOS; i++ ) {
    if(servos[i].active == true) {
      response = true;
    }
  }
  return response;
}

void initialize()
{

  int i;

  nextServo      = -1;
  servoCount     =  0;
  nextTableSlot  = -1;
  state = -1;
  firstAttach = true;
  
  for ( i = 0; i < MAX_SERVOS; i++ )
  {
    servos[i].pin         = -1;
    servos[i].ticks       = usToTicks(DEFAULT_PULSE_WIDTH);
    servos[i].minTicks    = 0;
    servos[i].maxTicks    = 0;
    servos[i].active      = false;
  }

  state = START_FRAME;
}

/****  Occam - C interface (FFI)        ****/

void __addServo( int w[] ) {
  printf( "__addServo called" );
  addServo( (int) w[0] );
}

int addServo(int pin)
{

  // TODO: validate pin

  return( addServoWithLimits(pin, MIN_PULSE_WIDTH, MAX_PULSE_WIDTH) );
}

int addServoWithLimits(int pin, int minPulse, int maxPulse)
{
  
  // TODO: validate pin

  if( pin < MAX_SERVOS ) {

    pinMode( pin, OUTPUT) ;
    
    nextTableSlot++;
    servoTableMap[nextTableSlot].servoTableIndex = pin;

    servos[pin].pin = pin;
    // todo min/max check: abs(min - MIN_PULSE_WIDTH) /4 < 128 
    servos[pin].minTicks  = (MIN_PULSE_WIDTH - minPulse)/4; //resolution of min/max is 4 uS
    servos[pin].maxTicks  = (MAX_PULSE_WIDTH - maxPulse)/4;
 
    if(firstAttach==true) {
      initialize();
      initISR();
      firstAttach = false;
    } 
  
    servos[pin].active = true;  // this must be set after the check for isTimerActive
  } 
  return pin ;

}

void removeServo(int pin)  
{

  // TODO: validate pin

  servos[pin].active = false;

  // TODO: take care of servoMap
  
  // TURN OFF THE INTERRUPT ??
  
}

/****  Occam - C interface (FFI)        ****/

void __setServoPositionInDegrees( int w[] ) {
  printf( "__write called" );
  setServoPositionInDegrees( (int)w[0], (int)w[1] );
}

void setServoPositionInDegrees( int pin, int value )
{  
 
  // TODO: validate pin

  printf( "write called" );

  // treat values less than 900 as angles in degrees
  // (valid values in microseconds are handled as microseconds)
  
  if ( value < MIN_PULSE_WIDTH ) {
   
    if(value < 0) value = 0;
    if(value > 180) value = 180;
    value = map( value, 1, 180, MIN_PULSE_WIDTH,  MAX_PULSE_WIDTH );
          
  }
  
  setServoPositionInMicroSeconds(pin, value);
}

void setServoPositionInMicroSeconds(int pin, int value)
{
  // calc and store the values for the pin the used to
  // control servo

  if( pin < MAX_SERVOS )
  {  
    if( value < SERVO_MIN( servos[pin].minTicks ) )         // ensure pulse width is valid
      value =  SERVO_MIN( servos[pin].minTicks );
    else if( value >  SERVO_MAX( servos[pin].maxTicks ) )
      value =  SERVO_MAX( servos[pin].maxTicks );   

    value = value - TRIM_DURATION;
    value = usToTicks(value);  // convert to ticks after compensating for interrupt overhead - 12 Aug 2009
    /*
      Serial.println( "writeM" );
      Serial.println( pin );
      Serial.println( value );
    */
    uint8_t oldSREG = SREG;
    cli();
    servos[pin].ticks = value;  
    SREG = oldSREG;   
  } 

}

int getServoPositioninDegrees(int pin) // return the value as degrees
{
 
  // TODO: validate pin

  return  map( getServoPositioninMicroseconds(pin), MIN_PULSE_WIDTH, MAX_PULSE_WIDTH, 0, 180 );
}

int getServoPositioninMicroseconds(int pin)
{
  unsigned int pulsewidth;
  if( pin != INVALID_SERVO )
    pulsewidth = ticksToUs(servos[pin].ticks)  + TRIM_DURATION ;   // 12 aug 2009
  else 
    pulsewidth  = 0;

  return pulsewidth;   
}

bool isServoActive(int pin)
{
 
  // TODO: validate pin

  return servos[pin].active ;
}


inline int getServo( int nextServo ) {   

  return( servoTableMap[nextServo].servoTableIndex );
  
}

/**********************************************************************/
/*                                                                    */
/*                                                                    */
/*                                                                    */
/**********************************************************************/


int map( int x, int in_min, int in_max, int out_min, int out_max)
{
  return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
}
