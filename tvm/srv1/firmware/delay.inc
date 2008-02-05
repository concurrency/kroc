#IF NOT DEFINED (DELAY.INC)
#DEFINE DELAY.INC

#INCLUDE "srv1.inc"

PROC delay.us (VAL INT us)
  TIMER tim:
  INT t:
  SEQ
    tim ? t
    tim ? AFTER t PLUS us
:

PROC delay.ms (VAL INT ms)
  TIMER tim:
  INT t:
  SEQ
    tim ? t
    tim ? AFTER t PLUS (ms * MILLIS)
:

PROC delay.s (VAL INT seconds)
  TIMER tim:
  INT t:
  SEQ
    tim ? t
    tim ? AFTER t PLUS (seconds * SECONDS)
:

#ENDIF -- !DELAY.INC
