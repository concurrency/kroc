--
-- KRoC Complex Arithmetic Library
--
-- $Id: complex.inc,v 1.1 1997/03/31 11:57:45 djb1 Exp $
--
-- $Source:
--
-- DATA TYPEs and constants
--
-- (C) Copyright 1997 D.C. Wood <D.C.Wood@ukc.ac.uk>
-- University of Kent at Canterbury

DATA TYPE COMPLEX32
  RECORD
    REAL32 real, imag:
:

VAL COMPLEX32 Zero.COMPLEX32 IS [0.0, 0.0]:
VAL COMPLEX32  One.COMPLEX32 IS [1.0, 0.0]:
VAL COMPLEX32    I.COMPLEX32 IS [0.0, 1.0]:

DATA TYPE COMPLEX64
  RECORD
    REAL64 real, imag:
:

VAL COMPLEX64 Zero.COMPLEX64 IS [0.0, 0.0]:
VAL COMPLEX64  One.COMPLEX64 IS [1.0, 0.0]:
VAL COMPLEX64    I.COMPLEX64 IS [0.0, 1.0]:
