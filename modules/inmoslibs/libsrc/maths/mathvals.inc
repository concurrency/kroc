--
--	Inmos Libraries
--	Copyright (C) 1988, 1989, 1990, 1991, 1993  Inmos Limited
--
--	This library is free software; you can redistribute it and/or
--	modify it under the terms of the GNU Lesser General Public
--	License as published by the Free Software Foundation; either
--	version 2 of the License, or (at your option) any later version.
--
--	This library is distributed in the hope that it will be useful,
--	but WITHOUT ANY WARRANTY; without even the implied warranty of
--	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--	Lesser General Public License for more details.
--
--	You should have received a copy of the GNU Lesser General Public
--	License along with this library; if not, write to the Free Software
--	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
--

--** @module maths

-- mathvals.inc
-- Copyright 1989 INMOS Limited
-- Appended the error condition NaNs for the implementation of the
-- maths libraries; 4/Oct/90 SRH
--{{{  Maths constants

--{{{  REAL32 Constants
VAL REAL32 INFINITY RETYPES #7F800000(INT32) :
VAL REAL32 MINREAL  RETYPES #00000001(INT32) :  -- 1.40129846E-45
VAL REAL32 MAXREAL  RETYPES #7F7FFFFF(INT32) :  -- 3.40282347E+38
--* Single-precision e.
VAL REAL32 E        RETYPES #402DF854(INT32) :  -- 2.71828174E+00
--* Single-precision pi.
VAL REAL32 PI       RETYPES #40490FDB(INT32) :  -- 3.14159274E+00
VAL REAL32 LOGE2    RETYPES #3F317218(INT32) :  -- 6.93147182E-01
VAL REAL32 LOG10E   RETYPES #3EDE5BD9(INT32) :  -- 4.34294492E-01
VAL REAL32 ROOT2    RETYPES #3FB504F3(INT32) :  -- 1.41421354E+00
VAL LOGEPI IS 1.1447298858(REAL32) : -- log to the base e of pi
VAL RADIAN IS 57.295779513(REAL32) : -- the number of degrees in 1 radian
VAL DEGREE IS 1.74532925199E-2(REAL32) : -- the number of radians in 1 degree
VAL GAMMA  IS 0.5772156649(REAL32) : -- Euler's constant

--{{{  implementation defined NaNs
VAL REAL32 undefined.NaN RETYPES #7F800010(INT32) :
VAL REAL32 unstable.NaN  RETYPES #7F800008(INT32) :
VAL REAL32 inexact.NaN   RETYPES #7F800004(INT32) :
--}}}
--}}}

--{{{  REAL64 Constants
VAL REAL64 DINFINITY RETYPES #7FF0000000000000(INT64):
VAL REAL64 DMINREAL  RETYPES #0000000000000001(INT64):-- 4.9406564584124654E-324
VAL REAL64 DMAXREAL  RETYPES #7FEFFFFFFFFFFFFF(INT64):-- 1.7976931348623157E+308
--* Double-precision e.
VAL REAL64 DE        RETYPES #4005BF0A8B145769(INT64):-- 2.7182818284590451E+000
--* Double-precision pi.
VAL REAL64 DPI       RETYPES #400921FB54442D18(INT64):-- 3.1415926535897931E+000
VAL REAL64 DLOGE2    RETYPES #3FE62E42FEFA39EF(INT64):-- 6.9314718055994529E-001
VAL REAL64 DLOG10E   RETYPES #3FDBCB7B1526E50E(INT64):-- 4.3429448190325182E-001
VAL REAL64 DROOT2    RETYPES #3FF6A09E667F3BCD(INT64):-- 1.4142135623730951E+000
VAL DLOGEPI IS 1.1447298858494001741(REAL64) : -- log to the base e of pi
VAL DRADIAN IS 57.295779513082320877(REAL64) : -- the number of degrees in 1 radian
VAL DDEGREE IS 1.7453292519943295769E-2(REAL64) : -- the number of radians in 1 degree
VAL DGAMMA  IS 0.57721566490153286061(REAL64) : -- Euler's constant

--{{{  implementation defined NaNs
VAL REAL64 Dundefined.NaN RETYPES #7FF0000200000000(INT64) :
VAL REAL64 Dunstable.NaN  RETYPES #7FF0000100000000(INT64) :
VAL REAL64 Dinexact.NaN   RETYPES #7FF0000080000000(INT64) :
--}}}
--}}}
--}}}
