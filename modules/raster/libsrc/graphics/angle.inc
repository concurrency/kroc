--
--	rastergraphics: raster graphics primitives
--	Copyright (C) 2007  David Wood <dcw@kent.ac.uk>
--
--	This library is free software; you can redistribute it and/or
--	modify it under the terms of the GNU Lesser General Public
--	License as published by the Free Software Foundation, either
--	version 2 of the License, or (at your option) any later version.
--
--	This library is distributed in the hope that it will be useful,
--	but WITHOUT ANY WARRANTY; without even the implied warranty of
--	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--	Lesser General Public License for more details.
--
--	You should have received a copy of the GNU Lesser General Public
--	License along with this library.  If not, see
--	<http://www.gnu.org/licenses/>.
--

--** @module rastergraphics

--* ANGLE data type
--
--   An [@code ANGLE] is measured in units of 2^32 bits = 1 turn
--   = 4 right angles = 2 pi radians = 360 degrees = 400 grades.
--   Like times, angles wrap round without overflowing.

DATA TYPE ANGLE IS INT:

--*{ ANGLE constants

VAL ANGLE ANGLE.STRAIGHT   IS #80000000:		--* 2^31 (exact)
VAL ANGLE ANGLE.RIGHT      IS #40000000:		--* 2^30 (exact)
VAL ANGLE ANGLE.RADIAN     IS 683565276:		--* 2^31 / pi (approx.)
VAL ANGLE ANGLE.DEGREE     IS  11930465:		--* 2^32 / 360 (approx.)
VAL ANGLE ANGLE.MINUTE     IS    198841:
VAL ANGLE ANGLE.SECOND     IS      3314:
VAL ANGLE ANGLE.GRADE      IS  10737418:		--* 2^32 / 400 (approx.)
VAL ANGLE ANGLE.CENTIGRADE IS    107374:

--*}

--* Addition operator
--
--  @param X, Y Operands
--  @return [@code X + Y]

ANGLE INLINE FUNCTION "+"  (VAL ANGLE X, Y)		IS X PLUS  Y:
ANGLE INLINE FUNCTION "-"  (VAL ANGLE X, Y)		IS X MINUS Y:
ANGLE INLINE FUNCTION "**" (VAL ANGLE X, VAL INT Y)	IS X TIMES (ANGLE Y):
ANGLE INLINE FUNCTION "**" (VAL INT X, VAL ANGLE Y)	IS (ANGLE X) TIMES Y:
ANGLE INLINE FUNCTION "/"  (VAL ANGLE X, VAL INT Y)	IS X / (ANGLE Y):
ANGLE INLINE FUNCTION "-"  (VAL ANGLE X)		IS MINUS X:
BOOL  INLINE FUNCTION ">"  (VAL ANGLE X, Y)		IS X AFTER Y:
BOOL  INLINE FUNCTION "<"  (VAL ANGLE X, Y)		IS Y AFTER X:
BOOL  INLINE FUNCTION "<=" (VAL ANGLE X, Y)		IS NOT (X AFTER Y):
BOOL  INLINE FUNCTION ">=" (VAL ANGLE X, Y)		IS NOT (Y AFTER X):

--* Reverse operator
--
--  @param X Operands
--  @return Opposite direction

ANGLE INLINE FUNCTION "~"  (VAL ANGLE X)		IS X PLUS ANGLE.STRAIGHT:
