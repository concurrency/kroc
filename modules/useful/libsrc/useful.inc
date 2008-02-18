--
--	Definitions for the useful module
--	Copyright (C) 2008 Adam Sampson <ats@offog.org>
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

--** General utilities.
--
-- This module contains miscellaneous utilities for occam-pi programs. The aim
-- is to provide somewhere to collect code that should obviously be in the
-- standard library (that is: it's been needed by more than one occam-pi
-- program), but where there isn't an existing module that it should obviously
-- go into.
--
-- @module useful

--{{{  DATA TYPE VECTOR2
DATA TYPE VECTOR2
  RECORD
    REAL32 x, y:
:
--}}}
--{{{  VECTOR2 operators
VECTOR2 INLINE FUNCTION "+" (VAL VECTOR2 a, b) IS [a[x] + b[x], a[y] + b[y]]:
VECTOR2 INLINE FUNCTION "-" (VAL VECTOR2 a, b) IS [a[x] - b[x], a[y] - b[y]]:
VECTOR2 INLINE FUNCTION "**" (VAL VECTOR2 a, VAL REAL32 b) IS [a[x] * b, a[y] * b]:
VECTOR2 INLINE FUNCTION "/" (VAL VECTOR2 a, VAL REAL32 b) IS [a[x] / b, a[y] / b]:
--}}}
--{{{  FUNCTION mag.squared2
--* Compute the square of the magnitude of a [@ref VECTOR2].
REAL32 INLINE FUNCTION mag.squared2 (VAL VECTOR2 v) IS (v[x] * v[x]) + (v[y] * v[y]):
--}}}
--{{{  FUNCTION dot.product2
REAL32 INLINE FUNCTION dot.product2 (VAL VECTOR2 a, b) IS (a[x] * b[x]) + (a[y] * b[y]):
--}}}

--{{{  DATA TYPE VECTOR3
DATA TYPE VECTOR3
  RECORD
    REAL32 x, y, z:
:
--}}}
--{{{  VECTOR3 operators
VECTOR3 INLINE FUNCTION "+" (VAL VECTOR3 a, b) IS [a[x] + b[x], a[y] + b[y], a[z] + b[z]]:
VECTOR3 INLINE FUNCTION "-" (VAL VECTOR3 a, b) IS [a[x] - b[x], a[y] - b[y], a[z] - b[z]]:
VECTOR3 INLINE FUNCTION "**" (VAL VECTOR3 a, VAL REAL32 b) IS [a[x] * b, a[y] * b, a[z] * b]:
VECTOR3 INLINE FUNCTION "/" (VAL VECTOR3 a, VAL REAL32 b) IS [a[x] / b, a[y] / b, a[z] / b]:
--}}}
--{{{  FUNCTION mag.squared3
--* Compute the square of the magnitude of a [@ref VECTOR3].
REAL32 INLINE FUNCTION mag.squared3 (VAL VECTOR3 v) IS
  ((v[x] * v[x]) + (v[y] * v[y])) + (v[z] * v[z]):
--}}}
--{{{  FUNCTION dot.product3
REAL32 INLINE FUNCTION dot.product3 (VAL VECTOR3 a, b) IS ((a[x] * b[x]) + (a[y] * b[y])) + (a[z] * b[z]):
--}}}
