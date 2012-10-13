--
--	Definitions for the useful module
--	Copyright (C) 1997 D.C. Wood <D.C.Wood@kent.ac.uk>
--	Copyright (C) 2008, 2009, 2010 Adam Sampson <ats@offog.org>
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
-- This module includes a large number of [@code PROC]s for formatted output
-- that aren't individually documented. These take varying numbers of
-- arguments, and have names that end with a "format string" indicating the
-- types of the arguments they support. Several families of [@code PROC]s are
-- provided for different purposes:
--
-- [@item] [@code print.*] write to a [@code CHAN BYTE];
-- [@item] [@code prints.*] write to a [@code SHARED CHAN BYTE], claiming it
-- first;
-- [@item] [@code format.*] return a string as a [@code RESULT MOBILE []BYTE];
-- [@item] [@code trace.*] write directly to standard error, taking no
-- additional parameters.
--
-- The output channel or variable always appears at the end of the argument
-- list.
--
-- The formatting characters supported are:
--
-- [@item] [@code s] for a string as a [@code VAL []BYTE];
-- [@item] [@code i] for a decimal integer as a [@code VAL INT];
-- [@item] [@code r] for a decimal floating-point value as a [@code VAL REAL32];
-- [@item] [@code b] for a boolean value as a [@code VAL BOOL];
-- [@item] [@code n] for a newline (taking no parameter).
--
-- For example, if you wanted to print a string and an integer followed by a
-- newline to a channel, you could use [@code PROC print.sin (VAL []BYTE s, VAL
-- INT i, CHAN BYTE out!)] as follows:
--
-- [@code
-- print.sin ("Number of interrupts: ", count, out!)
-- ]
--
-- [@code PROC]s are provided for all valid combinations of up to 5 formatting
-- characters, with the restrictions that [@code n] is only permitted at the
-- end of a format string, and [@code i], [@code r] and [@code b] cannot occur
-- next to each other. (For example, [@code trace.sisin] is provided, but
-- [@code trace.siin] and [@code trace.sinin] are not.)
--
-- @module useful

--{{{  DATA TYPE VECTOR2
--* A two-dimensional vector of [@code REAL]s.
DATA TYPE VECTOR2
  RECORD
    REAL32 x, y:
:
--}}}
--{{{  VECTOR2 operators
VECTOR2 INLINE FUNCTION "+" (VAL VECTOR2 a, b) IS [a[x] + b[x], a[y] + b[y]]:
VECTOR2 INLINE FUNCTION "-" (VAL VECTOR2 a, b) IS [a[x] - b[x], a[y] - b[y]]:
VECTOR2 INLINE FUNCTION "-" (VAL VECTOR2 a) IS [-a[x], -a[y]]:
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

--{{{  DATA TYPE VECTOR2.INT
--* A two-dimensional vector of [@code INT]s.
DATA TYPE VECTOR2.INT
  RECORD
    INT x, y:
:
--}}}
--{{{  VECTOR2.INT operators
BOOL INLINE FUNCTION "=" (VAL VECTOR2.INT a, b) IS ((a[x] = b[x]) AND (a[y] = b[y])):
VECTOR2.INT INLINE FUNCTION "+" (VAL VECTOR2.INT a, b) IS [a[x] + b[x], a[y] + b[y]]:
VECTOR2.INT INLINE FUNCTION "-" (VAL VECTOR2.INT a, b) IS [a[x] - b[x], a[y] - b[y]]:
VECTOR2.INT INLINE FUNCTION "-" (VAL VECTOR2.INT a) IS [-a[x], -a[y]]:
VECTOR2.INT INLINE FUNCTION "**" (VAL VECTOR2.INT a, VAL INT b) IS [a[x] * b, a[y] * b]:
VECTOR2.INT INLINE FUNCTION "/" (VAL VECTOR2.INT a, VAL INT b) IS [a[x] / b, a[y] / b]:
--}}}

--{{{  DATA TYPE VECTOR3
--* A three-dimensional vector.
DATA TYPE VECTOR3
  RECORD
    REAL32 x, y, z:
:
--}}}
--{{{  VECTOR3 operators
VECTOR3 INLINE FUNCTION "+" (VAL VECTOR3 a, b) IS [a[x] + b[x], a[y] + b[y], a[z] + b[z]]:
VECTOR3 INLINE FUNCTION "-" (VAL VECTOR3 a, b) IS [a[x] - b[x], a[y] - b[y], a[z] - b[z]]:
VECTOR3 INLINE FUNCTION "-" (VAL VECTOR3 a) IS [-a[x], -a[y], -a[z]]:
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

--{{{  DATA TYPE COMPLEX32
--* A 2*32-bit complex number.
DATA TYPE COMPLEX32
  RECORD
    REAL32 real, imag:
:
--}}}
--{{{  COMPLEX32 constants
VAL COMPLEX32 Zero.COMPLEX32 IS [0.0, 0.0]:
VAL COMPLEX32  One.COMPLEX32 IS [1.0, 0.0]:
VAL COMPLEX32    I.COMPLEX32 IS [0.0, 1.0]:
--}}}
--{{{  COMPLEX32 operations
REAL32 INLINE FUNCTION COMPLEX32ABSSQ (VAL COMPLEX32 x) IS
  (x[real]*x[real]) + (x[imag]*x[imag]):

COMPLEX32 INLINE FUNCTION "-" (VAL COMPLEX32 x) IS
  [-x[real], -x[imag]]:

COMPLEX32 INLINE FUNCTION "~" (VAL COMPLEX32 x) IS
  [x[real], -x[imag]]:

COMPLEX32 INLINE FUNCTION "+" (VAL COMPLEX32 x, y) IS
  [x[real] + y[real], x[imag] + y[imag]]:

COMPLEX32 INLINE FUNCTION "-" (VAL COMPLEX32 x, y) IS
  [x[real] - y[real], x[imag] - y[imag]]:

COMPLEX32 INLINE FUNCTION "**" (VAL COMPLEX32 x, y) IS
  [(x[real]*y[real]) - (x[imag]*y[imag]),
   (x[imag]*y[real]) + (x[real]*y[imag])]:

COMPLEX32 INLINE FUNCTION "**" (VAL COMPLEX32 x, VAL REAL32 r) IS
  [x[real]*r, x[imag]*r]:

COMPLEX32 INLINE FUNCTION "**" (VAL REAL32 r, VAL COMPLEX32 x) IS
  [x[real]*r, x[imag]*r]:

COMPLEX32 INLINE FUNCTION "/" (VAL COMPLEX32 x, VAL REAL32 r) IS
  [x[real]/r, x[imag]/r]:

COMPLEX32 INLINE FUNCTION REAL32TOCOMPLEX32 (VAL REAL32 x) IS
  [x, 0.0]:

BOOL INLINE FUNCTION "=" (VAL COMPLEX32 x, y) IS
  (x[real] = y[real]) AND (x[imag] = y[imag]):

BOOL INLINE FUNCTION "<>" (VAL COMPLEX32 x, y) IS
  (x[real] <> y[real]) OR (x[imag] <> y[imag]):

REAL32, REAL32 INLINE FUNCTION COMPLEX32TOREAL32 (VAL COMPLEX32 x) IS
  x[real], x[imag]:

COMPLEX32 INLINE FUNCTION COMPLEX32SQR (VAL COMPLEX32 z) IS
  [(z[real] + z[imag])*(z[real] - z[imag]), (z[real]*z[imag])*2.0]:
--}}}

--{{{  DATA TYPE COMPLEX64
--* A 2*64-bit complex number.
DATA TYPE COMPLEX64
  RECORD
    REAL64 real, imag:
:
--}}}
--{{{  COMPLEX64 constants
VAL COMPLEX64 Zero.COMPLEX64 IS [0.0, 0.0]:
VAL COMPLEX64  One.COMPLEX64 IS [1.0, 0.0]:
VAL COMPLEX64    I.COMPLEX64 IS [0.0, 1.0]:
--}}}
--{{{  COMPLEX64 operations
REAL64 INLINE FUNCTION COMPLEX64ABSSQ (VAL COMPLEX64 x) IS
  (x[real]*x[real]) + (x[imag]*x[imag]):

COMPLEX64 INLINE FUNCTION "-" (VAL COMPLEX64 x) IS
  [-x[real], -x[imag]]:

COMPLEX64 INLINE FUNCTION "~" (VAL COMPLEX64 x) IS
  [x[real], -x[imag]]:

COMPLEX64 INLINE FUNCTION "+" (VAL COMPLEX64 x, y) IS
  [x[real] + y[real], x[imag] + y[imag]]:

COMPLEX64 INLINE FUNCTION "-" (VAL COMPLEX64 x, y) IS
  [x[real] - y[real], x[imag] - y[imag]]:

COMPLEX64 INLINE FUNCTION "**" (VAL COMPLEX64 x, y) IS
  [(x[real]*y[real]) - (x[imag]*y[imag]),
   (x[imag]*y[real]) + (x[real]*y[imag])]:

COMPLEX64 INLINE FUNCTION "**" (VAL COMPLEX64 x, VAL REAL64 r) IS
  [x[real]*r, x[imag]*r]:

COMPLEX64 INLINE FUNCTION "**" (VAL REAL64 r, VAL COMPLEX64 x) IS
  [x[real]*r, x[imag]*r]:

COMPLEX64 INLINE FUNCTION "/" (VAL COMPLEX64 x, VAL REAL64 r) IS
  [x[real]/r, x[imag]/r]:

COMPLEX64 INLINE FUNCTION REAL64TOCOMPLEX64 (VAL REAL64 x) IS
  [x, 0.0]:

BOOL INLINE FUNCTION "=" (VAL COMPLEX64 x, y) IS
  (x[real] = y[real]) AND (x[imag] = y[imag]):

BOOL INLINE FUNCTION "<>" (VAL COMPLEX64 x, y) IS
  (x[real] <> y[real]) OR (x[imag] <> y[imag]):

REAL64, REAL64 INLINE FUNCTION COMPLEX64TOREAL64 (VAL COMPLEX64 x) IS
  x[real], x[imag]:

COMPLEX64 INLINE FUNCTION COMPLEX32TO64 (VAL COMPLEX32 x) IS
  [REAL64 x[real], REAL64 x[imag]]:

COMPLEX32 INLINE FUNCTION COMPLEX64ROUND (VAL COMPLEX64 x) IS
  [REAL32 ROUND x[real], REAL32 ROUND x[imag]]:

COMPLEX32 INLINE FUNCTION COMPLEX64TRUNC (VAL COMPLEX64 x) IS
  [REAL32 TRUNC x[real], REAL32 TRUNC x[imag]]:

COMPLEX64 INLINE FUNCTION COMPLEX64SQR (VAL COMPLEX64 z) IS
  [(z[real] + z[imag])*(z[real] - z[imag]), (z[real]*z[imag])*2.0]:
--}}}

--{{{  PROTOCOL P.LEX.BYTESTREAM
--* Byte-stream protocol.
PROTOCOL P.LEX.BYTESTREAM
  CASE
    bytes; MOBILE []BYTE            --* Some bytes.
    end.of.file                     --* End of file.
:
--}}}
--{{{  PROTOCOL P.LEX.LINESTREAM
--* Line-stream protocol.
PROTOCOL P.LEX.LINESTREAM
  CASE
    line; INT; MOBILE []BYTE        --* Whole line of text with line-number (without the trailing newline).
    end.of.file                     --* End of file.
:
--}}}
--{{{  PROTOCOL P.LEX.TOKENSTREAM
--* Token-stream protocol.
PROTOCOL P.LEX.TOKENSTREAM
  CASE
    word; INT; MOBILE []BYTE        --* Single word/name/etc prefixed with line-number.
    symbol; INT; BYTE               --* Distinct symbol (character) prefixed with line-number.
    integer; INT; INT               --* Integer prefixed with line-number.
    string; INT; MOBILE []BYTE      --* Quoted string prefixed with line-number.
    indent; INT; INT                --* Indent to new level (when tracking indentation) prefixed with line-number.
    outdent; INT; INT               --* Outdent to old level (when tracking indentation) prefixed with line-number.
    end.of.file                     --* End of file.
:
--}}}


