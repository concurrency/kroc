--
--	rastergraphics: raster graphics primitives
--	Copyright (C) 2007  David Wood <dcw@kent.ac.uk>
--	Copyright (C) 2007  Adam Sampson <ats@offog.org>
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

--** Graphics primitives for rasters.
--
--   @module rastergraphics
--
--   @maintainer David Wood <D.C.Wood@kent.ac.uk>

--*{ COLOUR Common colours
--
--   Assume coded as [@code #rrggbb], which ain't necessarily so.

VAL INT COLOUR.BLACK   IS #000000:	--*
VAL INT COLOUR.WHITE   IS #FFFFFF:	--*
VAL INT COLOUR.RED     IS #FF0000:	--*
VAL INT COLOUR.GREEN   IS #00FF00:	--*
VAL INT COLOUR.BLUE    IS #0000FF:	--*
VAL INT COLOUR.YELLOW  IS #FFFF00:	--*
VAL INT COLOUR.CYAN    IS #00FFFF:	--*
VAL INT COLOUR.MAGENTA IS #FF00FF:	--*
VAL INT COLOUR.GREY    IS #808080:	--*
VAL INT COLOUR.ORANGE  IS #FFA000:	--*
VAL INT COLOUR.PINK    IS #FFA0A0:	--*
VAL INT COLOUR.SKY     IS #A0A0FF:	--*
VAL INT COLOUR.MAROON  IS #A00000:	--*
VAL INT COLOUR.BROWN   IS #804000:	--*
VAL INT COLOUR.NAVY    IS #000080:	--*
VAL INT COLOUR.PURPLE  IS #C000FF:	--*
VAL INT COLOUR.VIOLET  IS #8000FF:	--*

VAL INT COLOUR.OCTARINE IS #FF000000:	--*

--*}

--{{{  FUNCTION pack.colour
--* Pack RGB [@code BYTEs] to an [@code INT].
--
--  Assume coded as [@code #rrggbb], which ain't necessarily so.
--
--  @param r, g, b Primary colour components
--  @return [@code #rrggbb]

INT INLINE FUNCTION pack.colour (VAL BYTE r, g, b) IS
  ((((INT r) << 8) \/ (INT g)) << 8) \/ (INT b):
--}}}
--{{{  FUNCTION pack.colour.float
--* As [@ref pack.colour], but taking [@code REAL32] arguments.
--
--  @param r, g, b Primary colour components in the range 0.0 to 1.0
--  @return [@code #rrggbb]
INT INLINE FUNCTION pack.colour.float (VAL REAL32 r, g, b) IS
  pack.colour (BYTE ROUND (r * 255.0), BYTE ROUND (g * 255.0), BYTE ROUND (b * 255.0)):
--}}}
--{{{  FUNCTION unpack.colour
--* Unpack an [@code INT] to RGB [@code BYTEs].
--
--  @param c [@code #rrggbb]
--  @returns r, g, b Primary colour components

BYTE, BYTE, BYTE INLINE FUNCTION unpack.colour (VAL INT c) IS
  BYTE ((c >> 16) /\ #FF), BYTE ((c >> 8) /\ #FF), BYTE (c /\ #FF):
--}}}

--*{ PALETTE Codes for [@code permute] arguments.

-- There is a reason for this order ...

VAL INT PALETTE.RGB IS 0:	--*
VAL INT PALETTE.BGR IS 1:	--*
VAL INT PALETTE.RBG IS 2:	--*
VAL INT PALETTE.BRG IS 3:	--*
VAL INT PALETTE.GRB IS 4:	--*
VAL INT PALETTE.GBR IS 5:	--*

--*}
