--
--	raster: basic raster graphics support
--	Copyright (C) 2007  University of Kent
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

--** Basic raster graphics support.
-- This module provides basic data types for two-dimensional bitmap ("raster")
-- graphics.
-- To display graphics, you'll want to use a module like [@ref sdlraster].
--
-- @module raster

--{{{  DATA TYPE RASTER
--* A bitmap image.
--
-- This is really a [@code MOBILE [][]INT], with the first dimension being the
-- Y coordinate and the second the X coordinate.
-- Each element represents an RGB-colourspace pixel in [@code #AARRGGBB]
-- format, with eight bits for each of red, green, blue and alpha. (Most things
-- ignore the alpha component.)
--
-- The pixel representation uses the native endianism, so if you retype it to
-- an array of [@code BYTE]s then the order of the components will vary between
-- platforms.
--
-- It's sometimes useful to have a sentinel value of this type (for example, to
-- use as a poison value in process networks); a zero-by-zero raster can be
-- used for this purpose ([@code MOBILE [0][0]INT]).
-- 
DATA TYPE RASTER IS MOBILE [][]INT:
--}}}

--{{{  DATA TYPE RASTER.EVENT
--* An input event.
DATA TYPE RASTER.EVENT
  RECORD
    --* What type of event this is -- one of [@ref RASTER.ET].
    INT type:
    --* Identifier.
    -- [@item] For keyboard events, this is the key symbol -- a Unicode
    -- character, or one of the SDL constants for special keys ([@ref
    -- SDLK.LEFT], etc.).
    -- [@item] For mouse button events, this is the mouse button number ([@ref
    -- SDL.BUTTON.LEFT], etc.).
    INT id:
    --* Modifier keys.
    -- [@item] For keyboard and mouse button events, this is the modifier state
    -- -- a combination of the SDL modifier constants ([@ref KMOD.LALT], etc.).
    INT mod:
    --* Pointer coordinates.
    -- [@item] For mouse events, this is the pointer position.
    INT16 x, y:
:
--}}}

--*{{{ RASTER.ET Input event types.

--* The event channel is shutting down.
-- This will always be the last event you receive.
VAL INT RASTER.ET.SHUTDOWN IS 1 << 1:
VAL INT RASTER.ET.QUIT IS 1 << 2: --* Window closed
VAL INT RASTER.ET.KEYDOWN IS 1 << 3: --* Key down
VAL INT RASTER.ET.KEYUP IS 1 << 4: --* Key up
VAL INT RASTER.ET.MOUSEBUTTONDOWN IS 1 << 5: --* Mouse button down
VAL INT RASTER.ET.MOUSEBUTTONUP IS 1 << 6: --* Mouse button up
VAL INT RASTER.ET.MOUSEMOTION IS 1 << 7: --* Mouse motion
VAL INT RASTER.ET.EXPOSE IS 1 << 8: --* Window exposure
--*}}}
