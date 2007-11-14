--
--	sdlraster: SDL-based raster display
--	Copyright (C) 2007  University of Kent
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

--** @module sdlraster

--{{{  DATA TYPE RASTER.EVENT
--* An input event.
-- This is a more occam-ish repackaging of SDL events.
DATA TYPE RASTER.EVENT
  MOBILE RECORD
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
VAL INT RASTER.ET.SHUTDOWN IS 1:
VAL INT RASTER.ET.QUIT IS 2: --* Window closed
VAL INT RASTER.ET.KEYDOWN IS 3: --* Key down
VAL INT RASTER.ET.KEYUP IS 4: --* Key up
VAL INT RASTER.ET.MOUSEBUTTONDOWN IS 5: --* Mouse button down
VAL INT RASTER.ET.MOUSEBUTTONUP IS 6: --* Mouse button up
VAL INT RASTER.ET.MOUSEMOTION IS 7: --* Mouse motion
VAL INT RASTER.ET.EXPOSE IS 8: --* Window exposure
--*}}}
