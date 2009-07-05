--
--	sdlrcompat.inc -- backwards compatibility for sdlraster
--	Copyright (C) 2005 Fred Barnes  <frmb@kent.ac.uk>
--	Copyright (C) 2007 Adam Sampson <ats@offog.org>
--
--	This program is free software; you can redistribute it and/or modify
--	it under the terms of the GNU General Public License as published by
--	the Free Software Foundation; either version 2 of the License, or
--	(at your option) any later version.
--
--	This program is distributed in the hope that it will be useful,
--	but WITHOUT ANY WARRANTY; without even the implied warranty of
--	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--	GNU General Public License for more details.
--
--	You should have received a copy of the GNU General Public License
--	along with this program; if not, write to the Free Software
--	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
--

--** @module sdlraster

--{{{  sdlraster 0.1 compatibility
--* An input event.
-- Button press and release event data are encoded as
-- [@code ((x /\ #FFF) << 20) \/ ((y /\ #FFF) << 8) \/ (button /\ #FF)].
--
-- Key press and release event data are encoded as
-- [@code (unicode.char /\ #FFFF)].
--
-- Motion events are encoded as:
-- [@code ((x /\ #FFF) << 20) \/ ((y /\ #FFF) << 8) \/ (state /\ #FF)].
--
-- @deprecated
-- @private
DATA TYPE SDLEVENT
  RECORD
    INT type:
    INT data:
    INT id:
    INT mod:
    INT16 x, y:
:

VAL EVENT.UNKNOWN IS 0:
VAL EVENT.EXPOSE IS RASTER.ET.EXPOSE:
VAL EVENT.BUTTON.PRESS IS RASTER.ET.MOUSEBUTTONDOWN:
VAL EVENT.BUTTON.RELEASE IS RASTER.ET.MOUSEBUTTONUP:
VAL EVENT.KEY.PRESS IS RASTER.ET.KEYDOWN:
VAL EVENT.KEY.RELEASE IS RASTER.ET.KEYUP:
VAL EVENT.QUIT IS RASTER.ET.QUIT:
VAL EVENT.MOTION IS RASTER.ET.MOUSEMOTION:
VAL EVENT.CKEY.PRESS IS -1:
VAL EVENT.CKEY.RELEASE IS -2:

VAL INT CKEY.UNKNOWN IS INT SDLK.UNKNOWN:
VAL INT CKEY.BACKSPACE IS INT SDLK.BACKSPACE:
VAL INT CKEY.NUMLOCK IS INT SDLK.NUMLOCK:
VAL INT CKEY.CAPSLOCK IS INT SDLK.CAPSLOCK:
VAL INT CKEY.SCROLLOCK IS INT SDLK.SCROLLOCK:
VAL INT CKEY.RSHIFT IS INT SDLK.RSHIFT:
VAL INT CKEY.LSHIFT IS INT SDLK.LSHIFT:
VAL INT CKEY.RCTRL IS INT SDLK.RCTRL:
VAL INT CKEY.LCTRL IS INT SDLK.LCTRL:
VAL INT CKEY.RALT IS INT SDLK.RALT:
VAL INT CKEY.LALT IS INT SDLK.LALT:
VAL INT CKEY.RMETA IS INT SDLK.RMETA:
VAL INT CKEY.LMETA IS INT SDLK.LMETA:
VAL INT CKEY.RSUPER IS INT SDLK.RSUPER:
VAL INT CKEY.LSUPER IS INT SDLK.LSUPER:
VAL INT CKEY.SYSREQ IS INT SDLK.SYSREQ:
VAL INT CKEY.MENU IS INT SDLK.MENU:
VAL INT CKEY.MODE IS INT SDLK.MODE:
VAL INT CKEY.UP IS INT SDLK.UP:
VAL INT CKEY.DOWN IS INT SDLK.DOWN:
VAL INT CKEY.LEFT IS INT SDLK.LEFT:
VAL INT CKEY.RIGHT IS INT SDLK.RIGHT:
VAL INT CKEY.HOME IS INT SDLK.HOME:
VAL INT CKEY.END IS INT SDLK.END:
VAL INT CKEY.PAGEUP IS INT SDLK.PAGEUP:
VAL INT CKEY.PAGEDOWN IS INT SDLK.PAGEDOWN:
VAL INT CKEY.INSERT IS INT SDLK.INSERT:
VAL INT CKEY.DELETE IS INT SDLK.DELETE:

VAL CTL.UNKNOWN IS 0:
VAL CTL.SHUTDOWN IS 1:
--}}}
