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

VAL CKEY.UNKNOWN IS SDLK.UNKNOWN:
VAL CKEY.BACKSPACE IS SDLK.BACKSPACE:
VAL CKEY.NUMLOCK IS SDLK.NUMLOCK:
VAL CKEY.CAPSLOCK IS SDLK.CAPSLOCK:
VAL CKEY.SCROLLOCK IS SDLK.SCROLLOCK:
VAL CKEY.RSHIFT IS SDLK.RSHIFT:
VAL CKEY.LSHIFT IS SDLK.LSHIFT:
VAL CKEY.RCTRL IS SDLK.RCTRL:
VAL CKEY.LCTRL IS SDLK.LCTRL:
VAL CKEY.RALT IS SDLK.RALT:
VAL CKEY.LALT IS SDLK.LALT:
VAL CKEY.RMETA IS SDLK.RMETA:
VAL CKEY.LMETA IS SDLK.LMETA:
VAL CKEY.RSUPER IS SDLK.RSUPER:
VAL CKEY.LSUPER IS SDLK.LSUPER:
VAL CKEY.SYSREQ IS SDLK.SYSREQ:
VAL CKEY.MENU IS SDLK.MENU:
VAL CKEY.MODE IS SDLK.MODE:
VAL CKEY.UP IS SDLK.UP:
VAL CKEY.DOWN IS SDLK.DOWN:
VAL CKEY.LEFT IS SDLK.LEFT:
VAL CKEY.RIGHT IS SDLK.RIGHT:
VAL CKEY.HOME IS SDLK.HOME:
VAL CKEY.END IS SDLK.END:
VAL CKEY.PAGEUP IS SDLK.PAGEUP:
VAL CKEY.PAGEDOWN IS SDLK.PAGEDOWN:
VAL CKEY.INSERT IS SDLK.INSERT:
VAL CKEY.DELETE IS SDLK.DELETE:

VAL CTL.UNKNOWN IS 0:
VAL CTL.SHUTDOWN IS 1:
--}}}
