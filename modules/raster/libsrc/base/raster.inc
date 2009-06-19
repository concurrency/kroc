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
    -- character, or one of [@ref RASTER.KEY].
    -- [@item] For mouse button events, this is one of [@ref RASTER.BUTTON].
    INT id:
    --* Modifier keys.
    -- [@item] For keyboard and mouse button events, this is the modifier state
    -- -- one or more of [@ref RASTER.MOD] bitwise-ORed together.
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
--*{{{  RASTER.KEY Key symbols.

-- These are deliberately chosen to correspond to SDL's SDLK constants, with
-- printable characters removed.

VAL INT RASTER.KEY.BACKSPACE IS 8: --* Backspace.
VAL INT RASTER.KEY.TAB IS 9: --* Tab.
VAL INT RASTER.KEY.CLEAR IS 12: --* Clear.
VAL INT RASTER.KEY.RETURN IS 13: --* Return.
VAL INT RASTER.KEY.PAUSE IS 19: --* Pause.
VAL INT RASTER.KEY.ESCAPE IS 27: --* Escape.
VAL INT RASTER.KEY.DELETE IS 127: --* Delete.
VAL INT RASTER.KEY.KP0 IS 256: --* Keypad 0.
VAL INT RASTER.KEY.KP1 IS 257: --* Keypad 1.
VAL INT RASTER.KEY.KP2 IS 258: --* Keypad 2.
VAL INT RASTER.KEY.KP3 IS 259: --* Keypad 3.
VAL INT RASTER.KEY.KP4 IS 260: --* Keypad 4.
VAL INT RASTER.KEY.KP5 IS 261: --* Keypad 5.
VAL INT RASTER.KEY.KP6 IS 262: --* Keypad 6.
VAL INT RASTER.KEY.KP7 IS 263: --* Keypad 7.
VAL INT RASTER.KEY.KP8 IS 264: --* Keypad 8.
VAL INT RASTER.KEY.KP9 IS 265: --* Keypad 9.
VAL INT RASTER.KEY.KP.PERIOD IS 266: --* Keypad [@text .].
VAL INT RASTER.KEY.KP.DIVIDE IS 267: --* Keypad [@text /].
VAL INT RASTER.KEY.KP.MULTIPLY IS 268: --* Keypad [@text *].
VAL INT RASTER.KEY.KP.MINUS IS 269: --* Keypad [@text -].
VAL INT RASTER.KEY.KP.PLUS IS 270: --* Keypad [@text +].
VAL INT RASTER.KEY.KP.ENTER IS 271: --* Keypad Enter.
VAL INT RASTER.KEY.KP.EQUALS IS 272: --* Keypad [@text =].
VAL INT RASTER.KEY.UP IS 273: --* Up arrow.
VAL INT RASTER.KEY.DOWN IS 274: --* Down arrow.
VAL INT RASTER.KEY.RIGHT IS 275: --* Right arrow.
VAL INT RASTER.KEY.LEFT IS 276: --* Left arrow.
VAL INT RASTER.KEY.INSERT IS 277: --* Insert.
VAL INT RASTER.KEY.HOME IS 278: --* Home.
VAL INT RASTER.KEY.END IS 279: --* End.
VAL INT RASTER.KEY.PAGEUP IS 280: --* Page up.
VAL INT RASTER.KEY.PAGEDOWN IS 281: --* Page down.
VAL INT RASTER.KEY.F1 IS 282: --* F1.
VAL INT RASTER.KEY.F2 IS 283: --* F2.
VAL INT RASTER.KEY.F3 IS 284: --* F3.
VAL INT RASTER.KEY.F4 IS 285: --* F4.
VAL INT RASTER.KEY.F5 IS 286: --* F5.
VAL INT RASTER.KEY.F6 IS 287: --* F6.
VAL INT RASTER.KEY.F7 IS 288: --* F7.
VAL INT RASTER.KEY.F8 IS 289: --* F8.
VAL INT RASTER.KEY.F9 IS 290: --* F9.
VAL INT RASTER.KEY.F10 IS 291: --* F10.
VAL INT RASTER.KEY.F11 IS 292: --* F11.
VAL INT RASTER.KEY.F12 IS 293: --* F12.
VAL INT RASTER.KEY.F13 IS 294: --* F13.
VAL INT RASTER.KEY.F14 IS 295: --* F14.
VAL INT RASTER.KEY.F15 IS 296: --* F15.
VAL INT RASTER.KEY.NUMLOCK IS 300: --* Num lock.
VAL INT RASTER.KEY.CAPSLOCK IS 301: --* Caps lock.
VAL INT RASTER.KEY.SCROLLOCK IS 302: --* Scroll lock.
VAL INT RASTER.KEY.RSHIFT IS 303: --* Right shift.
VAL INT RASTER.KEY.LSHIFT IS 304: --* Left shift.
VAL INT RASTER.KEY.RCTRL IS 305: --* Right control.
VAL INT RASTER.KEY.LCTRL IS 306: --* Left control.
VAL INT RASTER.KEY.RALT IS 307: --* Right alt.
VAL INT RASTER.KEY.LALT IS 308: --* Left alt.
VAL INT RASTER.KEY.RMETA IS 309: --* Right meta.
VAL INT RASTER.KEY.LMETA IS 310: --* Left meta.
VAL INT RASTER.KEY.LSUPER IS 311: --* Left super.
VAL INT RASTER.KEY.RSUPER IS 312: --* Right super.
VAL INT RASTER.KEY.MODE IS 313: --* Mode.
VAL INT RASTER.KEY.COMPOSE IS 314: --* Compose.
VAL INT RASTER.KEY.HELP IS 315: --* Help.
VAL INT RASTER.KEY.PRINT IS 316: --* Print.
VAL INT RASTER.KEY.SYSREQ IS 317: --* SysReq.
VAL INT RASTER.KEY.BREAK IS 318: --* Break.
VAL INT RASTER.KEY.MENU IS 319: --* Menu.
VAL INT RASTER.KEY.POWER IS 320: --* Power.
VAL INT RASTER.KEY.EURO IS 321: --* Euro
VAL INT RASTER.KEY.UNDO IS 322: --* Undo.
--*}}}
--*{{{  RASTER.BUTTON Mouse buttons.

VAL INT RASTER.BUTTON.LEFT IS 1: --* Left button.
VAL INT RASTER.BUTTON.MIDDLE IS 2: --* Middle button.
VAL INT RASTER.BUTTON.RIGHT IS 3: --* Right button.
--*}}}
--*{{{  RASTER.MOD Modifier keys.

-- These are deliberately chosen to correspond to SDL's KMOD constants.

VAL INT RASTER.MOD.LSHIFT IS 1: --* Left shift.
VAL INT RASTER.MOD.RSHIFT IS 2: --* Right shift.
VAL INT RASTER.MOD.LCTRL IS 64: --* Left control.
VAL INT RASTER.MOD.RCTRL IS 128: --* Right control.
VAL INT RASTER.MOD.LALT IS 256: --* Left alt.
VAL INT RASTER.MOD.RALT IS 512: --* Right alt.
VAL INT RASTER.MOD.LMETA IS 1024: --* Left meta.
VAL INT RASTER.MOD.RMETA IS 2048: --* Right meta.
VAL INT RASTER.MOD.NUM IS 4096: --* Num lock.
VAL INT RASTER.MOD.CAPS IS 8192: --* Caps lock.
VAL INT RASTER.MOD.MODE IS 16384: --* Mode lock.
--*}}}
