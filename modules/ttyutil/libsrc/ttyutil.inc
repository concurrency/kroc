--
--	ttyutil.inc -- constants, protocols and channel-types for tty-utilities
--	Copyright (C) 2009 Fred Barnes <frmb@kent.ac.uk>
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
--	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
--


--** A utility library for terminal-based input and output.
--
-- This library provides various utilities for use with VT220
-- compatible terminals.
--
-- @module ttyutil

#IF NOT DEFINED (TTYUTIL.INC)
#DEFINE TTYUTIL.INC

--{{{  constants

--*{ ANSI VT220/ANSI rendering attributes.
VAL BYTE ANSI.NORMAL IS 0:        --* Normal/reset.
VAL BYTE ANSI.BOLD IS 1:          --* Bold rendering.
VAL BYTE ANSI.INVERSE IS 2:       --* Inverse rendering.
VAL BYTE ANSI.BLINK IS 5:         --* Blinking.

VAL BYTE ANSI.FG.BLACK IS 30:     --* Black foreground.
VAL BYTE ANSI.FG.RED IS 31:       --* Red foreground.
VAL BYTE ANSI.FG.GREEN IS 32:     --* Green foreground.
VAL BYTE ANSI.FG.YELLOW IS 33:    --* Yellow foreground.
VAL BYTE ANSI.FG.BLUE IS 34:      --* Blue foreground.
VAL BYTE ANSI.FG.MAGENTA IS 35:   --* Magenta foreground.
VAL BYTE ANSI.FG.CYAN IS 36:      --* Cyan foreground.
VAL BYTE ANSI.FG.WHITE IS 37:     --* White foreground.

VAL BYTE ANSI.BG.BLACK IS 40:     --* Black background.
VAL BYTE ANSI.BG.RED IS 41:       --* Red background.
VAL BYTE ANSI.BG.GREEN IS 42:     --* Green background.
VAL BYTE ANSI.BG.YELLOW IS 43:    --* Yellow background.
VAL BYTE ANSI.BG.BLUE IS 44:      --* Blue background.
VAL BYTE ANSI.BG.MAGENTA IS 45:   --* Magenta background.
VAL BYTE ANSI.BG.CYAN IS 46:      --* Cyan background.
VAL BYTE ANSI.BG.WHITE IS 47:     --* White background.
--*}
--*{ KEY Key constants.  Values of 0-255 represent ASCII/locale-specific characters.

--* Arrow keys.
VAL INT KEY.UP IS #100:
VAL INT KEY.DOWN IS #101:
VAL INT KEY.LEFT IS #102:
VAL INT KEY.RIGHT IS #103:

--* Function keys.
VAL INT KEY.F1 IS #201:
VAL INT KEY.F2 IS #202:
VAL INT KEY.F3 IS #203:
VAL INT KEY.F4 IS #204:
VAL INT KEY.F5 IS #205:
VAL INT KEY.F6 IS #206:
VAL INT KEY.F7 IS #207:
VAL INT KEY.F8 IS #208:
VAL INT KEY.F9 IS #209:
VAL INT KEY.F10 IS #20A:
VAL INT KEY.F11 IS #20B:
VAL INT KEY.F12 IS #20C:
VAL INT KEY.F13 IS #20D:
VAL INT KEY.F14 IS #20E:
VAL INT KEY.F15 IS #20F:
VAL INT KEY.F16 IS #210:
VAL INT KEY.F17 IS #211:
VAL INT KEY.F18 IS #212:
VAL INT KEY.F19 IS #213:
VAL INT KEY.F20 IS #214:
VAL INT KEY.F21 IS #215:
VAL INT KEY.F22 IS #216:
VAL INT KEY.F23 IS #217:
VAL INT KEY.F24 IS #218:

--*}

--}}}
--{{{  protocols and channel-types

--* Basic display protocol.
PROTOCOL TTYUTIL.DPY
  CASE
    following; INT                            --* Number of outputs to follow (when dealing with an unshared
                                              -- screen channel, this controls flushing.  When shared, this
                                              -- controls the extent of the claim.

    clear.screen                              --* Clear the screen.
    int.xy; INT; INT; INT                     --* Integer at specific position (x, y, value).
    string.xy; INT; INT; MOBILE []BYTE        --* String at specific position (x, y, string).
    colours; MOBILE []BYTE                    --* Set ANSI/VT220 attributes (see [@ref ANSI]).

    quit                                      --* Quit the display process.
:

--* Menu item handler input protocol (menu -> handler).
PROTOCOL TTYUTIL.MENU.IN
  CASE
    get.text                                  --* Get item text.
    key.left                                  --* Key item left.
    key.right                                 --* Key item right.
    key.other; INT                            --* Other key (e.g. return).

    quit                                      --* Menu quit (signals handler to quit).
:

--* Menu item handler output protocol (handler -> menu).
PROTOCOL TTYUTIL.MENU.OUT
  CASE
    text; MOBILE []BYTE                       --* Text update for menu.
    remove                                    --* Remove this item (self).
:

--* Menu item channel type.
CHAN TYPE TTYUTIL.MENUITEM
  MOBILE RECORD
    CHAN TTYUTIL.MENU.IN in?:                 --* Menu item handler requests.
    CHAN TTYUTIL.MENU.OUT out!:               --* Menu item handler responses.
:

--* Menu control protocol.
PROTOCOL TTYUTIL.MENU
  CASE
    add.item; TTYUTIL.MENUITEM!               --* Add menu item.
    insert.item; INT; TTYUTIL.MENUITEM!       --* Add menu item at a specific position.
    del.item; INT                             --* Delete a particular item.
    quit                                      --* Shutdown menu.

    key.in; INT                               --* Incoming key press.
:

--}}}

#ENDIF


