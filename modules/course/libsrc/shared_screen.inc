--
--      Shared screen definitions
--      Copyright (C) 1997  P.H. Welch
--      Modified 2005 Jonathan Whiting, Fred Barnes
--
--	This library is free software; you can redistribute it and/or
--	modify it under the terms of the GNU Lesser General Public
--	License as published by the Free Software Foundation; either
--	version 2 of the License, or (at your option) any later version.
--
--	This library is distributed in the hope that it will be useful,
--	but WITHOUT ANY WARRANTY; without even the implied warranty of
--	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--	Lesser General Public License for more details.
--
--	You should have received a copy of the GNU Lesser General Public
--	License along with this library; if not, write to the Free Software
--	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
--

--** @module shared_screen

--{{{  DATA TYPE COLOUR
--* A colour.
DATA TYPE COLOUR IS BYTE:
--*{{{  COLOURS Colour constants
VAL COLOUR default IS 255:        --* Default colour
VAL COLOUR black IS 0:            --* Black
VAL COLOUR red IS 1:              --* Red
VAL COLOUR green IS 2:            --* Green
VAL COLOUR yellow IS 3:           --* Yellow
VAL COLOUR blue IS 4:             --* Blue
VAL COLOUR magenta IS 5:          --* Magenta
VAL COLOUR cyan IS 6:             --* Cyan
VAL COLOUR white IS 7:            --* White
VAL COLOUR bold.black IS 8:       --* Bold black (grey)
VAL COLOUR bold.red IS 9:         --* Bold red
VAL COLOUR bold.green IS 10:      --* Bold green
VAL COLOUR bold.yellow IS 11:     --* Bold yellow
VAL COLOUR bold.blue IS 12:       --* Bold blue
VAL COLOUR bold.magenta IS 13:    --* Bold magenta
VAL COLOUR bold.cyan IS 14:       --* Bold cyan
VAL COLOUR bold.white IS 15:      --* Bold white
--*}}}
--}}}
--{{{  DATA TYPE COORDINATES
--* A pair of coordinates.
DATA TYPE COORDINATES
  RECORD
    BYTE X:                  --* X coordinate
    BYTE Y:                  --* Y coordinate
:
--}}}
--{{{  DATA TYPE COORDINATES.COLOUR
--* A pair of coordinates and a colour.
DATA TYPE COORDINATES.COLOUR
  RECORD
    BYTE X:                  --* X coordinate
    BYTE Y:                  --* Y coordinate
    COLOUR col:              --* Colour
:
--}}}
--{{{  DATA TYPE SCREEN.NODE
--*{{{  SCREEN.COMMAND Command tags for [@ref SCREEN.NODE].
VAL BYTE coordinates.tag IS 0:
VAL BYTE colour.tag IS 1:
VAL BYTE byte.tag IS 2:
VAL BYTE character.tag IS 3:
VAL BYTE left.tag IS 4:
VAL BYTE right.tag IS 5:
VAL BYTE up.tag IS 6:
VAL BYTE down.tag IS 7:
VAL BYTE draw.left.tag IS 8:
VAL BYTE draw.right.tag IS 9:
VAL BYTE draw.up.tag IS 10:
VAL BYTE draw.down.tag IS 11:
VAL BYTE erase.eol.tag IS 12:
VAL BYTE erase.bol.tag IS 13:
VAL BYTE erase.line.tag IS 14:
VAL BYTE erase.eos.tag IS 15:
VAL BYTE erase.bos.tag IS 16:
VAL BYTE erase.screen.tag IS 17:
VAL BYTE cursor.visible.tag IS 18:
VAL BYTE cursor.invisible.tag IS 19:
VAL BYTE flush.tag IS 20:
--*}}}

--* A batch instruction to the shared screen.
-- This is really a union type; the argument tags have different meanings
-- depending on the command.
DATA TYPE SCREEN.NODE
  RECORD
    --* The command to execute.
    -- See [@ref SCREEN.COMMAND].
    BYTE tag:
    --* First argument
    BYTE a:
    --* Second argument
    BYTE b:
:
--}}}

--{{{  PROTOCOL SCREEN.CONTROL
-- FIXME OccamDoc here is rather incomplete
--* Control protocol for [@ref screen.driver].
PROTOCOL SCREEN.CONTROL
  CASE
    --{{{  atomic
    coordinates; COORDINATES --* Set coordinates
    colour; COLOUR           --* Set colour
    string; BYTE::[]BYTE     --* Draw a string
    integer; INT             --* Draw an integer
    byte; BYTE               --* Draw a byte
    character; BYTE          --* Draw a character
    left; BYTE               --* Move left
    right; BYTE              --* Move right
    up; BYTE                 --* Move up
    down; BYTE               --* Move down
    draw.left; BYTE; BYTE    --* Move left drawing character
    draw.right; BYTE; BYTE   --* Move right drawing character
    draw.up; BYTE; BYTE      --* Move up drawing character
    draw.down; BYTE; BYTE    --* Move down drawing character
    erase.eol                --* Erase to end of line
    erase.bol                --* Erase to beginning of line
    erase.line               --* Erase whole line
    erase.eos                --* Erase to end of screen
    erase.bos                --* Erase to beginning of screen
    flush                    --* Force a screen update
    --}}}
    --{{{  structured
    pcs; COORDINATES.COLOUR; BYTE::[]BYTE
    pci; COORDINATES.COLOUR; INT
    pcb; COORDINATES.COLOUR; BYTE
    pcsi; COORDINATES.COLOUR; BYTE::[]BYTE; INT
    pcis; COORDINATES.COLOUR; INT; BYTE::[]BYTE
    pcsb; COORDINATES.COLOUR; BYTE::[]BYTE; BYTE
    pcbs; COORDINATES.COLOUR; BYTE; BYTE::[]BYTE
    pc.draw.left; COORDINATES.COLOUR; BYTE; BYTE
    pc.draw.right; COORDINATES.COLOUR; BYTE; BYTE
    pc.draw.up; COORDINATES.COLOUR; BYTE; BYTE
    pc.draw.down; COORDINATES.COLOUR; BYTE; BYTE
    c.erase.eol; COORDINATES      --* Move then erase to end of line
    c.erase.bol; COORDINATES      --* Move then erase to beginning of line
    c.erase.line; COORDINATES     --* Move then erase whole line
    c.erase.eos; COORDINATES      --* Move then erase to end of screen
    c.erase.bos; COORDINATES      --* Move then erase to beginning of screen
    erase.screen             --* Clear the screen
    cursor.visible           --* Make the cursor visible
    cursor.invisible         --* Make the cursor invisible
    --* Interpret a list of commands.
    -- See [@ref SCREEN.NODE].
    interpret; BYTE::[]SCREEN.NODE
    poison                   --* Exit
    --}}}
:
--}}}

