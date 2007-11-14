--
--  Button
--  Copyright (C) 2007  University of Kent
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation, either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
--  You should have received a copy of the GNU Lesser General Public
--  License along with this library.  If not, see
--  <http://www.gnu.org/licenses/>.
--


--** A Button library that uses OpenGL and SDL to render a window of clickable/configurable buttons.
-- To use this library:
--
-- [@code
-- #INCLUDE "button.module"]
--
-- @maintainer Damian Dimmich <djd20@kent.ac.uk>
-- @module button

#IF NOT DEFINED (BUTTON.INC)
#DEFINE BUTTON.INC

--*  Sends a copy of the name of the button (when clicked).
PROTOCOL CLICK IS MOBILE []BYTE:
 
--* This is used for button setup. 
PROTOCOL BUTTONCFG
  CASE
    enable --* enable button
    disable --* disable button
    name; MOBILE []BYTE --* Change the name of the button
    colour; REAL32; REAL32; REAL32 --* set the button color by specifying Red, Blue and Green values
:

PROTOCOL INPUT
  CASE
    kyb; BYTE -- May want to add modifiers to this (ie, shift/alt/ctrl etc).
    mousedown; BYTE;INT16;INT16 --Which button, X, Y 
    mouseup; BYTE;INT16;INT16 --Which button, X, Y 
:

#ENDIF

