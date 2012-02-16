--
--	Constants
--	Copyright (C) 1984  P.H. Welch
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

--** Course library.
--
-- The course library provides various simple utilities for occam programmers.
--
-- Strings (as handled by this library) are [@code BYTE] arrays left-justified
-- with trailing [@ref NULL]s. It is usually preferably to use mobile byte
-- arrays ([@code MOBILE []BYTE]) to represent strings in modern occam-pi
-- programs; the string features in this library exist for backwards
-- compatibility.
--
-- To use this library:
--
-- [@code
-- #INCLUDE "course.module"]
--
-- @module course

--*{{{  ASCII Character values.
VAL BYTE NULL IS 0:          --* ASCII NUL
VAL BYTE BELL IS 7:          --* ASCII BEL - terminal bell
VAL BYTE BACK IS 8:          --* ASCII BS - backspace key
VAL BYTE ESCAPE IS 27:       --* ASCII ESC - escape key
VAL BYTE DELETE IS 127:      --* ASCII DEL - delete key
--*}}}

--{{{  special constants
VAL BYTE FLUSH IS 255:       --* Flush output buffer
VAL BYTE END.OF.FILE IS 255: --* End of file
--}}}

--*{{{  COMPARE.STRING Return values from [@ref compare.string].
VAL INT string.less IS -1:   --* Left less than right
VAL INT string.same IS 0:    --* Left and right the same
VAL INT string.more IS 1:    --* Left more than right
--*}}}

