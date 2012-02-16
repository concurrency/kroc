--
--	Generic N-place overwriting buffer
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

--** Generic N-place overwriting buffer.
-- To use this in your code, you'll need to provide a definition of the data
-- type [@code ITEM] and then [@code #INCLUDE "overwriting-buffer.inc"]. You
-- can use channel retyping to connect it to existing channels.
--
-- @module overwriting-buffer

--* The buffer process.
INLINE PROC overwriting.buffer (CHAN ITEM in?, out!, CHAN BOOL kill?, VAL INT size)
  CHAN BOOL gimme:
  PROTOCOL ITEMS
    CASE
      item; ITEM
      done
  :
  CHAN ITEMS gotta:
  PAR
    --{{{ buffer items
    INITIAL BOOL running IS TRUE:
    INITIAL INT used IS 0:
    INITIAL MOBILE []ITEM buf IS MOBILE [size]ITEM:
    WHILE running
      BOOL b:
      ALT
        in ? buf[used]
          IF
            used = (size - 1)
              --{{{ buffer full; drop the oldest item
              SEQ i = 0 FOR used - 1
                buf[i] := buf[i + 1]
              --}}}
            TRUE
              used := used + 1
        (used > 0) & gimme ? b
          SEQ
            --{{{ output the oldest item
            gotta ! item; buf[0]
            SEQ i = 0 FOR used - 1
              buf[i] := buf[i + 1]
            used := used - 1
            --}}}
        kill ? b
          SEQ
            --{{{ done
            running := FALSE
            gimme ? b
            gotta ! done
            --}}}
    --}}}
    --{{{ request items (roll on output guard support)
    INITIAL BOOL running IS TRUE:
    WHILE running
      SEQ
        gimme ! TRUE
        gotta ? CASE
          ITEM i:
          item; i
            out ! i
          done
            running := FALSE
    --}}}
:

