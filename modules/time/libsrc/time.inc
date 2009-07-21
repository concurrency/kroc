--
--	Definitions for the time module
--	Copyright (C) 2009 Adam Sampson <ats@offog.org>
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

--** Time utilities.
--
-- occam [@code TIMER]s measure a [@code INT] count of timer ticks that wraps
-- around periodically. The length of a timer tick varies between platforms; on
-- 32-bit platforms it's typically one microsecond (one million ticks per
-- second), and on 16-bit platforms typically one millisecond (one thousand
-- ticks per second).
--
-- Using this module, you can write programs that are largely independent of
-- the timer resolution.
--
-- @module time

--{{{  FUNCTION seconds
--* Convert seconds to timer ticks.
-- @param s Time in seconds
-- @return Time in timer ticks
INT INLINE FUNCTION seconds (VAL INT s)
  INT ticks:
  VALOF
    #IF TARGET.BITS.PER.WORD = 16
    ticks := s * 1000
    #ELSE
    ticks := s * 1000000
    #ENDIF
    RESULT ticks
:
--}}}
--{{{  FUNCTION milliseconds
--* Convert milliseconds to timer ticks.
-- @param ms Time in milliseconds
-- @return Time in timer ticks
INT INLINE FUNCTION milliseconds (VAL INT ms)
  INT ticks:
  VALOF
    #IF TARGET.BITS.PER.WORD = 16
    ticks := ms
    #ELSE
    ticks := ms * 1000
    #ENDIF
    RESULT ticks
:
--}}}
--{{{  FUNCTION microseconds
--* Convert microseconds to timer ticks.
-- @param us Time in microseconds
-- @return Time in timer ticks
INT INLINE FUNCTION microseconds (VAL INT us)
  INT ticks:
  VALOF
    #IF TARGET.BITS.PER.WORD = 16
    ticks := us / 1000
    #ELSE
    ticks := us
    #ENDIF
    RESULT ticks
:
--}}}
