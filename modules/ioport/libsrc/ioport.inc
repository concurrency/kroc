--
--	Definitions for IOPort module
--	Copyright (C) 2008  Carl Ritson <cgr@kent.ac.uk>
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

-- @module ioport

VAL INT PARITY.NONE	IS 0:
VAL INT PARITY.ODD	IS 1:
VAL INT PARITY.EVEN	IS 2:

VAL INT FLOW.NONE	IS 0:
VAL INT FLOW.HARD	IS 1:
VAL INT FLOW.SOFT	IS 2:
