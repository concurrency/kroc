--
--	Definitions for Trap
--	Copyright (C) 2008, 2009 Adam Sampson <ats@offog.org>
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

--** An asynchronous network messaging system for occam-pi.
--
-- The Trap nameserver [@text trapns] must be running in order for Trap
-- programs to find each other. By default, it will assume the nameserver is
-- running on [@text localhost]; to specify where it really is, create a file
-- called [@text .trap.ns] in the current directory containing the hostname.
--
-- @module trap

--{{{  constants
VAL INT TRAP.NS.PORT IS 25098:
--}}}

--{{{  DATA TYPE TRAP.NS.PACKET
DATA TYPE TRAP.NS.PACKET
  PACKED RECORD
    BYTE cmd, dummy1, dummy2, dummy3:
    INT arg0, arg1:
:
--}}}

--{{{  CHAN TYPE TRAP.CHANNEL.CT
--* Interface for a listening Trap port.
CHAN TYPE TRAP.CHANNEL.CT
  MOBILE RECORD
    CHAN MOBILE []BYTE msg?: --* Messages received from the port.
:
--}}}

--{{{  CHAN TYPE TRAP.CT
PROTOCOL TRAP.REQ
  CASE
    open; INT                     --* Replies [@ref opened].
    send; INT; INT; MOBILE []BYTE
:

PROTOCOL TRAP.RESP
  CASE
    opened; TRAP.CHANNEL.CT?
:

--* Handle to the Trap server.
CHAN TYPE TRAP.CT
  MOBILE RECORD
    CHAN TRAP.REQ req?:
    CHAN TRAP.RESP resp!:
:
--}}}

