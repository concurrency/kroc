--
--	Definitions for the IO scheduler
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

--** IO selector.
--
-- An IO selector is a server that waits in an efficient way for events on a
-- set of file descriptors; it provides a process-oriented interface to the
-- [@text poll(2)] system call (or an equivalent).
--
-- To start an IO selector, call [@ref selector.start]; see [@ref SELECTOR.CT]
-- for the interface.
--
-- @module selector

--*{{{  SELECT Events to select upon.
VAL INT SELECT.READ   IS 1:         --* A read will not block.
VAL INT SELECT.WRITE  IS 2:         --* A write will not block.
--*}}}
--{{{  CHAN TYPE SELECTOR.CT
--* Requests from an IO selector to a waiting process.
PROTOCOL SELECTOR.SIGNAL.REQ
  CASE
    --* The FD has become ready.
    ready = 0
    --* The selector is shutting down.
    shutdown = 1
:

--* Responses from a waiting process to an IO selector.
PROTOCOL SELECTOR.SIGNAL.RESP
  CASE
    --* Wait for the FD again.
    again = 0
    --* Remove the FD from the waiting set.
    remove = 1
:

--* Channel bundle used for the selector to signal back to the client.
--
-- This may look dodgy from a C-S perspective, but it should be OK: at any
-- given time, the client-server relationship is only going one way...
CHAN TYPE SELECTOR.SIGNAL
  MOBILE RECORD
    CHAN SELECTOR.SIGNAL.REQ signal?:
    CHAN SELECTOR.SIGNAL.RESP ack!:
:

--* Requests to an IO selector.
PROTOCOL SELECTOR.REQ
  CASE
    --* Add a file descriptor to the set being waited upon.
    -- @item fd File descriptor (usually non-blocking)
    -- @item events Bitwise OR of events to wait for; see [@ref SELECT]
    -- @item signal Channel bundle to signal over when the event fires
    add = 0; INT; INT; SELECTOR.SIGNAL!
    --* Shut down the selector.
    shutdown = 1
:

--* Interface to an IO selector.
CHAN TYPE SELECTOR.CT
  MOBILE RECORD
    CHAN SELECTOR.REQ req?:
:
--}}}
