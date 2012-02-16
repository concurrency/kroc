--
--	Shutdown servers definitions
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

--** Shutdown servers.
--
-- Shutdown servers distribute shutdown messages to other processes that have
-- registered with them. They provide a clean way of shutting down a big
-- network of processes -- for example, you could use a shutdown server to
-- ensure that all the processes in a game exit at the end of a level.
--
-- @module control.shutdown

#IF NOT DEFINED (SHUTDOWN.INC)
#DEFINE SHUTDOWN.INC

--* A connection to a shutdown server.
CHAN TYPE SHUTDOWN
  MOBILE RECORD
    --* Disconnect from the server.
    -- Don't use this directly; call [@ref shutdown.finish] instead.
    CHAN BOOL disconnect?:
    --* Kill notifications.
    -- A connected client will receive at most one message on this channel.
    CHAN BOOL kill!:
:

--* Commands to the shutdown server.
PROTOCOL SHUTDOWN.REQ
  CASE
    --* Connect a new client.
    -- Returns [@code connect].
    connect
    --* Send a kill notification to all the connected clients.
    kill
    --* Tell the server to exit once it's dealt with all the pending
    -- notifications.
    quit
:

--* Responses from the shutdown server.
PROTOCOL SHUTDOWN.RESP
  CASE
    --* Client end of a new connection.
    connect; SHUTDOWN!
:

--* Shared connection to the shutdown server.
CHAN TYPE SHUTDOWN.CTL
  MOBILE RECORD
    --* Commands to the server.
    CHAN SHUTDOWN.REQ req?:
    --* Responses from the server.
    CHAN SHUTDOWN.RESP resp!:
:

#ENDIF

