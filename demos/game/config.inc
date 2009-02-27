--
--	config.inc -- configuration for the new game
--	Copyright (C) 2004-2006 Fred Barnes <frmb@ukc.ac.uk>
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
--	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
--

-- debugging..
#UNDEF DEBUG
#IF DEFINED (DEBUG)
#INCLUDE "useful.module"
#ENDIF

#DEFINE VERSION "0.3"

#DEFINE MAX.ROOMS 1020
#DEFINE MAX.USERS 512
#DEFINE MAX.USERS.PER.ROOM 32
#DEFINE MAX.OBJECTS.PER.ROOM 32

#DEFINE DYN.OBJ.CREATE


-- path to game data
#DEFINE OPER.NAME "operator"

#DEFINE USE.TELNET.IF

#IF DEFINED (USE.TELNET.IF)
  #RELAX
  #DEFINE LISTENHOST "0.0.0.0"
  #DEFINE LISTENPORT 26667
#ENDIF

-- #DEFINE USE.IRC.IF

#IF DEFINED (USE.IRC.IF)
  #RELAX
  #DEFINE IRCHOST "irc.pond.net"
  #DEFINE IRCPORT 6667
  #DEFINE IRCUSERNAME "occam"
  #DEFINE IRCHOSTNAME "tadpole.pond.net"
  #DEFINE IRCREALNAME "occam adventure"
  #DEFINE IRCNICKNAME "occgam"
  #DEFINE IRCCHANNAME "#local"
#ENDIF

#DEFINE DIEMESSAGE "DIEDIEDIE"

