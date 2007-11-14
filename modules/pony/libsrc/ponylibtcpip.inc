-- Copyright: Mario Schweigler (C) 2005-2006
-- Institution: Computing Laboratory, University of Kent, Canterbury, UK
-- Description: pony public TCP/IP include file

-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
-- MA 02110-1301, USA.

--{{{  Constants

--{{{  Results for startup (network-type-specific: starting with -11)
-- Illegal ANS-name (allowed: letters, digits, dash, dot, underscore
--   (or empty string))
VAL INT PONYC.RESULT.STARTUP.TCPIP.ILLEGALANSNAME IS -11:
-- Illegal application-name (not allowed: empty string)
VAL INT PONYC.RESULT.STARTUP.TCPIP.ILLEGALAPPNAME IS -12:
-- Illegal node-name (allowed: letters, digits, dash, dot, underscore
--   (or empty string))
VAL INT PONYC.RESULT.STARTUP.TCPIP.ILLEGALNODENAME IS -13:
-- Illegal node-type
VAL INT PONYC.RESULT.STARTUP.TCPIP.ILLEGALNODETYPE IS -14:
----------------------------------------------------------------------
-- Home directory not specified in environment
VAL INT PONYC.RESULT.STARTUP.TCPIP.HOMEDIRNOTINENV IS -15:
-- ANS-file not found or not readable
VAL INT PONYC.RESULT.STARTUP.TCPIP.ANSFILEACCESSERROR IS -16:
-- ANS-file contains invalid data
VAL INT PONYC.RESULT.STARTUP.TCPIP.ANSFILEINVALIDSETTINGS IS -17:
-- Node-file not readable
VAL INT PONYC.RESULT.STARTUP.TCPIP.NODEFILEACCESSERROR IS -18:
-- Node-file contains invalid data
VAL INT PONYC.RESULT.STARTUP.TCPIP.NODEFILEINVALIDSETTINGS IS -19:
-- IP address could not be resolved
VAL INT PONYC.RESULT.STARTUP.TCPIP.IPADDRNOTRESOLVED IS -20:
-- Network failure when creating listening socket
VAL INT PONYC.RESULT.STARTUP.TCPIP.LISTENSOCKFAILURE IS -21:
----------------------------------------------------------------------
-- Network failure when connecting to ANS
VAL INT PONYC.RESULT.STARTUP.TCPIP.ANSNETFAILURE IS -22:
-- Duplicate of master location
VAL INT PONYC.RESULT.STARTUP.TCPIP.DUPLICATEMASTER IS -23:
-- Duplicate of a slave's location
VAL INT PONYC.RESULT.STARTUP.TCPIP.DUPLICATESLAVE IS -24:
-- Master tries to log on to ANS, other master already there
VAL INT PONYC.RESULT.STARTUP.TCPIP.OTHERMASTER IS -25:
-- Slave tries to log on to ANS, no master there yet
VAL INT PONYC.RESULT.STARTUP.TCPIP.NOMASTERYET IS -26:
----------------------------------------------------------------------
-- Network failure when slave tries to connect to master
VAL INT PONYC.RESULT.STARTUP.TCPIP.MASTERNETFAILURE IS -27:
-- Slave tries to log on to master, master is currently shutting down
VAL INT PONYC.RESULT.STARTUP.TCPIP.MASTERSHUTTINGDOWN IS -28:
--}}}

--{{{  Error-codes
-- Accepting new connection failed
VAL INT PONYC.ERRCODE.TCPIP.ACCEPTFAILURE IS -1:
-- Connecting failed
VAL INT PONYC.ERRCODE.TCPIP.CONNECTFAILURE IS -2:
-- Setting no-delay option failed
VAL INT PONYC.ERRCODE.TCPIP.SETNODELAYFAILURE IS -3:
-- Read operation failed
VAL INT PONYC.ERRCODE.TCPIP.READFAILURE IS -4:
-- Write operation failed
VAL INT PONYC.ERRCODE.TCPIP.WRITEFAILURE IS -5:
--}}}

--}}}

