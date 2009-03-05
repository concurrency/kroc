--** Trap: an asynchronous network messaging system for occam-pi.
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
CHAN TYPE TRAP.CHANNEL.CT
  MOBILE RECORD
    CHAN MOBILE []BYTE msg?:
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

CHAN TYPE TRAP.CT
  MOBILE RECORD
    CHAN TRAP.REQ req?:
    CHAN TRAP.RESP resp!:
:
--}}}

