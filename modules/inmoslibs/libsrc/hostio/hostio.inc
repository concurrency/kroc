--
--	Inmos Libraries
--	Copyright (C) 1988, 1989, 1990, 1991, 1993  Inmos Limited
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

-- hostio.inc
-- Copyright 1989, 1992 INMOS Limited
--
--{{{  History
-- updated for iserver v1.42, apart from buffer size 5-Jun-90 SRH
-- updated for iserver v1.50, apart from buffer size 5-Feb-92 SRH
--}}}
--
--{{{  SP protocol
PROTOCOL SP IS INT16::[]BYTE :
--}}}
--{{{  Command tags
-- values up to 127 are reserved for use by INMOS
--{{{  File command tags
VAL sp.open.tag     IS 10(BYTE) :
VAL sp.close.tag    IS 11(BYTE) :
VAL sp.read.tag     IS 12(BYTE) :
VAL sp.write.tag    IS 13(BYTE) :
VAL sp.gets.tag     IS 14(BYTE) :
VAL sp.puts.tag     IS 15(BYTE) :
VAL sp.flush.tag    IS 16(BYTE) :
VAL sp.seek.tag     IS 17(BYTE) :
VAL sp.tell.tag     IS 18(BYTE) :
VAL sp.eof.tag      IS 19(BYTE) :
VAL sp.ferror.tag   IS 20(BYTE) :
VAL sp.remove.tag   IS 21(BYTE) :
VAL sp.rename.tag   IS 22(BYTE) :
VAL sp.getblock.tag IS 23(BYTE) :
VAL sp.putblock.tag IS 24(BYTE) :
VAL sp.isatty.tag   IS 25(BYTE) :
VAL sp.openrec.tag  IS 26(BYTE) :
VAL sp.getrec.tag   IS 27(BYTE) :
VAL sp.putrec.tag   IS 28(BYTE) :
VAL sp.puteof.tag   IS 29(BYTE) :
--}}}
--{{{  Host command tags
VAL sp.getkey.tag  IS 30(BYTE) :
VAL sp.pollkey.tag IS 31(BYTE) :
VAL sp.getenv.tag  IS 32(BYTE) :
VAL sp.time.tag    IS 33(BYTE) :
VAL sp.system.tag  IS 34(BYTE) :
VAL sp.exit.tag    IS 35(BYTE) :
--}}}
--{{{  Server command tags
VAL sp.commandline.tag IS 40(BYTE) :
VAL sp.core.tag        IS 41(BYTE) :
VAL sp.version.tag     IS 42(BYTE) :
VAL sp.getinfo.tag     IS 43(BYTE) :
--}}}
--{{{  OS specific command tags
-- These OS specific tags will be followed by another tag indicating
-- which OS specific function is required
VAL sp.DOS.tag    IS 50(BYTE) :
VAL sp.HELIOS.tag IS 51(BYTE) :
VAL sp.VMS.tag    IS 52(BYTE) :
VAL sp.SUNOS.tag  IS 53(BYTE) :
--}}}
VAL sp.fileexists.tag  IS 80(BYTE) :
VAL sp.translate.tag   IS 81(BYTE) :
VAL sp.ferrstat.tag    IS 82(BYTE) :
VAL sp.commandargs.tag IS 83(BYTE) :
--}}}
--{{{  Packet and buffer Sizes
VAL sp.max.packet.size IS 512 : -- bytes transferred, includes length & data
VAL sp.min.packet.size IS   8 : -- bytes transferred, includes length & data

VAL sp.max.packet.data.size IS sp.max.packet.size - 2 : -- INT16 length
VAL sp.min.packet.data.size IS sp.min.packet.size - 2 : -- INT16 length

--{{{  individual command maxima
VAL sp.max.openname.size      IS sp.max.packet.data.size - 5 :
VAL sp.max.readbuffer.size    IS sp.max.packet.data.size - 3 :
-- ditto for gets
VAL sp.max.writebuffer.size   IS sp.max.packet.data.size - 7 :
-- ditto for puts
VAL sp.max.removename.size    IS sp.max.packet.data.size - 3 :
VAL sp.max.renamename.size    IS sp.max.packet.data.size - 5 :
VAL sp.max.getenvname.size    IS sp.max.packet.data.size - 3 :
VAL sp.max.systemcommand.size IS sp.max.packet.data.size - 3 :
VAL sp.max.corerequest.size   IS sp.max.packet.data.size - 3 :

VAL sp.max.buffer.size IS sp.max.writebuffer.size : -- smaller of read & write
--}}}
--}}}
--{{{  Result values         (spr.)
VAL spr.ok                  IS   0(BYTE) : -- success

VAL spr.not.implemented     IS   1(BYTE) : -- command not implemented
VAL spr.bad.name            IS   2(BYTE) : -- filename is null
VAL spr.bad.type            IS   3(BYTE) : -- open file type is incorrect
VAL spr.bad.mode            IS   4(BYTE) : -- open file mode is incorrect
VAL spr.invalid.streamid    IS   5(BYTE) : -- never opened that streamid
VAL spr.bad.stream.use      IS   6(BYTE) : -- reading an output file, or vice versa
VAL spr.buffer.overflow     IS   7(BYTE) : -- buffer too small for required data
VAL spr.bad.packet.size     IS   8(BYTE) : -- data too big or small for packet
VAL spr.bad.origin          IS   9(BYTE) : -- seek origin is incorrect
VAL spr.full.name.too.short IS  10(BYTE) : -- a truncation of a filename would be
                                           -- required
VAL spr.notok               IS 127(BYTE) : -- a general fail result

-- anything 128 or above is a server dependent 'failure' result
VAL spr.operation.failed    IS 128(BYTE) : -- general failure
VAL spr.failed.operation    IS 129(BYTE) : -- identical in meaning to
                                           -- spr.operation.failed due
                                           -- to historical accident
VAL spr.no.privilege        IS 131(BYTE) : -- insufficient privilege
VAL spr.no.resource         IS 132(BYTE) : -- insufficient system resources available
VAL spr.no.file             IS 133(BYTE) : -- file not found
VAL spr.truncated           IS 134(BYTE) : -- data truncated
VAL spr.bad.streamid        IS 135(BYTE) : -- a bad stream id was specified
VAL spr.position.lost       IS 136(BYTE) : -- stream position has been lost
VAL spr.not.available       IS 137(BYTE) : -- the requested configuration cannot be $
VAL spr.eof                 IS 138(BYTE) : -- an end of file mark has been encounter$

VAL spr.bad.parameters      IS 141(BYTE) : -- invalid or inconsistent parameters

--}}}
--{{{  Predefined streams    (spid.)
VAL spid.stdin  IS 0(INT32) :
VAL spid.stdout IS 1(INT32) :
VAL spid.stderr IS 2(INT32) :
--}}}
--{{{  Open types            (spt.)
VAL spt.binary IS 1(BYTE) :
VAL spt.text   IS 2(BYTE) :
--}}}
--{{{  Open modes            (spm.)
VAL spm.input           IS 1(BYTE) :
VAL spm.output          IS 2(BYTE) :
VAL spm.append          IS 3(BYTE) :
VAL spm.existing.update IS 4(BYTE) :
VAL spm.new.update      IS 5(BYTE) :
VAL spm.append.update   IS 6(BYTE) :
--}}}
--{{{  Status values         (sps.)
VAL sps.success IS  999999999(INT32) :
VAL sps.failure IS -999999999(INT32) :
--}}}
--{{{  Seek origins          (spo.)
VAL spo.start   IS 1(INT32) :
VAL spo.current IS 2(INT32) :
VAL spo.end     IS 3(INT32) :
--}}}
--{{{  Version information   (sph., spo., spb.)
--{{{  Host types            (sph.)
-- values up to 127 are reserved for use by INMOS
VAL sph.unknown    IS 0(BYTE) :
VAL sph.PC         IS 1(BYTE) :
VAL sph.NECPC      IS 2(BYTE) :
VAL sph.VAX        IS 3(BYTE) :
VAL sph.SUN3       IS 4(BYTE) :
VAL sph.S370       IS 5(BYTE) :
VAL sph.BOX.SUN4   IS 6(BYTE) :
VAL sph.BOX.SUN386 IS 7(BYTE) :
VAL sph.BOX.APOLLO IS 8(BYTE) :
VAL sph.BOX.ATARI  IS 9(BYTE) : -- ST and TT
--}}}
--{{{  OS types              (spo.)
-- values up to 127 are reserved for use by INMOS
VAL spo.unknown IS 0(BYTE) :
VAL spo.DOS     IS 1(BYTE) :
VAL spo.HELIOS  IS 2(BYTE) :
VAL spo.VMS     IS 3(BYTE) :
VAL spo.SUNOS   IS 4(BYTE) :
VAL spo.CMS     IS 5(BYTE) :
VAL spo.TOS     IS 6(BYTE) :
--}}}
--{{{  Interface Board types (spb.)
-- This determines the interface between the link and the host
-- values up to 127 are reserved for use by INMOS
VAL spb.unknown IS 0(BYTE) :
VAL spb.B004    IS 1(BYTE) :
VAL spb.B008    IS 2(BYTE) :
VAL spb.B010    IS 3(BYTE) :
VAL spb.B011    IS 4(BYTE) :
VAL spb.B014    IS 5(BYTE) :
VAL spb.DRX11   IS 6(BYTE) :
VAL spb.QT0     IS 7(BYTE) :
VAL spb.B015    IS 8(BYTE) :
VAL spb.IBMCAT  IS 9(BYTE) :
VAL spb.B016    IS 10(BYTE) :
VAL spb.UDPLINK IS 11(BYTE) :
VAL spb.TCPLINK IS 12(BYTE) :
VAL spb.ACSILA  IS 13(BYTE) :
--}}}
--}}}
--{{{  Command line
VAL sp.short.commandline IS BYTE 0 : -- remove  server's own arguments
VAL sp.whole.commandline IS BYTE 1 : -- include server's own arguments

-- values for so.parse.commandline indicate whether
-- an option requires a following parameter
VAL spopt.never  IS 0 :
VAL spopt.maybe  IS 1 :
VAL spopt.always IS 2 :
--}}}
--{{{  Time string and date lengths
VAL so.time.string.len IS 19 : -- enough for "HH:MM:SS DD/MM/YYYY"
VAL so.date.len        IS  6 : -- enough for DDMMYY (as integers)
--}}}
--{{{  Temp filename length
VAL so.temp.filename.length IS 6 : -- six chars will work on anything
--}}}
