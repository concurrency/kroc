-- Copyright: Fred Barnes, Mario Schweigler (C) 2005-2006
-- Institution: Computing Laboratory, University of Kent, Canterbury, UK
-- Description: pony internal CIF include file

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

--{{{  CIF.pony.int.get.tdesc.data.{uc,sc,us,ss} -- get type-descriptor data
#PRAGMA EXTERNAL "PROC CIF.pony.int.get.tdesc.data.uc (MOBILE.CHAN! cli, RESULT INT nchans, nsvrread, typehash) = 4096"
#PRAGMA EXTERNAL "PROC CIF.pony.int.get.tdesc.data.sc (SHARED MOBILE.CHAN! cli, RESULT INT nchans, nsvrread, typehash) = 4096"
#PRAGMA EXTERNAL "PROC CIF.pony.int.get.tdesc.data.us (MOBILE.CHAN? svr, RESULT INT nchans, nsvrread, typehash) = 4096"
#PRAGMA EXTERNAL "PROC CIF.pony.int.get.tdesc.data.ss (SHARED MOBILE.CHAN? svr, RESULT INT nchans, nsvrread, typehash) = 4096"
--}}}
--{{{  CIF.pony.int.clone.ctb.{uc,sc,us,ss} -- cast and inc ref-count
#PRAGMA EXTERNAL "PROC CIF.pony.int.clone.ctb.uc (RESULT MOBILE.CHAN! cli, VAL INT ctb.ptr) = 4096"
#PRAGMA EXTERNAL "PROC CIF.pony.int.clone.ctb.sc (RESULT SHARED MOBILE.CHAN! cli, VAL INT ctb.ptr) = 4096"
#PRAGMA EXTERNAL "PROC CIF.pony.int.clone.ctb.us (RESULT MOBILE.CHAN? svr, VAL INT ctb.ptr) = 4096"
#PRAGMA EXTERNAL "PROC CIF.pony.int.clone.ctb.ss (RESULT SHARED MOBILE.CHAN? svr, VAL INT ctb.ptr) = 4096"
--}}}
--{{{  CIF.pony.int.alloc.ctb.{uc,sc,us,ss} -- allocate networked channel-type
#PRAGMA EXTERNAL "PROC CIF.pony.int.alloc.ctb.uc (RESULT MOBILE.CHAN! cli, VAL INT nct.id, PONY.NETHOOKHANDLE! net.hook.handle, MOBILE []PONY.DECODEHANDLE! dec.handle.array, MOBILE []PONY.ENCODEHANDLE! enc.handle.array, RESULT INT ctb.ptr) = 4096"
#PRAGMA EXTERNAL "PROC CIF.pony.int.alloc.ctb.sc (RESULT SHARED MOBILE.CHAN! cli, VAL INT nct.id, PONY.NETHOOKHANDLE! net.hook.handle, MOBILE []PONY.DECODEHANDLE! dec.handle.array, MOBILE []PONY.ENCODEHANDLE! enc.handle.array, RESULT INT ctb.ptr) = 4096"
#PRAGMA EXTERNAL "PROC CIF.pony.int.alloc.ctb.us (RESULT MOBILE.CHAN? svr, VAL INT nct.id, PONY.NETHOOKHANDLE! net.hook.handle, MOBILE []PONY.DECODEHANDLE! dec.handle.array, MOBILE []PONY.ENCODEHANDLE! enc.handle.array, RESULT INT ctb.ptr) = 4096"
#PRAGMA EXTERNAL "PROC CIF.pony.int.alloc.ctb.ss (RESULT SHARED MOBILE.CHAN? svr, VAL INT nct.id, PONY.NETHOOKHANDLE! net.hook.handle, MOBILE []PONY.DECODEHANDLE! dec.handle.array, MOBILE []PONY.ENCODEHANDLE! enc.handle.array, RESULT INT ctb.ptr) = 4096"
--}}}
--{{{  other external C routines
#PRAGMA EXTERNAL "PROC CIF.pony.int.shutdown.ctbs (MOBILE []INT ctbs) = 4096"
#PRAGMA EXTERNAL "PROC CIF.pony.int.get.nct.id (VAL INT ctb.ptr, RESULT INT nct.id, result) = 4096"
--}}}

