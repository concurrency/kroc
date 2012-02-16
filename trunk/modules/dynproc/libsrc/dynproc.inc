--
--	dynproc.inc - dynamic processes occam include file
--	Copyright (C) 2000, 2001 Fred Barnes <frmb2@ukc.ac.uk>
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

DATA TYPE DPROCESS IS INT:
VAL DPROCESS NOTPROCESS.D IS 0:
VAL DPROCESS.FINISHED IS 0:
VAL DPROCESS.SUSPENDED IS 1:
VAL DPROCESS.RESUMED IS 2:
VAL DPROCESS.FAULTED IS 3:
VAL DPROCESS.ACTIVE IS (-1):

#PRAGMA EXTERNAL "PROC C.ccsp.openlib (VAL []BYTE filename, INT handle) = 0"
#PRAGMA EXTERNAL "PROC C.ccsp.closelib (VAL INT handle) = 0"
#PRAGMA EXTERNAL "PROC C.ccsp.loadproc (VAL INT handle, VAL []BYTE procname, DPROCESS p) = 0"
#PRAGMA EXTERNAL "PROC C.ccsp.freeproc (DPROCESS p) = 0"
#PRAGMA EXTERNAL "PROC C.ccsp.suspendproc (VAL DPROCESS p, INT result) = 0"
#PRAGMA EXTERNAL "PROC C.ccsp.sizeproc (DPROCESS p, INT bytes) = 0"
#PRAGMA EXTERNAL "PROC C.ccsp.libhandleof (VAL DPROCESS p, INT result) = 0"
#PRAGMA EXTERNAL "PROC C.ccsp.writeproc (DPROCESS p, VAL []BYTE filename, INT result) = 0"
#PRAGMA EXTERNAL "PROC C.ccsp.readproc (DPROCESS p, VAL []BYTE filename, INT result) = 0"
#PRAGMA EXTERNAL "PROC KR.run (DPROCESS p, []CHAN OF ANY in, out, INT result) = 4"

--{{{  ccsp.openlib
INLINE PROC ccsp.openlib (VAL []BYTE filename, INT handle)
  C.ccsp.openlib (filename, handle)
:
--}}}  
--{{{  ccsp.closelib
INLINE PROC ccsp.closelib (VAL INT handle)
  C.ccsp.closelib (handle)
:
--}}}  
--{{{  ccsp.loadproc
INLINE PROC ccsp.loadproc (VAL INT handle, VAL []BYTE procname, DPROCESS p)
  C.ccsp.loadproc (handle, procname, p)
:
--}}}  
--{{{  ccsp.freeproc
INLINE PROC ccsp.freeproc (DPROCESS p)
  C.ccsp.freeproc (p)
:
--}}}  
--{{{  ccsp.runproc
INLINE PROC ccsp.runproc (DPROCESS p, []CHAN OF ANY in, out, INT result)
  KR.run (p, in, out, result)
:
--}}}  
--{{{  ccsp.run
INLINE PROC ccsp.run (VAL []BYTE procname, VAL INT libhandle, []CHAN OF ANY in, out, INT result)
  DPROCESS p:
  SEQ
    ccsp.loadproc (libhandle, procname, p)
    IF
      p = NOTPROCESS.D
        result := (-1)
      TRUE
        SEQ
          ccsp.runproc (p, in, out, result)
          ccsp.freeproc (p)
:
--}}}  
--{{{  ccsp.suspendproc
PROC ccsp.suspendproc (VAL DPROCESS p, INT result)
  C.ccsp.suspendproc (p, result)
:
--}}}  
--{{{  ccsp.sizeproc
INLINE PROC ccsp.sizeproc (DPROCESS p, INT bytes)
  C.ccsp.sizeproc (p, bytes)
:
--}}}  
--{{{  ccsp.writeproc
INLINE PROC ccsp.writeproc (DPROCESS p, VAL []BYTE filename, INT result)
  C.ccsp.writeproc (p, filename, result)
:
--}}}  
--{{{  ccsp.readproc
INLINE PROC ccsp.readproc (DPROCESS p, VAL []BYTE filename, INT result)
  C.ccsp.readproc (p, filename, result)
:
--}}}  
--{{{  ccsp.libhandleof
PROC ccsp.libhandleof (VAL DPROCESS p, INT handle)
  C.ccsp.libhandleof (p, handle)
:
--}}}  
--{{{  ccsp.resumeproc
INLINE PROC ccsp.resumeproc (DPROCESS p, []CHAN OF ANY in, out, INT result)
  KR.run (p, in, out, result)
:
--}}}  


