--
--	ocuda.inc -- occam-CUDA interface definitions
--	Copyright (C) 2013 Fred Barnes <frmb@kent.ac.uk>
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
--	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
--

--** @module ocuda

#IF NOT DEFINED (OCUDA.INC)
#DEFINE OCUDA.INC

--* Data-type that describes a single CUDA device.
DATA TYPE OCUDA.DEVINFO
  PACKED RECORD
    INT dnum:           --* Device number.
    [64]BYTE dname:     --* Device name (descriptive).
    INT dname.len:      --* Device name length (<=64).
    INT cversion:       --* Compute capability (16.16 bits).
:

#PRAGMA EXTERNAL "PROC C.cuda.init (INT d.count, RESULT INT res) = 0"
#PRAGMA EXTERNAL "PROC C.cuda.devinfo (VAL INT dnum, RESULT OCUDA.DEVINFO data, RESULT INT res) = 0"


#ENDIF

