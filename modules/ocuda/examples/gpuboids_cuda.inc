--
--	gpuboids_cuda.inc -- occam-pi/CUDA interface for boids
--	Copyright (C) 2012-2013 Fred Barnes <frmb@kent.ac.uk>
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

#IF NOT DEFINED (GPUBOIDS.CUDA.INC)
#DEFINE GPUBOIDS.CUDA.INC


#IF DEFINED (N.CPU.WORKERS)
VAL INT CPU.WORKERS IS ##N.CPU.WORKERS:
#ELSE
VAL INT CPU.WORKERS IS 1:
#ENDIF
#IF DEFINED (N.INITIAL.DCOUNT)
VAL INT INITIAL.DCOUNT IS ##N.INITIAL.DCOUNT:
#ELSE
VAL INT INITIAL.DCOUNT IS 0:
#ENDIF

DATA TYPE CPUPTR IS INT:
DATA TYPE GPUPTR IS INT:

#PRAGMA EXTERNAL "PROC C.cuda.alloc.global (OCUDA.DEVINFO device, VAL INT all.agents.addr.0, all.agents.addr.1, viewable.addr, viewcount.addr, viewmap.addr, parms.addr, RESULT CPUPTR cpu, gpu) = 0"
#PRAGMA EXTERNAL "PROC C.cuda.free.global (OCUDA.DEVINFO device, CPUPTR cpu, gpu) = 0"
#PRAGMA EXTERNAL "PROC C.cuda.copy.viewmap (OCUDA.DEVINFO device, VAL CPUPTR cpu, gpu) = 0"
#PRAGMA EXTERNAL "PROC C.cuda.copy.global (OCUDA.DEVINFO device, VAL INT act.cycle, VAL CPUPTR cpu, gpu) = 0"
#PRAGMA EXTERNAL "PROC C.cuda.copyback.global (OCUDA.DEVINFO device, VAL INT act.cycle, astart, acount, VAL CPUPTR cpu, gpu) = 0"
#PRAGMA EXTERNAL "PROC C.cuda.doboids1 (OCUDA.DEVINFO device, VAL INT act.cycle, astart0, acount0, VAL CPUPTR gpu) = 0"
#PRAGMA EXTERNAL "PROC B.cuda.doboids1 (OCUDA.DEVINFO device, VAL INT act.cycle, astart0, acount0, VAL CPUPTR gpu) = 4"
#PRAGMA EXTERNAL "PROC C.cuda.runcycle (OCUDA.DEVINFO device, VAL INT act.cycle, astart0, acount0, VAL CPUPTR cpu, gpu) = 0"
#PRAGMA EXTERNAL "PROC B.cuda.runcycle (OCUDA.DEVINFO device, VAL INT act.cycle, astart0, acount0, VAL CPUPTR cpu, gpu) = 4"


#ENDIF

