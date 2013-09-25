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

VAL INT OCUDA.CMODE.DEFAULT IS 0:           --* Default: multiple threads can use this device.
VAL INT OCUDA.CMODE.EXCLUSIVE IS 1:         --* Exclusive: one thread in one process can use this device.
VAL INT OCUDA.CMODE.PROHIBITED IS 2:        --* Prohibited: nothing can use this for CUDA.
VAL INT OCUDA.CMODE.EXCLUSIVEPROC IS 3:     --* Exclusive-process: many threads in one process can use this device.

--* Data-type that describes a single CUDA device.  Second part based on stuff from NVidia's API description.
DATA TYPE OCUDA.DEVINFO
  PACKED RECORD
    INT dnum:                     --* Device number.
    [64]BYTE dname:               --* Device name (descriptive).
    INT dname.len:                --* Device name length (<=64).
    INT cversion:                 --* Compute capability (16.16 bits).
    INT cmode:                    --* Compute mode (OCUDA.CMODE...).

    INT global.mem:               --* Global memory on device (in KiB).
    INT shared.mem.per.block:     --* Shared memory per block (in KiB).
    INT regs.per.block:           --* 32-bit registers available per block.
    INT warp.size:                --* Warp-size in threads.
    INT mem.pitch:                --* Maximum pitch in bytes allowed by memory copies.
    INT max.threads.per.block:    --* Maximum number of threads per block.
    [3]INT max.threads.dim:       --* Maximum in each dimension of a block (of threads).
    [3]INT max.grid.size:         --* Maximum in each dimension of a grid (of blocks).
    INT clock.rate:               --* Clock frequency (in KHz).
    INT total.const.mem:          --* Total constant memory available (in KiB).
    INT mproc.count:              --* Number of multiprocessors on device.
    INT max.threads.per.mproc:    --* Maximum resident threads per multiprocessor.
    INT integrated:               --* GPU is integrated as opposed to discrete.
    INT can.map.host.memory:      --* Non-zero if can access host memory directly.
    INT concur.kernels:           --* Device can possibly handle concurrent kernels.
    INT async.engine.count:       --* Number of asynchronous engines (for overlapping exec/memcpy streams).
    INT pci.bus.id:               --* PCI bus ID of device.
    INT pci.device.id:            --* PCI device ID.
    INT pci.domain.id:            --* PCI domain ID (?).
    INT tcc.driver:               --* Non-zero if using Tesla and TCC driver.
    INT unified.addressing:       --* Non-zero if device shares a unified address space with the host.
    INT mem.clock.rate:           --* Memory clock (in KHz).
    INT mem.bus.width:            --* Global memory bus width (in bits).
    INT l2.cache.size:            --* Size of the L2 cache (in KiB).
:

#PRAGMA EXTERNAL "PROC C.cuda.init (INT d.count, RESULT INT res) = 0"
#PRAGMA EXTERNAL "PROC C.cuda.shutdown () = 0"
#PRAGMA EXTERNAL "PROC C.cuda.devinfo (VAL INT dnum, RESULT OCUDA.DEVINFO data, RESULT INT res) = 0"


#ENDIF

