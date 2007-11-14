--
--	Semaphore synchronisation
--	Copyright (C) 1996 P.H. Welch and D.C. Wood
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

--{{{  copyright notice
#COMMENT "(Semaphore stuff): Copyright P.H.Welch and D.C.Wood, September, 1996"
--}}}

--{{{  DATA TYPE SEMAPHORE
DATA TYPE SEMAPHORE
  RECORD
    INT Fptr, Bptr:
    INT count:
:
--}}}

--{{{  constants
VAL INT NotProcess.p IS 0:
VAL INT pw.Link IS -2:
VAL INT pw.Priority IS -3:
--}}}

--{{{  INLINE PROC initialise.semaphore (SEMAPHORE s, VAL INT count)
INLINE PROC initialise.semaphore (SEMAPHORE s, VAL INT count)
  s := [NotProcess.p, NotProcess.p, count]
:
--}}}

--{{{  INLINE PROC claim.semaphore (SEMAPHORE s)
INLINE PROC claim.semaphore (SEMAPHORE s)
  ASM
    --{{{  (s[count] > 0) ?
    LD      s[count]          -- s[count] > 0?
    LDC     0
    GT
    --}}}
    --{{{  no  --> :L1
    CJ      :L1               -- no
    --}}}
    --{{{  yes --> s[count] := s[count] - 1 --> :L4
    LD      s[count]          -- yes -- s[count] := s[count] - 1
    ADC     -1
    ST      s[count]
    J       :L4
    --}}}

    :L1
    --{{{  (s[Fptr] = NotProcess.p) ?
    LD      s[Fptr]           -- s[Fptr] = NotProcess.p?
    NULL
    DIFF
    --}}}
    --{{{  yes --> :L2
    CJ      :L2               -- yes
    --}}}
    --{{{  no  --> s[Bptr]^[pw.Link] := Wptr --> :L3
    LDLP    0                 -- no  -- s[Bptr]^[pw.Link] := Wptr
    LD      s[Bptr]
    STNL    pw.Link
    J       :L3
    --}}}

    :L2
    --{{{  s[Fptr] := Wptr
    LDLP    0                 -- s[Fptr] := Wptr
    ST      s[Fptr]
    --}}}

    :L3
    --{{{  s[Bptr] := Wptr
    LDLP    0                 -- s[Bptr] := Wptr
    ST      s[Bptr]
    --}}}
    --{{{  Wptr[Link] = NotProcess.p
    LDC 0
    STL pw.Link
    --}}}
    --{{{  STOPP
    STOPP
    --}}}

    :L4
:
--}}}

--{{{  INLINE PROC release.semaphore (SEMAPHORE s)
INLINE PROC release.semaphore (SEMAPHORE s)
  INT temp:
  ASM
    --{{{  (s[Fptr] = NotProcess.p) ?
    LD      s[Fptr]           -- s[Fptr] = NotProcess.p?
    NULL
    DIFF
    --}}}
    --{{{  yes --> :L2
    CJ      :L2               -- yes
    --}}}
    --{{{  no  --> temp := s[Fptr]^[pw.Link]
    LD      s[Fptr]           -- no  -- s[Fptr] := s[Fptr]^[pw.Link]
    LDNL    pw.Link
    ST      temp
    --}}}
    --{{{  schedule s[Fptr]
    LD      s[Fptr]
    RUNP
    --}}}
    --{{{  (s[Fptr] = s[Bptr]) ?
    LD      s[Fptr]           -- s[Fptr] = s[Bptr]?
    LD      s[Bptr]
    DIFF
    --}}}
    --{{{  yes --> :L1
    CJ      :L1               -- yes
    --}}}
    --{{{  no  --> s[Fptr] := temp --> :L3
    LD      temp              -- no
    ST      s[Fptr]
    J       :L3
    --}}}

    :L1
    --{{{  s[Fptr] := NotProcess.p --> :L3
    NULL                      -- s[Fptr] := NotProcess.p
    ST      s[Fptr]
    J       :L3
    --}}}

    :L2
    --{{{  s[count] := s[count] + 1
    LD      s[count]          -- s[count] := s[count] + 1
    ADC     1
    ST      s[count]
    --}}}

    :L3
    --{{{  reschedule this process
    LDC 0
    LDLP 0
    STARTP
    STOPP
    --}}}
:
--}}}

