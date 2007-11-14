--
--	Bucket synchronisation
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
#COMMENT "(Bucket stuff): Copyright P.H.Welch and D.C.Wood, October, 1996"
--}}}

--{{{  DATA TYPE BUCKET
DATA TYPE BUCKET
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

--{{{  INLINE PROC initialise.bucket (BUCKET b)
INLINE PROC initialise.bucket (BUCKET b)
  b := [NotProcess.p, NotProcess.p, 0]
:
--}}}

--{{{  INT INLINE FUNCTION number.in.bucket (VAL BUCKET b)
INT INLINE FUNCTION number.in.bucket (VAL BUCKET b) IS b[count]:
--}}}

--{{{  INLINE PROC fall.into.bucket (BUCKET b)
INLINE PROC fall.into.bucket (BUCKET b)
  SEQ
    b[count] := b[count] PLUS 1
    --{{{  enqueue this process on b-queue
    IF
      b[Fptr] = NotProcess.p
        ASM
          LDC   NotProcess.p
          STL   pw.Link
          LDLP  0
          ST    b[Fptr]
          LDLP  0
          ST    b[Bptr]
      TRUE
        ASM
          LDC   NotProcess.p
          STL   pw.Link
          LDLP  0
          LD    b[Bptr]
          STNL  pw.Link
          LDLP  0
          ST    b[Bptr]
    --}}}
    --{{{  save instruction pointer and schedule next process
    ASM
      STOPP
    --}}}
:
--}}}

--{{{  INLINE PROC flush.bucket (BUCKET b)
INLINE PROC flush.bucket (BUCKET b)
  --{{{  DATA TYPE QUEUE
  DATA TYPE QUEUE
    RECORD
      INT Fptr, Bptr:
  :
  --}}}
  SEQ
    IF
      b[Fptr] <> NotProcess.p
        --{{{  attach the whole (non-empty) b-queue to the run-queue
        QUEUE run:
        ASM
          LD    ADDRESSOF run               -- run := [Fptr, Bptr]
          SAVEL
          LD    run[Fptr]                   -- run queue empty?
          NULL
          DIFF
          CJ    :L1                         -- yes
        
          LD    b[Fptr]                     -- run[Bptr]^[pw.Link] := b[Fptr]
          LD    run[Bptr]
          STNL  pw.Link
          J	:L2
        
          :L1
          LD    b[Fptr]                     -- Fptr := b[Fptr]
          STLF
        
          :L2
          LD    b[Bptr]                     -- Bptr := b[Bptr]
          STLB
        --}}}
      TRUE
        SKIP
    b := [NotProcess.p, NotProcess.p, 0]
:
--}}}

