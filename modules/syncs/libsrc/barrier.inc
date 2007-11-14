--
--	Barrier synchronisation
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
#COMMENT "(Barrier stuff): Copyright P.H.Welch and D.C.Wood, September, 1996"
--}}}

--{{{  DATA TYPE UBARRIER
DATA TYPE UBARRIER
  RECORD
    INT Fptr, Bptr:
    INT active, count:
:
--}}}

--{{{  constants
VAL INT NotProcess.p IS 0:
VAL INT pw.Link IS -2:
--}}}

--{{{  INLINE PROC initialise.barrier (UBARRIER e, VAL INT count)
INLINE PROC initialise.barrier (UBARRIER e, VAL INT count)
  --{{{  
  e := [NotProcess.p, NotProcess.p, count, count]
  --}}}
:
--}}}

--{{{  INLINE PROC enroll.barrier (UBARRIER e, VAL INT count)
INLINE PROC enroll.barrier (UBARRIER e, VAL INT count)
  --{{{  
  SEQ
    e[active] := e[active] + count
    e[count] := e[count] + count
  --}}}
:
--}}}

--{{{  INLINE PROC resign.barrier (UBARRIER e)
INLINE PROC resign.barrier (UBARRIER e)
  --{{{  
  --{{{  DATA TYPE QUEUE
  DATA TYPE QUEUE
    RECORD
      INT Fptr, Bptr:
  :
  --}}}
  SEQ
    e[active], e[count] := e[active] - 1, e[count] - 1
    IF
      (e[count] = 0) AND (e[active] > 0)
        SEQ
          --{{{  attach the whole (non-empty) e-queue to the run-queue
          QUEUE run:
          #PRAGMA DEFINED run
          ASM
            LD    ADDRESSOF run              -- run := [Fptr, Bptr]
            SAVEL
            LD    run[Fptr]                  -- run queue empty?
            NULL
            DIFF
            CJ    :L1                        -- yes
          
            LD    e[Fptr]                    -- run[Bptr]^[pw.Link] := e[Fptr]
            LD    run[Bptr]
            STNL  pw.Link
            J     :L2
          
            :L1
            LD    e[Fptr]                    -- Bptr := e[Fptr]
            STLF
          
            :L2
            LD    e[Bptr]                    -- Fptr := e[Bptr]
            STLB
          --}}}
          e := [NotProcess.p, NotProcess.p, e[active], e[active]]
      TRUE
        SKIP
  --}}}
:
--}}}

--{{{  INLINE PROC synchronise.barrier (UBARRIER e)
INLINE PROC synchronise.barrier (UBARRIER e)
  --{{{  don't reschedule if last one
  --{{{  DATA TYPE QUEUE
  DATA TYPE QUEUE
    RECORD
      INT Fptr, Bptr:
  :
  --}}}
  SEQ
    e[count] := e[count] - 1
    IF
      e[count] = 0
        SEQ
          --{{{  attach the whole e-queue to the run-queue
          IF
            e[Fptr] <> NotProcess.p
              --{{{  attach the whole (non-empty) e-queue to the run-queue
              QUEUE run:
              #PRAGMA DEFINED run
              ASM
                LD    ADDRESSOF run                  -- run := [Fptr, Bptr]
                SAVEL
                LD    run[Fptr]                      -- run queue empty?
                NULL
                DIFF
                CJ    :L1                            -- yes
              
                LD    e[Fptr]                        -- run[Bptr]^[pw.Link] := e[Fptr]
                LD    run[Bptr]
                STNL  pw.Link
                J     :L2
              
                :L1
                LD    e[Fptr]                        -- Fptr := e[Fptr]
                STLF
              
                :L2
                LD    e[Bptr]                        -- Bptr := e[Bptr]
                STLB
              --}}}
            TRUE
              SKIP
          --}}}
          e := [NotProcess.p, NotProcess.p, e[active], e[active]]
      TRUE
        SEQ
          --{{{  enqueue this process on e-queue
          IF
            e[Fptr] = NotProcess.p
              ASM
                LDLP  0
                ST    e[Fptr]
                LDLP  0
                ST    e[Bptr]
            TRUE
              ASM
                LDLP  0
                LD    e[Bptr]
                STNL  pw.Link
                LDLP  0
                ST    e[Bptr]
          --}}}
          --{{{  save instruction pointer and schedule next process
          ASM
            LDC 0
            LDLP 0
            STNL pw.Link
            STOPP
          --}}}
  --}}}
:
--}}}

