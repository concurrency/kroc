--
--	CREW synchronisation
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
#COMMENT "(CREW stuff): Copyright P.H.Welch and D.C.Wood, September, 1996"
--}}}

--{{{  VAL definitions
VAL INT NotProcess.p IS 0:
--}}}  

--{{{  DATA TYPE CREW
DATA TYPE CREW
  RECORD
    SEMAPHORE notify:            -- readers/writers queue here
    INT n.readers:               -- number of current readers
    -- SEMAPHORE n.readers.sem:  -- not needed for uni-processor
    INT waiting.writer:          -- temporary parking for a writer
:
--}}}

--{{{  INLINE PROC initialise.crew (CREW B)
INLINE PROC initialise.crew (CREW B)
  SEQ
    initialise.semaphore (B[notify], 1)
    -- initialise.semaphore (B[n.readers.sem], 1)  -- needed for (virtual) SMP
    B[n.readers], B[waiting.writer] := 0, NotProcess.p
:
--}}}

--{{{  INLINE PROC claim.read.crew (CREW B)
INLINE PROC claim.read.crew (CREW B)
  SEQ
    claim.semaphore (B[notify])
    -- claim.semaphore (B[n.readers.sem])          -- needed for (virtual) SMP
    B[n.readers] := B[n.readers] + 1
    -- release.semaphore (B[n.readers.sem])        -- needed for (virtual) SMP
    release.semaphore (B[notify])
:
--}}}

--{{{  INLINE PROC release.read.crew (CREW B)
INLINE PROC release.read.crew (CREW B)
  SEQ
    -- claim.semaphore (B[n.readers.sem])          -- needed for (virtual) SMP
    B[n.readers] := B[n.readers] - 1
    IF
      (B[n.readers] = 0) AND (B[waiting.writer] <> NotProcess.p)
        SEQ
          --{{{  schedule waiting writer
          ASM
            LD     B[waiting.writer]
            RUNP
          --}}}
          B[waiting.writer] := NotProcess.p
      TRUE
        SKIP
    -- release.semaphore (B[n.readers.sem])        -- needed for (virtual) SMP
:
--}}}

--{{{  INLINE PROC claim.write.crew (CREW B)
INLINE PROC claim.write.crew (CREW B)
  SEQ
    claim.semaphore (B[notify])
    -- claim.semaphore (B[n.readers.sem])          -- needed for (virtual) SMP
    IF
      B[n.readers] > 0
        SEQ
          --{{{  set B[waiting.writer] to this process descriptor
          ASM
            LDLP   0
            ST     B[waiting.writer]
          --}}}
          -- release.semaphore (B[n.readers.sem])  -- needed for (virtual) SMP
          --{{{  deschedule
          ASM
            STOPP
          --}}}
      TRUE
        SEQ
          -- release.semaphore (B[n.readers.sem])  -- needed for (virtual) SMP
          SKIP
:
--}}}

--{{{  INLINE PROC release.write.crew (CREW B)
INLINE PROC release.write.crew (CREW B)
  release.semaphore (B[notify])
:
--}}}

