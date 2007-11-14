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

-- streamio.inc
-- Copyright 1989 INMOS Limited
-- Updated to match TDS3 strmhdr list; 4-Feb-91 SRH
VAL st.max.string.size IS 256 :
VAL ft.terminated   IS  -8 : -- used to terminate a keystream
VAL ft.number.error IS -11 :
PROTOCOL KS IS INT:
PROTOCOL SS
  CASE
    st.reset
    st.up
    st.down
    st.left
    st.right
    st.goto; INT32; INT32
    st.ins.char; BYTE
    st.del.char
    st.out.string; INT32::[]BYTE
    st.clear.eol
    st.clear.eos
    st.ins.line
    st.del.line
    st.beep
    st.spare
    st.terminate
    st.help
    st.initialise
    st.out.byte; BYTE
    st.out.int; INT32
    st.key.raw
    st.key.cooked
    st.release
    st.claim
    st.endstream
    st.set.poll; INT32
:
