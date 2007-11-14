--
--	trace.inc -- EXTERNAL declarations for trace.c
--	Copyright (C) 2005-2006 Adam Sampson <ats@offog.org>
--	Computing Laboratory, University of Kent, Canterbury, UK
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
--	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
--	MA 02110-1301, USA.
--

#PRAGMA EXTERNAL "PROC C.trace (VAL []BYTE message) = 0"
#PRAGMA EXTERNAL "PROC C.trace.i (VAL []BYTE message, VAL INT i) = 0"
#PRAGMA EXTERNAL "PROC C.trace.x (VAL []BYTE message, VAL INT i) = 0"
#PRAGMA EXTERNAL "PROC C.trace.results (VAL []INT data) = 0"
#PRAGMA EXTERNAL "PROC C.printarg (MOBILE.CHAN? arg) = 0"
#PRAGMA EXTERNAL "PROC C.mark (VAL INT n) = 0"

