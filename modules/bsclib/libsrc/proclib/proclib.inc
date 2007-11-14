--
--	proclib.inc - include file for proc library
--	Copyright (C) 2000 Fred Barnes <frmb2@ukc.ac.uk>
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

--** @module proc

--{{{  data types
--* An environment variable
DATA TYPE ENVIRONMENT
  PACKED RECORD
    [128]BYTE ident:      --* Variable name
    INT ident.size:       --* Length of variable name
    [128]BYTE value:      --* Variable value
    INT value.size:       --* Length of variable value
:
--}}}  

--{{{  external declarations for killable blocking calls
#PRAGMA EXTERNAL "PROC BX.pl.run2 (CHAN OF INT c, VAL []ENVIRONMENT env, VAL [][]BYTE command, VAL INT kyb.fd, scr.fd, err.fd, INT result) = 2"
--}}}  

