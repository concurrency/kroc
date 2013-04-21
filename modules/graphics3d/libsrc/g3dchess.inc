--
--	g3dchess.inc -- 3D raster graphics routines, chess piece constants
--	Copyright (C) 2009 Fred Barnes <frmb@kent.ac.uk>
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

--** @module graphics3d

#IF NOT DEFINED (G3DCHESS.INC)
#DEFINE G3DCHESS.INC

--*{{{  G3D.CP Chess piece constants.
VAL INT G3D.CP.PAWN IS 1:               --* Pawn.
VAL INT G3D.CP.KNIGHT IS 2:             --* Knight.
VAL INT G3D.CP.BISHOP IS 3:             --* Bishop.
VAL INT G3D.CP.ROOK IS 4:               --* Rook/castle.
VAL INT G3D.CP.QUEEN IS 5:              --* Queen.
VAL INT G3D.CP.KING IS 6:               --* King.

--*}}}

--* Chess piece control.
PROTOCOL G3DC.PIECE.CTL.IN
  CASE
    set.pos; INT; INT                   --* Set piece position on board (0-7,0-7).
    move.pos; INT; INT                  --* Move piece to specified position (animate!).
    hide; BOOL                          --* Whether to make hidden.
:

--* Used to control a particular piece.
CHAN TYPE G3DC.PIECE.CTL
  MOBILE RECORD
    CHAN G3DC.PIECE.CTL.IN in?:         --* Piece control.
    CHAN G3D.GEN.CONTROL gen.ctl?:      --* Piece generator control.
    CHAN G3D.PRJ.CONTROL prj.ctl?:      --* Projector control.
    CHAN G3D.PGN.CONTROL pgn.ctl?:      --* Polygon convert control.
    CHAN G3D.RDR.CONTROL rdr.ctl?:      --* Render control.
:

#ENDIF      -- G3DCHESS.INC

