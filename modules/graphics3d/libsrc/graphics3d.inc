--
--	graphics3d.inc -- 3D raster graphics for occam-pi
--	Copyright (C) 2008-2009 Fred Barnes <frmb@kent.ac.uk>
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


--** A library for programming simple 3D graphics in occam-pi.
--
-- This library provides for simple 3D graphics in occam-pi, using simple
-- rasters.
--
-- @module graphics3d

#IF NOT DEFINED (GRAPHICS3D.INC)
#DEFINE GRAPHICS3D.INC

--{{{  constants
--* Default width and height.
-- @private
VAL INT G3D.DEFAULT.HEIGHT IS 480:
VAL INT G3D.DEFAULT.WIDTH IS 640:

--* Maximum number of points in a polygon (see [@ref G3D.POLY]).
VAL INT G3D.MAX.POLY.POINTS IS 4:

--* Maximum height of any particular polygon.
VAL INT G3D.MAX.SCAN.HEIGHT IS 500:

--*{ G3D.RENDER.MODE  polygon rendering modes.
VAL INT G3D.RENDER.MODE.NONE IS #0000:          --* Constant shading.
VAL INT G3D.RENDER.MODE.GLENZ IS #0001:         --* Glenz-effect shading.
VAL INT G3D.RENDER.MODE.NORM IS #0002:          --* Normal vector based shading.
--*}
--*{ G3D.ROTATE.MODE  point/polygon rotation modes.
VAL INT G3D.ROTATE.MODE.NONE IS #0000:          --* No rotation.
VAL INT G3D.ROTATE.MODE.DEMO IS #0001:          --* Demo-effect rotations.
VAL INT G3D.ROTATE.MODE.X IS #0002:             --* Rotate on X axis.
VAL INT G3D.ROTATE.MODE.NX IS #0004:            --* Negative rotate on X axis.
VAL INT G3D.ROTATE.MODE.Y IS #0008:             --* Rotate on Y axis.
VAL INT G3D.ROTATE.MODE.NY IS #0010:            --* Negative rotate on Y axis.
VAL INT G3D.ROTATE.MODE.Z IS #0020:             --* Rotate on Z axis.
VAL INT G3D.ROTATE.MODE.NZ IS #0040:            --* Negative rotate on Z axis.
--*}

--}}}
--{{{  point types
--* Real point in 3D space (REAL32s).
DATA TYPE G3D.POINT3
  RECORD
    REAL32 x, y, z:
:

--* Integer point in 2D space, retains original Z as a REAL32.
DATA TYPE G3D.POINT2.3
  RECORD
    INT x, y:
    REAL32 z:
:

--* Integer point in 2D space.
DATA TYPE G3D.POINT2
  RECORD
    INT x, y:
:
--}}}
--{{{  polygon types
--* General polygon in 2D space, retails original Z as REAL32s.
DATA TYPE G3D.POLY
  RECORD
    [G3D.MAX.POLY.POINTS]INT x:
    [G3D.MAX.POLY.POINTS]INT y:
    [G3D.MAX.POLY.POINTS]REAL32 z:
    INT npoints:
    INT norm:
    INT pixel:
:

--* Scan-line converted polygon in 2D space.
DATA TYPE G3D.POLY.SCAN
  RECORD
    [G3D.MAX.SCAN.HEIGHT]INT start:
    [G3D.MAX.SCAN.HEIGHT]INT end:
    INT norm:
    INT scan.s, scan.e:
    INT pixel:
:

--}}}
--{{{  protocols for controlling components

--* Polygon points generator control.
PROTOCOL G3D.GEN.CONTROL
  CASE
    reset
    add.scale; [3]REAL32
    set.scale; [3]REAL32
    add.trans; [3]REAL32
    set.trans; [3]REAL32
:

--* Points rotation control.
PROTOCOL G3D.ROT.CONTROL
  CASE
    reset
    add.increment; REAL32
    set.angle; REAL32
    set.mode; INT
:

--* Polygon points 3D-to-2D projection control.
PROTOCOL G3D.PRJ.CONTROL
  CASE
    reset
    set.scaling; [2]REAL32
    set.zdepth; REAL32
    set.2dshift; [2]INT
    set.3dshift; [3]REAL32
:

--* Polygon generator control.
PROTOCOL G3D.PGN.CONTROL
  CASE
    reset
    set.trimap; BOOL
    set.backfaces; BOOL
    set.face.colour; INT; INT
    set.face.colours; MOBILE []INT
:

--* Polygon scan-line control.
PROTOCOL G3D.PSC.CONTROL
  CASE
    reset
:

--* Polygon render (shader) control.
PROTOCOL G3D.RDR.CONTROL
  CASE
    reset
    set.mode; INT
:

--* Overall control for graphics pipeline.
PROTOCOL G3D.ALL.CONTROL
  CASE
    reset.all
:

--* Graphics pipeline speed control.
PROTOCOL G3D.TMR.CONTROL
  CASE
    reset
    pause
    faster
    slower
    set.fps; INT
:

--}}}

CHAN TYPE G3D.RENDER.ENGINE:

PROTOCOL G3D.RENDER.ENGINE.LINK
  CASE
    raster; RASTER
    set.next; INT; G3D.RENDER.ENGINE!
:

CHAN TYPE G3D.RENDER.ENGINE
  MOBILE RECORD
    CHAN G3D.RENDER.ENGINE.LINK link?:
:

#ENDIF

