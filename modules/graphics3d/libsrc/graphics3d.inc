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
-- Note: the general [serial] techniques/algorithms used in this library
-- are based on code from lightsrc.cpp and glenz.cpp by Marcus Lindblom.
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

--* Integer point in 2D space, original Z multiplied up.
DATA TYPE G3D.POINT2.3
  RECORD
    INT x, y, z:
:

--* Integer point in 2D space.
DATA TYPE G3D.POINT2
  RECORD
    INT x, y:
:
--}}}
--{{{  polygon types
--* General polygon in 2D space, orignal Z multiplied up.
DATA TYPE G3D.POLY
  RECORD
    [G3D.MAX.POLY.POINTS]INT x:
    [G3D.MAX.POLY.POINTS]INT y:
    [G3D.MAX.POLY.POINTS]INT z:
    INT npoints:
    INT norm:
    INT pixel:
    INT polyid:
:

--* Scan-line converted polygon in 2D space.
DATA TYPE G3D.POLY.SCAN
  RECORD
    [G3D.MAX.SCAN.HEIGHT]INT start:
    [G3D.MAX.SCAN.HEIGHT]INT end:
    [G3D.MAX.SCAN.HEIGHT]INT z.start:
    [G3D.MAX.SCAN.HEIGHT]INT z.end:
    INT norm:
    INT scan.s, scan.e:
    INT pixel:
    INT polyid:
:

--}}}
--{{{  other data-types

--* Z-buffer is a 2D array of integers, passed around with rasters.
DATA TYPE ZBUFFER IS MOBILE [][]INT:

--}}}
--{{{  protocols and channel-types for controlling components

CHAN TYPE G3D.GEN.LINK:

--* Polygon points generator control.
PROTOCOL G3D.GEN.CONTROL
  CASE
    reset                           --* Reset generator to default state.
    add.scale; [3]REAL32            --* Add to the current scale.
    set.scale; [3]REAL32            --* Absolutely set the scale.
    add.trans; [3]REAL32            --* Add to the current translation.
    set.trans; [3]REAL32            --* Absolutely set the translation.
    relink; G3D.GEN.LINK?           --* Reconnect I/O channels.
:

--* Points rotation control.
PROTOCOL G3D.ROT.CONTROL
  CASE
    reset                     --* Reset rotation to default state.
    add.increment; REAL32     --* Add to the increment.
    set.increment; REAL32     --* Absolutely set the increment.
    set.angle; REAL32         --* Absolutely set the angle of rotation.
    set.mode; INT             --* Set the rotation mode (see [@ref G3D.ROTATE.MODE]).
:

--* Points translation control.
PROTOCOL G3D.TRN.CONTROL
  CASE
    reset                     --* Reset translation to default state (0, 0, 0).
    set.3dshift; [3]REAL32    --* Set 3D translation.
:

--* Points scaler control.
PROTOCOL G3D.SCL.CONTROL
  CASE
    reset                     --* Reset scale to default state (1, 1, 1)
    set.3dscale; [3]REAL32    --* Set 3D scaling.
:

--* Polygon points 3D-to-2D projection control.
PROTOCOL G3D.PRJ.CONTROL
  CASE
    reset                     --* Reset projector to default state.
    set.scaling; [2]REAL32    --* Set 2D scaling.
    set.zdepth; REAL32        --* Set Z depth offset (pushes scene away from camera).
    set.2dshift; [2]INT       --* Set 2D translation.
    set.3dshift; [3]REAL32    --* Set 3D translation (applied before projection).
:

--* Polygon generator control.
PROTOCOL G3D.PGN.CONTROL
  CASE
    reset                               --* Reset polygon generator to default state.
    set.trimap; BOOL                    --* Generate triangular polygons only.
    set.backfaces; BOOL                 --* Whether back faces of solid objects should be generated.
    set.face.colour; INT; INT           --* Set individual face colour.
    set.face.colours; MOBILE []INT      --* Set all face colours.
    set.polyid; INT                     --* Set polygon ID (0-255).
:

--* Polygon scan-line generator control.
PROTOCOL G3D.PSC.CONTROL
  CASE
    reset                     --* Resets scan-line generator to default state.
    set.zcolour; BOOL         --* Whether the pixel colour used is adjusted for Z depth.
:

--* Polygon render (shader) control.
PROTOCOL G3D.RDR.CONTROL
  CASE
    reset                     --* Resets shader to default state.
    set.mode; INT             --* Set shader mode (see [@ref G3D.RENDER.MODE]).
:

--* Overall control for graphics pipeline.
PROTOCOL G3D.ALL.CONTROL
  CASE
    reset.all                 --* Reset all components to default state.
:

--* Graphics pipeline speed control.
PROTOCOL G3D.TMR.CONTROL
  CASE
    reset                     --* Resets the timer control.
    pause                     --* Pause/unpause the rasters flowing through the network.
    faster                    --* Go faster.
    slower                    --* Go slower.
    set.fps; INT              --* Set frames-per-second absolutely.
:

--}}}
--{{{  combined channel-types for component control
CHAN TYPE G3D.GEN.LINK
  MOBILE RECORD
    CHAN INT trigger?:
    CHAN MOBILE []G3D.POINT3 out!:
    CHAN G3D.GEN.CONTROL ctrl?:
:

--}}}
--{{{  render-engine channel-type and protocols

CHAN TYPE G3D.RENDER.ENGINE:

--* Graphics pipeline link protocol.
PROTOCOL G3D.RENDER.ENGINE.LINK
  CASE
    raster; RASTER; ZBUFFER                 --* Raster to render on and forward.
    set.next; INT; G3D.RENDER.ENGINE!       --* Set the next renderer in the pipeline.
                                            -- The integer passed is used to identify
                                            -- which particular 'next' to replace (counts down).
:

CHAN TYPE G3D.RENDER.ENGINE
  MOBILE RECORD
    CHAN G3D.RENDER.ENGINE.LINK link?:      --* Link channel.
:

--}}}
--{{{  scene transformation constants, channel-type and protocols

--*{{{  G3D.SCENE.RO Scene rotation ordering.
VAL INT G3D.SCENE.RO.NONE IS #0000:             --* No rotations applied
VAL INT G3D.SCENE.RO.X IS #0001:                --* Rotate on X axis only.
VAL INT G3D.SCENE.RO.Y IS #0002:                --* Rotate on Y axis only.
VAL INT G3D.SCENE.RO.Z IS #0003:                --* Rotate on Z axis only.
VAL INT G3D.SCENE.RO.YX IS #0012:               --* Rotate on Y then X.
VAL INT G3D.SCENE.RO.ZX IS #0013:               --* Rotate on Z then X.
VAL INT G3D.SCENE.RO.XY IS #0021:               --* Rotate on X then Y.
VAL INT G3D.SCENE.RO.ZY IS #0023:               --* Rotate on Z then Y.
VAL INT G3D.SCENE.RO.XZ IS #0031:               --* Rotate on X then Z.
VAL INT G3D.SCENE.RO.YZ IS #0032:               --* Rotate on Y then Z.
VAL INT G3D.SCENE.RO.ZYX IS #0123:              --* Rotate on Z then Y then X.
VAL INT G3D.SCENE.RO.YZX IS #0132:              --* Rotate on Y then Z then X.
VAL INT G3D.SCENE.RO.ZXY IS #0213:              --* Rotate on Z then X then Y.
VAL INT G3D.SCENE.RO.XZY IS #0231:              --* Rotate on X then Z then Y.
VAL INT G3D.SCENE.RO.YXZ IS #0312:              --* Rotate on Y then X then Z.
VAL INT G3D.SCENE.RO.XYZ IS #0321:              --* Rotate on X then Y then Z.
--*}}}

-- Note: the general "scene" control connects into transform, rotate and
-- 3D projection control.  In effect allows the "camera" to be moved and
-- manipulated.

CHAN TYPE G3D.SCENE.TRANS:

--* Scene rotate pipeline link protocol.
PROTOCOL G3D.SCENE.TRANS.LINK
  CASE
    reset                                   --* Reset to default settings.
    set.pre.3dshift; [3]REAL32              --* Set 3D shift (before rotation, determines centre of scene).
    set.rot.angles; [3]REAL32               --* Set rotations in X, Y and Z planes.
    set.rot.order; INT                      --* Set rotation order (see [@ref G3D.SCENE.RO]).
    set.rot.diffs; [3]REAL32                --* Set differences for continuous rotation.
    set.post.3dshift; [3]REAL32             --* Set 3D shift (after rotation, determines camera position).
    set.xyscale; [2]REAL32                  --* Set X-Y scaling (lens angle).

    set.next; INT; G3D.SCENE.TRANS!         --* Set the next scene transformer in a pipeline.
                                            -- The integer passed is used to identify which
                                            -- particular 'next' to replace (counts down).
:

CHAN TYPE G3D.SCENE.TRANS
  MOBILE RECORD
    CHAN G3D.SCENE.TRANS.LINK link?:        --* Link channel.
:

--}}}
--{{{  reporting for polygon clicking

--* Used to provide information about polygon clicks, order is (id; x; y).
PROTOCOL G3D.POLY.CLICK IS [3]INT:

--}}}

#ENDIF

