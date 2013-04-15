--** Constants and types for boids simulation.
--
--	gpuboids_params.inc -- various parameters for GPU boids implementation
--	Copyright (C) 2012-2013 Fred Barnes <frmb@kent.ac.uk>
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

#IF NOT DEFINED (GPUBOIDS.PARAMS.INC)
#DEFINE GPUBOIDS.PARAMS.INC


#IF DEFINED (N.WIDTH)
VAL INT WIDTH.LOCATIONS IS ##N.WIDTH:
#ELSE
VAL INT WIDTH.LOCATIONS IS 16:
#ENDIF
#IF DEFINED (N.HEIGHT)
VAL INT HEIGHT.LOCATIONS IS ##N.HEIGHT:
#ELSE
VAL INT HEIGHT.LOCATIONS IS 12:
#ENDIF
VAL INT NUM.LOCATIONS IS WIDTH.LOCATIONS * HEIGHT.LOCATIONS:
VAL INT PIXEL.SCALE IS 768 / HEIGHT.LOCATIONS:
VAL REAL32 MAX.INIT.VELOCITY IS 0.1:
VAL REAL32 MIN.CYLINDER.RADIUS IS 0.1:
VAL REAL32 MAX.CYLINDER.RADIUS IS 0.3:
#IF NOT DEFINED (N.INITIAL.BOIDS)
VAL INT INITIAL.BOIDS IS 1000:
#ELSE
VAL INT INITIAL.BOIDS IS ##N.INITIAL.BOIDS:
#ENDIF
#IF NOT DEFINED (N.INITIAL.OBST)
VAL INT INITIAL.CYLINDERS IS (NUM.LOCATIONS * 2) / 5:
#ELSE
VAL INT INITIAL.CYLINDERS IS ##N.INITIAL.OBST:
#ENDIF

-- hard limit for number of agents in the system.
VAL INT MAX.AGENTS IS ((INITIAL.BOIDS + INITIAL.CYLINDERS) + 32):

-- hard limit for maximum agents per location (and surrounding space)
#IF DEFINED (N.MAX.VIEWABLE.AGENTS)
VAL INT MAX.VIEWABLE.AGENTS IS ##N.MAX.VIEWABLE.AGENTS:
#ELSE
VAL INT MAX.VIEWABLE.AGENTS IS INITIAL.BOIDS / WIDTH.LOCATIONS:
#ENDIF
VAL INT VIEWABLE.ARRAY.SIZE IS (MAX.VIEWABLE.AGENTS * NUM.LOCATIONS):

VAL REAL32 MEAN.VELOCITY.FRACTION IS 8.0:
VAL REAL32 CENTRE.OF.MASS.FRACTION IS 45.0:
VAL REAL32 REPULSION.DISTANCE IS 0.07:
VAL REAL32 REPULSION.FRACTION IS 3.0:
VAL REAL32 SOFT.THRESHOLD IS 0.05:
VAL REAL32 OBSTACLE.FRACTION IS 8.0:
VAL REAL32 SPEED.LIMIT IS 0.05:
--* How far you can see -- this can't be greater than 1.0.
VAL REAL32 VISION.RADIUS IS 0.35:
--* Your field of view, in degrees.
--VAL REAL32 VISION.ANGLE IS 200.0:
VAL REAL32 VISION.ANGLE IS 200.0:
VAL REAL32 VISION.MAX.ANGULAR.DIFF IS ((VISION.ANGLE / 2.0) * Pi) / 180.0:
VAL INT DISPLAY.TIME.CYCLES IS 10:
VAL INT SCREEN.UPDATE.TIME IS 1000000 / 50:         -- when synchronising with display, max frames-per-second.
#IF DEFINED (SYNC.VIS)
VAL INT CYCLE.TIME IS SCREEN.UPDATE.TIME:
#ELSE
VAL INT CYCLE.TIME IS 0:
#ENDIF
VAL REAL32 SMOOTH.ACCELERATION IS 5.0:
VAL REAL32 EPSILON IS 1.0E-8:
VAL REAL32 ALIVE.EPSILON IS 2.0E-4:

-- connectivity
VAL INT DIRECTIONS IS 8:
VAL [DIRECTIONS + 1][2]INT OFFSETS IS [[-1, -1], [0, -1], [1, -1],
                                       [-1, 0],           [1, 0],
                                       [-1, 1],  [0, 1],  [1, 1],  [0, 0]]:

--* Initial seed, zero is random (from timer).
VAL INT INITIAL.SEED IS 125354:

--* Colours.
-- #DEFINE WHITE.BACKGROUND

#IF DEFINED (WHITE.BACKGROUND)
VAL BOID.BG.COLOUR IS #FFFFFF:
VAL REAL32 BOID.RCOL.SAT IS 0.5:
VAL REAL32 BOID.RCOL.VAL IS 0.8:
VAL INTERACTIVE.AGENT.COLOUR IS #4040F0:
VAL INFO.TEXT.COLOUR IS #000000:
VAL BOID.TAIL.COLOUR IS #8080A0:
VAL BOID.GRID.COLOUR IS #C0C0C0:
VAL CYLINDER.COLOUR IS #00B000:
VAL DEBUG.BOID.COLOUR IS #000000:
#ELSE
VAL BOID.BG.COLOUR IS #000000:
VAL REAL32 BOID.RCOL.SAT IS 0.6:
VAL REAL32 BOID.RCOL.VAL IS 0.8:
VAL INTERACTIVE.AGENT.COLOUR IS #1010A0:
VAL INFO.TEXT.COLOUR IS #FFFFFF:
VAL BOID.TAIL.COLOUR IS #606080:
VAL BOID.GRID.COLOUR IS #404040:
VAL CYLINDER.COLOUR IS #00A000:
VAL DEBUG.BOID.COLOUR IS #FFFFFF:
#ENDIF


DATA TYPE VECTOR
  PACKED RECORD
    REAL32 x, y:
:

DATA TYPE AGENT.INFO
  PACKED RECORD
    INT type.id:                -- type encoded in top 8 bits.
    INT locn:                   -- boundary encoded in top 4 bits; movement for GPU result encoded in #0F000000 bits.
                                -- boundary: #01 = left, #02 = top, #04 = right, #08 = bottom;
                                -- moving:   #01 = left, #02 = up, #04 = right, #08 = down.
    VECTOR position:
    VECTOR velocity:
    REAL32 radius:
    INT colour:                 -- "need to move" encoded in top 8 bits (alpha normally).
:

VAL AGENT.INFO BLANK.AGENT.INFO IS [0, 0, [0.0, 0.0], [0.0, 0.0], 0.0, #000000]:

DATA TYPE SIMPARAMS
  PACKED RECORD
    REAL32 cmfrac:              -- centre of mass fraction.
    REAL32 rdist:               -- repulsion distance.
    REAL32 rdist.sq:            -- repulsion distance (squared).
    REAL32 rfrac:               -- repulsion fraction.
    REAL32 mvfrac:              -- mean velocity fraction.
    REAL32 vrad:                -- view radius.
    REAL32 vrad.sq:             -- view radius (squared).
    REAL32 vmaxdiff:            -- vision max angular diff.
    REAL32 epsilon:             -- threshold for not moving.
    REAL32 sthres:              -- soft threshold for obstacles.
    REAL32 ofrac:               -- obstacle fraction.
    REAL32 slimit:              -- speed limit.
    REAL32 slimit.sq:           -- speed limit (squared).
    REAL32 saccel:              -- smooth acceleration.

    INT width:                  -- grid width.
    INT height:                 -- grid height.
    INT max.agents:             -- maximum number of agents.
    INT viewsize:               -- size of shared viewable array in elements.
    INT vperboid:               -- view-size per boid per location.
    INT nloc:                   -- number of grid locations.
    INT ndir:                   -- number of directions (in grid ref. array).
:

                                -- cmfrac,  rdist,  rdist.sq,   rfrac,  mvfrac, vrad,   vrad.sq,
                                -- vmaxdiff,                            epsilon,sthres, ofrac,
                                -- slimit, slimit.sq,    saccel,width,          height
                                -- max.agents,viewsize,           vperboid,           nloc,         ndir
VAL []SIMPARAMS PRESET.PARAMS IS [[45.0,    0.07,   0.07*0.07,  3.0,    8.0,    0.35,   0.35*0.35,
                                   ((200.0 / 2.0) * PI) / 180.0,        1.0E-8, 0.05,   8.0,
                                   0.05,   0.05*0.05,    5.0,   WIDTH.LOCATIONS,HEIGHT.LOCATIONS,
                                   MAX.AGENTS,VIEWABLE.ARRAY.SIZE,MAX.VIEWABLE.AGENTS,NUM.LOCATIONS,DIRECTIONS],
                                  [45.0,    0.07,   0.07*0.07,  3.0,    8.0,    0.39,   0.39*0.39,
                                   ((90.0 / 2.0) * PI) / 180.0,         1.0E-8, 0.05,   8.0,
                                   0.08,   0.08*0.08,    5.0,   WIDTH.LOCATIONS,HEIGHT.LOCATIONS,
                                   MAX.AGENTS,VIEWABLE.ARRAY.SIZE,MAX.VIEWABLE.AGENTS,NUM.LOCATIONS,DIRECTIONS],
                                  [13.0,    0.12,   0.12*0.12,  2.0,    0.5,    0.61,   0.61*0.61,
                                   ((360.0 / 2.0) * PI) / 180.0,        1.0E-8, 0.05,   2.0,
                                   0.09,   0.09*0.09,    6.5,   WIDTH.LOCATIONS,HEIGHT.LOCATIONS,
                                   MAX.AGENTS,VIEWABLE.ARRAY.SIZE,MAX.VIEWABLE.AGENTS,NUM.LOCATIONS,DIRECTIONS],
                                  [22.0,    0.11,   0.11*0.11,  3.5,    3.0,    0.57,   0.57*0.57,
                                   ((360.0 / 2.0) * PI) / 180.0,        1.0E-8, 0.05,   2.5,
                                   0.09,   0.09*0.09,    2.5,   WIDTH.LOCATIONS,HEIGHT.LOCATIONS,
                                   MAX.AGENTS,VIEWABLE.ARRAY.SIZE,MAX.VIEWABLE.AGENTS,NUM.LOCATIONS,DIRECTIONS],
                                  [3.0,     0.21,   0.21*0.21,  3.0,    1.0,    0.61,   0.61*0.61,
                                   ((62.0 / 2.0) * PI) / 180.0,         1.0E-8, 0.05,   1.0,
                                   0.09,   0.09*0.09,    6.5,   WIDTH.LOCATIONS,HEIGHT.LOCATIONS,
                                   MAX.AGENTS,VIEWABLE.ARRAY.SIZE,MAX.VIEWABLE.AGENTS,NUM.LOCATIONS,DIRECTIONS]]:

#ENDIF
