--** @module Robot

#INCLUDE "rastergraphics.module"

--* Draw a fan of lines radiating from a point.
--
--  Stop when they reach a distance of [@code r.max],
--  or hit something of colour [@code c.0]
--  or the edge of the raster.
--
--  Number of rays = [@code SIZE ranges]
--
--  @param x.0, y.0 Centre
--  @param r.max Maximum range
--  @param theta Central direction
--  @param phi Half-angle
--  @param c.0 Stop when it hits this colour
--  @param c.1 Colour of fan
--  @param ranges Return ranges of rays
--  @param raster Reaster

PROC Fan (VAL INT x.0, y.0, r.max, VAL ANGLE theta, phi, VAL INT c.0, c.1,
          RESULT RANGES ranges, RASTER raster)

  VAL INT n IS SIZE ranges:
  VAL INT n1 IS n MINUS 1:					-- can't do this in one go
  VAL ANGLE delta IS 2 * (phi / n1):

  INT x, y, d.x, d.y:
  SEQ
    INITIAL ANGLE psi IS theta - phi:
    SEQ k = 0 FOR n
      SEQ
        x, y := x.0, y.0
        d.x, d.y := CORDIC (r.max, 0, psi)
        draw.line (x, y, d.x, d.y, c.0, c.1, raster)
        psi := psi + delta
        ANGLE junk:
        ranges[k], junk := CIDROC (x MINUS x.0, y MINUS y.0)	-- we know the angles!
    x, y := x.0, y.0
    d.x, d.y := CORDIC (r.max, 0, theta)
    draw.line (x, y, d.x, d.y, c.0, COLOUR.RED, raster)		-- show which way we're going
:
