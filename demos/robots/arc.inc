--** @module Graphics

--* Draws a circular arc.
--
--  Uses [@ref Cordic] algorithm, and its inverse, [@ref Cidroc].
--
--  @param x.0, y.0 Coordinates of centre
--  @param radius Radius
--  @param t.0, t.1 Range of [@ref ANGLE]s
--  @param colour Colour
--  @param raster Raster

PROC Arc (VAL INT x.0, y.0, radius, VAL ANGLE t.0, t.1, VAL INT colour, [][]INT raster)

  --* Basic PROC; [@code arc <= ANGLE.RIGHT]
  --
  --  @param t.0, t.1 Range of [@ref ANGLE]s

  PROC arc (VAL ANGLE t.0, t.1)

    INITIAL ANGLE t IS t.0:
    ANGLE d.t:
    SEQ
      INT junk:
      junk, d.t := Cidroc (radius, 1)
      -- d.t := d.t >> 1
      WHILE t.1 AFTER t
        INT x, y:
        SEQ
          x, y := Cordic (radius, 0, t)
          raster[y.0 PLUS y][x.0 PLUS x] := colour
          t := t PLUS d.t
  :

  VAL INT count IS (INT (t.1 MINUS t.0)) >> 30:

  INITIAL ANGLE t.2 IS t.0:
  INITIAL ANGLE t.3 IS t.0 PLUS ANGLE.RIGHT:
  SEQ
    SEQ i = 0 FOR count			-- whole right angles
      SEQ
        arc (t.2, t.3)
        t.2, t.3 := t.3, t.3 PLUS ANGLE.RIGHT
    arc (t.2, t.1)			-- whatever's left
:
