--* Does circle hit anything?
--
--  @param X, Y Centre of circle
--  @param radius Radius
--  @param colour Colour of target
--  @param raster Raster

BOOL FUNCTION Hit (VAL INT X, Y, radius, colour, VAL [][]INT raster)

  INT INLINE FUNCTION "%>" (VAL INT X, Y) IS ASHIFTRIGHT (X, Y):

  VAL INT FUDGE IS 4:

  BOOL hit:
  VALOF
    SEQ
      VAL INT W IS SIZE raster[0]:
      VAL INT D IS SIZE raster:
      IF
        (X < radius) OR (Y < radius) OR ((X + radius) >= W) OR ((Y + radius) >= D)
          hit := TRUE
        TRUE
          SEQ
            hit := FALSE
            INITIAL INT i IS radius << FUDGE:
            INITIAL INT j IS 0:
            INITIAL INT x IS 0:
            INITIAL INT y IS Y:
            VAL INT SHIFT IS 3:
            SEQ k = 0 FOR 50			-- 2 pi << SHIFT
              SEQ
                i := i - (j %> SHIFT)
                x := X + (i %> FUDGE)
                hit := hit OR (raster[y][x] = colour)
                j := j + (i %> SHIFT)
                y := Y + (j %> FUDGE)
                hit := hit OR (raster[y][x] = colour)
    RESULT hit
:

