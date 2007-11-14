--** @module Graphics

--* Draws a solid rectangle.
--
--  Clips as necessary.
--
--  @param x.0, y.0 Top left corner
--  @param w.0 Width
--  @param h.0 Height/depth
--  @param colour Colour
--  @param raster Raster

PROC Rectangle (VAL INT x.0, y.0, w.0, h.0, VAL INT colour, [][]INT raster)

  VAL INT W IS SIZE raster[0]:
  VAL INT H IS SIZE raster:

  -- Flatten raster to eliminate multiplications
  -- raster[y][x] -> raster[(y TIMES W) PLUS x]

  -- [Y][X]INT raster RETYPES raster:
  -- [W TIMES H]INT raster RESHAPES raster:

  IF
    (w.0 <= 0) OR (h.0 <= 0) OR
    ((x.0 PLUS w.0) < 0) OR (x.0 >= W) OR
    ((y.0 PLUS h.0) < 0) OR (y.0 >= H)
      SKIP					-- entirely outside raster
    TRUE
      INT x, y, w, h:
      SEQ
        IF					-- clip
          x.0 < 0
            x, w := 0, x.0 PLUS w.0
          TRUE
            x, w := x.0, w.0
        IF
          (x PLUS w) > W
            w := W MINUS x
          TRUE
            SKIP
        IF
          y.0 < 0
            y, h := 0, y.0 PLUS h.0
          TRUE
            y, h := y.0, h.0
        IF
          (y PLUS h) > H
            h := H MINUS y
          TRUE
            SKIP
        SEQ i = y FOR h
          SEQ j = x FOR w
            raster[i][j] := colour
:

--* Special case of [@ref Rectangle]

PROC Square (VAL INT x, y, s, VAL INT colour, [][]INT raster)
  Rectangle (x, y, s, s, colour, raster)
:
