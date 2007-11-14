--** @module Graphics

--* Draws a horizontal line.
--
--  Clips as necessary.
--
--  @param x, y Starting point
--  @param l Length
--  @param colour Colour
--  @param raster Raster

PROC Horizontal (VAL INT x, y, l, VAL INT colour, [][]INT raster)

  VAL INT W IS SIZE raster[0]:
  VAL INT H IS SIZE raster:

  INITIAL INT x.0 IS x:
  INITIAL INT x.1 IS x + l:
  IF
    (l <= 0) OR
    (y < 0) OR (y >= H) OR (x >= W) OR (x.1 < 0)
      SKIP				-- wholly outside
    TRUE
      SEQ
        IF
          x.0 < 0
            x.0 := 0
          TRUE
            SKIP
        IF
          x.1 >= W
            x.1 := W
          TRUE
            SKIP
        SEQ i = x.0 FOR x.1 - x.0
          raster[y][i] := colour
:

--* Draws a vertical line.
--
--  Clips as necessary.
--
--  @param x, y Starting point
--  @param l Length
--  @param colour Colour
--  @param raster Raster

PROC Vertical (VAL INT x, y, l, VAL INT colour, [][]INT raster)

  VAL INT W IS SIZE raster[0]:
  VAL INT H IS SIZE raster:

  INITIAL INT y.0 IS y:
  INITIAL INT y.1 IS y + l:
  IF
    (l <= 0) OR
    (x < 0) OR (x >= W) OR (y >= H) OR (y.1 < 0)
      SKIP				-- wholly outside
    TRUE
      SEQ
        IF
          y.0 < 0
            y.0 := 0
          TRUE
            SKIP
        IF
          y.1 >= H
            y.1 := H
          TRUE
            SKIP
        SEQ i = y.0 FOR y.1 - y.0
          raster[i][x] := colour
:

--* Draws a rectangular box.
--
--  Clips as necessary.
--
--  @param x, y Top left corner
--  @param w Width
--  @param h Height/depth
--  @param colour Colour
--  @param raster Raster

PROC Box (VAL INT x, y, w, h, VAL INT colour, [][]INT raster)

  SEQ
    Horizontal (x, y, w, colour, raster)
    Horizontal (x, y + (h - 1), w, colour, raster)
    Vertical   (x, y, h, colour, raster)
    Vertical   (x + (w - 1), y, h, colour, raster)
:
