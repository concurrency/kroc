--** @module Graphics

--* Draw a straight line.
--
--  Returns end point in [@code (x.0, y.0)].
--
--  Does nothing if [@code (x.0, y.0)] is outside [@code raster].
--
--  Based on Foley & van Dam, Computer Graphics Principles and Practice,
--  Second Edition in C, Fig. 3.8.
--
--  @param x.0, y.0 Starting point
--  @param d.x, d.y Relative position of end point
--  @param c.0 Stop when line hits this colour
--  @param c.1 Draw in this colour ...
--  @param raster ... on this raster

PROC Line (INT x.0, y.0, VAL INT d.x, d.y, VAL INT c.0, c.1, [][]INT raster)

  VAL INT W IS SIZE raster[0]:			-- width
  VAL INT H IS SIZE raster:			-- height

  -- Flatten raster to eliminate multiplications:

  -- [H][W]INT raster RETYPES raster:
  -- [W TIMES H]INT raster RESHAPES raster:

  -- raster[y][x] -> raster[(y TIMES W) PLUS x]

  IF
    (x.0 < 0) OR (x.0 >= W) OR
    (y.0 < 0) OR (y.0 >= H)
      SKIP-- starts outside raster
    TRUE
      INT x, y, a.d.x, s.x, a.d.y, s.y:
      INT d, strt, diag:
      BOOL run:
      SEQ
        x, y := x.0, y.0
        IF
          d.x >= 0
            a.d.x, s.x :=  d.x,  1		-- |d.x|, sign(d.x)
          TRUE
            a.d.x, s.x := -d.x, -1
        IF
          d.y >= 0
            a.d.y, s.y :=  d.y,  1
          TRUE
            a.d.y, s.y := -d.y, -1
        run := TRUE
        IF
          a.d.x >= a.d.y
            SEQ
              strt := 2*a.d.y			-- straight
              diag := strt MINUS (2*a.d.x)	-- diagonal
              d := strt MINUS a.d.x
              WHILE run
                SEQ
                  IF
                    a.d.x < 0
                      run := FALSE		-- reached end
                    (x < 0) OR (x >= W) OR (y < 0) OR (y >= H)
                      run := FALSE		-- outside raster
                    raster[y][x] = c.0
                      run := FALSE		-- hit something
                    TRUE
                      SEQ
                        raster[y][x] := c.1	-- mark
                        x.0, y.0 := x, y	-- remember
                        x := x PLUS s.x		-- next pixel
                        a.d.x := a.d.x MINUS 1
                        IF
                          d <= 0
                            d := d PLUS strt
                          TRUE
                            d, y := d PLUS diag, y PLUS s.y
          TRUE					-- a.d.x < a.d.y
            SEQ
              strt := 2*a.d.x
              strt := 2*a.d.x
              diag := strt MINUS (2*a.d.y)
              d := strt MINUS a.d.y
              WHILE run
                SEQ
                  IF
                    a.d.y < 0
                      run := FALSE
                    (x < 0) OR (x >= W) OR (y < 0) OR (y >= H)
                      run := FALSE
                    raster[y][x] = c.0
                      run := FALSE
                    TRUE
                      SEQ
                        raster[y][x] := c.1
                        x.0, y.0 := x, y
                        y := y PLUS s.y
                        a.d.y := a.d.y MINUS 1
                        IF
                          d <= 0
                            d := d PLUS strt
                          TRUE
                            d, x := d PLUS diag, x PLUS s.x
:
