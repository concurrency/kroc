--** @module Graphics

--* Draws a solid disc.
--  Clips as necessary.
--
--  @param x, y Coordinates of centre
--  @param r Radius
--  @param colour Colour
--  @param raster Raster

PROC Disc (VAL INT x, y, r, VAL INT colour, [][]INT raster)

  VAL INT W IS SIZE raster[0]:			-- width
  VAL INT H IS SIZE raster:			-- height
  IF
    (r < 0) OR
    ((x PLUS r) < 0) OR ((x MINUS r) >= W) OR
    ((y PLUS r) < 0) OR ((y MINUS r) >= H)
      SKIP					-- entirely outside raster
    TRUE
      VAL INT rr IS (r TIMES r) PLUS r:		-- ~ (r + 1/2)^2

      -- Just r^2 gives a single pixel at the N, S, E, & W "poles"

      INT i.min, i.max, j.min, j.max:
      INT d.i, d.j:
      SEQ
        IF					-- clip
          y < r
            i.min := MINUS y
          TRUE
            i.min := MINUS r
        IF
          x < r
            j.min := MINUS x
          TRUE
            j.min := MINUS r
        VAL INT H.1 IS H MINUS 1:
        IF
          (y PLUS r) > H.1
            i.max := H.1 MINUS y
          TRUE
            i.max := r
        VAL INT W.1 IS W MINUS 1:
        IF
          (x PLUS r) > W.1
            j.max := W.1 MINUS x
          TRUE
            j.max := r
        d.i := (i.min TIMES i.min) MINUS rr	-- i^2 - r^2
        VAL INT h IS (i.max MINUS i.min) PLUS 1:
        VAL INT w IS (j.max MINUS j.min) PLUS 1:
        VAL INT jj IS j.min TIMES j.min:
        SEQ i = i.min FOR h
          SEQ
            d.j := d.i PLUS jj			-- i^2 + j^2 - r^2
            -- [W]INT raster IS raster[y PLUS i]:
            SEQ j = j.min FOR w
              SEQ
                IF
                  d.j <= 0			-- or < ?
                    raster[y PLUS i][x PLUS j] := colour
                  TRUE
                    SKIP
                d.j := (d.j PLUS (2 TIMES j)) PLUS 1	-- (j + 1)^2 - j^2
            d.i := (d.i PLUS (2 TIMES i)) PLUS 1	-- (i + 1)^2 - i^2
:
