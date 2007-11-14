--** @module Graphics

--* Copy one raster onto another
--
--  @param src Source raster
--  @param x, y Top left of target in [@code dst]
--  @param c Transparent colour -- don't copy
--  @param f Orientation
--  @param dst Destination raster

--  One corner of src always maps to (x, y) of dst
--
--  Let the least significant three bits of f be abc:
--
--  a = 1:  flip about \, then
--  b = 1:  flip about |
--  c = 1:  flip about -
--
--  Hence
--  f = 7:  flip about /

PROC Copy (VAL [][]INT src, VAL INT x, y, c, f, [][]INT dst)

  VAL INT w IS SIZE src[0]:		-- width of src
  VAL INT d IS SIZE src:		-- depth of src
  VAL INT W IS SIZE dst[0]:		-- width of dst
  VAL INT D IS SIZE dst:		-- depth of dst
  VAL INT f IS f /\ 7:

  SEQ
    IF
      (f < 4) AND ((x < 0) OR (y < 0) OR ((x + w) > W) OR ((y + d) > D))
        SKIP				-- (partly) outside dst
      (f > 3) AND ((x < 0) OR (y < 0) OR ((x + d) > W) OR ((y + w) > D))
        SKIP
      TRUE
        CASE f
          0
            SEQ i = 0 FOR d
              VAL []INT s.i IS src[i]:
              []INT d.i IS dst[y + i]:
              SEQ j = 0 FOR w
                VAL INT s.i.j IS s.i[j]:
                IF
                  s.i.j <> c
                    d.i[x + j] := s.i.j
                  TRUE
                    SKIP
          1
            SEQ i = 0 FOR d
              VAL []INT s.i IS src[i]:
              []INT d.i IS dst[(y + d) - (i + 1)]:
              SEQ j = 0 FOR w
                VAL INT s.i.j IS s.i[j]:
                IF
                  s.i.j <> c
                    d.i[x + j] := s.i.j
                  TRUE
                    SKIP
          2
            SEQ i = 0 FOR d
              VAL []INT s.i IS src[i]:
              []INT d.i IS dst[y + i]:
              SEQ j = 0 FOR w
                VAL INT s.i.j IS s.i[j]:
                IF
                  s.i.j <> c
                    d.i[(x + w) - (j + 1)] := s.i.j
                  TRUE
                    SKIP
          3
            SEQ i = 0 FOR d
              VAL []INT s.i IS src[i]:
              []INT d.i IS dst[(y + d) - (i + 1)]:
              SEQ j = 0 FOR w
                VAL INT s.i.j IS s.i[j]:
                IF
                  s.i.j <> c
                    d.i[(x + w) - (j + 1)] := s.i.j
                  TRUE
                    SKIP
          4
            SEQ i = 0 FOR d
              VAL []INT s.i IS src[i]:
              SEQ j = 0 FOR w
                VAL INT s.i.j IS s.i[j]:
                IF
                  s.i.j <> c
                    dst[y + j][x + i] := s.i.j
                  TRUE
                    SKIP
          5
            SEQ i = 0 FOR d
              VAL []INT s.i IS src[i]:
              SEQ j = 0 FOR w
                VAL INT s.i.j IS s.i[j]:
                IF
                  s.i.j <> c
                    dst[(y + w) - (j + 1)][x + i] := s.i.j
                  TRUE
                    SKIP
          6
            SEQ i = 0 FOR d
              VAL []INT s.i IS src[i]:
              SEQ j = 0 FOR w
                VAL INT s.i.j IS s.i[j]:
                IF
                  s.i.j <> c
                    dst[y + j][(x + d) - (i + 1)] := s.i.j
                  TRUE
                    SKIP
          7
            SEQ i = 0 FOR d
              VAL []INT s.i IS src[i]:
              SEQ j = 0 FOR w
                VAL INT s.i.j IS s.i[j]:
                IF
                  s.i.j <> c
                    dst[(y + w) - (j + 1)][(x + d) - (i + 1)] := s.i.j
                  TRUE
                    SKIP
:

PROC Overwrite (VAL [][]INT src, VAL INT x, y, f, [][]INT dst)

  Copy (src, x, y, -1, f, dst)		-- impossible colour
:
