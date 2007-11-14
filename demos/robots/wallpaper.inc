--** @module Graphics

--* Cover [@code dst] with copies of [@code dst]
--
--  @param src Source raster
--  @param dst Destination raster

PROC Wallpaper (VAL [][]INT src, [][]INT dst)

  VAL INT w IS SIZE src[0]:             -- width of src
  VAL INT d IS SIZE src:                -- depth of src
  VAL INT W IS SIZE dst[0]:             -- width of dst
  VAL INT D IS SIZE dst:                -- depth of dst
 
  SEQ i = 0 FOR D
    VAL INT ii IS i \ d:
    SEQ j = 0 FOR W
      VAL INT jj IS j \ w:
      dst[i][j] := src[ii][jj]
:
