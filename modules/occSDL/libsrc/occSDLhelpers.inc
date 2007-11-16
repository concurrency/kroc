-- External definitions for helpers.c.

--** @module occSDL

--{{{  externals
#PRAGMA EXTERNAL "PROC C.occ.SDL.make.surface (VAL [][]INT pixels, RESULT SDL.Surface surface) = 0"
--}}}

--{{{  PROC occ.SDL.make.surface
--* Turn an occam array into an SDL surface.
-- This is a wrapper around [@ref SDL.CreateRGBSurfaceFrom].
--
-- @param pixels Array of pixels.
--   The first dimension is Y; the second is X.
--   Each [@code INT] is a single pixel in [@text #00RRGGBB] format.
-- @param surface The resulting surface.
--   This contains a pointer to [@code pixels], so you must free it with
--   [@ref SDL.FreeSurface] before whatever you passed as [@code pixels] goes
--   out of scope.
INLINE PROC occ.SDL.make.surface (VAL [][]INT pixels, RESULT SDL.Surface surface)
  C.occ.SDL.make.surface (pixels, surface)
:
--}}}
