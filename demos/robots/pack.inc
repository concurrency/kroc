--** @module Graphics

--* Pack RGB [@code BYTEs] to an [@code INT].
--
--  Assume coded as [@code #rrggbb], which ain't necessarily so.
--
--  @param r, g, b Primary colour components
--  @return [@code #rrggbb]

INT INLINE FUNCTION pack (VAL BYTE r, g, b) IS
  ((((INT r) << 8) \/ (INT g)) << 8) \/ (INT b):

--* Unpack an [@code INT] to RGB [@code BYTEs].
--
--  @param c [@code #rrggbb]
--  @returns r, g, b Primary colour components

BYTE, BYTE, BYTE INLINE FUNCTION unpack (VAL INT c) IS
  BYTE ((c >> 16) /\ #FF), BYTE ((c >> 8) /\ #FF), BYTE (c /\ #FF):
