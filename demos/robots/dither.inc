--** @module Graphics

--* Dither
--
--  @param n Order
--  @param i, j Coordinates
--  @return Dither

INT FUNCTION Dither (VAL INT n, i, j)

  INT r:
  VALOF
    INITIAL INT p IS i >< j:
    INITIAL INT q IS i:
    SEQ
      r := 0
      SEQ k = 0 FOR n
        SEQ
          r := (r << 1) \/ (p /\ 1)
          p := p >> 1
          r := (r << 1) \/ (q /\ 1)
          q := q >> 1
    RESULT r
:

--* Reverse Dither
--
--  @param n Order
--  @param k Count
--  @returns i, j Coordinates

INT, INT FUNCTION Undither (VAL INT n, k)

  INT p, q:
  VALOF
    INITIAL INT r IS k:
    SEQ
      p, q := 0, 0
      SEQ j = 0 FOR n
        SEQ
          p := (p << 1) \/ (r /\ 1)
          r := r >> 1
          q := (q << 1) \/ (r /\ 1)
          r := r >> 1
    RESULT p, p >< q
:
