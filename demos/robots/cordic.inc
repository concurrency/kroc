--** @module Graphics

--* Signed right shift
--  @private

INT INLINE FUNCTION "%>" (VAL INT X, Y) IS ASHIFTRIGHT (X, Y):

--* Fractional multiplication
--  @private

INT INLINE FUNCTION "@"  (VAL INT X, Y) IS FRACMUL     (X, Y):

--* Table of constants for [@ref Cordic] and [@ref Cidroc]:
--  [@code round (arctan(1/2^k) * 2^31 / pi)].
--  @private

VAL []ANGLE CORDIC.ATAN IS [536870912, 316933406, 167458907, 85004756, 42667331,
                            21354465, 10679838, 5340245, 2670163, 1335087, 667544,
                            333772, 166886, 83443, 41722, 20861, 10430, 5215, 2608,
                            1304, 652, 326, 163, 81, 41, 20, 10, 5, 3, 1, 1]:

--* CORDIC rotation algorithm.
--
--  See [@link http://en.wikipedia.org/wiki/CORDIC, here] for details.
--
--  Rotates vector through angle
--
--  @param X, Y Coordinates of vector
--  @param theta Angle
--  @returns X, Y Rotated through [@code theta]

-- The bottom few bits of X, Y are noisy, hence the offset

INT, INT FUNCTION Cordic (VAL INT X, Y, VAL ANGLE theta)

  -- The following should override the global "@", but the compiler says it's not used.
  -- Without the global "@", the compiler says 'type of operand invalid for operator "@"'.
  -- No such problems with "%>"!

  INT INLINE FUNCTION "@"  (VAL INT X, Y) IS FRACMUL     (X, Y):

  --* Magic constant

  VAL INT CORDIC.CONST IS 1304065748:		-- 0.6072529350 * 2^31

  VAL INT OFFSET IS 8:
  VAL INT FACTOR IS CORDIC.CONST >> OFFSET:

  INT x, y:
  VALOF
    INITIAL ANGLE theta IS theta:
    SEQ
      x, y := X << OFFSET, Y << OFFSET
      IF					-- deal with |theta| > ANGLE.RIGHT
        theta >= 0
          x, y, theta := -y,  x, theta - ANGLE.RIGHT
        TRUE					-- theta < 0
          x, y, theta :=  y, -x, theta + ANGLE.RIGHT
      -- compiler bug for unary MINUS
      SEQ k = 0 FOR SIZE CORDIC.ATAN
        INT d.x, d.y:
        ANGLE d.theta:
        SEQ
          d.x, d.y, d.theta := x %> k, y %> k, CORDIC.ATAN[k]
          IF
            theta >= 0
              x, y, theta := x MINUS d.y, y PLUS  d.x, theta - d.theta
            TRUE				-- theta < 0
              x, y, theta := x PLUS  d.y, y MINUS d.x, theta + d.theta
    RESULT x @ FACTOR, y @ FACTOR
:

--* Inverse [@ref Cordic] algorithm;
--  Converts Cartesian [@code (X, Y)] to polar coordinates.
--
--  @param X, Y Coordinate of vector
--  @return radius [@code sqrt(X^2 + Y^2)]
--  @return angle [@code arctan(Y/X)]

INT, ANGLE FUNCTION Cidroc (VAL INT X, Y)

  --* Magic constant

  VAL INT CORDIC.CONST IS 1304065748:		-- 0.6072529350 * 2^31

  VAL INT OFFSET IS 8:
  VAL INT FACTOR IS CORDIC.CONST >> OFFSET:

  INT x:
  ANGLE theta:
  VALOF
    INT y:
    SEQ
      x, y := X << OFFSET, Y << OFFSET
      IF
        y <= 0
          x, y, theta := -y,  x, -ANGLE.RIGHT
        TRUE					-- y > 0
          x, y, theta :=  y, -x,  ANGLE.RIGHT
      SEQ k = 0 FOR SIZE CORDIC.ATAN
        INT d.x, d.y:
        ANGLE d.theta:
        SEQ
          d.x, d.y, d.theta := x %> k, y %> k, CORDIC.ATAN[k]
          IF
            y <= 0
              x, y, theta := x MINUS d.y, y PLUS  d.x, theta - d.theta
            TRUE				-- y > 0
              x, y, theta := x PLUS  d.y, y MINUS d.x, theta + d.theta
    RESULT x @ FACTOR, theta
:
