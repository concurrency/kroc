-- mandelgpu_types.inc -- for Mandelbrot

DATA TYPE COMPLEX64
  RECORD
    REAL64 real:
    REAL64 imag:
:

VAL INT IMAGE.WIDTH IS 1024:
VAL INT IMAGE.HEIGHT IS 768:

REAL64 INLINE FUNCTION COMPLEX64ABSSQ (VAL COMPLEX64 x) IS
  (x[real]*x[real]) + (x[imag]*x[imag]):

COMPLEX64 INLINE FUNCTION "+" (VAL COMPLEX64 x, y) IS
  [x[real] + y[real], x[imag] + y[imag]]:

COMPLEX64 INLINE FUNCTION "**" (VAL COMPLEX64 x, y) IS
  [(x[real]*y[real]) - (x[imag]*y[imag]),
   (x[imag]*y[real]) + (x[real]*y[imag])]:

