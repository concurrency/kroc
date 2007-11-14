--
-- KRoC Complex Arithmetic Library
--
-- $Id: complex.oci,v 1.1 1997/03/31 11:58:06 djb1 Exp $
--
-- $Source: /proj/kroc/develop/examples/udo/RCS/complex.oci,v $
--
-- In-line functions
--
-- (C) Copyright 1997 D.C. Wood <D.C.Wood@ukc.ac.uk>
-- University of Kent at Canterbury

-- 32-bit Complex operations

REAL32 INLINE FUNCTION COMPLEX32ABSSQ (VAL COMPLEX32 x) IS
  (x[real]*x[real]) + (x[imag]*x[imag]):

COMPLEX32 INLINE FUNCTION "-" (VAL COMPLEX32 x) IS
  [-x[real], -x[imag]]:

COMPLEX32 INLINE FUNCTION "~" (VAL COMPLEX32 x) IS
  [x[real], -x[imag]]:

COMPLEX32 INLINE FUNCTION "+" (VAL COMPLEX32 x, y) IS
  [x[real] + y[real], x[imag] + y[imag]]:

COMPLEX32 INLINE FUNCTION "-" (VAL COMPLEX32 x, y) IS
  [x[real] - y[real], x[imag] - y[imag]]:

COMPLEX32 INLINE FUNCTION "**" (VAL COMPLEX32 x, y) IS
  [(x[real]*y[real]) - (x[imag]*y[imag]),
   (x[imag]*y[real]) + (x[real]*y[imag])]:

COMPLEX32 INLINE FUNCTION "**" (VAL COMPLEX32 x, VAL REAL32 r) IS
  [x[real]*r, x[imag]*r]:

COMPLEX32 INLINE FUNCTION "**" (VAL REAL32 r, VAL COMPLEX32 x) IS
  [x[real]*r, x[imag]*r]:

COMPLEX32 INLINE FUNCTION "/" (VAL COMPLEX32 x, VAL REAL32 r) IS
  [x[real]/r, x[imag]/r]:

COMPLEX32 INLINE FUNCTION REAL32TOCOMPLEX32 (VAL REAL32 x) IS
  [x, 0.0]:

BOOL INLINE FUNCTION "=" (VAL COMPLEX32 x, y) IS
  (x[real] = y[real]) AND (x[imag] = y[imag]):

BOOL INLINE FUNCTION "<>" (VAL COMPLEX32 x, y) IS
  (x[real] <> y[real]) OR (x[imag] <> y[imag]):

REAL32, REAL32 INLINE FUNCTION COMPLEX32TOREAL32 (VAL COMPLEX32 x) IS
  x[real], x[imag]:

COMPLEX32 INLINE FUNCTION COMPLEX64ROUND (VAL COMPLEX64 x) IS
  [REAL32 ROUND x[real], REAL32 ROUND x[imag]]:

COMPLEX32 INLINE FUNCTION COMPLEX64TRUNC (VAL COMPLEX64 x) IS
  [REAL32 TRUNC x[real], REAL32 TRUNC x[imag]]:

COMPLEX32 INLINE FUNCTION COMPLEX32SQR (VAL COMPLEX32 z) IS
  [(z[real] + z[imag])*(z[real] - z[imag]), (z[real]*z[imag])*2.0]:

-- 64-bit Complex operations

REAL64 INLINE FUNCTION COMPLEX64ABSSQ (VAL COMPLEX64 x) IS
  (x[real]*x[real]) + (x[imag]*x[imag]):

COMPLEX64 INLINE FUNCTION "-" (VAL COMPLEX64 x) IS
  [-x[real], -x[imag]]:

COMPLEX64 INLINE FUNCTION "~" (VAL COMPLEX64 x) IS
  [x[real], -x[imag]]:

COMPLEX64 INLINE FUNCTION "+" (VAL COMPLEX64 x, y) IS
  [x[real] + y[real], x[imag] + y[imag]]:

COMPLEX64 INLINE FUNCTION "-" (VAL COMPLEX64 x, y) IS
  [x[real] - y[real], x[imag] - y[imag]]:

COMPLEX64 INLINE FUNCTION "**" (VAL COMPLEX64 x, y) IS
  [(x[real]*y[real]) - (x[imag]*y[imag]),
   (x[imag]*y[real]) + (x[real]*y[imag])]:

COMPLEX64 INLINE FUNCTION "**" (VAL COMPLEX64 x, VAL REAL64 r) IS
  [x[real]*r, x[imag]*r]:

COMPLEX64 INLINE FUNCTION "**" (VAL REAL64 r, VAL COMPLEX64 x) IS
  [x[real]*r, x[imag]*r]:

COMPLEX64 INLINE FUNCTION "/" (VAL COMPLEX64 x, VAL REAL64 r) IS
  [x[real]/r, x[imag]/r]:

COMPLEX64 INLINE FUNCTION REAL64TOCOMPLEX64 (VAL REAL64 x) IS
  [x, 0.0]:

BOOL INLINE FUNCTION "=" (VAL COMPLEX64 x, y) IS
  (x[real] = y[real]) AND (x[imag] = y[imag]):

BOOL INLINE FUNCTION "<>" (VAL COMPLEX64 x, y) IS
  (x[real] <> y[real]) OR (x[imag] <> y[imag]):

REAL64, REAL64 INLINE FUNCTION COMPLEX64TOREAL64 (VAL COMPLEX64 x) IS
  x[real], x[imag]:

COMPLEX64 INLINE FUNCTION COMPLEX32TO64 (VAL COMPLEX32 x) IS
  [REAL64 x[real], REAL64 x[imag]]:

COMPLEX64 INLINE FUNCTION COMPLEX64SQR (VAL COMPLEX64 z) IS
  [(z[real] + z[imag])*(z[real] - z[imag]), (z[real]*z[imag])*2.0]:
