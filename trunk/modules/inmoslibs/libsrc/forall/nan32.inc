-- NaN values for a 32-bit processor

--{{{  occam refernce manual NaNs
VAL INT32 undefined.NaN         IS #7F800010( INT32 ) :
VAL INT32 unstable.NaN          IS #7F800008( INT32 ) :
VAL INT32 inexact.NaN           IS #7F800004( INT32 ) :

VAL INT64 Dundefined.NaN        IS #7FF0000200000000( INT64 ) :
VAL INT64 Dunstable.NaN         IS #7FF0000100000000( INT64 ) :
VAL INT64 Dinexact.NaN          IS #7FF0000080000000( INT64 ) :

VAL INT Dundefined.NaN.hi       IS #7FF00002 :
VAL INT Dundefined.NaN.lo       IS 0 :
--}}}

--{{{  Common NaNs
--  These are the values of NaNs returned by the IMS T800
VAL INT InvalidOperandsNaN.32   IS #7FF00000 :
VAL INT ZeroZeroDivNaN.32       IS #7FC00000 :
VAL INT InfInfDivNaN.32         IS #7FA00000 :
VAL INT ZeroInfMulNaN.32        IS #7F900000 :
VAL INT AddSubInfInfNaN.32      IS #7F880000 :
VAL INT NegSqrtNaN.32           IS #7F840000 :
VAL INT Real64to32NaN.32        IS #7F820000 :
VAL INT RemInfNaN.32            IS #7F804000 :
VAL INT RemZeroNaN.32           IS #7F802000 :

--  These are the values of NaNs returned by the IMS T800
VAL INT64 InvalidOperandsNaN.64 IS #7FFF000000000000 (INT64) :
VAL INT64 ZeroZeroDivNaN.64     IS #7FF8000000000000 (INT64) :
VAL INT64 InfInfDivNaN.64       IS #7FF4000000000000 (INT64) :
VAL INT64 ZeroInfMulNaN.64      IS #7FF2000000000000 (INT64) :
VAL INT64 AddSubInfInfNaN.64    IS #7FF1000000000000 (INT64) :
VAL INT64 NegSqrtNaN.64         IS #7FF0800000000000 (INT64) :
VAL INT64 Real64to32NaN.64      IS #7FF0400000000000 (INT64) :
VAL INT64 RemInfNaN.64          IS #7FF0080000000000 (INT64) :
VAL INT64 RemZeroNaN.64         IS #7FF0040000000000 (INT64) :

--  These are the high values of NaNs returned by the IMS T800
VAL INT InvalidOperandsNaN.hi   IS #7FFF0000 :
VAL INT ZeroZeroDivNaN.hi       IS #7FF80000 :
VAL INT InfInfDivNaN.hi         IS #7FF40000 :
VAL INT ZeroInfMulNaN.hi        IS #7FF20000 :
VAL INT AddSubInfInfNaN.hi      IS #7FF10000 :
VAL INT NegSqrtNaN.hi           IS #7FF08000 :
VAL INT Real64to32NaN.hi        IS #7FF04000 :
VAL INT RemInfNaN.hi            IS #7FF00800 :
VAL INT RemZeroNaN.hi           IS #7FF00400 :
--}}}

--{{{  Other names for NaNs
VAL INT32  root.NaN               RETYPES NegSqrtNaN.32 :
VAL INT64 Droot.NaN               RETYPES NegSqrtNaN.64 :

VAL INT Real64to32NaNfrac         RETYPES #02000000 :

VAL REAL32 InvalidOperandsNaN.r32 RETYPES InvalidOperandsNaN.32 :
VAL REAL64 InvalidOperandsNaN.r64 RETYPES InvalidOperandsNaN.64 :
--}}}
