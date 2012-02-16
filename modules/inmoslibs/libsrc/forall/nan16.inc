-- NaN values for a 16-bit processor

--{{{  occam refernce manual NaNs
VAL []INT undefined.NaN  IS [#0010, #7F80] :
VAL []INT unstable.NaN   IS [#0008, #7F80] :
VAL []INT inexact.NaN    IS [#0004, #7F80] :

VAL []INT Dundefined.NaN IS [#0000, #0000, #0002, #7FF0] :
VAL []INT Dunstable.NaN  IS [#0000, #0000, #0001, #7FF0] :
VAL []INT Dinexact.NaN   IS [#0000, #8000, #0000, #7FF0] :

--}}}

--{{{  Common NaNs
--  These are the values of NaNs returned by the IMS T800
VAL []INT InvalidOperandsNaN.32 IS [#0000, #7FF0] :
VAL []INT ZeroZeroDivNaN.32     IS [#0000, #7FC0] :
VAL []INT InfInfDivNaN.32       IS [#0000, #7FA0] :
VAL []INT ZeroInfMulNaN.32      IS [#0000, #7F90] :
VAL []INT AddSubInfInfNaN.32    IS [#0000, #7F88] :
VAL []INT NegSqrtNaN.32         IS [#0000, #7F84] :
VAL []INT RemInfNaN.32          IS [#4000, #7F80] :
VAL []INT RemZeroNaN.32         IS [#2000, #7F80] :

--  These are the values of NaNs returned by the IMS T800
VAL []INT InvalidOperandsNaN.64 IS [0,0,#0000,#7FFF] :
VAL []INT ZeroZeroDivNaN.64     IS [0,0,#0000,#7FF8] :
VAL []INT InfInfDivNaN.64       IS [0,0,#0000,#7FF4] :
VAL []INT ZeroInfMulNaN.64      IS [0,0,#0000,#7FF2] :
VAL []INT AddSubInfInfNaN.64    IS [0,0,#0000,#7FF1] :
VAL []INT NegSqrtNaN.64         IS [0,0,#8000,#7FF0] :
VAL []INT RemInfNaN.64          IS [0,0,#0800,#7FF0] :
VAL []INT RemZeroNaN.64         IS [0,0,#0400,#7FF0] :

--}}}

--{{{  Other names for NaNs
VAL INT32  root.NaN RETYPES NegSqrtNaN.32 :
VAL INT64 Droot.NaN RETYPES NegSqrtNaN.64 :

VAL INT Real64to32NaNfrac RETYPES  #0200 :

VAL REAL32 InvalidOperandsNaN.r32 RETYPES InvalidOperandsNaN.32 :
VAL REAL64 InvalidOperandsNaN.r64 RETYPES InvalidOperandsNaN.64 :

--}}}
