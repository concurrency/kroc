-- These constants should be used for HI and LO access
-- to words of REAL64's that are RETYPED to different types.
-- This only applies if we are using native FPU, software
-- FPU is always little-endian.

-- NATIVEISH is defined by skroc on systems which use the 
--  nativish memory interface on the transterpreter.
-- TARGET.BIGENDIAN is defined by occ21 when compiling 
--  for a big-endian machine.

#IF DEFINED(TARGET.HAS.FPU) AND (DEFINED(NATIVEISH) OR DEFINED(TARGET.BIGENDIAN))
-- big-endian
VAL hi IS 0:
VAL lo IS 1:
#ELSE
-- little-endian
VAL hi IS 1:
VAL lo IS 0:
#ENDIF
