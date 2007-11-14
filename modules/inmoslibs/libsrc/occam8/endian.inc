-- These constants should be used for HI and LO accessing of of REAL64's that are RETYPED to different types.
-- NATIVEISH is automatically defined by skroc on systems which use the NATIVEISH memory interface on the transterpreter.
-- TARGET.BIGENDIAN is defined by occ21 when compiling for a big-endian machine.

#IF DEFINED(NATIVEISH) OR DEFINED(TARGET.BIGENDIAN)
-- Bigenidan machines
VAL hi IS 0:
VAL lo IS 1:
#ELSE
-- Little endian
VAL hi IS 1:
VAL lo IS 0:
#ENDIF
