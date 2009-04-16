#PRAGMA COMMENT "(spragma (dynlib misc))"

#PRAGMA EXTERNAL "PROC C.magic.string (VAL []BYTE str) = 0"
PROC magic.string (VAL []BYTE str)
  C.magic.string(str)
:

#PRAGMA EXTERNAL "PROC C.magic.int (VAL INT x) = 0"
PROC magic.int (VAL INT x)
  C.magic.int(x)
:

#PRAGMA EXTERNAL "PROC C.magic.real32 (VAL REAL32 x) = 0"
PROC magic.real32 (VAL REAL32 x)
  C.magic.real32(x)
:

#PRAGMA EXTERNAL "PROC C.magic.debug.init () = 0"
PROC magic.debug.init()
  C.magic.debug.init()
:

#PRAGMA EXTERNAL "PROC C.magic.newline () = 0"
PROC magic.newline()
  C.magic.newline()
:
