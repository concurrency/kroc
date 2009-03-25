--** Include file for the transterpreter's magical C hooks for Windows/Unix systems.
-- @maintainer Damian Dimmich <djd20@kent.ac.uk>
-- @module tvm.specials

#PRAGMA EXTERNAL "PROC C.tvmspecial.4.get.cmdline.ptr (RESULT INT argc, RESULT INT argv) = 0"
--* Returns a pointer to argc and the number argv (the C command line arguments).
--@param argc pointer to argc
--@param argv the number of elements in the array argc
PROC tvm.get.cmdline.ptr (RESULT INT argc, RESULT INT argv)
  C.tvmspecial.4.get.cmdline.ptr (argc, argv)
:

#PRAGMA EXTERNAL "PROC C.tvmspecial.5.get.version (RESULT []BYTE version) = 0"
--* Returns a string with the transterpreter's version number that is being used.
--@param version the string that will contain the version number of the transterpreter.
PROC tvm.get.version (RESULT []BYTE version)
  C.tvmspecial.5.get.version (version)
:

#IF FALSE
#PRAGMA EXTERNAL "PROC C.tvmspecial.7.exit (INT exit.code) = 0"
--* Set an exit code for the transterpreter
-- @param exit.code The exit code that you want to set.
--   This becomes the parameter to C's [@text exit()] function.
PROC tvm.exit (INT exit.code)
  C.tvmspecial.7.exit (exit.code)
:

#PRAGMA EXTERNAL "PROC C.tvmspecial.8.debug.str (VAL []BYTE debug.str) = 0"
--* Print a debug string from anywhere in your code
--@param debug.str the string that you want to print
PROC tvm.debug.str (VAL []BYTE debug.str)
  C.tvmspecial.8.debug.str (debug.str)
:

#PRAGMA EXTERNAL "PROC C.tvmspecial.9.debug.int (VAL INT debug.int) = 0"
--* Print a debug integer from anywhere in your code
--@param debug.int the integer that you want to print
PROC tvm.debug.int (VAL INT debug.int)
  C.tvmspecial.9.debug.int (debug.int)
:
#ENDIF

