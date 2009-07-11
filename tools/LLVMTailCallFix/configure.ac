AC_INIT([LLVMTailCallFix], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AC_PROG_RANLIB
AC_PROG_MAKE_SET
AC_CHECK_TOOL(LD,ld)

AM_C_PROTOTYPES

case "$host_os" in
cygwin*)
 LIBNAME="LLVMTailCallFix.dll"
 ;;
*Darwin* | *darwin*)
 LIBNAME="libLLVMTailCallFix.dylib"
 ;;
*)
 SLINKFLAGS="-shared"
 LIBNAME="libLLVMTailCallFix.so"
 ;;
esac

CXXFLAGS="`llvm-config --cxxflags` $CXXFLAGS"
LDFLAGS="`llvm-config --ldflags` $LDFLAGS"
LIBS="`llvm-config --libs core` $LIBS"

AC_SUBST(SLINKFLAGS)
AC_SUBST(LIBNAME)

AC_OUTPUT([Makefile])

