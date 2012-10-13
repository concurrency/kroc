AC_INIT([LLVMTailCallFix], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AC_PROG_RANLIB
AC_PROG_MAKE_SET
AC_CHECK_TOOL(LD,ld)

dnl Check for LLVM.
AC_CHECK_TOOL([LLVM_CONFIG], [llvm-config], [no])
if test "x$LLVM_CONFIG" = "xno"; then
  AC_MSG_ERROR([LLVM not found]) # FIXME: check version
fi

dnl Configure output name and flags. 
case "$host_os" in
cygwin*)
 LIBNAME="LLVMTailCallFix.dll"
 ;;
*Darwin* | *darwin*)
 SLINKFLAGS="-Wl,-flat_namespace -Wl,-undefined -Wl,suppress -dynamiclib"
 LIBNAME="libLLVMTailCallFix.dylib"
 ;;
*)
 SLINKFLAGS="-shared"
 LIBNAME="libLLVMTailCallFix.so"
 ;;
esac

CXXFLAGS="`$LLVM_CONFIG --cxxflags` $CXXFLAGS"
LDFLAGS="`$LLVM_CONFIG --ldflags` $LDFLAGS"
LIBS="`$LLVM_CONFIG --libs core` $LIBS"

dnl Make sure binary matches LLVM's host target.
LLVM_TARGET="`$LLVM_CONFIG --host-target`"
case "$LLVM_TARGET" in
 i*86-*)
  CXXFLAGS="-m32 $CXXFLAGS"
  ;;
esac

AC_SUBST(SLINKFLAGS)
AC_SUBST(LIBNAME)

AC_OUTPUT([Makefile])

