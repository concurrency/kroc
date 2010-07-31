## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS=foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

EXTRA_LIBRARIES = libLLVMTailCallFix.a
shared_libs = @LIBNAME@

libLLVMTailCallFix_a_SOURCES = \
	TailCallFix.cpp

all-local: libLLVMTailCallFix.a $(shared_libs)

@LIBNAME@: libLLVMTailCallFix.a
	$(CXX) $(SLINKFLAGS) $(CXXFLAGS) $(LDFLAGS) -o @LIBNAME@ $(libLLVMTailCallFix_a_OBJECTS) $(libLLVMTailCallFix_a_LIBADD)

install-exec-local: libLLVMTailCallFix.a $(shared_libs)
	$(mkinstalldirs) "$(DESTDIR)$(libdir)"
	$(INSTALL_DATA) @LIBNAME@ "$(DESTDIR)$(libdir)/@LIBNAME@"

CLEANFILES = libLLVMTailCallFix.a $(shared_libs) *~
