## process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS=foreign no-dependencies

bin_SCRIPTS=kroc kmakef kmakemodule occbuild setup.sh setup.csh

dist_man1_MANS=kroc.1 kmakef.1

aclocaldir = @ACLOCALDIR@
aclocal_DATA = occbuild.m4

DISTCLEANFILES=kroc setup.sh setup.csh
EXTRA_DIST=setup.sh.in setup.csh.in kroc.in kmakemodule.in occbuild.in


