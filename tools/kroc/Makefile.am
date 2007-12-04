## process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS=foreign no-dependencies

bin_SCRIPTS=kroc kmakef kmakemodule occbuild kroc-setup.sh kroc-setup.csh

dist_man1_MANS=kroc.1 kmakef.1

aclocaldir = @ACLOCALDIR@
aclocal_DATA = occbuild.m4

DISTCLEANFILES=kroc kroc-setup.sh kroc-setup.csh
EXTRA_DIST=kroc-setup.sh.in kroc-setup.csh.in kroc.in kmakemodule.in occbuild.in


