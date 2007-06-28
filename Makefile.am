## process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS=foreign no-dependencies

bin_SCRIPTS=kroc kmakef kmakemodule occbuild setup.sh setup.csh

install-exec-local: kroc.conf
	$(mkdir_p) "$(DESTDIR)$(sysconfdir)"
	$(INSTALL_DATA) "kroc.conf" "$(DESTDIR)$(sysconfdir)/@CONFFILEPREFIX@kroc.conf"

dist_man1_MANS=kroc.1 kmakef.1
dist_man5_MANS=kroc.conf.5

aclocaldir = @ACLOCALDIR@
aclocal_DATA = occbuild.m4

DISTCLEANFILES=kroc setup.sh setup.csh kroc.conf
EXTRA_DIST=kroc.conf.in setup.sh.in setup.csh.in kroc.in kmakemodule.in occbuild.in


