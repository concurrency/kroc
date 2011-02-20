
AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

SOURCES = \
	tenclib.py \
	tencdump.py \
	tencid.py \
	tencstrip.py

generated = \
	tencdump \
	tencid \
	tencstrip

CLEANFILES = $(generated)

bin_SCRIPTS = $(generated) 
pkgpython_PYTHON = tenclib.py

all-local: $(generated)

%: %.py
	sed -e 's|@pkgpythondir[@]|$(pkgpythondir)|g' $< > $@

$(generated): Makefile
