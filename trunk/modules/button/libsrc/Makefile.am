@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	button.occ \
	button.inc \
	font2.occ  \
	nine_by_fifteen.inc

if BUILD_BUTTON
button_lib_objs = \
	button.tce

button_libs = button.lib
button_includes = $(srcdir)/button.inc
endif

libs = $(button_libs)
includes = $(button_includes)

all-local: $(libs) $(includes)

button.lib: $(button_lib_objs)
	$(OCCBUILD) --library button.lib --need occSDL --need occGL --need course --include button.inc $(button_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(button_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

## begin pideps dependencies
button.tce: $(srcdir)/button.inc
button.tce: $(srcdir)/font2.occ
button.tce: $(srcdir)/nine_by_fifteen.inc
## end pideps dependencies
