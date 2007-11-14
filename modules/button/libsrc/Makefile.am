@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	button.occ \
	button.inc \
	font2.occ  \
	nine_by_fifteen.occ

button_lib_objs = \
	button.tce

all-local: button.lib

button.lib: $(button_lib_objs)
	$(OCCBUILD) --library button.lib --need occSDL --need occGL --need course --include button.inc $(button_lib_objs)

install-exec-local: button.lib button.inc
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" button.lib button.inc

clean-local:
	$(OCCBUILD) --clean button.lib $(button_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

## begin pideps dependencies
button.tce: $(srcdir)/button.inc
button.tce: $(srcdir)/font2.occ
button.tce: $(srcdir)/nine_by_fifteen.inc
## end pideps dependencies
