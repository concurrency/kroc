@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ --search ../forall --search ../convert

EXTRA_DIST = \
	string0.occ \
	string1.occ \
	string2.occ \
	string3.occ \
	string4.occ \
	string5.occ

string_lib_objs = \
	string0.tce \
	string1.tce \
	string2.tce \
	string3.tce \
	string4.tce \
	string5.tce

all-local: string.lib

string.lib: $(string_lib_objs)
	$(OCCBUILD) --library string.lib --need convert $(string_lib_objs)

install-exec-local: string.lib
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" string.lib

clean-local:
	$(OCCBUILD) --clean string.lib $(string_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

## begin pideps dependencies
## end pideps dependencies
