@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ --search ../forall

libs = \
	convert.lib \
	ioconv.lib

EXTRA_DIST = \
	boltostr.occ \
	hex16ta.occ \
	hex32ta.occ \
	hex64ta.occ \
	hextostr.occ \
	i64strta.occ \
	int16ta.occ \
	int32ta.occ \
	inttostr.occ \
	r32strta.occ \
	r64strta.occ \
	stri64ta.occ \
	strr32ta.occ \
	strr64ta.occ \
	strtobol.occ \
	strtohex.occ \
	strtoint.occ \
	validch.occ

convert_lib_objs = \
	boltostr.tce \
	hex16ta.tce \
	hex32ta.tce \
	hex64ta.tce \
	hextostr.tce \
	i64strta.tce \
	int16ta.tce \
	int32ta.tce \
	inttostr.tce \
	r32strta.tce \
	r64strta.tce \
	stri64ta.tce \
	strr32ta.tce \
	strr64ta.tce \
	strtobol.tce \
	strtohex.tce \
	strtoint.tce

ioconv_lib_objs = \
	boltostr.tce \
	hextostr.tce \
	inttostr.tce \
	strtobol.tce \
	strtohex.tce \
	strtoint.tce

all-local: $(libs)

convert.lib: $(convert_lib_objs)
	$(OCCBUILD) --library convert.lib $(convert_lib_objs)

ioconv.lib: $(ioconv_lib_objs)
	$(OCCBUILD) --library ioconv.lib $(ioconv_lib_objs)

install-exec-local: $(libs)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs)

clean-local:
	$(OCCBUILD) --clean $(libs) $(convert_lib_objs) $(ioconv_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

## begin pideps dependencies
hex16ta.tce: ioconv.lib
hex32ta.tce: ioconv.lib
hex64ta.tce: $(srcdir)/validch.occ
hex64ta.tce: ioconv.lib
int16ta.tce: ioconv.lib
int32ta.tce: ioconv.lib
stri64ta.tce: $(srcdir)/validch.occ
strtohex.tce: $(srcdir)/validch.occ
strtoint.tce: $(srcdir)/validch.occ
## end pideps dependencies
