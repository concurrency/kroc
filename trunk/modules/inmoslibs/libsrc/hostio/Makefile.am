@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ --search ../forall --search ../convert

libs = \
	hostio.lib \
	solib.lib \
	splib.lib
includes = \
	$(srcdir)/hostio.inc \
	$(srcdir)/unixerrs.inc

EXTRA_DIST = \
	ask.occ \
	buffer.occ \
	close.occ \
	command.occ \
	conv.occ \
	echoline.occ \
	eof.occ \
	exists.occ \
	exit.occ \
	ferror.occ \
	flush.occ \
	fwstring.occ \
	getenv.occ \
	getkey.occ \
	gets.occ \
	mux.occ \
	open.occ \
	opentemp.occ \
	ovbuffer.occ \
	ovmux.occ \
	ovprimux.occ \
	packet.occ \
	parse.occ \
	pollkey.occ \
	popen.occ \
	primux.occ \
	puts.occ \
	readline.occ \
	reaint.occ \
	rehint.occ \
	rehint32.occ \
	rehint64.occ \
	reint.occ \
	reint32.occ \
	reint64.occ \
	remove.occ \
	rename.occ \
	rereal32.occ \
	rereal64.occ \
	seek.occ \
	soread.occ \
	sowrite.occ \
	spread.occ \
	spsocore.occ \
	spwrite.occ \
	system.occ \
	tell.occ \
	time.occ \
	times.occ \
	version.occ \
	whint.occ \
	whint32.occ \
	whint64.occ \
	wint.occ \
	wint32.occ \
	wint64.occ \
	wreal32.occ \
	wreal64.occ \
	wstring.occ \
	$(includes)

hostio_lib_objs = \
	ask.tce \
	buffer.tce \
	close.tce \
	command.tce \
	conv.tce \
	echoline.tce \
	eof.tce \
	exists.tce \
	exit.tce \
	ferror.tce \
	flush.tce \
	fwstring.tce \
	getenv.tce \
	getkey.tce \
	gets.tce \
	mux.tce \
	open.tce \
	opentemp.tce \
	ovbuffer.tce \
	ovmux.tce \
	ovprimux.tce \
	packet.tce \
	parse.tce \
	pollkey.tce \
	popen.tce \
	primux.tce \
	puts.tce \
	readline.tce \
	reaint.tce \
	rehint.tce \
	rehint32.tce \
	rehint64.tce \
	reint.tce \
	reint32.tce \
	reint64.tce \
	remove.tce \
	rename.tce \
	rereal32.tce \
	rereal64.tce \
	seek.tce \
	soread.tce \
	sowrite.tce \
	spread.tce \
	spsocore.tce \
	spwrite.tce \
	system.tce \
	tell.tce \
	time.tce \
	times.tce \
	version.tce \
	whint.tce \
	whint32.tce \
	whint64.tce \
	wint.tce \
	wint32.tce \
	wint64.tce \
	wreal32.tce \
	wreal64.tce \
	wstring.tce

solib_lib_objs = \
	echoline.tce \
	exists.tce \
	fwstring.tce \
	opentemp.tce \
	parse.tce \
	popen.tce \
	readline.tce \
	times.tce \
	whint.tce \
	whint64.tce \
	wint.tce \
	wint64.tce \
	wreal32.tce \
	wreal64.tce \
	wstring.tce

splib_lib_objs = \
	buffer.tce \
	close.tce \
	command.tce \
	conv.tce \
	eof.tce \
	exit.tce \
	ferror.tce \
	flush.tce \
	getenv.tce \
	getkey.tce \
	gets.tce \
	mux.tce \
	open.tce \
	ovbuffer.tce \
	ovmux.tce \
	packet.tce \
	pollkey.tce \
	puts.tce \
	remove.tce \
	rename.tce \
	seek.tce \
	soread.tce \
	sowrite.tce \
	spread.tce \
	spsocore.tce \
	spwrite.tce \
	system.tce \
	tell.tce \
	time.tce \
	version.tce

all-local: $(libs)

hostio.lib: $(hostio_lib_objs)
	$(OCCBUILD) --library hostio.lib --need convert --include hostio.inc $(hostio_lib_objs)

solib.lib: $(solib_lib_objs)
	$(OCCBUILD) --library solib.lib --need convert --need splib --include hostio.inc $(solib_lib_objs)

splib.lib: $(splib_lib_objs)
	$(OCCBUILD) --library splib.lib --need convert --include hostio.inc $(splib_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(hostio_lib_objs) $(solib_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

## begin pideps dependencies
ask.tce: $(srcdir)/hostio.inc
ask.tce: solib.lib
ask.tce: splib.lib
buffer.tce: $(srcdir)/hostio.inc
close.tce: $(srcdir)/hostio.inc
close.tce: $(srcdir)/unixerrs.inc
command.tce: $(srcdir)/hostio.inc
command.tce: conv.tce
echoline.tce: $(srcdir)/hostio.inc
echoline.tce: splib.lib
eof.tce: $(srcdir)/hostio.inc
exists.tce: $(srcdir)/hostio.inc
exists.tce: splib.lib
exit.tce: $(srcdir)/hostio.inc
ferror.tce: $(srcdir)/hostio.inc
flush.tce: $(srcdir)/hostio.inc
flush.tce: $(srcdir)/unixerrs.inc
fwstring.tce: $(srcdir)/hostio.inc
fwstring.tce: splib.lib
getenv.tce: $(srcdir)/hostio.inc
getenv.tce: conv.tce
getkey.tce: $(srcdir)/hostio.inc
gets.tce: $(srcdir)/hostio.inc
gets.tce: $(srcdir)/unixerrs.inc
gets.tce: conv.tce
mux.tce: $(srcdir)/hostio.inc
open.tce: $(srcdir)/hostio.inc
open.tce: $(srcdir)/unixerrs.inc
open.tce: conv.tce
opentemp.tce: $(srcdir)/hostio.inc
opentemp.tce: splib.lib
ovbuffer.tce: $(srcdir)/hostio.inc
ovmux.tce: $(srcdir)/hostio.inc
ovprimux.tce: $(srcdir)/hostio.inc
packet.tce: $(srcdir)/hostio.inc
parse.tce: $(srcdir)/hostio.inc
parse.tce: splib.lib
pollkey.tce: $(srcdir)/hostio.inc
popen.tce: $(srcdir)/hostio.inc
popen.tce: splib.lib
primux.tce: $(srcdir)/hostio.inc
puts.tce: $(srcdir)/hostio.inc
puts.tce: spwrite.tce
readline.tce: $(srcdir)/hostio.inc
readline.tce: splib.lib
reaint.tce: $(srcdir)/hostio.inc
reaint.tce: solib.lib
rehint.tce: $(srcdir)/hostio.inc
rehint.tce: solib.lib
rehint32.tce: $(srcdir)/hostio.inc
rehint32.tce: solib.lib
rehint64.tce: $(srcdir)/hostio.inc
rehint64.tce: solib.lib
reint.tce: $(srcdir)/hostio.inc
reint.tce: solib.lib
reint32.tce: $(srcdir)/hostio.inc
reint32.tce: solib.lib
reint64.tce: $(srcdir)/hostio.inc
reint64.tce: solib.lib
remove.tce: $(srcdir)/hostio.inc
remove.tce: conv.tce
rename.tce: $(srcdir)/hostio.inc
rename.tce: conv.tce
rereal32.tce: $(srcdir)/hostio.inc
rereal32.tce: solib.lib
rereal64.tce: $(srcdir)/hostio.inc
rereal64.tce: solib.lib
seek.tce: $(srcdir)/hostio.inc
soread.tce: $(srcdir)/hostio.inc
soread.tce: spread.tce
sowrite.tce: $(srcdir)/hostio.inc
sowrite.tce: spwrite.tce
spread.tce: $(srcdir)/hostio.inc
spread.tce: $(srcdir)/unixerrs.inc
spread.tce: conv.tce
spsocore.tce: $(srcdir)/hostio.inc
spwrite.tce: $(srcdir)/hostio.inc
spwrite.tce: $(srcdir)/unixerrs.inc
spwrite.tce: conv.tce
system.tce: $(srcdir)/hostio.inc
system.tce: conv.tce
tell.tce: $(srcdir)/hostio.inc
time.tce: $(srcdir)/hostio.inc
times.tce: $(srcdir)/hostio.inc
times.tce: splib.lib
version.tce: $(srcdir)/hostio.inc
version.tce: $(srcdir)/unixerrs.inc
whint.tce: $(srcdir)/hostio.inc
whint.tce: splib.lib
whint32.tce: $(srcdir)/hostio.inc
whint32.tce: splib.lib
whint64.tce: $(srcdir)/hostio.inc
whint64.tce: splib.lib
wint.tce: $(srcdir)/hostio.inc
wint.tce: splib.lib
wint32.tce: $(srcdir)/hostio.inc
wint32.tce: splib.lib
wint64.tce: $(srcdir)/hostio.inc
wint64.tce: splib.lib
wreal32.tce: $(srcdir)/hostio.inc
wreal32.tce: splib.lib
wreal64.tce: $(srcdir)/hostio.inc
wreal64.tce: splib.lib
wstring.tce: $(srcdir)/hostio.inc
wstring.tce: splib.lib
## end pideps dependencies
