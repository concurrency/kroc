@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ --search ../forall --search ../convert

libs = \
	hostio.lib \
	solib.lib \
	splib.lib
includes = \
	hostio.inc \
	unixerrs.inc

EXTRA_DIST = \
	ask.occ \
	buffer.occ \
	close.occ \
	command.occ \
	conv.occ \
	core.occ \
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
	core.tce \
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
	core.tce \
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
ask.occ: $(srcdir)/hostio.inc
ask.occ: solib.lib
ask.occ: splib.lib
buffer.occ: $(srcdir)/hostio.inc
close.occ: $(srcdir)/hostio.inc
close.occ: $(srcdir)/unixerrs.inc
command.occ: $(srcdir)/hostio.inc
command.occ: conv.tce
core.occ: $(srcdir)/hostio.inc
echoline.occ: $(srcdir)/hostio.inc
echoline.occ: splib.lib
eof.occ: $(srcdir)/hostio.inc
exists.occ: $(srcdir)/hostio.inc
exists.occ: splib.lib
exit.occ: $(srcdir)/hostio.inc
ferror.occ: $(srcdir)/hostio.inc
flush.occ: $(srcdir)/hostio.inc
flush.occ: $(srcdir)/unixerrs.inc
fwstring.occ: $(srcdir)/hostio.inc
fwstring.occ: splib.lib
getenv.occ: $(srcdir)/hostio.inc
getenv.occ: conv.tce
getkey.occ: $(srcdir)/hostio.inc
gets.occ: $(srcdir)/hostio.inc
gets.occ: $(srcdir)/unixerrs.inc
gets.occ: conv.tce
mux.occ: $(srcdir)/hostio.inc
open.occ: $(srcdir)/hostio.inc
open.occ: $(srcdir)/unixerrs.inc
open.occ: conv.tce
opentemp.occ: $(srcdir)/hostio.inc
opentemp.occ: splib.lib
ovbuffer.occ: $(srcdir)/hostio.inc
ovmux.occ: $(srcdir)/hostio.inc
ovprimux.occ: $(srcdir)/hostio.inc
packet.occ: $(srcdir)/hostio.inc
parse.occ: $(srcdir)/hostio.inc
parse.occ: splib.lib
pollkey.occ: $(srcdir)/hostio.inc
popen.occ: $(srcdir)/hostio.inc
popen.occ: splib.lib
primux.occ: $(srcdir)/hostio.inc
puts.occ: $(srcdir)/hostio.inc
puts.occ: spwrite.tce
readline.occ: $(srcdir)/hostio.inc
readline.occ: splib.lib
reaint.occ: $(srcdir)/hostio.inc
reaint.occ: solib.lib
rehint.occ: $(srcdir)/hostio.inc
rehint.occ: solib.lib
rehint32.occ: $(srcdir)/hostio.inc
rehint32.occ: solib.lib
rehint64.occ: $(srcdir)/hostio.inc
rehint64.occ: solib.lib
reint.occ: $(srcdir)/hostio.inc
reint.occ: solib.lib
reint32.occ: $(srcdir)/hostio.inc
reint32.occ: solib.lib
reint64.occ: $(srcdir)/hostio.inc
reint64.occ: solib.lib
remove.occ: $(srcdir)/hostio.inc
remove.occ: conv.tce
rename.occ: $(srcdir)/hostio.inc
rename.occ: conv.tce
rereal32.occ: $(srcdir)/hostio.inc
rereal32.occ: solib.lib
rereal64.occ: $(srcdir)/hostio.inc
rereal64.occ: solib.lib
seek.occ: $(srcdir)/hostio.inc
soread.occ: $(srcdir)/hostio.inc
soread.occ: spread.tce
sowrite.occ: $(srcdir)/hostio.inc
sowrite.occ: spwrite.tce
spread.occ: $(srcdir)/hostio.inc
spread.occ: $(srcdir)/unixerrs.inc
spread.occ: conv.tce
spwrite.occ: $(srcdir)/hostio.inc
spwrite.occ: $(srcdir)/unixerrs.inc
spwrite.occ: conv.tce
system.occ: $(srcdir)/hostio.inc
system.occ: conv.tce
tell.occ: $(srcdir)/hostio.inc
time.occ: $(srcdir)/hostio.inc
times.occ: $(srcdir)/hostio.inc
times.occ: splib.lib
version.occ: $(srcdir)/hostio.inc
version.occ: $(srcdir)/unixerrs.inc
whint.occ: $(srcdir)/hostio.inc
whint.occ: splib.lib
whint32.occ: $(srcdir)/hostio.inc
whint32.occ: splib.lib
whint64.occ: $(srcdir)/hostio.inc
whint64.occ: splib.lib
wint.occ: $(srcdir)/hostio.inc
wint.occ: splib.lib
wint32.occ: $(srcdir)/hostio.inc
wint32.occ: splib.lib
wint64.occ: $(srcdir)/hostio.inc
wint64.occ: splib.lib
wreal32.occ: $(srcdir)/hostio.inc
wreal32.occ: splib.lib
wreal64.occ: $(srcdir)/hostio.inc
wreal64.occ: splib.lib
wstring.occ: $(srcdir)/hostio.inc
wstring.occ: splib.lib
## end pideps dependencies
