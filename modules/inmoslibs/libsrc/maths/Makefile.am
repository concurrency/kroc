@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ --search ../forall

libs = \
	snglmath.lib \
	dblmath.lib \
	maths.lib
includes = \
	mathhdr.inc \
	mathvals.inc

EXTRA_DIST = \
	acosfl.occ \
	alogfl.occ \
	asinfl.occ \
	atanfl.occ \
	cosfl.occ \
	dacosfl.occ \
	dalogfl.occ \
	dasinfl.occ \
	datanfl.occ \
	dcosfl.occ \
	dexpfl.occ \
	dhyperfl.occ \
	dpowfl.occ \
	dranfl.occ \
	dsinfl.occ \
	expfl.occ \
	hyperfl.occ \
	powfl.occ \
	ranfl.occ \
	sinfl.occ \
	tanfl.occ \
	$(includes)

snglmath_lib_objs = \
	acosfl.tce \
	alogfl.tce \
	asinfl.tce \
	atanfl.tce \
	cosfl.tce \
	expfl.tce \
	hyperfl.tce \
	powfl.tce \
	ranfl.tce \
	sinfl.tce \
	tanfl.tce

dblmath_lib_objs = \
	dacosfl.tce \
	dalogfl.tce \
	dasinfl.tce \
	datanfl.tce \
	dcosfl.tce \
	dexpfl.tce \
	dhyperfl.tce \
	dpowfl.tce \
	dranfl.tce \
	dsinfl.tce \
	dtanfl.tce

all-local: $(libs)

snglmath.lib: $(snglmath_lib_objs)
	$(OCCBUILD) --library snglmath.lib $(snglmath_lib_objs)

dblmath.lib: $(dblmath_lib_objs)
	$(OCCBUILD) --library dblmath.lib $(dblmath_lib_objs)

maths.lib: snglmath.lib dblmath.lib
	$(OCCBUILD) --library maths.lib --include mathhdr.inc --include mathvals.inc --need snglmath --need dblmath

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(snglmath_lib_objs) $(dblmath_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

## begin pideps dependencies
acosfl.occ: $(srcdir)/mathhdr.inc
alogfl.occ: $(srcdir)/mathhdr.inc
asinfl.occ: $(srcdir)/mathhdr.inc
atanfl.occ: $(srcdir)/mathhdr.inc
cosfl.occ: $(srcdir)/mathhdr.inc
dacosfl.occ: $(srcdir)/mathhdr.inc
dalogfl.occ: $(srcdir)/mathhdr.inc
dasinfl.occ: $(srcdir)/mathhdr.inc
datanfl.occ: $(srcdir)/mathhdr.inc
dcosfl.occ: $(srcdir)/mathhdr.inc
dexpfl.occ: $(srcdir)/mathhdr.inc
dhyperfl.occ: $(srcdir)/mathhdr.inc
dpowfl.occ: $(srcdir)/mathhdr.inc
dsinfl.occ: $(srcdir)/mathhdr.inc
dtanfl.occ: $(srcdir)/mathhdr.inc
expfl.occ: $(srcdir)/mathhdr.inc
hyperfl.occ: $(srcdir)/mathhdr.inc
powfl.occ: $(srcdir)/mathhdr.inc
sinfl.occ: $(srcdir)/mathhdr.inc
tanfl.occ: $(srcdir)/mathhdr.inc
## end pideps dependencies
