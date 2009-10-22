@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ --search ../forall

libs = \
	snglmath.lib \
	dblmath.lib \
	maths.lib
includes = \
	$(srcdir)/mathhdr.inc \
	$(srcdir)/mathvals.inc

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
	dtanfl.occ \
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
acosfl.tce: $(srcdir)/mathhdr.inc
alogfl.tce: $(srcdir)/mathhdr.inc
asinfl.tce: $(srcdir)/mathhdr.inc
atanfl.tce: $(srcdir)/mathhdr.inc
cosfl.tce: $(srcdir)/mathhdr.inc
dacosfl.tce: $(srcdir)/mathhdr.inc
dalogfl.tce: $(srcdir)/mathhdr.inc
dasinfl.tce: $(srcdir)/mathhdr.inc
datanfl.tce: $(srcdir)/mathhdr.inc
dcosfl.tce: $(srcdir)/mathhdr.inc
dexpfl.tce: $(srcdir)/mathhdr.inc
dhyperfl.tce: $(srcdir)/mathhdr.inc
dpowfl.tce: $(srcdir)/mathhdr.inc
dsinfl.tce: $(srcdir)/mathhdr.inc
dtanfl.tce: $(srcdir)/mathhdr.inc
expfl.tce: $(srcdir)/mathhdr.inc
hyperfl.tce: $(srcdir)/mathhdr.inc
powfl.tce: $(srcdir)/mathhdr.inc
sinfl.tce: $(srcdir)/mathhdr.inc
tanfl.tce: $(srcdir)/mathhdr.inc
## end pideps dependencies
