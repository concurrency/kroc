@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ --search ../maths --no-std-libs

libs = \
	occam8.lib

EXTRA_DIST = \
	bitops.occ \
	cosfl.occ \
	crcops.occ \
	dargred.occ \
	dcosfl.occ \
	dcpysign.occ \
	dfunpack.occ \
	dlogb.occ \
	dnxtaftr.occ \
	dscaleb.occ \
	dsinfl.occ \
	i16add.occ \
	i16band.occ \
	i16bnot.occ \
	i16bor.occ \
	i16div.occ \
	i16eq.occ \
	i16gt.occ \
	i16lshft.occ \
	i16minus.occ \
	i16mul.occ \
	i16plus.occ \
	i16rem.occ \
	i16rshft.occ \
	i16sub.occ \
	i16times.occ \
	i16xor.occ \
	i64divrm.occ \
	i64mul.occ \
	ie32comp.occ \
	ie32op.occ \
	ie32rem.occ \
	ie64comp.occ \
	ie64op.occ \
	ie64rem.occ \
	occampi.occ \
	r32eq.occ \
	r32gt.occ \
	r32op.occ \
	r32rem.occ \
	r64eq.occ \
	r64gt.occ \
	r64op.occ \
	r64rem.occ \
	rargred.occ \
	rcpysign.occ \
	rfunpack.occ \
	ridiv.occ \
	rimul.occ \
	rlogb.occ \
	rnxtaftr.occ \
	roundsn.occ \
	rscaleb.occ \
	sinfl.occ \
	unpacksn.occ

# We have to build some objects before the bulk of the library, because the
# occam compiler expects to always be able to see the standard library.
occam8_early_objs = \
	ridiv.tce \
	rimul.tce \
	dargred.tce \
	ie32op.tce \
	ie64op.tce \
	r32eq.tce \
	r32gt.tce \
	r64eq.tce \
	r64gt.tce

occam8_lib_objs = \
	$(occam8_early_objs) \
	bitops.tce \
	cosfl.tce \
	crcops.tce \
	dcosfl.tce \
	dcpysign.tce \
	dfunpack.tce \
	dlogb.tce \
	dnxtaftr.tce \
	dscaleb.tce \
	dsinfl.tce \
	i16add.tce \
	i16band.tce \
	i16bnot.tce \
	i16bor.tce \
	i16div.tce \
	i16eq.tce \
	i16gt.tce \
	i16lshft.tce \
	i16minus.tce \
	i16mul.tce \
	i16plus.tce \
	i16rem.tce \
	i16rshft.tce \
	i16sub.tce \
	i16times.tce \
	i16xor.tce \
	i64divrm.tce \
	i64mul.tce \
	ie32comp.tce \
	ie32rem.tce \
	ie64comp.tce \
	ie64rem.tce \
	occampi.tce \
	r32op.tce \
	r32rem.tce \
	r64op.tce \
	r64rem.tce \
	rargred.tce \
	rcpysign.tce \
	rfunpack.tce \
	rlogb.tce \
	rnxtaftr.tce \
	roundsn.tce \
	rscaleb.tce \
	sinfl.tce \
	unpacksn.tce

all-local: occam8-real $(libs)

# As above: this builds a "early" version of the runtime library containing
# just the stuff that the rest of the library needs.
occam8-early: $(occam8_early_objs)
	$(OCCBUILD) --library occam8.lib $(occam8_early_objs)
	touch occam8-early

occam8-real: occam8-early $(occam8_lib_objs)
	$(OCCBUILD) --library occam8.lib $(occam8_lib_objs)
	touch occam8-real

occam8.lib: occam8-real

install-exec-local: $(libs)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs)

clean-local:
	$(OCCBUILD) --clean $(libs) $(occam8_lib_objs)
	rm -f occam8-early occam8-real

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

## begin pideps dependencies
cosfl.tce: $(srcdir)/../maths/mathhdr.inc
dargred.tce: ridiv.tce rimul.tce
dcosfl.tce: $(srcdir)/../maths/mathhdr.inc
dsinfl.tce: $(srcdir)/../maths/mathhdr.inc
ie64op.tce: ridiv.tce rimul.tce
r64op.tce: ridiv.tce rimul.tce
sinfl.tce: $(srcdir)/../maths/mathhdr.inc
## end pideps dependencies
