@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ --search ../maths --no-std-libs

libs = \
	occam8.lib

EXTRA_DIST = \
	bitcount.occ \
	brevnbit.occ \
	brevword.occ \
	clip2d.occ \
	crcbyte.occ \
	crcword.occ \
	dabsfint.occ \
	danalyse.occ \
	dargred.occ \
	ddivby2.occ \
	dcpysign.occ \
	dfunpack.occ \
	dlogb.occ \
	dminusx.occ \
	dmulby2.occ \
	dnotfnte.occ \
	dnxtaftr.occ \
	draw2d.occ \
	dscaleb.occ \
	dsqrt.occ \
	fracmul.occ \
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
	i32tor32.occ \
	i32tor64.occ \
	i64divrm.occ \
	i64gt.occ \
	i64mul.occ \
	i64tor32.occ \
	i64tor64.occ \
	ie32comp.occ \
	ie32op.occ \
	ie32rem.occ \
	ie64comp.occ \
	ie64op.occ \
	ie64rem.occ \
	move2d.occ \
	nan32.inc \
	occampi.occ \
	r32eq.occ \
	r32eqerr.occ \
	r32gt.occ \
	r32gterr.occ \
	r32op.occ \
	r32operr.occ \
	r32rem.occ \
	r32remer.occ \
	r32toi32.occ \
	r32toi64.occ \
	r32tor64.occ \
	r64eq.occ \
	r64eqerr.occ \
	r64gt.occ \
	r64gterr.occ \
	r64op.occ \
	r64operr.occ \
	r64rem.occ \
	r64remer.occ \
	r64toi32.occ \
	r64toi64.occ \
	r64tor32.occ \
	rabsfint.occ \
	rargred.occ \
	ranalyse.occ \
	rcpysign.occ \
	rdivby2.occ \
	realvals.inc \
	rfunpack.occ \
	ridiv.occ \
	rimul.occ \
	rlogb.occ \
	rmulby2.occ \
	rnxtaftr.occ \
	roundsn.occ \
	rsqrt.occ \
	rscaleb.occ \
	unpacksn.occ

# We have to build some objects before the bulk of the library, because the
# occam compiler expects to always be able to see the standard library.
occam8_early_objs = \
	ridiv.tce \
	rimul.tce \
	dargred.tce \
	rargred.tce \
	ie32op.tce \
	ie64op.tce \
	r32eq.tce \
	r32gt.tce \
	r64eq.tce \
	r64gt.tce

occam8_lib_objs = \
	$(occam8_early_objs) \
	bitcount.tce \
	brevnbit.tce \
	brevword.tce \
	clip2d.tce \
	crcbyte.tce \
	crcword.tce \
	dabsfint.tce \
	danalyse.tce \
	ddivby2.tce \
	dcpysign.tce \
	dfunpack.tce \
	dlogb.tce \
	dminusx.tce \
	dmulby2.tce \
	dnotfnte.tce \
	dnxtaftr.tce \
	draw2d.tce \
	dscaleb.tce \
	dsqrt.tce \
	fracmul.tce \
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
	i32tor32.tce \
	i32tor64.tce \
	i64divrm.tce \
	i64gt.tce \
	i64mul.tce \
	i64tor32.tce \
	i64tor64.tce \
	ie32comp.tce \
	ie32rem.tce \
	ie64comp.tce \
	ie64rem.tce \
	move2d.tce \
	occampi.tce \
	r32eqerr.tce \
	r32gterr.tce \
	r32op.tce \
	r32operr.tce \
	r32rem.tce \
	r32remer.tce \
	r32toi32.tce \
	r32toi64.tce \
	r32tor64.tce \
	r64eqerr.tce \
	r64gterr.tce \
	r64op.tce \
	r64operr.tce \
	r64rem.tce \
	r64remer.tce \
	r64toi32.tce \
	r64toi64.tce \
	r64tor32.tce \
	rabsfint.tce \
	ranalyse.tce \
	rcpysign.tce \
	rdivby2.tce \
	rfunpack.tce \
	rlogb.tce \
	rmulby2.tce \
	rnxtaftr.tce \
	roundsn.tce \
	rsqrt.tce \
	rscaleb.tce \
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
