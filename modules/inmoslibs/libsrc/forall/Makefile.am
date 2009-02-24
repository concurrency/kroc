@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ --search ../maths --no-std-libs

libs = \
	forall.lib

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
	dcpysign.occ \
	ddivby2.occ \
	dfunpack.occ \
	dlogb.occ \
	dminusx.occ \
	dmulby2.occ \
	dnotfnte.occ \
	dnxtaftr.occ \
	draw2d.occ \
	dscaleb.occ \
	dsqrt.occ \
	endian.inc \
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
	i16toi32.occ \
	i16toi64.occ \
	i16tor32.occ \
	i16tor64.occ \
	i16xor.occ \
	i32add.occ \
	i32band.occ \
	i32bnot.occ \
	i32bor.occ \
	i32divrm.occ \
	i32eq.occ \
	i32gt.occ \
	i32lshft.occ \
	i32minus.occ \
	i32mul.occ \
	i32plus.occ \
	i32rshft.occ \
	i32sub.occ \
	i32times.occ \
	i32toi16.occ \
	i32toi64.occ \
	i32tor32.occ \
	i32tor64.occ \
	i32xor.occ \
	i64add.occ \
	i64band.occ \
	i64bnot.occ \
	i64bor.occ \
	i64divrm.occ \
	i64eq.occ \
	i64gt.occ \
	i64lshft.occ \
	i64minus.occ \
	i64mul.occ \
	i64plus.occ \
	i64rshft.occ \
	i64sub.occ \
	i64times.occ \
	i64toi16.occ \
	i64toi32.occ \
	i64tor32.occ \
	i64tor64.occ \
	i64xor.occ \
	ie32comp.occ \
	ie32op.occ \
	ie32rem.occ \
	ie64comp.occ \
	ie64op.occ \
	ie64rem.occ \
	intrinsics.occ \
	move2d.occ \
	nan16.inc \
	nan32.inc \
	occampi.occ \
	qnorm.occ \
	qridiv.occ \
	qrimul.occ \
	qshiftl.occ \
	qshiftr.occ \
	r32eqerr.occ \
	r32eq.occ \
	r32gterr.occ \
	r32gt.occ \
	r32operr.occ \
	r32op.occ \
	r32remer.occ \
	r32rem.occ \
	r32toi16.occ \
	r32toi32.occ \
	r32toi64.occ \
	r32tor64.occ \
	r64eqerr.occ \
	r64eq.occ \
	r64gterr.occ \
	r64gt.occ \
	r64operr.occ \
	r64op.occ \
	r64remer.occ \
	r64rem.occ \
	r64toi16.occ \
	r64toi32.occ \
	r64toi64.occ \
	r64tor32.occ \
	rabsfint.occ \
	ranalyse.occ \
	rargred.occ \
	rcpysign.occ \
	rdivby2.occ \
	realvals.inc \
	rfunpack.occ \
	ridiv.occ \
	rimul.occ \
	rlogb.occ \
	rminusx.occ \
	rmulby2.occ \
	rnotfnte.occ \
	rnxtaftr.occ \
	roundsn.occ \
	rscaleb.occ \
	rsqrt.occ \
	unpacksn.occ 

# We have to build some objects before the bulk of the library, because the
# occam compiler expects to always be able to see the standard library.
forall_early_objs = \
	ridiv.tce \
	rimul.tce \
	qridiv.tce \
	qrimul.tce \
	qshiftl.tce \
	qshiftr.tce \
	qnorm.tce \
	dargred.tce \
	rargred.tce \
	ie32op.tce \
	ie64op.tce \
	r32eq.tce \
	r32gt.tce \
	r64eq.tce \
	r64gt.tce

forall_late_objs = \
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
	i16toi32.tce \
	i16toi64.tce \
	i16tor32.tce \
	i16tor64.tce \
	i32add.tce \
	i32band.tce \
	i32bnot.tce \
	i32bor.tce \
	i32divrm.tce \
	i32eq.tce \
	i32gt.tce \
	i32lshft.tce \
	i32minus.tce \
	i32mul.tce \
	i32plus.tce \
	i32rshft.tce \
	i32sub.tce \
	i32times.tce \
	i32toi16.tce \
	i32toi64.tce \
	i32tor32.tce \
	i32tor64.tce \
	i64add.tce \
	i64band.tce \
	i64bnot.tce \
	i64bor.tce \
	i64divrm.tce \
	i64eq.tce \
	i64gt.tce \
	i64lshft.tce \
	i64minus.tce \
	i64mul.tce \
	i64plus.tce \
	i64rshft.tce \
	i64sub.tce \
	i64times.tce \
	i64toi16.tce \
	i64toi32.tce \
	i64tor32.tce \
	i64tor64.tce \
	i64xor.tce \
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
	r32toi16.tce \
	r32toi32.tce \
	r32toi64.tce \
	r32tor64.tce \
	r64eqerr.tce \
	r64gterr.tce \
	r64op.tce \
	r64operr.tce \
	r64rem.tce \
	r64remer.tce \
	r64toi16.tce \
	r64toi32.tce \
	r64toi64.tce \
	r64tor32.tce \
	rabsfint.tce \
	ranalyse.tce \
	rcpysign.tce \
	rdivby2.tce \
	rfunpack.tce \
	rlogb.tce \
	rminusx.tce \
	rnotfnte.tce \
	rmulby2.tce \
	rnxtaftr.tce \
	roundsn.tce \
	rsqrt.tce \
	rscaleb.tce \
	unpacksn.tce

forall_lib_objs = \
	$(forall_early_objs) \
	$(forall_late_objs)

all-local: forall-real $(libs)

# As above: this builds a "early" version of the runtime library containing
# just the stuff that the rest of the library needs.
forall-early: $(forall_early_objs)
	$(OCCBUILD) --library forall.lib $(forall_early_objs)
	touch forall-early

forall-real: $(forall_lib_objs)
	$(OCCBUILD) --library forall.lib $(forall_lib_objs)
	touch forall-real

forall.lib: forall-real

install-exec-local: $(libs)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs)

clean-local:
	$(OCCBUILD) --clean $(libs) $(forall_lib_objs)
	rm -f forall-early forall-real

SUFFIXES = .occ .tce .lib

$(forall_late_objs): forall-early

.occ.tce:
	$(OCCBUILD) --object $<

## begin pideps dependencies
dargred.tce: ridiv.tce rimul.tce
ie64op.tce: ridiv.tce rimul.tce
r64op.tce: ridiv.tce rimul.tce
rargred.tce: ridiv.tce
## end pideps dependencies
