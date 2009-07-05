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

forall_phase1_objs = \
	danalyse.tce \
	ie32op.tce \
	ranalyse.tce \
	ridiv.tce \
	rimul.tce \
	qridiv.tce \
	qrimul.tce \
	qshiftl.tce \
	qshiftr.tce \
	qnorm.tce

forall_phase2_objs = \
	dnotfnte.tce \
	i16toi32.tce \
	i16toi64.tce \
	i16tor32.tce \
	i16tor64.tce \
	i32toi16.tce \
	i32toi64.tce \
	i32tor32.tce \
	i32tor64.tce \
	i64add.tce \
	i64band.tce \
	i64bnot.tce \
	i64bor.tce \
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
	ie64op.tce \
	r32eq.tce \
	r32eqerr.tce \
	r32gt.tce \
	r32gterr.tce \
	r32op.tce \
	r32operr.tce \
	r32rem.tce \
	r32remer.tce \
	r32toi16.tce \
	r32toi32.tce \
	r32toi64.tce \
	r32tor64.tce \
	r64eq.tce \
	r64eqerr.tce \
	r64gt.tce \
	r64gterr.tce \
	r64op.tce \
	r64operr.tce \
	r64rem.tce \
	r64remer.tce \
	r64toi16.tce \
	r64toi32.tce \
	r64toi64.tce \
	r64tor32.tce \
	rnotfnte.tce \
	roundsn.tce \
	unpacksn.tce

forall_phase3_objs = \
	bitcount.tce \
	brevnbit.tce \
	brevword.tce \
	clip2d.tce \
	crcbyte.tce \
	crcword.tce \
	dabsfint.tce \
	dargred.tce \
	dcpysign.tce \
	ddivby2.tce \
	dfunpack.tce \
	dlogb.tce \
	dminusx.tce \
	dmulby2.tce \
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
	i64divrm.tce \
	ie32comp.tce \
	ie32rem.tce \
	ie64comp.tce \
	ie64rem.tce \
	move2d.tce \
	occampi.tce \
	rabsfint.tce \
	rargred.tce \
	rcpysign.tce \
	rdivby2.tce \
	rfunpack.tce \
	rlogb.tce \
	rminusx.tce \
	rmulby2.tce \
	rnxtaftr.tce \
	rscaleb.tce \
	rsqrt.tce

forall_lib_objs = \
	$(forall_phase1_objs) \
	$(forall_phase2_objs) \
	$(forall_phase3_objs)

all-local: forall-real $(libs)

forall-phase1: $(forall_phase1_objs)
	$(OCCBUILD) --library forall.lib $(forall_phase1_objs)
	touch forall-phase1

forall-phase2: $(forall_phase2_objs)
	$(OCCBUILD) --library forall.lib $(forall_phase1_objs) $(forall_phase2_objs)
	touch forall-phase2

forall-real: $(forall_lib_objs)
	$(OCCBUILD) --library forall.lib $(forall_lib_objs)
	touch forall-real

forall.lib: forall-real

install-exec-local: $(libs)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs)

clean-local:
	$(OCCBUILD) --clean $(libs) $(forall_lib_objs)
	rm -f forall-phase1 forall-phase2 forall-real

SUFFIXES = .occ .tce .lib

$(forall_phase2_objs): forall-phase1
$(forall_phase3_objs): forall-phase2

.occ.tce:
	$(OCCBUILD) --object $<

## begin pideps dependencies
dabsfint.tce: $(srcdir)/realvals.inc
danalyse.tce: $(srcdir)/realvals.inc
dargred.tce: $(srcdir)/endian.inc
dargred.tce: $(srcdir)/nan16.inc
dargred.tce: $(srcdir)/nan32.inc
dargred.tce: $(srcdir)/realvals.inc
dargred.tce: qnorm.tce
dargred.tce: qridiv.tce
dargred.tce: qshiftr.tce
dargred.tce: ridiv.tce
dcpysign.tce: $(srcdir)/endian.inc
dfunpack.tce: $(srcdir)/endian.inc
dfunpack.tce: $(srcdir)/nan16.inc
dfunpack.tce: $(srcdir)/nan32.inc
dfunpack.tce: $(srcdir)/realvals.inc
dlogb.tce: $(srcdir)/endian.inc
dlogb.tce: $(srcdir)/realvals.inc
dnotfnte.tce: $(srcdir)/realvals.inc
dnxtaftr.tce: $(srcdir)/endian.inc
dnxtaftr.tce: $(srcdir)/realvals.inc
dscaleb.tce: $(srcdir)/endian.inc
dscaleb.tce: $(srcdir)/realvals.inc
dsqrt.tce: $(srcdir)/nan16.inc
dsqrt.tce: $(srcdir)/nan32.inc
dsqrt.tce: $(srcdir)/realvals.inc
i16tor32.tce: $(srcdir)/realvals.inc
i16tor64.tce: $(srcdir)/realvals.inc
i32tor32.tce: $(srcdir)/realvals.inc
i32tor64.tce: $(srcdir)/realvals.inc
i64toi32.tce: $(srcdir)/realvals.inc
i64tor32.tce: $(srcdir)/realvals.inc
i64tor64.tce: $(srcdir)/realvals.inc
i64tor64.tce: qnorm.tce
ie32op.tce: $(srcdir)/nan16.inc
ie32op.tce: $(srcdir)/nan32.inc
ie32op.tce: $(srcdir)/realvals.inc
ie32op.tce: ridiv.tce
ie32op.tce: rimul.tce
ie32rem.tce: $(srcdir)/nan16.inc
ie32rem.tce: $(srcdir)/nan32.inc
ie32rem.tce: $(srcdir)/realvals.inc
ie32rem.tce: ridiv.tce
ie64op.tce: $(srcdir)/endian.inc
ie64op.tce: $(srcdir)/nan16.inc
ie64op.tce: $(srcdir)/realvals.inc
ie64op.tce: qnorm.tce
ie64op.tce: qridiv.tce
ie64op.tce: qrimul.tce
ie64op.tce: qshiftl.tce
ie64op.tce: qshiftr.tce
ie64op.tce: ridiv.tce
ie64op.tce: rimul.tce
ie64rem.tce: $(srcdir)/endian.inc
ie64rem.tce: $(srcdir)/nan16.inc
ie64rem.tce: $(srcdir)/nan32.inc
ie64rem.tce: $(srcdir)/realvals.inc
ie64rem.tce: qnorm.tce
ie64rem.tce: qridiv.tce
ie64rem.tce: qshiftr.tce
ie64rem.tce: ridiv.tce
qnorm.tce: $(srcdir)/realvals.inc
qridiv.tce: $(srcdir)/realvals.inc
qrimul.tce: $(srcdir)/realvals.inc
qshiftl.tce: $(srcdir)/realvals.inc
qshiftr.tce: $(srcdir)/realvals.inc
r32eq.tce: ranalyse.tce
r32op.tce: $(srcdir)/nan16.inc
r32op.tce: $(srcdir)/realvals.inc
r32op.tce: ridiv.tce
r32op.tce: rimul.tce
r32operr.tce: $(srcdir)/nan16.inc
r32operr.tce: $(srcdir)/realvals.inc
r32operr.tce: ridiv.tce
r32operr.tce: rimul.tce
r32rem.tce: $(srcdir)/nan16.inc
r32rem.tce: $(srcdir)/nan32.inc
r32rem.tce: $(srcdir)/realvals.inc
r32rem.tce: ridiv.tce
r32remer.tce: $(srcdir)/nan16.inc
r32remer.tce: $(srcdir)/nan32.inc
r32remer.tce: $(srcdir)/realvals.inc
r32remer.tce: ridiv.tce
r32toi16.tce: $(srcdir)/realvals.inc
r32toi32.tce: $(srcdir)/realvals.inc
r32toi64.tce: $(srcdir)/realvals.inc
r32tor64.tce: $(srcdir)/realvals.inc
r64eq.tce: $(srcdir)/endian.inc
r64gt.tce: $(srcdir)/endian.inc
r64op.tce: $(srcdir)/endian.inc
r64op.tce: $(srcdir)/nan16.inc
r64op.tce: $(srcdir)/nan32.inc
r64op.tce: $(srcdir)/realvals.inc
r64op.tce: qnorm.tce
r64op.tce: qridiv.tce
r64op.tce: qrimul.tce
r64op.tce: qshiftl.tce
r64op.tce: qshiftr.tce
r64op.tce: ridiv.tce
r64op.tce: rimul.tce
r64operr.tce: $(srcdir)/nan16.inc
r64operr.tce: $(srcdir)/nan32.inc
r64operr.tce: $(srcdir)/realvals.inc
r64operr.tce: qnorm.tce
r64operr.tce: qridiv.tce
r64operr.tce: qrimul.tce
r64operr.tce: qshiftl.tce
r64operr.tce: qshiftr.tce
r64operr.tce: ridiv.tce
r64operr.tce: rimul.tce
r64rem.tce: $(srcdir)/endian.inc
r64rem.tce: $(srcdir)/nan16.inc
r64rem.tce: $(srcdir)/nan32.inc
r64rem.tce: $(srcdir)/realvals.inc
r64rem.tce: qnorm.tce
r64rem.tce: qridiv.tce
r64rem.tce: qshiftr.tce
r64rem.tce: ridiv.tce
r64remer.tce: $(srcdir)/nan16.inc
r64remer.tce: $(srcdir)/nan32.inc
r64remer.tce: $(srcdir)/realvals.inc
r64remer.tce: qnorm.tce
r64remer.tce: qridiv.tce
r64remer.tce: qshiftr.tce
r64remer.tce: ridiv.tce
r64toi16.tce: $(srcdir)/realvals.inc
r64toi16.tce: qshiftl.tce
r64toi32.tce: $(srcdir)/realvals.inc
r64toi64.tce: $(srcdir)/realvals.inc
r64tor32.tce: $(srcdir)/nan16.inc
r64tor32.tce: $(srcdir)/nan32.inc
r64tor32.tce: $(srcdir)/realvals.inc
rabsfint.tce: $(srcdir)/realvals.inc
ranalyse.tce: $(srcdir)/realvals.inc
rargred.tce: $(srcdir)/endian.inc
rargred.tce: $(srcdir)/nan16.inc
rargred.tce: $(srcdir)/nan32.inc
rargred.tce: $(srcdir)/realvals.inc
rargred.tce: ridiv.tce
rfunpack.tce: $(srcdir)/nan16.inc
rfunpack.tce: $(srcdir)/nan32.inc
rfunpack.tce: $(srcdir)/realvals.inc
ridiv.tce: $(srcdir)/endian.inc
rimul.tce: $(srcdir)/endian.inc
rlogb.tce: $(srcdir)/realvals.inc
rnotfnte.tce: $(srcdir)/realvals.inc
rnxtaftr.tce: $(srcdir)/realvals.inc
roundsn.tce: $(srcdir)/realvals.inc
rscaleb.tce: $(srcdir)/realvals.inc
rsqrt.tce: $(srcdir)/nan16.inc
rsqrt.tce: $(srcdir)/nan32.inc
rsqrt.tce: $(srcdir)/realvals.inc
rsqrt.tce: roundsn.tce
rsqrt.tce: unpacksn.tce
unpacksn.tce: $(srcdir)/realvals.inc
## end pideps dependencies
