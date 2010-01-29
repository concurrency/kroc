## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../m4

srcdir = @srcdir@
VPATH = @srcdir@
INCLUDES = -I. -I@srcdir@/include
DEFS = @DEFS@ @ARCH_DEFS@
EXTRALDFLAGS = @EXTRA_LDFLAGS@

bin_PROGRAMS = occ21

dist_man1_MANS = occ21.1

# Sources are all kept in sub-directories
occ21_SOURCES = \
	fe/vti1.c fe/vti2.c fe/vti3.c fe/vti4.c fe/vti5.c \
	fe/err1.c fe/err2.c fe/list.c fe/const1.c fe/strpool.c fe/objlib.c \
	misc/txlib.c \
	fe/occamfe.c fe/lex1.c fe/syn1.c fe/syn2.c fe/syn3.c fe/chk1.c \
	fe/chk2.c fe/chk4.c fe/use1.c fe/use2.c fe/use3.c fe/use4.c fe/use5.c fe/objrdr.c \
	fe/objtrans.c misc/occompfe.c misc/inst.c misc/tran1.c \
	misc/tran2.c misc/tran3.c misc/mobiles.c misc/compdyn.c \
	be/bind1.c be/bind2.c be/bind3.c be/debug.c be/gen1.c be/gen2.c be/gen3.c \
	be/gen4.c be/gen5.c be/gen7.c be/gen8.c be/gen9.c be/gen10.c be/gen11.c \
	be/gen12.c be/gen13.c be/generror.c be/objwrt.c be/profile.c be/code1k.c \
	version.c \
	misc/harness.c \
	arglib/arg.c arglib/argnew.c arglib/argcmn.c \
	extlib/extconv.c extlib/exti8.c extlib/exti16.c extlib/exti32.c extlib/exti64.c \
	extlib/extlong.c extlib/extr32.c extlib/extr64.c extlib/extstr.c extlib/extui8.c \
	extlib/extui16.c extlib/extui32.c extlib/extui64.c \
	fnilib/fni.c fnilib/popen.c \
	maplib/mapwrt1.c \
	suplib/sort.c suplib/strlwr.c suplib/strupr.c \
	tcofflib/alloc1.c tcofflib/alloc2.c \
	tcofflib/command1.c tcofflib/command2.c tcofflib/command3.c tcofflib/command4.c \
	tcofflib/command6.c tcofflib/com.c tcofflib/error.c \
	tcofflib/strutil3.c tcofflib/strutil4.c tcofflib/strutil5.c tcofflib/strutil6.c \
	tcofflib/strutil7.c tcofflib/tcoff_io.c \
	tcofflib/lff_io.c \
	arglib/arg0.h \
	fnilib/fnidef.h \
	include/arg.h include/bind1def.h include/bind2def.h include/bind3def.h include/casedops.h \
	include/casemops.h include/cfbdefs.h include/chk1def.h include/chk2def.h include/chk4def.h \
	include/chkdef.h include/chkerror.h include/code1def.h include/compharn.h include/compdyn.h \
	include/confdef.h \
	include/constdef.h include/dbgdef.h include/debugdef.h include/debughdr.h include/disalpha.h \
	include/err2.h include/errhdr.h include/extlib.h include/feinc.h include/fnilib.h \
	include/gen10def.h include/gen11def.h include/gen12def.h include/gen1def.h include/gen2def.h \
	include/gen4def.h include/gen5def.h include/gen7def.h include/gen8def.h include/gen9def.h \
	include/generror.h include/genhdr.h include/genkroc.h include/h1defs.h include/harndef.h \
	include/hdblib.h include/imsmisc.h include/imsopen.h include/imsstd.h include/imstype.h \
	include/imsvals.h include/includes.h include/instdef.h include/instruct.h include/lexdef.h \
	include/lexerror.h include/lexhdr.h include/listhdr.h include/localfe.h include/maplib.h \
	include/memlib.h include/midinc.h include/mmmlib.h include/mobiles.h include/mobile_types.h \
	include/nameshdr.h include/objlib.h include/objrdr.h include/objtrans.h include/objwrt.h \
	include/occamfe.h include/occompfe.h include/ochdr.h include/ocmisc.h include/pdata.h \
	include/pdefs.h include/popen.h include/popen_re.h include/predefhd.h include/profile.h \
	include/proflib.h include/strpool.h include/suplib.h include/syn1def.h include/syn2def.h \
	include/syn3def.h include/syndef.h include/synerror.h include/tcof2lib.h include/tcoff.h \
	include/tcofflib.h include/toolkit.h include/tran1def.h include/trandef.h include/treedef.h \
	include/txlib.h include/use1def.h include/use2def.h include/use4def.h include/usedef.h \
	include/useerror.h include/usehdr.h include/version.h include/vtierror.h include/vti.h

occ21_LDFLAGS = $(EXTRALDFLAGS)
