## -*- makefile -*-
##
## Process this file with automake to produce Makefile.in
##
## $Source:$
##
## $Id:$
##

VPATH = @srcdir@
AM_CPPFLAGS = -I.
DEFS=@DEFS@ @ARCH_DEFS@ @KROC_CCSP_CINCPATH@
EXTRALDFLAGS=@ARCH_DEFS@

AUTOMAKE_OPTIONS=foreign
ACLOCAL_AMFLAGS = -I ../../m4

EXTRA_DIST = \
	example.tdef

bin_PROGRAMS = tranx86

# Sources are all kept in libraries in sub-directories
tranx86_SOURCES = main.c structs.h tceread.c tceread.h netcread.c netcread.h machine.h main.h support.h support.c \
	transputer.h tcoff.h trancomm.h etcdump.h etcdump.c etcrtl.h etcrtl.c tstack.h tstack.c intel.h \
	postmortem.h rtlops.h rtldump.h rtldump.c rtlops.c regcolour.h regcolour.c asm386.h \
	asm386.c optimise.c optimise.h elf.h elf.c cpuid.h cpuid.c tstate.h tstate.c etcops.h \
	etcops.c stubtable.h stubtable.c archdef.h arch386.h arch386.c mips.h archmips.h archmips.c \
	asmmips.h asmmips.c mips.h tcedump.c tcedump.h sparc.h archsparc.h archsparc.c asmsparc.h asmsparc.c \
	ppc.h archppc.h archppc.c asmppc.h asmppc.c kif.c kif.h
tranx86_LDFLAGS = $(EXTRALDFLAGS)

include_HEADERS = 

