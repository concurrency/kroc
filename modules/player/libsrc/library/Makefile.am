@SET_MAKE@

AUTOMAKE_OPTIONS=foreign no-dependencies


SKROC_LIBS=@LIBRARY_DIRS@
SKROC=@top_srcdir@/skroc/@SKROC@
OLD_PATH := $(PATH)
override PATH = @top_srcdir@/skroc:@top_srcdir@/slinker:$(OLD_PATH)

SUFFIXES= .occ .lib .precomp

occlibdir = $(prefix)/lib/transterpreter

occlib_DATA = player.inc pioneer_hud.inc points.inc misc.so debug.occ misc.inc

points.inc: gen-arcs.ss
	mzscheme -mgf gen-arcs.ss > points.inc

#Couldn't get elif or else if to work... hmm..

if MAC
dynflag=-dynamiclib
else
dynflag=-shared
endif
if SOLARIS
dynflag+=-mimpure-text
endif

misc.so: misc.c
	$(CC) $(dynflag) -o misc.so misc.c

CLEANFILES=points.inc misc.so 
