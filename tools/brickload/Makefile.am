AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../m4

srcdir = @srcdir@
VPATH = @srcdir@

DEFS = @DEFS@ -Wall
#INCLUDES = -I.

SUBDIRS =

bin_PROGRAMS = brickload

noinst_HEADERS = \
	brickload.h

brickload_SOURCES = \
	flash_driver.h \
	brickload.c \
	nxt.c \
	rcx.c \
	mac.c \
	nix.c \
	win.c

#brickload_LDADD =
	
