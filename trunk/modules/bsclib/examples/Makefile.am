## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	args.occ \
	getopt.occ \
	getoptlong.occ \
	lsdemo.occ \
	pingdemo.occ \
	sockdaytime.occ \
	sockstreamcli.occ \
	sockstreamserv.occ \
	socktcpcli.occ \
	socktcpserv.occ \
	sockudpcli.occ \
	sockudpserv.occ \
	test-getopt.sh

if BUILD_BSCLIB_EXAMPLES
examples = \
	args \
	getopt \
	getoptlong \
	lsdemo \
	sockdaytime \
	sockstreamcli \
	sockstreamserv \
	socktcpcli \
	socktcpserv \
	sockudpcli \
	sockudpserv

if OCCBUILD_KROC
examples += \
	pingdemo
endif
endif

all-local: $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<
