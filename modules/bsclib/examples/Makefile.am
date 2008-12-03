## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	args.occ \
	getopt.occ \
	lsdemo.occ \
	pingdemo.occ \
	sockdaytime.occ \
	sockstreamcli.occ \
	sockstreamserv.occ \
	socktcpcli.occ \
	socktcpserv.occ \
	sockudpcli.occ \
	sockudpserv.occ

examples = \
	args \
	getopt \
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

all-local: $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<
