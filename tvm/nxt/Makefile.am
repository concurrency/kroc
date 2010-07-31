AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../m4

CPPFLAGS = \
	-Ilibtvm \
	-I../../runtime/libtvm \
	@CPPFLAGS@
LDFLAGS = -Llibtvm @LDFLAGS@
LIBS = -ltvm -lnxos @LIBGCC@ @LIBS@

noinst_PROGRAMS = tvm_nxt

tvm_nxt_SOURCES = \
	sffi.c \
	tbc.c \
	tvm.c

%.samba.elf: $(tvm_nxt_OBJECTS)
	$(LD) $(LDFLAGS) \
		-o $@ -T samba.lds -Os --gc-sections --no-check-sections \
		$(tvm_nxt_OBJECTS) \
		$(LIBS)

%.rom.elf: $(tvm_nxt_OBJECTS)
	$(LD) $(LDFLAGS) \
		-o $@ -T rom.lds -Os --gc-sections --no-check-sections \
		$(tvm_nxt_OBJECTS) \
		$(LIBS)

%.bin: %.elf
	$(OBJCOPY) -O binary $< $@

tvm_nxt: tvm-nxt.rom.bin tvm-nxt.samba.bin
	touch $@

# All the source files need libtvm to be built first.
$(tvm_nxt_SOURCES): libtvm/libtvm.a

# Rebuild libtvm if we've been reconfigured.
libtvm/libtvm.a: config.status
	rm -fr libtvm
	mkdir libtvm
	cd libtvm && ../../../runtime/libtvm/configure \
		--host=arm-elf \
		--with-memory-allocator=tlsf \
		--enable-custom-memory-ops \
		--disable-packed-ectx \
		CFLAGS="@CFLAGS@"
	$(MAKE) -C libtvm

CLEANFILES = \
	tvm_nxt \
	tvm-nxt.rom.bin tvm-nxt.rom.elf \
	tvm-nxt.samba.bin tvm-nxt.samba.elf

distclean-local:
	rm -Rf libtvm
