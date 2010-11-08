AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../m4

CPPFLAGS = \
	-Ilibtvm \
	-I../../runtime/libtvm \
	@CPPFLAGS@
LDFLAGS = -Llibtvm @LDFLAGS@
LIBS = -ltvm @LIBGCC@ @LIBS@

noinst_PROGRAMS = tvm_nxt

tvm_nxt_SOURCES = \
	aic.c \
	asm_decls.h \
	at91sam7s256.h \
	avr.c \
	debug.c \
	fat12.c \
	font8x8.h \
	init.S \
	interrupts.S \
	lcd.c \
	lock.S \
	mem.c \
	nxt.c \
	samba_init.S \
	sffi.c \
	tbc.c \
	tvm.c \
	usb.c \
	vectors.S

%.samba.elf: $(tvm_nxt_OBJECTS) samba.lds
	$(LD) $(LDFLAGS) \
		-o $@ -T samba.lds -Os --gc-sections --no-check-sections \
		$(tvm_nxt_OBJECTS) \
		$(LIBS)

%.rom.elf: $(tvm_nxt_OBJECTS) rom.lds
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
