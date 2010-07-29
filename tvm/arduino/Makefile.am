AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../m4

CPPFLAGS = \
	-Ilibtvm \
	-I../../runtime/libtvm \
	-I$(top_srcdir)/../../runtime/libtvm \
	-DF_CPU=$(F_CPU) \
	-D$(MCU) \
	-DBYTECODE_ADDR=$(BYTECODE_ADDR) \
	@CPPFLAGS@
LDFLAGS = -Llibtvm @LDFLAGS@
LIBS = -ltvm -lm @LIBS@
FIRMWARE=tvm-avr-$(MCU)-$(F_CPU)

firmware_SOURCES = \
	ffi.c \
	interrupts.c \
	serial.c \
	tbc.c \
	time.c \
	tvm-arduino.h \
	tvm.c

noinst_PROGRAMS = firmware
CLEANFILES = firmware.hex

upload: tvm-arduino.hex
	$(AVRDUDE) $(AVRDUDE_FLAGS) -U flash:w:tvm-arduino.hex
	./read-arduino $(UPLOAD_PORT)

install: all
	cp $(FIRMWARE).hex $(prefix)/share/

firmware.hex: firmware
	$(OBJCOPY) -O ihex -R .eeprom firmware firmware.hex
	mv firmware.hex $(FIRMWARE).hex

# All the source files need libtvm to be built first.
$(firmware_SOURCES): libtvm/libtvm.a

# Rebuild libtvm if we've been reconfigured.
libtvm/libtvm.a: config.status
	rm -fr libtvm
	mkdir libtvm
	cd libtvm && ../$(top_srcdir)/../../runtime/libtvm/configure \
		--host=avr \
		--with-memory-allocator=none \
		CFLAGS="@CFLAGS@"
	$(MAKE) -C libtvm

distclean-local:
	rm -fr libtvm
