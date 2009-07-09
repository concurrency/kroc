AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../m4

CPPFLAGS = \
	-Ilibtvm \
	-I../../runtime/libtvm \
	-DF_CPU=$(F_CPU) \
	-DBYTECODE_ADDR=$(BYTECODE_ADDR) \
	@CPPFLAGS@
LDFLAGS = -Llibtvm @LDFLAGS@
LIBS = -ltvm -lm @LIBS@

noinst_PROGRAMS = tvm-arduino

tvm_arduino_SOURCES = \
	ffi.c \
	interrupts.c \
	serial.c \
	tbc.c \
	time.c \
	tvm-arduino.h \
	tvm.c

tvm-arduino.hex: tvm-arduino
	$(OBJCOPY) -O ihex -R .eeprom tvm-arduino tvm-arduino.hex

upload: tvm-arduino.hex
	./reset-arduino $(UPLOAD_PORT)
	$(AVRDUDE) -C avrdude.conf $(AVRDUDE_FLAGS) -U flash:w:tvm-arduino.hex
	./read-arduino $(UPLOAD_PORT)

# All the source files need libtvm to be built first.
$(tvm_arduino_SOURCES): libtvm/libtvm.a

# Rebuild libtvm if we've been reconfigured.
libtvm/libtvm.a: config.status
	rm -fr libtvm
	mkdir libtvm
	cd libtvm && ../../../runtime/libtvm/configure \
		--host=avr \
		--with-memory-allocator=none \
		CFLAGS="@CFLAGS@"
	$(MAKE) -C libtvm

CLEANFILES = tvm-arduino.hex

distclean-local:
	rm -fr libtvm
