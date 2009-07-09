AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../../../m4

CPPFLAGS = \
	-I../libtvm \
	-I../../../runtime/libtvm \
	-DF_CPU=$(F_CPU) \
	-DBYTECODE_ADDR=$(BYTECODE_ADDR) \
	@CPPFLAGS@
LDFLAGS = -L../libtvm @LDFLAGS@
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
	../reset-arduino $(UPLOAD_PORT)
	$(AVRDUDE) $(AVRDUDE_FLAGS) -U flash:w:tvm-arduino.hex
	../read-arduino $(UPLOAD_PORT)

CLEANFILES = tvm-arduino.hex
