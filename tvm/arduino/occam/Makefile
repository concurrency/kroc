include ../config.mk

PROG = blink

PROGS = \
	adc \
	blink \
	blink-array \
	button \
	commstime \
	exttimer \
	hello \
	ledmatrix \
	nothing \
	rawblink \
	testdiv

all: $(addsuffix .hex,$(PROGS))

# Compile occam program.
%.hex: %.occ
	avr-occbuild -v -DF.CPU=$(F_CPU) --program $<
	# FIXME: check address doesn't overlap the TVM
	../binary-to-ihex $(BYTECODE_ADDR) $(basename $<).tbc $@

AVRDUDE_WRITE_OCCAM = -D -U flash:w:$(PROG).hex

upload: $(PROG).hex
	../reset-arduino $(UPLOAD_PORT)
	$(AVRDUDE) -C ../avrdude.conf $(AVRDUDE_FLAGS) $(AVRDUDE_WRITE_OCCAM)
	../read-arduino $(UPLOAD_PORT)

clean:
	rm -f $(PROGS) $(foreach suffix,.tce .tbc .hex,$(addsuffix $(suffix),$(PROGS)))

#{{{ dependencies
avr.module: iom328p.inc
blink.hex: wiring.module
commstime.hex: wiring.module
hello.hex: wiring.module
testdiv.hex: wiring.module
rawblink.hex: wiring.module
wiring.module: avr.module
#}}}
