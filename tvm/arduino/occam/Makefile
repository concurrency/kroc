include ../config.mk

PROG = blink

PROGS = \
	adc \
	blink \
	blink-array \
	button \
	ch1 \
	ch2 \
	ch2b \
	ch3 \
	ch3b \
	ch3c \
	ch4 \
	ch5 \
	ch5b \
	ch5c \
	commstime \
	exttimer \
	hello \
	ledmatrix \
	nothing \
	rawblink \
	testdiv

all: $(addsuffix .hex,$(PROGS))

# Compile occam program.
# FIXME: check $(BYTECODE_ADDR) doesn't overlap the TVM
%.hex: %.occ
	avr-occbuild -DF.CPU=$(F_CPU) --program $<
	../binary-to-ihex $(BYTECODE_ADDR) $(basename $<).tbc $@

AVRDUDE_WRITE_OCCAM = -D -U flash:w:$(PROG).hex

upload: $(PROG).hex
	../reset-arduino $(UPLOAD_PORT)
	$(AVRDUDE) -C ../avrdude.conf $(AVRDUDE_FLAGS) $(AVRDUDE_WRITE_OCCAM)
	../read-arduino $(UPLOAD_PORT)

clean:
	rm -f $(PROGS) $(foreach suffix,.tce .tbc .hex,$(addsuffix $(suffix),$(PROGS)))

#{{{ dependencies
adc.hex: wiring.module
avr.module: iom328p.inc
blink-array.hex: wiring.module
blink.hex: wiring.module
button.hex: wiring.module
ch1.hex: plumbing.module
ch2.hex: plumbing.module
ch2b.hex: plumbing.module
ch3.hex: plumbing.module
ch3b.hex: plumbing.module
ch3c.hex: plumbing.module
ch4.hex: plumbing.module
ch5.hex: plumbing.module
ch5b.hex: plumbing.module
ch5c.hex: plumbing.module
commstime.hex: wiring.module
exttimer.hex: wiring.module
hello.hex: wiring.module
ledmatrix.hex: wiring.module font8x8.inc
plumbing.module: avr.module wiring.module
rawblink.hex: wiring.module
testdiv.hex: wiring.module
wiring.module: avr.module
#}}}
