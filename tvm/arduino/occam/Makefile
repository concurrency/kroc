include ../config.mk

PROG = blink

PROGS = \
	blink \
	commstime \
	nothing \
	testdiv

all: $(addsuffix .hex,$(PROGS))

# Compile occam program.
%.hex: %.occ
	occbuild -v --program $<
	# FIXME: check OCCAMADDR doesn't overlap the TVM
	../binary-to-ihex $(OCCAMADDR) $(basename $<).tbc $@

AVRDUDE_WRITE_OCCAM = -D -U flash:w:$(PROG).hex

upload: $(PROG).hex
	../reset-arduino $(PORT)
	$(AVRDUDE) $(AVRDUDE_FLAGS) $(AVRDUDE_WRITE_OCCAM)
	../read-arduino $(PORT)

clean:
	rm -f \
		$(addsuffix .tce,$(PROGS)) \
		$(addsuffix .tbc,$(PROGS)) \
		$(addsuffix .hex,$(PROGS))
