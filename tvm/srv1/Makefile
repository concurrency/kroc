AS = bfin-elf-as
CC = bfin-elf-gcc
LD = bfin-elf-ld
LDR := bfin-elf-ldr

ELF_LDR := $(shell which $(LDR))
ifneq ($(notdir $(ELF_LDR)), $(LDR))
  LDR := bfin-uclinux-ldr
endif

FLASHLOAD = flashload --backend=spi_flash.dxe
H2INC = h2inc.pl
MAKEDEPEND = makedepend
PERL = perl

CC_FPATH := $(shell which $(CC))
CC_BASE := $(dir $(CC_FPATH))
CC_VERS := $(shell $(CC) -dumpversion)
LIBPATH := $(CC_BASE)../lib/gcc/bfin-elf/$(CC_VERS)

CPUDEFINES = -D__ADSPBF537__ -D__ADSPLPBLACKFIN__

INCLUDES = \
	-I./include \
	-I$(CC_BASE)../include \
	-I./libtvm \
	-I../../runtime/libtvm \
	-I./support

ASMFLAGS = -x assembler-with-cpp $(CPUDEFINES) $(INCLUDES) -g

CFLAGS	= $(INCLUDES) -Wall -g -Os $(CPUDEFINES)
LDFLAGS = -T bftiny.x \
	-Map tvm.map -O binary -o tvm.bin \
	-L$(LIBPATH)

ASMOBJS += flush_cache.o
ASMOBJS += notlibc.o
ASMOBJS	+= startup.o
ASMOBJS	+= support/r8x8dct.o
ASMINCS += include/bfin_config.h
ASMINCS += include/asmmacros.h

COBJS	+= debug.o
COBJS	+= gpio.o
COBJS	+= imagery.o
COBJS	+= main.o
COBJS	+= ppi_dma.o
COBJS	+= timer.o
COBJS	+= tvm.o
COBJS	+= twi.o
COBJS	+= uart.o
COBJS	+= version.o
COBJS	+= support/jpeg.o

GENHDRS += include/bfin_config.inc
GENHDRS += include/memory_map.inc
GENHDRS += include/ppi_dma_const.inc
GENHDRS += include/twi_const.inc
GENHDRS += include/def_LPBlackfin.inc
GENHDRS += include/defBF534.inc
GENHDRS += include/defBF537.inc

OBJS 	= $(ASMOBJS) $(COBJS)
TVMOBJS = libtvm/libtvm.a

ASMSRCS	= $(patsubst %.o,%.asm,$(ASMOBJS))
CSRCS	= $(patsubst %.o,%.c,$(COBJS))

%.o: %.asm $(ASMINCS)
	$(CC) $(ASMFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.inc: %.h $(H2INC)
	$(PERL) $(H2INC) $<

default: tvm.ldr

firmware.h: firmware.occ firmware/*.occ firmware/*.inc $(GENHDRS)
	skroc --keep-temp-files \
	-L firmware \
	-L include \
	--no-std-libs \
	--blackfin \
	--slinker-opts "--tlp-types \"()\"" \
	-t t4 --c -f $@ $<

tvm.o: firmware.h

version.o: $(ASMSRCS) $(CSRCS)

tvm.bin: $(OBJS) $(TVMOBJS) bftiny.x
	$(LD) $(LDFLAGS) $(LDADD) $(OBJS) $(TVMOBJS) -lgcc

tvm.ldr: tvm.bin bftiny.x
	$(LDR) -f -T BF537 -c $@ $<

flash: tvm.ldr
	$(FLASHLOAD) --erasesectors=0,1 --program=$<

clean:
	rm -f 	.c.deps \
		tvm.bin tvm.ldr \
		firmware.h firmware.tce \
		$(OBJS) \
		$(GENHDRS)

#{{{  dependency tracking for C files
update-deps:
	@echo "# DO NOT DELETE THIS LINE -- make depend depends on it." > .c.deps
	$(MAKEDEPEND) $(CFLAGS) -f.c.deps $(ASMSRCS) $(CSRCS) 2> /dev/null 1> /dev/null
.PHONY: update-deps

ifeq ($(UPDATING_DEPS),yes)
else
.c.deps: $(CSRCS)
	$(MAKE) -s update-deps UPDATING_DEPS=yes
include .c.deps
endif
#}}}
