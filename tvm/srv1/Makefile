AS = bfin-elf-as
CC = bfin-elf-gcc
LD = bfin-elf-ld
LDR := bfin-elf-ldr
PERL = perl

ELF_LDR := $(shell which $(LDR))
ifneq ($(notdir $(ELF_LDR)), $(LDR))
  LDR := bfin-uclinux-ldr
endif

FLASHLOAD = flashload --backend=spi_flash.dxe
H2INC = h2inc.pl

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

CFLAGS	= $(INCLUDES) -g -O2 $(CPUDEFINES)
LDFLAGS = -T bftiny.x \
	-Map tvm.map -O binary -o tvm.bin \
	-L$(LIBPATH)

ASMOBJS	+= startup.o
ASMOBJS	+= support/r8x8dct.o
ASMINCS += include/bfin_config.h
ASMINCS += include/asmmacros.h

COBJS	+= main.o
COBJS	+= support/camera.o
COBJS	+= support/jpeg.o
COBJS	+= support/i2cwrite.o
CINCS	+= include/*.h
CINCS	+= support/*.h

GENHDRS += include/bfin_config.inc
GENHDRS += include/memory_map.inc
GENHDRS += include/def_LPBlackfin.inc
GENHDRS += include/defBF534.inc
GENHDRS += include/defBF537.inc

OBJS = $(ASMOBJS) $(COBJS)
#$(ASMSRCS:%.asm=%.o) $(CSRCS:%.c=%.o)

TVMOBJS = libtvm/libtvm.a

%.o: %.asm $(ASMINCS)
	$(CC) $(ASMFLAGS) -c -o $@ $<

%.o: %.c $(CINCS)
	$(CC) $(CFLAGS) -c -o $@ $<

%.inc: %.h $(H2INC)
	$(PERL) $(H2INC) $<

default: tvm.ldr

external_srv_ffi_hooks.c external_srv_ffi_pragmas.occ: srv_ffi.c
	tinyswig -n srv_ffi.c

firmware.h: firmware.occ firmware/*.occ firmware/*.inc $(GENHDRS)
	skroc --keep-temp-files \
	-L firmware \
	-L include \
	--no-std-libs \
	--blackfin \
	--slinker-opts "--tlp-types \"()\"" \
	-t t4 --c -f $@ $<

main.o: firmware.h

tvm.bin: $(OBJS) $(TVMOBJS)
	$(LD) $(LDFLAGS) $(LDADD) $(OBJS) $(TVMOBJS) -lgcc

tvm.ldr: tvm.bin
	$(LDR) -f -T BF537 -c $@ $<

flash: tvm.ldr
	$(FLASHLOAD) --erasesectors=0,1 --program=$<

clean:
	rm -f 	tvm.bin tvm.ldr \
		firmware.h \
		$(OBJS) \
		$(GENHDRS)

