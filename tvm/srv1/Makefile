AS = bfin-elf-as
CC = bfin-elf-gcc
LD = bfin-elf-ld
LDR := bfin-elf-ldr

ELF_LDR := $(shell which $(LDR))
ifneq ($(notdir $(ELF_LDR)), $(LDR))
  LDR := bfin-uclinux-ldr
endif

FLASHLOAD=flashload --backend=spi_flash.dxe

CC_FPATH := $(shell which $(CC))
CC_BASE := $(dir $(CC_FPATH))
CC_VERS := $(shell $(CC) -dumpversion)
LIBPATH := $(CC_BASE)../lib/gcc/bfin-elf/$(CC_VERS)

CPUDEFINES = -D__ADSPBF537__ -D__ADSPLPBLACKFIN__

INCLUDES = \
	-I$(CC_BASE)../include \
	-I. \
	-I./libtvm \
	-I../../runtime/libtvm \
	-I./include \
	-I./support

ASMFLAGS = -x assembler-with-cpp $(CPUDEFINES) $(INCLUDES) -g

CFLAGS	= $(INCLUDES) -g -O2 $(CPUDEFINES)
LDFLAGS = -T bftiny.x \
	-Map tvm.map -O binary -o tvm.bin \
	-L$(LIBPATH)

ASMSRCS	+= startup.asm
ASMSRCS += support/r8x8dct.asm
ASMINCS += asmmacros.h config.h
CSRCS	+= main.c
#CSRCS	+= support/srv.c
#CSRCS	+= srv_ffi.c
#CSRCS	+= external_srv_ffi_hooks.c
#CSRCS	+= support/uart.c
CSRCS	+= support/camera.c
CSRCS	+= support/jpeg.c
CSRCS	+= support/i2cwrite.c
#CSRCS	+= support/stm_m25p32.c
#CSRCS	+= support/xmodem.c
#CSRCS	+= support/c.c
CINCS	+= camera.h c.h config.h font8x8.h \
	i2cwrite.h jpeg.h memory_map.h \
	ov9655.h srv.h stm_m25p32.h uart.h \
	userspace.h xmodem.h

OBJS = $(ASMSRCS:%.asm=%.o) $(CSRCS:%.c=%.o)

TVMOBJS = libtvm/libtvm.a

%.o: %.asm $(ASMINCS)
	$(CC) $(ASMFLAGS) -c -o $@ $<

%.o: %.c $(CINCS)
	$(CC) $(CFLAGS) -c -o $@ $<

default: tvm.ldr

external_srv_ffi_hooks.c external_srv_ffi_pragmas.occ: srv_ffi.c
	tinyswig -n srv_ffi.c

firmware.h: firmware.occ firmware/*.occ firmware/*.inc
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
	rm -f 	*.o *.bin *.ldr \
		support/*.o \
		firmware.h \
		external_srv_ffi_hooks.c \
		external_srv_ffi_pragmas.occ

