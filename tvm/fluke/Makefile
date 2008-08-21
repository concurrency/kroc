ARMGCC   ?= /Users/jadudm/svn/arm-elf
VERSION  = 4.2.0
CROSS    = $(ARMGCC)/bin/arm-elf-
CC       = $(CROSS)gcc
LD       = $(CROSS)ld
AR       = $(CROSS)ar
AS       = $(CROSS)as
CP       = $(CROSS)objcopy
OD       = $(CROSS)objdump
RANLIB   = $(CROSS)ranlib

LIBGCC   = -L$(ARMGCC)/lib/gcc/arm-elf/$(VERSION)/ -lgcc 

INCLUDES = \
		-I./include \
		-I./libtvm \
		-I../../runtime/libtvm \
		-I./firmware

CFLAGS   = -Wall $(INCLUDES) -c -fno-common -O3
AFLAGS   = -ahls -mapcs-32
LDFLAGS  = -Map main.map -Tldscript.cmd
CPFLAGS  = -O ihex
ODFLAGS  = -x --syms

OBJECTS  = crt.o irqs.o tvm.o timer.o debug.o main.o 
TVMOBJS = libtvm/libtvm.a

all: main.out
	$(CP) $(CPFLAGS) main.out main.hex
	
main.out: $(TVMOBJS) $(OBJECTS) ldscript.cmd
	$(LD) $(LDFLAGS) -o main.out $(OBJECTS) $(TVMOBJS) $(LIBGCC) 

libtvm/libtvm.a: libtvm.sh
	CC=$(CC) CFLAGS=-nostdlib ./libtvm.sh
	$(RANLIB) libtvm/libtvm.a


clean:
	@echo "Cleanliness is close to godliness."
	rm -f $(OBJECTS) main.out main.hex main.map
