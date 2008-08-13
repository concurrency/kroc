ARMGCC   = /Users/jadudm/svn/arm-elf
VERSION  = 3.3.2
CROSS    = $(ARMGCC)/bin/arm-elf-
CC       = $(CROSS)gcc
LD       = $(CROSS)ld
AR       = $(CROSS)ar
AS       = $(CROSS)as
CP       = $(CROSS)objcopy
OD       = $(CROSS)objdump
CFLAGS   = -Wall -I./include -c -fno-common -O3
AFLAGS   = -ahls -mapcs-32
LDFLAGS  = -Map main.map -Tldscript.cmd
CPFLAGS  = -O ihex
ODFLAGS  = -x --syms

LIGBCC   = -L$(ARMGCC)/lib/gcc/arm-elf/$(VERSION)/ -lgcc

INCLUDES = \
		-I./include \
		-I./libtvm \
		-I../../runtime/libtvm \

OBJECTS  = crt.o irqs.o main.o

all: main.out
	$(CP) $(CPFLAGS) main.out main.hex
	
main.out: $(OBJECTS) ldscript.cmd
	$(LD) $(LDFLAGS) -o main.out $(OBJECTS) $(LIBGCC)

clean:
	@echo "Cleanliness is close to godliness."
	rm -f $(OBJECTS) main.out main.hex main.map
