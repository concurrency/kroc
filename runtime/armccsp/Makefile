TARGET = armccsp.a
SOURCES = support.c kfunc.c kernel.c

CROSS_COMPILE = arm-linux-gnueabi-
CC = gcc-4.6
LD = ld
AR = ar

OBJS=$(SOURCES:%.c=%.o)
DEPS=$(OBJS:%.o=%.d)

# BASE points at where the EV3 sources are
BASE = /home/frmb/research/ev3/ev3sources

DEVKIT = $(BASE)/extra/linux-devkit/arm-none-linux-gnueabi
INCLUDES += -I.
INCLUDES += -I$(DEVKIT)/usr/include/dbus-1.0
INCLUDES += -I$(DEVKIT)/usr/lib/dbus-1.0/include
INCLUDES += -I$(DEVKIT)/usr/include
CFLAGS = -DPCASM $(INCLUDES) -O0 -Wall -fPIC -fomit-frame-pointer
#LDFLAGS = -L$(DEVKIT)/usr/lib
#LDFLAGS = --shared
ARFLAGS = -r

all: $(TARGET)

%.o: %.c
	$(CROSS_COMPILE)$(CC) $(CFLAGS) -c -MMD -MP -o $@ $<

$(TARGET): $(OBJS)
	$(CROSS_COMPILE)$(AR) $(ARFLAGS) $@ $(OBJS)

.PHONY: clean
clean:
	rm -f $(OBJS) $(TARGET) $(DEPS)

-include $(DEPS)

