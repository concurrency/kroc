srcdir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

CPPFLAGS=-Wall -Os

VPATH=$(srcdir)

Transterpreter.exe: winstub.o resource.o
	g++ -mwindows -static $? -o $@

resource.o: resource.rc

%.o:%.rc
	windres -I$(srcdir) $< -O coff -o $@

clean:
	rm -rf Transterpreter.exe *.o
