srcdir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

VPATH=$(srcdir)

Transterpreter.exe: winstub.o resource.o
	g++ -mwindows -static $? -o $@

resource.o: resource.rc

%.o:%.rc
	windres $< -O coff -o $@

clean:
	rm Transterpreter.exe *.o
