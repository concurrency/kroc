KROC = kroc
CC = gcc
CXX = g++
GHC = ghc
ERLC = erlc
JAVAC = javac
6G := $(shell which 6g 8g | head -n 1)
6L := $(shell which 6l 8l | head -n 1)

CC_FLAGS := $(shell $(KROC) --cflags)
CC_INC := $(shell $(KROC) --ccincpath)
CC_LIB := $(shell $(KROC) --cclibpath)
CFLAGS := $(CFLAGS) $(CC_FLAGS)

targets = \
	ts \
	ring.beam \
	ring.ccsp \
	ring.go-exe \
	ring.haskell \
	ring.occam \
	ring.pthread \
	java6/Ring.class \
	jcsp/Ring.class \
	mtring.beam \
	mtring.ccsp \
	mtring.haskell \
	mtring.go-exe \
	mtring.occam \
	mtring.pthread \
	java6/MTRing.class \
	jcsp/MTRing.class \
	agents.beam \
	agents.ccsp \
	agents.go-exe \
	agents.haskell \
	agents.occam \
	agents.pthread \
	agents-dp.pthread \
	agents.serial \
	agents.tbb \
	java6/Agents.class \
	jcsp/Agents.class \
	blur.ccsp \
	blur.pthread \
	mandelbrot.occam

all: $(targets)

ts:	ts.c
	$(CC) $(CFLAGS) -O2 -Wall $< -o $@

%.beam:		erlang/%.erl
	$(ERLC) +native +"{hipe, [o3]}" $<

%.ccsp:		ccsp/%.c
	$(CC) $(CFLAGS) $(CC_INC) -O2 -Wall $< -o $@ $(CC_LIB) -lccsp -lpthread

%.chp:		chp/%.hs
	$(GHC) --make -O2 -fglasgow-exts -threaded $< -o $@

%.class:	%.java
	$(JAVAC) -Xlint:unchecked -classpath jcsp/jcsp.jar $<

%.go-exe:	go/%.go
	$(6G) -o go/$*.68 $< 
	$(6L) -o $@ go/$*.68

%.haskell:	haskell/%.hs
	$(GHC) --make -O2 -fglasgow-exts -threaded $< -o $@

%.occam:	occam/%.occ
	$(KROC) -v -o $@ $<

%.pthread:	pthread/%.c
	$(CC) $(CFLAGS) -O2 -Wall $< -o $@ -lpthread

%.serial:	serial/%.c
	$(CC) $(CFLAGS) -O2 -Wall $< -o $@

%.tbb:		tbb/%.cc
	$(CXX) -std=c++0x -O2 -Wall `pkg-config --cflags tbb` $< -o $@ `pkg-config --libs tbb`

clean:
	rm -f $(targets) 
