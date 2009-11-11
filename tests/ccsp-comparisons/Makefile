KROC = kroc
CC = gcc
GHC = ghc
ERLC = erlc
JAVAC = javac
CC_FLAGS := $(shell $(KROC) --cflags)
CC_INC := $(shell $(KROC) --ccincpath)
CC_LIB := $(shell $(KROC) --cclibpath)
CFLAGS := $(CFLAGS) $(CC_FLAGS)

targets = \
	ts \
	ring.beam \
	ring.ccsp \
	ring.haskell \
	ring.occam \
	ring.pthread \
	java6/Ring.class \
	jcsp/Ring.class \
	mtring.beam \
	mtring.ccsp \
	mtring.haskell \
	mtring.occam \
	mtring.pthread \
	java6/MTRing.class \
	jcsp/MTRing.class \
	agents.beam \
	agents.ccsp \
	agents.haskell \
	agents.occam \
	agents.pthread \
	agents-dp.pthread \
	agents.serial \
	java6/Agents.class \
	jcsp/Agents.class \
	blur.ccsp \
	blur.pthread

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

%.haskell:	haskell/%.hs
	$(GHC) --make -O2 -fglasgow-exts -threaded $< -o $@

%.occam:	occam/%.occ
	$(KROC) -v -o $@ $<

%.pthread:	pthread/%.c
	$(CC) $(CFLAGS) -O2 -Wall $< -o $@ -lpthread

%.serial:	serial/%.c
	$(CC) $(CFLAGS) -O2 -Wall $< -o $@

clean:
	rm -f $(targets) 
