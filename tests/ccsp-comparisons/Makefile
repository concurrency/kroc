KROC = kroc
CC = gcc
GHC = ghc
ERLC = erlc
JAVAC = javac
CC_INC := $(shell $(KROC) --ccincpath)
CC_LIB := $(shell $(KROC) --cclibpath)

targets = \
	ts \
	ring.beam \
	ring.ccsp \
	ring.haskell \
	ring.occam \
	ring.pthread \
	jcsp/Ring.class \
	mtring.beam \
	mtring.ccsp \
	mtring.haskell \
	mtring.occam \
	mtring.pthread \
	jcsp/MTRing.class \
	agents.beam \
	agents.ccsp \
	agents.haskell \
	agents.occam \
	agents.pthread \
	agents-dp.pthread \
	agents.serial \
	jcsp/Agents.class \
	blur.ccsp \
	blur.pthread

all: $(targets)

ts:	ts.c
	$(CC) -O2 -Wall $< -o $@

%.beam:		erlang/%.erl
	$(ERLC) +native +"{hipe, [o3]}" $<

%.ccsp:		ccsp/%.c
	$(CC) $(CC_INC) -O2 -Wall $< -o $@ $(CC_LIB) -lccsp -lpthread

%.chp:		chp/%.hs
	$(GHC) --make -O2 -fglasgow-exts -threaded $< -o $@

%.class:	%.java
	$(JAVAC) -Xlint:unchecked -classpath jcsp/jcsp.jar $<

%.haskell:	haskell/%.hs
	$(GHC) --make -O2 -fglasgow-exts -threaded $< -o $@

%.occam:	occam/%.occ
	$(KROC) -v -o $@ $<

%.pthread:	pthread/%.c
	$(CC) -O2 -Wall $< -o $@ -lpthread

%.serial:	serial/%.c
	$(CC) -O2 -Wall $< -o $@

clean:
	rm -f $(targets) 
