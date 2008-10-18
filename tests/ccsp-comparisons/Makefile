KROC = kroc
CC = gcc
GHC = ghc
ERLC = erlc
JAVAC = javac
CC_INC := $(shell $(KROC) --ccincpath)
CC_LIB := $(shell $(KROC) --cclibpath)

targets = \
	ring.beam \
	ring.ccsp \
	ring.chp \
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
	agents.c \
	agents.ccsp \
	agents.haskell \
	agents.occam \
	agents.pthread \
	jcsp/Agents.class \
	blur.ccsp \
	blur.pthread

all: $(targets)

%.beam:		erlang/%.erl
	$(ERLC) +native +"{hipe, [o3]}" $<

%.c:		c/%.c
	$(CC) -O2 -Wall $< -o $@

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

clean:
	rm -f $(targets) 
