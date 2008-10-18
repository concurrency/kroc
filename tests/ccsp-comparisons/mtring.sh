#!/bin/bash

SPYTHON=/opt/stackless/bin/python
TIMEOUT=90

CORES=2
if [ -f /proc/cpuinfo ]; then
  CORES=`cat /proc/cpuinfo | grep processor | wc -l`
fi

LOOPS=$1
CYCLES=$2
TOKENS=$3

run_cmd ()
{
  LABEL=$1
  CMD=$2
  perl timeout.pl $TIMEOUT "$CMD" &> /dev/null
  RET=$?
  if [[ $RET == 0 ]]; then
    for ((i = 0; i < $LOOPS; i = i + 1)); do
      echo "-- $LABEL $CYCLES $TOKENS"
      $CMD | ./ts | head -n 3
    done
  fi
}

run_cmd "CCSP C" "./mtring.ccsp $CYCLES $TOKENS"
run_cmd "CCSP occam-pi" "./mtring.occam $CYCLES $TOKENS"
run_cmd "Erlang" "erl -noshell -run mtring main $CYCLES $TOKENS"
run_cmd "Haskell" "./mtring.haskell $CYCLES $TOKENS +RTS -N$CORES -RTS"
run_cmd "JCSP" "java -classpath jcsp/jcsp.jar:jcsp/. MTRing $CYCLES $TOKENS"
run_cmd "pthread" "./mtring.pthread $CYCLES $TOKENS"
#run_cmd "PyCSP" "python pycsp/mtring.py $CYCLES $TOKENS"

if [ -x "$SPYTHON" ]; then
	run_cmd "Stackless Python" "$SPYTHON stackless/mtring.py $CYCLES $TOKENS"
fi

