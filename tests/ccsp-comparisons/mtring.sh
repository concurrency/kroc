#!/bin/bash

TIMEOUT=90

if [ -z "$CORES" ]; then
  CORES=2
  if [ -f /proc/cpuinfo ]; then
    CORES=`cat /proc/cpuinfo | grep processor | wc -l`
  fi
fi

LOOPS=$1
CYCLES=$2
TOKENS=$3

export CCSP_RUNTIME_THREADS=$CORES
export THREADS=$CORES

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
run_cmd "Go" "./mtring.go-exe $CYCLES $TOKENS"
run_cmd "Haskell" "./mtring.haskell $CYCLES $TOKENS +RTS -N$CORES -RTS"
run_cmd "Java" "java -Xss128k -classpath java6/. MTRing $CYCLES $TOKENS"
run_cmd "JCSP" "java -Xss128k -classpath jcsp/jcsp.jar:jcsp/. MTRing $CYCLES $TOKENS"
run_cmd "pthread C" "./mtring.pthread $CYCLES $TOKENS"
#run_cmd "PyCSP" "python pycsp/mtring.py $CYCLES $TOKENS"
#run_cmd "Stackless Python" "$SPYTHON stackless/mtring.py $CYCLES $TOKENS"

unset CCSP_RUNTIME_THREADS
unset THREADS

