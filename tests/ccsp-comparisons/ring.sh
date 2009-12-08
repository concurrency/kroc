#!/bin/bash

if [ -z "$CORES" ]; then
  CORES=2
  if [ -f /proc/cpuinfo ]; then
    CORES=`cat /proc/cpuinfo | grep processor | wc -l`
  fi
fi

LOOPS=$1
CYCLES=$2

export CCSP_RUNTIME_THREADS=$CORES
export THREADS=$CORES

run_cmd ()
{
  LABEL=$1
  CMD=$2
  $CMD | ./ts | head -n 3 &> /dev/null
  for ((i = 0; i < $LOOPS; i = i + 1)); do
    echo "-- $LABEL $CYCLES"
    $CMD | ./ts | head -n 3
  done
}

run_cmd "CCSP C" "./ring.ccsp $CYCLES"
run_cmd "CCSP occam-pi" "./ring.occam $CYCLES"
run_cmd "Erlang" "erl -noshell -run ring main $CYCLES"
run_cmd "Go" "./ring.go-exe $CYCLES"
run_cmd "Haskell" "./ring.haskell $CYCLES +RTS -N$CORES -RTS"
run_cmd "Java" "java -Xss128k -classpath java6/. Ring $CYCLES"
run_cmd "JCSP" "java -Xss128k -classpath jcsp/jcsp.jar:jcsp/. Ring $CYCLES"
run_cmd "pthread C" "./ring.pthread $CYCLES"
#run_cmd "PyCSP" "python pycsp/ring.py $CYCLES"
#run_cmd "Stackless Python" "$SPYTHON stackless/ring.py $CYCLES"

unset CCSP_RUNTIME_THREADS
unset THREADS

