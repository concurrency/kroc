#!/bin/bash

TIMEOUT=300

if [ -z "$CORES" ]; then
  CORES=2
  if [ -f /proc/cpuinfo ]; then
    CORES=`cat /proc/cpuinfo | grep processor | wc -l`
  fi
fi

LOOPS=$1
GRID=$2
AGENTS=$3

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
      echo "-- $LABEL $GRID $AGENTS"
      time $CMD > /dev/null
    done
  fi
}

run_cmd "CCSP C" "./agents.ccsp $GRID $AGENTS"
run_cmd "CCSP occam-pi" "./agents.occam $GRID $AGENTS"
run_cmd "Erlang" "erl -noshell -run agents main $GRID $AGENTS"
run_cmd "Go" "./agents.go-exe $GRID $AGENTS"
run_cmd "Haskell" "./agents.haskell $GRID $AGENTS +RTS -N$CORES -RTS"
run_cmd "Java" "java -Xss128k -classpath java6/. Agents $GRID $AGENTS"
run_cmd "JCSP" "java -Xss128k -classpath jcsp/jcsp.jar:jcsp/. Agents $GRID $AGENTS"
run_cmd "pthread C" "./agents.pthread $GRID $AGENTS"
run_cmd "pthread DP C" "./agents-dp.pthread $GRID $AGENTS"
run_cmd "serial C" "./agents.serial $GRID $AGENTS"
#run_cmd "PyCSP" "python pycsp/agents.py $GRID $AGENTS"
#run_cmd "Stackless Python" "$SPYTHON stackless/agents.py $GRID $AGENTS"

unset CCSP_RUNTIME_THREADS
unset THREADS

