#!/bin/bash

SPYTHON=/opt/stackless/bin/python
TIMEOUT=300

CORES=2
if [ -f /proc/cpuinfo ]; then
  CORES=`cat /proc/cpuinfo | grep processor | wc -l`
fi

LOOPS=$1
GRID=$2
AGENTS=$3

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
run_cmd "Haskell" "./agents.haskell $GRID $AGENTS +RTS -N$CORES -RTS"
run_cmd "JCSP" "java -classpath jcsp/jcsp.jar:jcsp/. Agents $GRID $AGENTS"
run_cmd "pthread" "./agents.pthread $GRID $AGENTS"
#run_cmd "PyCSP" "python pycsp/agents.py $GRID $AGENTS"

#if [ -x "$SPYTHON" ]; then
#	run_cmd "Stackless Python" "$SPYTHON stackless/agents.py $GRID $AGENTS"
#fi

