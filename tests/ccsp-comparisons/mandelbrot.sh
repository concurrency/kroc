#!/bin/bash

TIMEOUT=900

if [ -z "$CORES" ]; then
  CORES=2
  if [ -f /proc/cpuinfo ]; then
    CORES=`cat /proc/cpuinfo | grep processor | wc -l`
  fi
fi

LOOPS=$1

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

run_cmd "CCSP occam-pi" "./mandelbrot.occam"

unset CCSP_RUNTIME_THREADS
unset THREADS

