#!/bin/bash

TIMEOUT=90

CORES=2
if [ -f /proc/cpuinfo ]; then
  CORES=`cat /proc/cpuinfo | grep processor | wc -l`
fi

LOOPS=$1
FRAMES=$2
WORKERS=$3

run_cmd ()
{
  LABEL=$1
  CMD=$2
  perl timeout.pl $TIMEOUT "$CMD" &> /dev/null
  RET=$?
  if [[ $RET == 0 ]]; then
    for ((i = 0; i < $LOOPS; i = i + 1)); do
      echo "-- $LABEL $FRAMES $WORKERS"
      $CMD | ./ts | head -n 2
    done
  fi
}

run_cmd "CCSP C" "./blur.ccsp $FRAMES $WORKERS"
run_cmd "pthread C" "./blur.pthread $FRAMES $WORKERS"
#run_cmd "pthread (queue)" "./blur.pthread $FRAMES $WORKERS 1"

