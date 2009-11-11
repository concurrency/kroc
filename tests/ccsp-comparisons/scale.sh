#!/bin/sh

TESTS="$1 $2 $3 $4 $5 $6"

for ((i = 1; i <= 8; i = i + 1)); do
  export CORES=$i
  cpus="0"
  for ((j = 2; j < (2*$CORES); j = j + 2)); do
  	cpus="$cpus,$j"
  done
  echo taskset --cpu-list "$cpus" ./bench.sh ${CORES}c $TESTS
  taskset --cpu-list "$cpus" ./bench.sh ${CORES}c $TESTS
  unset CORES
done

