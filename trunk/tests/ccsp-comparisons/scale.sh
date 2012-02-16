#!/bin/sh

TESTS="$1 $2 $3 $4 $5 $6"

for ((i = 1; i <= 16; i = i + 1)); do
  export CORES=$i
  cpus="0"
  for ((j = 2; j < (2*$CORES) && j < 8; j = j + 2)); do
  	cpus="$cpus,$j"
  done
  for ((j = 1; j < (2*($CORES-4)) && j < 8; j = j + 2)); do
	cpus="$cpus,$j"
  done
  for ((j = 8; (j-8) < (2*($CORES-8)) && j < 16; j = j + 2)); do
	cpus="$cpus,$j"
  done
  for ((j = 9; (j-8) < (2*($CORES-12)) && j < 16; j = j + 2)); do
	cpus="$cpus,$j"
  done
  echo taskset --cpu-list "$cpus" ./bench.sh ${CORES}c $TESTS
  taskset --cpu-list "$cpus" ./bench.sh ${CORES}c $TESTS
  unset CORES
done

