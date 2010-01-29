#!/bin/sh

TAG=$1
TESTS="$2 $3 $4 $5 $6"

case "$TESTS" in
 *ring*)
   ./ring.sh 10 4096 2>&1 | tee -a results/ring-4096.$TAG
   ;;
esac

case "$TESTS" in
  *mt-scale*)
    for i in 1 2 4 6 8 12 16 24 32 48 64 96 128 160 192 224 255; do
      ./mtring.sh 6 1024 $i 2>&1 | tee -a results/mtring-1024.$TAG
    done
    ;;
esac

case "$TESTS" in
  *agents*)
    for ((i = 4; i <= 22; i = i + 1)); do
      ./agents.sh 6 $i 12 2>&1 | tee -a results/agents.$TAG
    done
    ;;
esac

case "$TESTS" in
  *blur*)
    for ((i = 1; i <= 16; i = i + 1)); do
      ./blur.sh 6 128 $i 2>&1 | tee -a results/blur.$TAG
    done
    ;;
esac

case "$TESTS" in
  *mandelbrot*)
     ./mandelbrot.sh 6 2>&1 | tee -a results/mandelbrot.$TAG
     ;;
esac
