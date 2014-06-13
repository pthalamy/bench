#!/bin/bash

for i in `cat util/OPSLIST`
  do
    for j in `seq 1 11`
      do
        ./run_bench.sh $i ${j}00
      done
  done
