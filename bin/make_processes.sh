#!/bin/bash

nprocs=${1:?"Required number of processes"}

echo $$

for i in `seq 1 ${nprocs}`; do
    /bin/bash -c "while true; do sleep 1; done" & 
done
