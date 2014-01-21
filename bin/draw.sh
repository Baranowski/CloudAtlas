#!/bin/bash

data_file=${1:-data.tmp}
printf "set title \"num_processes\"
plot for [col=1:8] \"${data_file}\" using 0:col with lines title columnheader
" | gnuplot -persist
