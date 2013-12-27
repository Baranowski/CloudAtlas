#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

inputfile="$1"
shift
for host in $*; do
    (cat "$inputfile"; echo "quit") | "$DIR/Client" "$host"
done
