#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

inputfile="$1"
cert="$2"
priv="$3"
shift 3
for host in $*; do
    (cat "$inputfile"; echo "quit") | "$DIR/Client" "$cert" "$priv" "$host"
done
