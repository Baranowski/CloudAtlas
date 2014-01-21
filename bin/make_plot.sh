#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

it=0
while true; do
    declare -A results
    for host in $*; do
        zones_list_bare=$(printf "get_zones\nquit\n" | "$DIR/Client" "$host")
        zones_list=${zones_list_bare//> /}
        for zone in $zones_list; do
            if [ ${#zone} -gt 1 ]; then
                zone=${zone%%/}
            fi
            attrs_bare=$(printf "zone %s\nquit\n" "${zone}" | "$DIR/Client" "$host")
            nprocs_line=$(echo "${attrs_bare}" | egrep "^num_processes")
            nprocs=${nprocs_line##* = }
            results[${zone}]=${nprocs}
        done
    done
    if [ ${it} -eq 0 ]; then
        for zone in "${!results[@]}"; do
            printf "%s " "$zone"
        done
        printf "\n"
        it=1
    fi
    for zone in "${!results[@]}"; do
        printf "%s " "${results[$zone]}"
    done
    printf "\n"
    sleep 0.5
    unset results
done
