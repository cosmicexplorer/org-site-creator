#!/bin/sh

len="$((${#1} + 2))"

for el in ${@:3}; do
  echo -n "$(readlink -f "$2/$(echo -n "$el" | cut -b"$len"- )") "
done
