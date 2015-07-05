#!/bin/sh

len="${#1}"
((++len))

# echo "--" 1>&2
# echo $@ 1>&2
# echo "--" 1>&2

for el in ${@:3}; do
  # echo -n "$2/$(echo -n "$el" | cut -b"$len"- ) " 1>&2
  echo -n "$(readlink -f "$2/$(echo -n "$el" | cut -b"$len"- )") "
done
