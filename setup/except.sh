#!/bin/bash

# returns only those lines of stdin that can't be found amongst the arguments
while read line; do
  [ "$(for arg in $@; do [ "$line" = "$arg" ] && echo hey; done)" = "" ] && \
    echo "$line"
done
