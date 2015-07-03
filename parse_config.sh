#!/bin/sh

grep "^$1:" "$2" | sed -e "s/^$1:\\s*//g"
