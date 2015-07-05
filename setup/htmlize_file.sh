#!/bin/sh

WORKING_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

tmpfile="$2"
output_file="output-file"

Xvfb "$1" -screen 1 800x600x24 &
xvfb_pid="$!"
rm -f "$output_file"
touch "$output_file"
tail -f "$output_file" &
tail_pid="$!"
TERM="xterm" DISPLAY="$1" emacs --quick -l "$WORKING_DIR/htmlize_this_file.el" \
    2>/dev/null

function finish {
  kill "$xvfb_pid"
  rm "$tmpfile"
  rm "$output_file"
}

trap finish EXIT
