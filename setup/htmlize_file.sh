#!/bin/sh

# has to be run in graphical mode to get the right colors, hence the xvfb
# shenanigans. if it seems to be failing and you can't see why because of this
# setup, add '-nw' before the '-l' argument and run it in a terminal.

# note that this won't work if you require interactivity with your emacs init
# when it sets up (for typing in a password or something). if you cannot disable
# that, you'll have to either 1) add '--quick' before '-l', and live without
# your emacs installation being loaded, or 2) perform the interaction in '-nw'
# mode

WORKING_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

tmpfile="$2"
output_file="$WORKING_DIR/output-file"

Xvfb "$1" -screen 1 800x600x24 &
xvfb_pid="$!"
rm -f "$output_file"
touch "$output_file"
tail -f "$output_file" &
tail_pid="$!"

function finish {
  kill "$xvfb_pid"
  kill "$tail_pid"
  rm "$tmpfile"
  rm "$output_file"
}

trap finish EXIT
TERM="xterm" DISPLAY="$1" emacs -l "$WORKING_DIR/htmlize_this_file.el"
