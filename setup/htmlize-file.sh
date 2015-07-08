#!/bin/sh

# has to be run in graphical mode to get the right colors, hence the xvfb
# shenanigans. if it seems to be failing and you can't see why because of this
# setup, add '-nw' before the '-l' argument and run it in a terminal.

# note that this will be annoying if you require interactivity with your emacs
# init when it sets up (for typing in a password or something). if you cannot
# disable that, you'll have to either add '--quick' before '-l', and live
# without your emacs installation being loaded, or have some custom setup

WORKING_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

tmpfile="$1"
output_file="$WORKING_DIR/output-file"

display_num="$2"
nw_arg=""
if [ "$display_num" = 'nw' ]; then
  nw_arg="-nw"
  display_num="$DISPLAY"
elif hash Xvfb 2>/dev/null; then
  Xvfb "$display_num" -screen 1 800x600x24 &
  xvfb_pid="$!"
fi

rm -f "$output_file"
touch "$output_file"
# tail is being used as a crude form of ipc since it's otherwise impossible to
# communicate with a running emacs unless that emacs is running in batch mode
tail -f "$output_file" &
tail_pid="$!"

function finish {
  [ "$xvfb_pid" != "" ] && kill "$xvfb_pid"
  kill "$tail_pid"
  rm "$tmpfile"
  rm "$output_file"
}

trap finish EXIT

# should work if in non-graphical environment too
emacs_args="$nw_arg -l $WORKING_DIR/htmlize-this-file.el"
TERM="xterm" DISPLAY="$display_num" emacs $emacs_args
