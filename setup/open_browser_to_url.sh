#!/bin/sh

function qe {
  hash 2>/dev/null $@
}

for option in xdg-open x-www-browser www-browser gnome-open open kde-open; do
  if [ "$option" != "" ] && qe "$option"; then
    $option "$1"
    exit 0
  fi
done

echo "could not locate appropriate browser utility" 1>&2
exit 1
