#!/bin/sh

inotifywait -e close_write,moved_to,create -m . |
while read -r directory events filename; do
  if [ "$filename" = "base.lisp" ]; then
    ./base.lisp
  fi
done
