#!/bin/sh

inotifywait -e close_write,moved_to,create -m . |
while read -r directory events filename; do
  if [ "$filename" = "instrument.lisp" ]; then
    sbcl --script instrument.lisp
  fi
done
