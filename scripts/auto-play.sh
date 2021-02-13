#!/bin/sh

while inotifywait -e close_write generated/output.wav; do play ./generated/output.wav; done
