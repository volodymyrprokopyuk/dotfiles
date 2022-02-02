#!/usr/bin/env zsh

exec nim --verbosity:0 r $(dirname "$0")/omedia.nim $@
