#!/bin/sh
find src -name '*.ml' -not -path 'src/zzzz/*' -print0 | xargs -0 grep -w -F "$1"
