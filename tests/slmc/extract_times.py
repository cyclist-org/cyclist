#!/usr/bin/python
import re
import sys

pattern = "Heap size: (\d+)\nGENERAL: Elapsed process time: (\d+) ms\n(?:.*\n){10}Model verified"

input = sys.stdin.read()
matches = re.finditer(pattern, input)
for m in matches:
    heap_size = m.group(1)
    milliseconds = m.group(2)
    sys.stdout.write(", ".join((heap_size, milliseconds)) + "\n")
