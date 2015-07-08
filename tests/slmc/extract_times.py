#!/usr/bin/python
import re
import sys

pattern = "Heap size: (\d+)\nGENERAL: Elapsed process time: (\d+) ms\n(?:.*\n){10}Model verified"

input = sys.stdin.read()
matches = re.finditer("Heap size: (\d+)\nModel verified\n\nreal\t\d+m\d+\.\d+s\nuser\t(\d+)m(\d+\.\d+)s", input)
for m in matches:
    heap_size = m.group(1)
    minutes = float(m.group(2))
    seconds = float(m.group(3))
    sys.stdout.write(", ".join((heap_size, str(minutes * 60 + seconds))) + "\n")
