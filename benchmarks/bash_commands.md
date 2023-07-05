# Bash Scripts Used to Generate and Analyse Logs

This file contains useful bash scripts for creating and analysing Cyclist log files for the benchmark test suites.

The following utilities are required to execute these scripts:

* grep
* pcregrep
* datamash

## The Scripts

### Generating Log Files

The following script runs all the first order logic benchmarks, and dumps the debug output to individual log files for each benchmark.
The log files for failing benchmarks (i.e., those for which a proof is not found) are immediately deleted.

```[bash]
for f in benchmarks/fo/*.tst; do
  i=1;
  while read -r SEQ; do
    tst=${f%.tst}; tst=${tst##*/};
    logfile="benchmarks/logs/fo-${tst}.$i.log";
    (dune exec src/firstorder/prove.exe -- -d -s -p -t 30 -OP -min -scc -ff -S "${SEQ}" > ${logfile} 2>&1);
    if `grep "NOT proved" ${logfile} > /dev/null`; then rm -f ${logfile}; fi;
    let i=i+1;
  done < "$f";
done;
```

The following command runs all the separation logic base benchmarks, and dumps the debug output to individual log files for each benchmark.

```[bash]
for f in benchmarks/sl/base/*.tst; do
  i=1;
  while read -r SEQ; do
    tst=${f%.tst}; tst=${tst##*/};
    logfile="benchmarks/logs/base-${tst}.$i.log";
    (dune exec src/seplog/prove.exe -- -d -s -p -t 30 -OP -min -scc -ff -S "${SEQ}" > ${logfile} 2>&1);
    if `grep "NOT proved" ${logfile} > /dev/null`; then rm -f ${logfile}; fi;
    let i=i+1;
  done < "$f";
done;
```

The following command runs all the separation logic songbird benchmarks, and dumps the debug output to individual log files for each benchmark.

```[bash]
for f in benchmarks/sl/songbird/*.tst; do
  i=1;
  while read -r SEQ; do
    tst=${f%.tst}; tst=${tst##*/};
    logfile="benchmarks/logs/songbird-${tst}.$i.log";
    (dune exec src/seplog/prove.exe -- -D examples/songbird.defs -d -s -p -t 30 -OP -min -scc -ff -S "${SEQ}" > ${logfile} 2>&1);
    if `grep "NOT proved" ${logfile} > /dev/null`; then rm -f ${logfile}; fi;
    let i=i+1;
  done < "$f";
done;
```

The following command runs all the separation logic ATVA2014 benchmarks, and dumps the debug output to individual log files for each benchmark.

```[bash]
for f in benchmarks/sl/atva-2014/*.tst; do
  i=1;
  while read -r SEQ; do
    tst=${f%.tst}; tst=${tst##*/};
    logfile="benchmarks/logs/atva-${tst}.$i.log";
    (dune exec src/seplog/prove.exe -- -D examples/IosifEtAl-ATVA2014.defs -d -s -p -t 30 -OP -min -scc -ff -S "${SEQ}" > ${logfile} 2>&1);
    if `grep "NOT proved" ${logfile} > /dev/null`; then rm -f ${logfile}; fi;
    let i=i+1;
  done < "$f";
done;
```

### Analysing Log Files

The following script examines each log file in turn and prints out, for each file:

* The name of the file minus the extension (i.e. the benchmark test name)
* The number of infinite descent checks generated during proof search
* The sum of the number of nodes in the graphs checked for infinite descent
* The min, max, mean, median and mode number of nodes in the checked graphs
* The sum of the trace widths of the graphs checked for infinite descent
* The min, max, mean, median and mode trace width of the checked graphs

It then prints a final line containing the following statistics, over all the log files:

* The total number of infinite descent checks generated during proof search
* The sum of the number of nodes in the graphs checked for infinite descent
* The min, max, mean, median and mode number of nodes in the checked graphs
* The sum of the trace widths of the graphs checked for infinite descent
* The min, max, mean, median and mode trace width of the checked graphs

```[bash]
echo '"Test Case","Num Checks","Sum Num Nodes","Min Num Nodes","Max Num Nodes","Mean Num Nodes","Median Num Nodes","Mode Num Nodes","Sum Trace Widths","Min Trace Width","Max Trace Width","Mean Trace Width","Median Trace Width","Mode Trace Width"'; \
for f in *.log; do
  echo -n "${f%.log} ";
  echo -n "`grep -o 'Checking soundness starts' $f | wc -l`,";
  nodes="`grep -P -o '((?<=nodes: )\d+)|(Checking soundness(?= starts))|(Found soundness)' $f | pcregrep -M -o '\d+(?=\nChecking)'`";
  if [ -z "$nodes" ]; then echo -n "0,0,0,0,0,0"; else echo -n "`echo "$nodes" | datamash -t "," sum 1 min 1 max 1 mean 1 median 1 mode 1`,"; fi;
  widths="`grep -P -o '((?<=width: )\d+)|(Checking soundness(?= starts))|(Found soundness)' $f | pcregrep -M -o '\d+(?=\nChecking)'`";
  if [ -z "$widths" ]; then echo -n "0,0,0,0,0,0"; else echo -n "`echo "$widths" | datamash -t "," sum 1 min 1 max 1 mean 1 median 1 mode 1`"; fi;
  echo;
done; \
echo -n ","; \
echo -n "`grep -o 'Checking soundness starts' *.log | wc -l`,"; \
nodes="`grep -P -h -o '((?<=nodes: )\d+)|(Checking soundness(?= starts))|(Found soundness)' *.log | pcregrep -M -o '\d+(?=\nChecking)'`"; \
if [ -z "$nodes" ]; then echo -n "0,0,0,0,0,0"; else echo -n "`echo "$nodes" | datamash -t "," sum 1  min 1 max 1 mean 1 median 1 mode 1`,"; fi; \
widths="`grep -P -h -o '((?<=width: )\d+)|(Checking soundness(?= starts))|(Found soundness)' *.log | pcregrep -M -o '\d+(?=\nChecking)'`"; \
if [ -z "$widths" ]; then echo -n "0,0,0,0,0,0"; else echo -n "`echo "$widths" | datamash -t "," sum 1  min 1 max 1 mean 1 median 1 mode 1`"; fi; \
echo;
```

