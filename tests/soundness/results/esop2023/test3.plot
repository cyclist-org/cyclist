set key autotitle columnhead
set xtics nomirror
set ytics nomirror
set border 3

# set term qt persist
set term pngcairo size 600, 400
set output "test3.png"

# set title "Family C"

set xlabel "Number of nodes"
set ylabel "Execution time (ms)"

set key at graph 1.0, 0.4

# Spot: dasthtype 2
# XSD: dashtype 5
# Rel: dashtype 6

plot 'test3.dat' using 1:2 smooth csplines with lines dashtype 6 lc rgb "black", \
              '' using 1:3 smooth csplines with lines dashtype 2 lc rgb "black"
