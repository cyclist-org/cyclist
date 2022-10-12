set key at graph 0.8, 0.9 autotitle columnhead
set xtics nomirror
set ytics nomirror
set border 3

# set term qt persist
set term pngcairo size 600, 400
set output "test2.png"

# set title "Family B"

set xlabel "Number of heights"
set ylabel "Execution time (ms)"

# Spot: dasthtype 2
# XSD: dashtype 5
# Rel: dashtype 6

plot 'test2.dat' using 1:2 smooth csplines with lines dashtype 2 lc rgb "black", \
              '' using 1:3 smooth csplines with lines dashtype 6 lc rgb "black"
