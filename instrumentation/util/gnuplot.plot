set xlabel "Iterations"
set ylabel "Cycles"
set xrange [*:6000]
set yrange [*:*]
set term post eps
set output 'Side_by_Side_Graph/Side_by_Side_Graph_MOVE.eps'
set output type eps enhanced color size 4in, 3in "Helvetica, 20"
set size '1,1'
set key off
set title 'Cycles per Operations for MOVE' font 'Helvetica,25'
set ytics font 'Helvetica,18' 
set ylabel font 'Helvetica,25' 
set xtics font 'Helvetica,18' 
set xlabel font 'Helvetica,25' 
set style line 1 pointtype 3 pointsize 1 linecolor rgb "red"
plot 'Side_by_Side_Graph/MOVE_COMPARISON.data' using 1:2 lt 1, 'Side_by_Side_Graph/MOVE_COMPARISON.data' using 1:3 with lines 
