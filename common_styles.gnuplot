set pointsize 1.0
set style line 80 lt rgb "#222222"

# Line style for grid
set style line 81 lt 3  # dashed
set style line 81 lt rgb "#AAAAAA"  # grey

set grid back linestyle 81
set border 3 back linestyle 80 # Remove border on top and right.  These
             # borders are useless and make it harder
             # to see plotted lines near the border.
    # Also, put it in grey; no need for so much emphasis on a border.
set xtics nomirror
set ytics nomirror

set mytics 10    # Makes logscale look good.
set mxtics 10    # Makes logscale look good.

# Line styles: try to pick pleasing colors, rather
# than strictly primary colors or hard-to-see colors
# like gnuplot's default yellow.  Make the lines thick
# so they're easy to see in small plots in papers.
set style line 1  lt rgb "#A00000" lw 2 pt 1
set style line 2  lt rgb "#00A000" lw 2 pt 6
set style line 3  lt rgb "#5060D0" lw 2 pt 2
set style line 4  lt rgb "#F25900" lw 2 pt 9
set style line 5  lt rgb "#5050A0" lw 2 pt 4
set style line 6  lt rgb "#0050A9" lw 2 pt 7
set style line 7  lt rgb "#445111" lw 2 pt 6
set style line 8  lt rgb "#035009" lw 2 pt 4
set style line 9  lt 3 lc rgb "#406030" lw 2 pt 8
set style line 10 lt 4 lc rgb "#5020F0" lw 2 pt 5
set style line 11 lt 1 lc rgb "#BB50A9" lw 2 pt 3
set style line 12 lt 2 lc rgb "#00BBA9" lw 2 pt 1
