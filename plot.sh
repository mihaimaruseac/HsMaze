#!/bin/bash

echo 'set term png; plot "trace" using 1:2 title "Best" with lines, "trace" using 1:3 title "Second best" with lines, "trace" using 1:4 title "Average" with lines' | gnuplot > trace.png
