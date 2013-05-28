#!/bin/sh

killall conky
killall dzen2
(sleep 1; ~/scripts/dzen/dz_top.sh) &
(sleep 1; ~/scripts/dzen/dz_bottom.sh) &
