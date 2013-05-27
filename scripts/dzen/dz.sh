#!/bin/sh

killall conky &
killall dzen2 &
/home/nathan/scripts/dzen/dz_top.sh &
/home/nathan/scripts/dzen/dz_bottom.sh &
