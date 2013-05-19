#!/bin/sh

nf='#888888'
nb='#2a2a2a'
sf='#ffffff'
sb='#2a2a2a'
font='-*-terminus-medium-r-*-*-12-*-*-*-*-*-*-*'
dmenucmd="dmenu -fn $font -nb $nb -nf $nf -sb $sb -sf $sf"

netprof=`ls -1 /etc/network.d | $dmenucmd $*`
if [ "x$netprof" != "x" ]; then
    sudo netcfg -a
    sudo netcfg $netprof
fi
