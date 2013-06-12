#!/bin/zsh
FG='#839496'
BG='#00141a'
FONT='Terminus-8'
CONKYBOTTOMRC='scripts/dzen/conkybottomrc'
CONKYTOPRC='scripts/dzen/conkytoprc'
CONKYCALRC='scripts/dzen/conky_cal'
count=1

dzcal () {
    while :
    do
        DATE=`date; cal`
        echo -n $topcount
        echo "^tw${DATE}"
        topcount=$((topcount + 1))
        sleep 1
    done | dzen2 -ta r -y 886 -w 800 -x 710 -l 7 -bg $BG -fg $FG -fn $FONT \
        -e 'button2=;entertitle=exec cal,uncollapse,unhide;leaveslave=collapse;leavetitle=collapse' &

}

cpubar () {
#    gcpubar -h 16 -s v | dzen2 -h 16 -w 100 -x 800 -bg $BG -fg $FG
}

arch () {
    while :
    do
        echo -n '^tw()^fg(#268bd2)^i(.dzen/icons/arch_10x10.xbm)^fg() '
        echo -n `uname -r`
        echo -n '^fg(#859900) '
        echo -n `hostname`
        echo -n '^fg() '
        echo -n `date +'%e %b %I:%M%P'`
        echo -n ' '
        battery_percent=$(acpi | sed -n -e 's/^.*,\s\([0-9]*\)%.*$/\1/p')
        if (($battery_percent < 30))
        then
            echo -n "^fg(#dc322f)^i(.dzen/icons/bat_empty_01.xbm)"
        elif (($battery_percent < 65))
        then
            echo -n "^fg(#b58900)^i(.dzen/icons/bat_low_01.xbm)"
        else
            echo -n "^fg(#859900)^i(.dzen/icons/bat_full_01.xbm)"
        fi
        echo -n $battery_percent"%"
        echo;
        sleep 1
    done | dzen2 -dock -ta l -y 0 -w 1600 -x 0 -bg $BG -fg $FG -fn $FONT \
        -e 'entertitle=,button1=togglecolapse'
}


# (trayer --widthtype request --heighttype pixel --height 16 --SetPartialStrut true --SetDockType true --expand true --transparent true --alpha 255) &

# dzcal &
arch &
cpubar &
