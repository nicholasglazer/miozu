#!/bin/sh

case ${MONS_NUMBER} in
    1)
        mons -o
        echo one
        ;;
    2)
        mons -S 2,0:R
        xrandr --output eDPT-1 --rotate right &
        xrandr --output DP-3 --mode 1920x1080 --rate 120 &
        echo two
        ;;
    *)
        # Handle it manually
        echo all
        ;;
esac
