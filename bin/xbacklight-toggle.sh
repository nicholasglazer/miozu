#!/bin/bash

backlight=$(xbacklight -get)
if [ $backlight != 0 ]; then
    xbacklight -set 0
    echo "set to 0"
else
    xbacklight -set 50
    echo "set to 50"
fi
