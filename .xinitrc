#!/bin/sh

# Enable numpad.
numlockx &

# Setup our monitors.
xrandr --output HDMI-A-0 --mode 1920x1080 --rotate left --output DisplayPort-0 --mode 2560x1440 --right-of HDMI-A-0 &

# Set a nice background.
feh --bg-center ~/Pictures/Wallpapers/wild_rose_flower_bush_pink_119697_2560x1440.jpg ~/Pictures/Wallpapers/wild_rose_bush_pink_flowers_119656_1080x1920.jpg &

# Launch our window manager.
exec xmonad
