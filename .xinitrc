#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

xrandr -s 0

xrdb -load $HOME/.Xdefaults

xsetroot -cursor_name left_ptr
#xsetroot -solid '#0f0f0f'

xset b off
xset s off
xset -dpms

#xmodmap -e "keysym Super_R = SunCompose"

fbsetbg -b -solid "#0f0f0f"
nitrogen --restore

xcompmgr &
#{ sleep 1; urxvt -name ncmpcpp-clock -e ncmpcpp; } &
#{ sleep 2; urxvt -name ncmpcpp-visualizer -e ncmpcpp; } &
#{ sleep 3; urxvt -name screen -e screen; } &
#{ sleep 4; urxvt -name irssi -e ssh and1@server; } &
#chromium-browser &
#gmpc &

export MPD_HOST=localhost
export MPD_PORT=6600
export OOO_FORCE_DESKTOP=gnome
exec wmname "LG3D"

exec xmonad > $HOME/.xmonad/log.txt 2>&1
