#!/bin/sh
# $XConsortium: xinitrc.cpp,v 1.4 91/08/22 11:41:34 rws Exp $

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/usr/X11R6/lib/X11/xinit/.Xresources
sysmodmap=/usr/X11R6/lib/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f $userresources ]; then
    xrdb -merge $userresources
fi

if [ -f $usermodmap ]; then
    xmodmap $usermodmap
fi

# start some nice programs
#xsetroot -solid SteelBlue
xbm=/tmp/bigmoon.xbm
zcat ./bin/bigmoon.xbm.gz > $xbm;  xsetroot -bitmap $xbm;  rm -f $xbm
xmodmap .xmodmaprc

xterm -g 80x6-2+2 -C -iconic -n "CONSOLE" -fn 6x10\
	-fg yellow -bg black -bd yellow &
xclock -padding 2 -hl yellow -bg NavyBlue -fg yellow -g 48x48-2-2 &
xload -fn 6x10 -bd green -bg black -fg orchid -geometry 48x48-2-61 &

# Use a single (uncompressed) copy of xgterm to share memory and speed startup.
xgterm=/tmp/xgterm
cp ./bin/xgterm $xgterm;  gzexe -d $xgterm

$xgterm -ls -g 80x50+015+15 -bg gray81 -fn 7x14 -e cl &
$xgterm -ls -g 80x50+005+05 -bg gray81 -fn 7x14 -T `hostname` &

$xgterm -ls -g 80x50+300+778 -bg gray81 -fn 7x14 -T "Aux1" &
$xgterm -ls -g 80x50+340+788 -bg gray81 -fn 7x14 -T "Aux2" &

rm -f $xgterm

fvwm
