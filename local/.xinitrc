# Xinitrc

xterm -geometry 80x60+10+10 -sl 500 -j -ls -fn 7x14 -title tucana \
    -e rsh tucana &
xterm -geometry 80x60+20+20 -sl 500 -j -ls -fn 7x14 -bg ivory -fg black &
xterm -geometry 80x60+30+30 -sl 500 -j -ls -fn 7x14 -bg gray85 -fg black &

if [ -d /slackware ]; then
    # Running redhat.
    fvwm2
else
    # Running slackware.
    fvwm95-2
fi
