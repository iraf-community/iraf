#vosamp quit
vosamp status
ps -u wyoung | grep Aladin > isrunning.Aladin
count=`wc -l isrunning.Aladin | awk '{print $1}'`
if [ $count -lt 2 ]; then
	open /Applications/Aladin.app
fi
rm isrunning.Aladin
ps -u wyoung | grep TOPCAT > isrunning.TOPCAT
count=`wc -l isrunning.TOPCAT | awk '{print $1}'`
if [ $count -lt 2 ]; then
	open /Applications/TOPCAT.app
fi
rm isrunning.TOPCAT
sleep 10
echo
voatlas --samp m13
voatlas -R 12 -D 30 -s 1d
