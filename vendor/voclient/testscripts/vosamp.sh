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
vosamp status
vosamp list
vosamp access topcat
vosamp access Aladin
vosamp load dpix.fits
vosamp -t Aladin loadImage ypix2.fits
vosamp load 2mass.xml
vosamp load http://www.nrao.edu/~wyoung/test-data/ypix.fits
vosamp loadImage http://www.nrao.edu/~wyoung/test-data/ypix.fits
vosamp showRow 5
vosamp -t Aladin showRow 13
vosamp -t Aladin selectRows 5-9
vosamp -t Aladin showRow 17
#vosamp quit
