for file in  dpix.fits mef.fits sif.fits ypix.fits ypix2.fits ypix3.fits
do
	echo
	echo ---- $file ----
	echo
	voiminfo -a $file
	voiminfo -i $file
	voiminfo -b $file
	voiminfo -c $file
	voiminfo -n $file
	voiminfo -s $file
	voiminfo --all $file
	voiminfo --info $file
	voiminfo --box $file
	voiminfo --corners $file
	voiminfo --naxes $file
	voiminfo --sex $file
	echo
	echo ---------------
	echo
done
for file in  dpix.fits mef.fits sif.fits ypix.fits ypix2.fits ypix3.fits
do
	echo
	echo ---- $file ----
	echo
	voiminfo -a http://www.nrao.edu/~wyoung/test-data/$file
	voiminfo -i http://www.nrao.edu/~wyoung/test-data/$file
	voiminfo -b http://www.nrao.edu/~wyoung/test-data/$file
	voiminfo -c http://www.nrao.edu/~wyoung/test-data/$file
	voiminfo -n http://www.nrao.edu/~wyoung/test-data/$file
	voiminfo -s http://www.nrao.edu/~wyoung/test-data/$file
	voiminfo --all http://www.nrao.edu/~wyoung/test-data/$file
	voiminfo --info http://www.nrao.edu/~wyoung/test-data/$file
	voiminfo --box http://www.nrao.edu/~wyoung/test-data/$file
	voiminfo --corners http://www.nrao.edu/~wyoung/test-data/$file
	voiminfo --naxes http://www.nrao.edu/~wyoung/test-data/$file
	voiminfo --sex http://www.nrao.edu/~wyoung/test-data/$file
	echo
	echo ---------------
	echo
done
