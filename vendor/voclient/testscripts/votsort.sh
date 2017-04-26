for file in ned.xml http://www.nrao.edu/~wyoung/test-data/ned.xml
do
	echo 
	echo ---- $file ----
	echo 
	votsort $file >sort1.txt
	votsort -c 3 $file > sort2.txt
	votsort -d $file
	votsort -f vot $file
	votsort -f asv $file
	votsort -f bsv $file
	votsort -f csv $file
	votsort -f tsv $file
	votsort -f html $file
	votsort -f shtml $file
	votsort -f fits $file > sort.fits
	votsort -f ascii $file
	votsort -f xml $file
	votsort -f raw $file
	votsort -s $file
	votsort -t 10 $file
	votsort -i 3 $file
	votsort -n -f csv $file
	votsort -N "RA(deg)" $file > sort3.txt
	votsort -I main_col4 $file > sort4.txt
	votsort -U src.redshift $file >sort5.tx

	echo 
	echo --------------
	echo 
done
