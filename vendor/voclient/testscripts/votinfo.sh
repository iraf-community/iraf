for file in *.xml http://www.nrao.edu/~wyoung/test-data/ned.xml
do
	echo
	echo ---- $file ----
	echo
	votinfo $file
	votinfo -w -b $file
	votinfo -w -d $file
	votinfo -w -i $file
	votinfo -w -n param $file
	votinfo -w -n info $file
	votinfo -w -n rows $file
	votinfo -w -n cols $file
	votinfo -w -n resources $file
	votinfo -w -p $file
	votinfo -w -s $file
	votinfo -w -v $file
	echo
	echo --------------
	echo
	echo
	echo ---- $file ----
	echo
	votinfo $file
	votinfo --warn --brief $file
	votinfo --warn --description $file
	votinfo --warn --info $file
	votinfo --warn --numberOf=param $file
	votinfo --warn --numberOf=info $file
	votinfo --warn --numberOf=rows $file
	votinfo --warn --numberOf=cols $file
	votinfo --warn --numberOf=resources $file
	votinfo --warn --param $file
	votinfo --warn --size $file
	votinfo --warn --verbose $file
	echo
	echo --------------
	echo
done
