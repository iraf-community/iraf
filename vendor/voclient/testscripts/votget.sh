for file in sia.xml http://www.nrao.edu/~wyoung/test-data/sia.xml
do
	echo 
	echo ---- $file ----
	votget $file
	votget -b stuff  $file
	votget -e one $file
	votget -s $file
	votget -u VOX:Image_accessReference $file
	votget -v $file
	votget -x $file
	rm -rf testresults
	votget -D testresults $file
	votget -N 3 $file
	votget -d $file
	votget -o download.txt $file
	votget --base stuff  $file
	votget --extn one $file
	votget --sum $file
	votget --ucd VOX:Image_accessReference $file
	votget --verbose $file
	votget --extract $file
	rm -rf testresults
	votget --download testresults $file
	votget --num 3 $file
	votget --debug $file
	votget --output download.txt $file
	echo 
	echo --------------
	echo 
done
