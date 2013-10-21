for file in ned.xml http://www.nrao.edu/~wyoung/test-data/ned.xml
do
	echo 
	echo ---- $file ----
	votcnv $file >convert0.txt
	votcnv -f vot $file >convert1.txt
	votcnv -f asv $file >convert2.txt
	votcnv -f bsv $file >convert3.txt
	votcnv -f csv $file >convert4.txt
	votcnv -f tsv $file >convert5.txt
	votcnv -f html $file >convert6.txt
	votcnv -f shtml $file >convert7.txt
	votcnv -f fits $file >convert8.txt
	votcnv -f ascii $file >convert9.txt
	votcnv -f xml $file >convert10.txt
	votcnv -f raw $file >convert11.txt
	votcnv -i 3 $file >convert12.txt
	votcnv -f asv -n $file >convert13.txt
	votcnv -f bsv -o convert14.txt -n $file 
	votcnv --fmt vot $file >convert1.txt
	votcnv --fmt asv $file >convert2.txt
	votcnv --fmt bsv $file >convert3.txt
	votcnv --fmt csv $file >convert4.txt
	votcnv --fmt tsv $file >convert5.txt
	votcnv --fmt html $file >convert6.txt
	votcnv --fmt shtml $file >convert7.txt
	votcnv --fmt fits $file >convert8.txt
	votcnv --fmt ascii $file >convert9.txt
	votcnv --fmt xml $file >convert10.txt
	votcnv --fmt raw $file >convert11.txt
	votcnv --indent 3 $file >convert12.txt
	votcnv --fmt asv --noheader $file >convert13.txt
	votcnv --fmt bsv --output convert14.txt -n $file 
	echo 
	echo --------------
	echo 
done
