for file in m31
do
	echo 
	echo ---- $file ----
	votopic -t catalog radio $file 0.10
	echo 
	echo --------------
	echo 
done
