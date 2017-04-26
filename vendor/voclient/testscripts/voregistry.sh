for resource in catalog image spectra table
do
	# voregistry --resolve --verbose --type $resource
	voregistry -rv -t $resource > /tmp/blah
	for band in Radio Optical GR Millimeter UV IR xr
	do
	# voregistry --resolve --verbose --bandpass $band --type $resource
	   voregistry -rv -b $band -t $resource > /tmp/blah
	   voregistry -rv -c -b $band -t $resource
	done
	voregistry -rvI -t $resource
	voregistry -rvR -t $resource
	voregistry -rvS -t $resource
	voregistry -rvT -t $resource
	voregistry -rvd -t $resource
	voregistry -rv -s "outflow" -t $resource
	#for flag in Title SerivceURL Subject Creator CoverageTemporal Version ShortName ReferenceURL ResourceType Publisher Waveband Identifier Description Type CoverageSpatial ContentLevel
	#do
	# voregistry --resolve --verbose --fields $flag --type $resource
	#done
done
