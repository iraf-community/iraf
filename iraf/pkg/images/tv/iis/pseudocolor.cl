#{ PSEUDOCOLOR -- Select pseudocolor enhancement.

# enhancement,s,a,linear,,,"type of pseudocolor enhancement:\n\
#     linear - map greyscale into a spectrum\n\
#     random - one randomly chosen color is assigned each greylevel\n\
#     8color - eight random colors\n\
# enter selection"
# window,b,h,yes,,,window display after enabling pseudocolor
# enhance,s,h

{
	# Query for enchancement and copy into local param, otherwise each
	# reference will cause a query.
	enhance = enhancement

	if (enhance == "linear")
	    _dcontrol (map = "linear", window=window)
	else if (enhance == "random")
	    _dcontrol (map = "random", window=window)
	else if (enhance == "8color")
	    _dcontrol (map = "8color", window=window)
	else
	    error (0, "unknown enhancement")
}
