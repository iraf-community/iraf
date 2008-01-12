procedure qstatistics (images)

begin
	string	tmp

	tmp = mktemp ("uparm$tmp")

	#quadsections (image, window=window, section="", template="",
	#xskip1=INDEF, xskip2=INDEF, xtrim1=INDEF, xtrim2=INDEF, ytrim1=INDEF,
	#ytrim2=INDEF, >> tmp)
	quadsections (image, window=window, section="", template="", >> tmp)

	# Calculate image statistics
	imstatistics ("@"//tmp, fields=fields, lower=lower, upper=upper,
	binwidth=binwidth, format=format)


	delete (tmp, ver-)
end
