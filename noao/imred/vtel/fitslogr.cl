#{ FITSLOGR -- Read all the headers on a FITS tape and print out some
# of the header information for each file.

{
    struct header, headline, tfile, irafname
    struct obsdate, lzero, keyword
    struct tape, outfile, zcm, meanafld, numpix, meanfld
    struct *fp
    int	sfnum, efnum, filenum, ssm
    int	hours, minutes, seconds
    bool append, mag

    if (!deftask ("rfits")) {
	print ("Task rfits not loaded. Load dataio and then try again.")
	bye
    }

    # Get the tape name and the output file name.
    tape = gettape
    outfile = getout

    # Get the starting and ending file numbers for the log.
    sfnum = getsfnum
    efnum = getefnum

    # Get the append flag.
    append = getapp

    # Get the mag flag.
    mag = getmag

    if (!append) {
	if (mag) {
            print ("File      fname     date    time     L-zero   zcm      meanafld   numpix", >> outfile)
	} else {
            print ("File      fname     date    time     L-zero   meanfld  numpix", >> outfile)
	}
    }

    filenum = sfnum
    while (YES) {
	
	# Read the next fits header from the tape.
        header = mktemp("temp")
	fp = header
	rfits (tape, filenum, make_image=no, long_header=yes, > header)

	# Initialize the output variables.
	tfile = "        "
	irafname = "       "
	obsdate = "        "
	lzero = "            "
	zcm = "     "
	meanafld = "     "
	numpix = "       "
	hours = 0
	minutes = 0
	seconds = 0

	# Now match keywords against this header to obtain needed output.
tfile = filenum
	while (fscan (fp, headline) != EOF) {
	    keyword = substr(headline, 1, 8)
	    if (keyword == "File: mt")
		tfile = substr(headline, 7, 15)
	    else if (keyword == "IRAFNAME")
		irafname = substr(headline, 12, 18)
	    else if (keyword == "OBS_DATE")
		obsdate = substr(headline, 23, 30)
	    else if (keyword == "OBS_TIME") {
		ssm = int(substr(headline, 23, 30)) # Seconds Since Midnight.
		hours = ssm/3600
		minutes = (ssm - (hours*3600))/60
		seconds = ssm - hours*3600 - minutes*60
	    }
	    else if (keyword == "L_ZERO  ")
		lzero = substr(headline, 19, 26)
	    else if (keyword == "ZCM     ")
		zcm = substr(headline, 18, 26)
	    else if (keyword == "MEANAFLD")
		meanafld = substr(headline, 18, 26)
	    else if (keyword == "MEAN_FLD")
		meanfld = substr(headline, 18, 26)
	    else if (keyword == "NUMPIX  ")
		numpix = substr(headline, 19, 30)
	    else if (keyword == "End of d") {
		print (headline, >> outfile)
		delete (header, verify-)
		bye
	    }
	}
	if (mag) {
	    print (tfile, irafname, obsdate, "  ", hours, minutes, seconds,
	        lzero, zcm, meanafld, numpix, >> outfile)
	} else {
	    print (tfile, irafname, obsdate, "  ", hours, minutes, seconds,
	        lzero, meanfld, numpix, >> outfile)
	}
	filenum = filenum + 1
        delete (header, verify-)
	if (filenum > efnum)
	    bye
    }
}
