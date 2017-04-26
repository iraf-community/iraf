#{ MROTLOGR -- Read all the headers on a FITS tape and print out some
# of the header information for each file. (for Carrington rotation maps)

{
    struct header, headline, tfile, irafname
    struct avbzero, keyword
    struct tape, outfile
    struct *fp
    int	sfnum, efnum, filenum
    bool append

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

    if (!append) {
        print ("File      fname    avbzero", >> outfile)
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
	avbzero = "     "

	# Now match keywords against this header to obtain needed output.
	while (fscan (fp, headline) != EOF) {
	    keyword = substr(headline, 1, 8)
	    if (keyword == "File: mt")
		tfile = substr(headline, 7, 15)
	    else if (keyword == "IRAFNAME")
		irafname = substr(headline, 12, 20)
	    else if (keyword == "AV_BZERO")
		avbzero = substr(headline, 19, 27)
	    else if (keyword == "L_ZERO  ")
		lzero = substr(headline, 19, 26)
	    else if (keyword == "End of d") {
		print (headline, >> outfile)
		delete (header, verify-)
		bye
	    }
	}
	print (tfile, irafname, avbzero, >> outfile)
	filenum = filenum + 1
        delete (header, verify-)
	if (filenum > efnum)
	    bye
    }
}
