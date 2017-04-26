procedure qnoproc (image_list)

begin
	string	image, buffer, imtype
	int	i, len, nampsx, nampsy, nlines
	bool	dofix, dotrim, doover

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	i = strlen (imtype)

	dofix  = fixpix
	doover = overscan
	dotrim = trim

	fd = image_list
	while (fscan (fd, image) != EOF) {

	    len = strlen (image)
	    if (substr(image, len-i+1, len) == imtype) {
		image = substr (image, 1, len-i)
	    }

	    # Report what processing steps will be performed by qproc
	    printf ("%s:", image)

	    if (fixpix) {
		ccdgetparam (image, "fixpix") | scan (buffer)
		dofix =  (buffer == "UNDEFINED!")
	    }

	    if (overscan) {
		ccdgetparam (image, "overscan") | scan (buffer)
		doover = (buffer == "UNDEFINED!") 
	    }

	    if (trim) {
		ccdgetparam (image, "trim") | scan (buffer)
		dotrim = (buffer == "UNDEFINED!") 
	    }

	    if (dofix || dotrim || doover) {
		ccdgetparam (image, "nampsyx") | scan (nampsy, nampsx)
		if (nampsx == 2 && nampsy == 2) {
		    printf (" (Quad-readout image)\n")
		} else if (nampsx == 2 || nampsy == 2) {
		    printf (" (Dual-readout image: nampsx=%d nampsy=%d)\n",
		    nampsx, nampsy)
		} else {
		    printf ("\n")
		}
		
		if (doover) {
		    printf ("  [TO BE DONE] Trim section is:\n")
		    #quadsections (image, window="trimsec", section="", 
		    #template="%18tAMP$A $S\n", xskip1=xskip1, xskip2=xskip2,
		    #xtrim1=xtrim1, xtrim2=xtrim2, ytrim1=ytrim1, ytrim2=ytrim2)
		    quadsections (image, window="trimsec", section="", 
		    template="%18tAMP$A $S\n")
		}

		if (dofix)
		    printf ("  [TO BE DONE] Bad pixel file is %s\n", fixfile)

		if (doover) {
		    printf ("  [TO BE DONE] Overscan section is:\n")
		    #quadsections (image, window="biassec", section="", 
		    #template="%18tAMP$A $S\n", xskip1=xskip1, xskip2=xskip2,
		    #xtrim1=xtrim1, xtrim2=xtrim2, ytrim1=ytrim1, ytrim2=ytrim2)
		    quadsections (image, window="biassec", section="", 
		    template="%18tAMP$A $S\n")
		}
	    }
	}
end
