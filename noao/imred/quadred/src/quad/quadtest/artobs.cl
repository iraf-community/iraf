# ARTOBS -- Simulate observe command using artificial data.

procedure artobs ()

begin
	string	image, oim, ccdt
	int	picnum, nexps, i
	real	exptime
	string	imtitle

	# Get ccdtype
	ccdt = obspars.ccdtype

	# Get number of pictures to take
	nexps   = npics

	# Get next picture number
	if (obspars.autopicnum) {
	    picnum = obspars.picture.p_value
	} else {
	    picnum = obspars.picture
	}

	# Get exposure time
	if (ccdt != "zero") {
	    exptime = obspars.exposure
	} else {
	    exptime = 0.0
	}

	# Set filter
	if (obspars.setfilter != "none" && ccdt != "zero" && ccdt != "dark") {
	    if (instrpars.instrname != "")
		mkquad.filter = instrpars.filter1
	}

	# Get imtitle. This MUST always be the last interactive prompt!
	imtitle = title

	for (i = picnum; i < picnum+nexps; i = i+1) {

	    # Make image name
	    if (ccdt == "object") {
		printf ("obj%03d\n", i) | scan (image)
	    } else {
		printf ("%s%03d\n", ccdt, i) | scan (image)
	    }
	    if (access (image//".imh")) {
		oim = image
		image = mktemp (image//".")
		printf ("Output image %s already exists...  %s used \n", oim, 
		image)
	    }

	    if (ccdt == "dflat" || ccdt == "pflat")
		ccdt = "flat"

	    if (ccdt == "sflat" || ccdt == "comp" )
	       ccdt = "other"

	    # Call MKQUAD task
	    mkquad (image, exptime, ccdt)
	    hedit  (image, "i_title", imtitle, add+, ver-, show-)
	    obspars.picture.p_value = i + 1
	    printf ("Image %s written to disk\n", image, > "STDERR")
	}

end
