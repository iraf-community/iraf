procedure qproc (image_list)

begin
	struct	buffer
	string	image, answr, imtype
	int	i, len, nampsx, nampsy

	imtype = "." // envget ("imtype")
	i = stridx (",", imtype)
	if (i > 0)
	    imtype = substr (imtype, 1, i-1)
	i = strlen (imtype)

	cache ("quadsplit", "quadjoin", "ccdproc", "quadproc")


	# Validate fixfile
	if (fixpix) {
	    match ("single_readout", fixfile) | scan (buffer)
	    if (stridx ("#", buffer) == 0) {
		buffer = "fixfile " // fixfile // 
		" cannot be used with multi-readout images"
		error (0, buffer)
	    }
	}

	# Initialise interactive query
	if (ccdproc.interactive) {
	    answer.p_value = "yes"
	    answr = "yes"
	} else {
	    answr = "NO"
	}

	fd = image_list
	while (fscan (fd, image) != EOF) {

	    len = strlen (image)
	    if (substr(image, len-i+1, len) == imtype) {
		image = substr (image, 1, len-i)
	    }

	    # Split out one image for each quadrant and set header sections
	    #quadsplit (image, output="",
	    #xskip1=xskip1, xskip2=xskip2, xtrim1=xtrim1, xtrim2=xtrim2,
	    #ytrim1=ytrim1, ytrim2=ytrim2, clobber=yes)
	    quadsplit (image, output="", clobber=yes)


	    # Find out of interactive fit is required for this image
	    if (answr == "yes" || answr == "no") {
		printf ("Fit overscan vector for %s interactively\n", image) | 
		scan (buffer)
		answer.p_prompt=buffer
		answr = answer
	    }

	    # Overscan correct and trim
	    if (answr == "yes" || answr == "YES") {
		ccdproc.interactive = yes

		print ("YES") | ccdproc (image//".??"//imtype, fixpix=fixpix,
		overscan=overscan, trim=trim, readaxis=readaxis,
		fixfile=fixfile, biassec="image", trimsec="image",
		ccdtype="", max_cache=0, noproc=no, zerocor=no, darkcor=no,
		flatcor=no, illumcor=no, fringecor=no, readcor=no,
		scancor=no, zero="", dark="", flat="", illum="", fringe="",
		minreplace=1., scantype="shortscan", nscan=1, backup="", 
		logfile="", verbose=no, >> "dev$null")

		# Set parameters of quadproc used for overscan fitting to match
                # the ccdproc values which may have been adjusted interactively.
		# We do this on every pass in case there is a later interupt
		# of task execution. 
		quadproc.function.p_value    = ccdproc.function
		quadproc.order.p_value       = ccdproc.order
		quadproc.sample.p_value      = ccdproc.sample
		quadproc.naverage.p_value    = ccdproc.naverage
		quadproc.niterate.p_value    = ccdproc.niterate
		quadproc.low_reject.p_value  = ccdproc.low_reject
		quadproc.high_reject.p_value = ccdproc.high_reject
		quadproc.grow.p_value        = ccdproc.grow

		# Force the parameter update
		update ("quadproc")

	    } else {
		ccdproc.interactive = no

		ccdproc (image//".??"//imtype, fixpix=fixpix,
		overscan=overscan, trim=trim, readaxis=readaxis,
		fixfile=fixfile, biassec="image", trimsec="image",
		ccdtype="", max_cache=0, noproc=no, zerocor=no, darkcor=no,
		flatcor=no, illumcor=no, fringecor=no, readcor=no,
		scancor=no, zero="", dark="", flat="", illum="", fringe="",
		minreplace=1., scantype="shortscan", nscan=1, backup="",
		logfile="", verbose=no)
	    }

	    # Combine processed quadrants into single image
	    quadjoin (image, output="", delete=yes)

	}

	# Reset interactive flag if we haven't recieved a definative NO
	if (answr == "no") {
	    ccdproc.interactive = yes
	}
end
