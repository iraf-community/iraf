procedure quadproc (images)

begin
	string	ims, in_list, cal_list, qp_list
	int	nims
	struct	buffer

	# Freeze input image list 
	in_list = mktemp ("uparm$tmp")
	ccdlist (images, ccdtype="", names+, > in_list)
	ims = "@"//in_list

	# Check that the input list contains some images of the specified type.
	ccdssselect (ims, ccdtype=ccdtype, subset="") | count | scan (nims)
	if (nims == 0) {	# Nothing to do !
	    delete (in_list, ver-)
	    return
	}

	# Set initial values for the ccdproc parameters used for fitting the
	# overscan. These parameters may be modified during the interactive
	# fitting process, the new values being used for all subsequent fits.
	qccdproc.function    = function
	qccdproc.order       = order
	qccdproc.sample	     = sample
	qccdproc.naverage    = naverage
	qccdproc.niterate    = niterate
	qccdproc.low_reject  = low_reject
	qccdproc.high_reject = high_reject
	qccdproc.grow        = grow
	qccdproc.interactive = interactive

	if (overscan || trim || fixpix) {
	    # Only those images which must be treated specialy are processed
	    # with qproc:
	    #	1) Multiple readout
	    # 	2) Not already trimmed

	    # First process [OT] any calibration images which WILL BE USED so
	    # that we can be sure they will be reduced explicitly by qproc
	    # rather than automaticaly within ccdproc.
	    qp_list = mktemp ("uparm$tmp")

	    if (noproc) {

		cal_list = mktemp ("uparm$tmp")
		qpcalimage (images=ims, only_param=yes, check=no, > cal_list)
		qpselect ("@"//cal_list, ccdtype="", stop=no, > qp_list)
		delete (cal_list, ver-)
		count (qp_list) | scan (nims)
		if (nims > 0) {
		    printf ("Calibration images which will be processed:\n")
		    qnoproc (qp_list, fixpix=fixpix, overscan=overscan,
		    trim=trim, fixfile=fixfile, xskip1=xskip1, xskip2=xskip2,
		    xtrim1=xtrim1, xtrim2=xtrim2, ytrim1=ytrim1, ytrim2=ytrim2)

		    printf ("\nImages from the input list:\n")
		}

	    } else {

		cal_list = mktemp ("uparm$tmp")
		qpcalimage (images=ims, only_param=no, check=no, > cal_list)
		qpselect ("@"//cal_list, ccdtype="", stop=no, > qp_list)
		delete (cal_list, ver-)
		count (qp_list) | scan (nims)
		if (nims > 0) {
		    qproc (qp_list, fixpix=fixpix, overscan=overscan, trim=trim,
		    readaxis=readaxis, fixfile=fixfile, xskip1=xskip1,
		    xskip2=xskip2, xtrim1=xtrim1, xtrim2=xtrim2, ytrim1=ytrim1,
		    ytrim2=ytrim2)

		}
	    }

	    delete (qp_list, ver-)

	    # Now process all the remaining images.
	    qpselect (ims, ccdtype=ccdtype, stop=no, >> qp_list) 

	    if (noproc) {
		qnoproc (qp_list, fixpix=fixpix, overscan=overscan,
		trim=trim, fixfile=fixfile, xskip1=xskip1, xskip2=xskip2,
		xtrim1=xtrim1, xtrim2=xtrim2, ytrim1=ytrim1, ytrim2=ytrim2)
	    } else {
		qproc (qp_list, fixpix=fixpix, overscan=overscan, trim=trim,
		readaxis=readaxis, fixfile=fixfile, xskip1=xskip1,
		xskip2=xskip2, xtrim1=xtrim1, xtrim2=xtrim2, ytrim1=ytrim1,
		ytrim2=ytrim2)
	    }

	    delete (qp_list, ver-)

	}

	# Do all other operations with the standard ccdproc.

	if (noproc) {

	    # Handle those images that will be processed with qproc ....
	    qpselect (ims, ccdtype=ccdtype, stop=no, >> qp_list) 

	    # We must also include the calibration images or ccdproc will
	    # complain about missing calibrations.
	    qpcalimage (images=ims, only_param=no, check=no, > cal_list)
	    qpselect ("@"//cal_list, ccdtype="", stop=yes, >> qp_list)

	    if (zerocor || darkcor || flatcor || illumcor || fringecor ||
	    readcor || scancor) {
		qccdproc ("@"//qp_list, noproc=yes, 
		fixpix=no, overscan=no, trim=no, zerocor=zerocor,
		darkcor=darkcor, flatcor=flatcor, illumcor=illumcor,
		fringecor=fringecor, readcor=readcor, scancor=scancor, 
		ccdtype=ccdtype, max_cache=max_cache, readaxis=readaxis,
		fixfile=fixfile, biassec="image", trimsec="image", zero=zero,
		dark=dark, flat=flat, illum=illum, fringe=fringe,
		minreplace=minreplace, scantype=scantype, nscan=nscan)
	    }

	    printf ("\n")
	    delete (qp_list, ver-)

	    # ..... and those that won't
	    qpselect (ims, ccdtype=ccdtype, stop=yes, >> qp_list) 

	    qccdproc ("@"//qp_list, noproc=yes, 
	    fixpix=fixpix, overscan=overscan, trim=trim, zerocor=zerocor,
	    darkcor=darkcor, flatcor=flatcor, illumcor=illumcor,
	    fringecor=fringecor, readcor=readcor, scancor=scancor, 
	    ccdtype=ccdtype, max_cache=max_cache, readaxis=readaxis,
	    fixfile=fixfile, biassec="image", trimsec="image", zero=zero,
	    dark=dark, flat=flat, illum=illum, fringe=fringe,
	    minreplace=minreplace, scantype=scantype, nscan=nscan)

	    delete (qp_list, ver-)

	} else {

	    # Validate fixfile
	    if (fixpix) {
		match ("single_readout", fixfile) | scan (buffer)
		if (stridx ("#", buffer) == 0) {
		    buffer = "fixfile " // fixfile // 
		    " cannot be used with multi-readout images"
		    error (0, buffer)
		}
	    }

	    qccdproc (ims, ccdtype=ccdtype, max_cache=max_cache, noproc=no,
	    fixpix=fixpix, overscan=overscan, trim=trim, zerocor=zerocor,
	    darkcor=darkcor, flatcor=flatcor, illumcor=illumcor, 
	    fringecor=fringecor, readcor=readcor, scancor=scancor, 
	    readaxis=readaxis, fixfile=fixfile, biassec="image",
	    trimsec="image", zero=zero, dark=dark, flat=flat, illum=illum,
	    fringe=fringe, minreplace=minreplace, scantype=scantype,
	    nscan=nscan, backup=backup, interactive=interactive,
	    verbose=verbose, logfile=logfile)

	    # Set task parameters used for overscan fitting to the ccdproc
	    # values which may have been adjusted interactively
	    function.p_value    = qccdproc.function
	    order.p_value       = qccdproc.order
	    sample.p_value      = qccdproc.sample
	    naverage.p_value    = qccdproc.naverage
	    niterate.p_value    = qccdproc.niterate
	    low_reject.p_value  = qccdproc.low_reject
	    high_reject.p_value = qccdproc.high_reject
	    grow.p_value        = qccdproc.grow

	}

	delete (in_list,   ver-)
end
