procedure qhistogram (image)

begin
	string	tmp, meta, im, subimage, amp, section
	int	nx, ny
#	real	zz1, zz2, mean, mode, min, max, sigma

	im = image

	tmp   = mktemp ("uparm$tmp")
	fdtmp = tmp
	meta  = mktemp ("uparm$meta")

	# Project image section on to quadrant boundaries.
	#quadsections (im, window=window, section="", template="$I$S $A $S\n", 
	#xskip1=INDEF, xskip2=INDEF, xtrim1=INDEF, xtrim2=INDEF,
	#ytrim1=INDEF, ytrim2=INDEF, >> tmp)
	quadsections (im, window=window, section="", template="$I$S $A $S\n", 
	    >> tmp)

#	# Set up histogram limits
#	switch (substr (scaling, 1, 1) {
#	case "s":	set
#	    zz1 = z1
#	    zz2 = z2

#	case minmax"


	if (listout) {
	    printf ("%s\n", im)
	    while (fscan (fdtmp, subimage, amp, section) != EOF) {

		printf ("\tAmp%s: section=%s\n\n", amp, section)

		imhist (subimage, z1=z1, z2=z2, binwidth=binwidth, nbins=nbins, 
		autoscale=autoscale, top_closed=top_closed, hist_type=hist_type,
		listout=listout, plot_type=plot_type, logy=logy, device=device)
	    }

	} else {
	    while (fscan (fdtmp, subimage) != EOF) {

		imhist (subimage, z1=z1, z2=z2, binwidth=binwidth, nbins=nbins, 
		autoscale=autoscale, top_closed=top_closed, hist_type=hist_type,
		listout=listout, plot_type=plot_type, logy=logy, device=device,
		>>G meta)

	    }
	    ccdgetparam (im, "nampsyx") | scan (ny, nx)
	    gkim (meta, device=device, output=plotfile, nx=nx, ny=ny, rotate=no,
	    fill=yes, interactive=no, cursor="")

	    delete (meta, ver-)
	}

	delete (tmp,  ver-)
end
