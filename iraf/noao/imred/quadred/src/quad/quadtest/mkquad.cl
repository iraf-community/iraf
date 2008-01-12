# MKQUAD -- Make an artifical multi-readout image

procedure mkquad (image, exptime, ccdtype)

begin
	string	im, ccdt
	real	exp, sky

	string	amps, as, bs, cs, ds, ts, nampsyx, amplist
	int	tx1, tx2, ty1, ty2
	int	bx1, bx2, by1, by2
	int	cx1, cx2, cy1, cy2
	int	dx1, dx2, dy1, dy2
	int	txs1, txs2, tys1, tys2
	int	bxs1, bxs2, bys1, bys2
	int	nx, ny, dnx, dny, onx, ony
	int	nampx, nampy
	bool	use_amp[4]

	im   = image
	if (access (im//".imh"))
	    error (0, "Output image already exists")

	exp  = exptime
	ccdt = ccdtype
	sky  = skyrate
	amps = ccdpars.amplifiers

	nx = ccdpars.ncols
	ny = ccdpars.nlines

	# Set number of amplifiers and use_amp. This is a bit kludgy
	if (amps == "Quad") {
	    nampx = 2
	    nampy = 2
	    use_amp[1] = yes
	    use_amp[2] = yes
	    use_amp[3] = yes
	    use_amp[4] = yes
	    amplist = "11 12 21 22"
	} else if (amps == "LowerPair") {
	    nampx = 2
	    nampy = 1
	    use_amp[1] = yes
	    use_amp[2] = yes
	    use_amp[3] = no
	    use_amp[4] = no
	    amplist = "11 12"
	} else if (amps == "UpperPair") {
	    nampx = 2
	    nampy = 1
	    use_amp[1] = no
	    use_amp[2] = no
	    use_amp[3] = yes
	    use_amp[4] = yes
	    amplist = "21 22"
	} else if (amps == "LowerLeft") {
	    nampx = 1
	    nampy = 1
	    use_amp[1] = yes
	    use_amp[2] = no
	    use_amp[3] = no
	    use_amp[4] = no
	    amplist = "11"
	}

	# Parse sections strings.
	ccdsection (ccdpars.trimsec) | scan (tx1, tx2, ty1, ty2)
	tx1 = max (1,  tx1)
	tx2 = min (nx, tx2)
	ty1 = max (1,  ty1)
	ty2 = min (ny, ty2)

	ccdsection (ccdpars.biassec) | scan (bx1, bx2, by1, by2)
	bx1 = max (1,  bx1)
	bx2 = min (nx, bx2)
	by1 = max (1,  by1)
	by2 = min (ny, by2)

	ccdsection (ccdpars.datasec) | scan (dx1, dx2, dy1, dy2)
	dx1 = max (1,  dx1)
	dx2 = min (nx, dx2)
	dy1 = max (1,  dy1)
	dy2 = min (ny, dy2)

	# Number of pixels to trim
	txs1 = tx1 - 1
	txs2 = dx2 - tx2
	tys1 = ty1 - 1
	tys2 = dy2 - ty2

	# Number of pixels to skip before overscan strip
	bxs1 = bx1 - dx2 - 1
	bxs2 = nx  - bx2
	bys1 = by1 - 1
	bys2 = ny  - by2

	# Number of pixels in subimages
	nx  = nx / nampx
	ny  = ny / nampy
	dnx = (dx2 - dx1 + 1) / nampx
	dny = (dy2 - dy1 + 1) / nampy
	onx = nx - dnx
	ony = ny

	# Set ampsec for all amps
	printf ("[1:%d,1:%d]\n", nx, ny) | scan (as)

	# Set sections for Amp11 & Amp21
	dx1 = 1
	dx2 = dx1 + dnx - 1
	dy1 = 1
	dy2 = dy1 + dny - 1
	printf ("[%d:%d,%d:%d]\n", dx1, dx2, dy1, dy2) | scan (ds)

	tx1 = dx1 + txs1 
	tx2 = dx2
	ty1 = dy1 + tys1
	ty2 = dy2
	printf ("[%d:%d,%d:%d]\n", tx1, tx2, ty1, ty2) | scan (ts)

	bx1 = dx2 + bxs1 + 1
	bx2 = nx  - bxs2
	by1 = 1 + bys1
	by2 = ny
	printf ("[%d:%d,%d:%d]\n", bx1, bx2, by1, by2) | scan (bs)

	if (use_amp[1]) {
	    mkamp (im//".11", exp, ccdt, ncols=nx, nlines=ny,
	    filter=filter, datasec=ds, trimsec=ts, biassec=bs, imdata=imdata,
	    skyrate=sky, zeroval=ccdpars.zero1, zeroslope=zeroslope,
	    badpix=badpix, badval=badval, flashval=flashval,
	    flashslope=flashslope, darkrate=darkrate, darkslope=darkslope,
	    flatslope=flatslope, gain=ccdpars.gain1, ron=ccdpars.ron1,
	    nonlin=ccdpars.nlin1, poisson=poisson, overwrite=yes)
	    hedit (im//".11", "asec11", as, show-, ver-, add+)
	    cx1 = 1 
	    cx2 = cx1 + dnx - 1
	    cy1 = 1
	    cy2 = cy1 + dny - 1
	    printf ("[%d:%d,%d:%d]\n", cx1, cx2, cy1, cy2) | scan (cs)
	    hedit (im//".11", "ccdsec", cs, show-, ver-, add+)
	}

	if (use_amp[3]) {
	    mkamp (im//".21", exp, ccdt, ncols=nx, nlines=ny,
	    filter=filter, datasec=ds, trimsec=ts, biassec=bs, imdata=imdata,
	    skyrate=sky, zeroval=ccdpars.zero3, zeroslope=zeroslope,
	    badpix=badpix, badval=badval, flashval=flashval,
	    flashslope=flashslope, darkrate=darkrate, darkslope=darkslope,
	    flatslope=flatslope, gain=ccdpars.gain3, ron=ccdpars.ron3,
	    nonlin=ccdpars.nlin3, poisson=poisson, overwrite=yes)
	    hedit (im//".21", "asec21", as, show-, ver-, add+)
	    cx1 = 1 
	    cx2 = cx1 + dnx - 1
	    cy1 = dny + 1
	    cy2 = cy1 + dny - 1
	    printf ("[%d:%d,%d:%d]\n", cx1, cx2, cy1, cy2) | scan (cs)
	    hedit (im//".21", "ccdsec", cs, show-, ver-, add+)
	}

	# Set sections for Amp12 & Amp22
	dx1 = onx + 1
	dx2 = nx
	dy1 = 1
	dy2 = dy1 + dny - 1
	printf ("[%d:%d,%d:%d]\n", dx1, dx2, dy1, dy2) | scan (ds)

	tx1 = dx1 + txs1
	tx2 = dx2
	ty1 = dy1 + tys1
	ty2 = dy2
	printf ("[%d:%d,%d:%d]\n", tx1, tx2, ty1, ty2) | scan (ts)

	bx1 = 1 + bxs1
	bx2 = onx - bxs2
	by1 = 1 + bys1
	by2 = ny
	printf ("[%d:%d,%d:%d]\n", bx1, bx2, by1, by2) | scan (bs)

	if (use_amp[2]) {
	    mkamp (im//".12", exp, ccdt, ncols=nx, nlines=ny,
	    filter=filter, datasec=ds, trimsec=ts, biassec=bs, imdata=imdata,
	    skyrate=sky, zeroval=ccdpars.zero2, zeroslope=zeroslope,
	    badpix=badpix, badval=badval, flashval=flashval,
	    flashslope=flashslope, darkrate=darkrate, darkslope=darkslope,
	    flatslope=flatslope, gain=ccdpars.gain2, ron=ccdpars.ron2,
	    nonlin=ccdpars.nlin2, poisson=poisson, overwrite=yes)
	    hedit (im//".12", "asec12", as, show-, ver-, add+)
	    cx1 = dnx + 1 
	    cx2 = cx1 + dnx - 1
	    cy1 = 1
	    cy2 = cy1 + dny - 1
	    printf ("[%d:%d,%d:%d]\n", cx1, cx2, cy1, cy2) | scan (cs)
	    hedit (im//".12", "ccdsec", cs, show-, ver-, add+)
	}

	if (use_amp[4]) {
	    mkamp (im//".22", exp, ccdt, ncols=nx, nlines=ny,
	    filter=filter, datasec=ds, trimsec=ts, biassec=bs, imdata=imdata,
	    skyrate=sky, zeroval=ccdpars.zero4, zeroslope=zeroslope,
	    badpix=badpix, badval=badval, flashval=flashval,
	    flashslope=flashslope, darkrate=darkrate, darkslope=darkslope,
	    flatslope=flatslope, gain=ccdpars.gain4, ron=ccdpars.ron4,
	    nonlin=ccdpars.nlin4, poisson=poisson, overwrite=yes)
	    hedit (im//".22", "asec22", as, show-, ver-, add+)
	    cx1 = dnx + 1 
	    cx2 = cx1 + dnx - 1
	    cy1 = dny + 1
	    cy2 = cy1 + dny - 1
	    printf ("[%d:%d,%d:%d]\n", cx1, cx2, cy1, cy2) | scan (cs)
	    hedit (im//".22", "ccdsec", cs, show-, ver-, add+)
	}

	# Set NAMPSYX  and amplistin header
	nampsyx = str (nampy) // " " // str (nampx)
	hedit (im//".??.imh", "nampsyx", nampsyx, show-, ver-, add+)
	hedit (im//".??.imh", "amplist", amplist, show-, ver-, add+)

	quadjoin (im, output="", delete=yes)

end
