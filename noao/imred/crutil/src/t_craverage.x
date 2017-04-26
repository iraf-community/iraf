include	<error.h>
include	<imhdr.h>
include	<mach.h>

define	MAXBUF		500000	# Maximum pixel buffer

define	PLSIG		15.87	# Low percentile
define	PHSIG		84.13	# High percentile


# T_CRAVERAGE -- Detect, fix, and flag cosmic rays.  Also detect objects.
# Deviant pixels relative to a local average with the candidate pixel
# excluded and sigma are detected and replaced by the average value
# and/or written to a cosmic ray mask.  Average values above a the median
# of a background annulus are detected as objects and cosmic rays are
# excluded.  The object positions may be output in the mask.

procedure t_craverage ()

int	inlist			# Input image list
int	outlist			# Output image list
int	crlist			# Output mask list
int	avglist			# Output average list
int	siglist			# Output sigma list
int	crval			# Output cosmic ray mask value
int	objval			# Output object mask value
int	navg			# Averaging box size
int	nrej			# Number of high pixels to reject from average
int	nbkg			# Background width
int	nsig			# Sigma box size
real	lobjsig, hobjsig	# Object threshold sigmas
real	lcrsig, hcrsig		# CR threshold sigmas outside of object
real	var0			# Variance coefficient for DN^0 term
real	var1			# Variance coefficient for DN^1 term
real	var2			# Variance coefficient for DN^2 term
real	crgrw			# Cosmic ray grow radius
real	objgrw			# Object grow radius

int	i, nc, nl, nlstep, nbox, l1, l2, l3, l4, nl1, pmmode
pointer	sp, input, output, crmask, crmask1, extname, average, sigma
pointer	in, out, pm, aim, sim
pointer	inbuf, pinbuf, outbuf, pbuf, abuf, sbuf

real	clgetr()
int	clgeti(), imtopenp(), imtgetim()
pointer	immap(), imgs2s(), imgs2r(), imps2r(), imps2s()
errchk	immap, imgs2s, imgs2r, imps2r, imps2s, craverage, crgrow, imgstr

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (crmask, SZ_FNAME, TY_CHAR)
	call salloc (crmask1, SZ_FNAME, TY_CHAR)
	call salloc (average, SZ_FNAME, TY_CHAR)
	call salloc (sigma, SZ_FNAME, TY_CHAR)
	call salloc (extname, SZ_FNAME, TY_CHAR)

	# Get parameters.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	crlist = imtopenp ("crmask")
	avglist = imtopenp ("average")
	siglist = imtopenp ("sigma")
	crval = clgeti ("crval")
	objval = clgeti ("objval")
	navg = max (1, clgeti ("navg") / 2)
	nrej = min (clgeti ("nrej"), navg-1)
	nbkg = clgeti ("nbkg")
	nsig = clgeti ("nsig")
	lobjsig = clgetr ("lobjsig")
	hobjsig = clgetr ("hobjsig")
	lcrsig = clgetr ("lcrsig")
	hcrsig = clgetr ("hcrsig")
	nbox = 2 * (navg + nbkg) + 1
	var0 = clgetr ("var0")
	var1 = clgetr ("var1")
	var2 = clgetr ("var2")
	crgrw = clgetr ("crgrow")
	objgrw = clgetr ("objgrow")

	# Do the input images.
	Memc[crmask1] = EOS
	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		Memc[output] = EOS
	    if (imtgetim (crlist, Memc[crmask], SZ_FNAME) == EOF)
	        call strcpy (Memc[crmask1], Memc[crmask], SZ_FNAME)
	    else if (Memc[crmask] == '!')
	        call strcpy (Memc[crmask], Memc[crmask1], SZ_FNAME)
	    if (imtgetim (avglist, Memc[average], SZ_FNAME) == EOF)
		Memc[average] = EOS
	    if (imtgetim (siglist, Memc[sigma], SZ_FNAME) == EOF)
		Memc[sigma] = EOS

	    # Map the input and output images.
	    iferr {
		in = NULL; out = NULL; pm = NULL; aim = NULL; sim = NULL
		inbuf = NULL; pinbuf = NULL; outbuf = NULL; pbuf = NULL;
		abuf = NULL; sbuf=NULL

		in = immap (Memc[input], READ_ONLY, 0)
		if (Memc[output] != EOS)
		    out = immap (Memc[output], NEW_COPY, in)
		if (Memc[crmask] != EOS) {
		    if (Memc[crmask] == '!')
			call imgstr (in, Memc[crmask+1], Memc[crmask], SZ_FNAME)
		    pmmode = READ_WRITE
		    iferr (call imgstr (in, "extname", Memc[extname], SZ_FNAME))
			call strcpy ("pl", Memc[extname], SZ_FNAME)
		    call xt_maskname (Memc[crmask], Memc[extname], pmmode,
			Memc[crmask], SZ_FNAME)
		    iferr (pm = immap (Memc[crmask], pmmode, 0)) {
			pmmode = NEW_COPY
			pm = immap (Memc[crmask], pmmode, in)
		    }
		}
		if (Memc[average] != EOS)
		    aim = immap (Memc[average], NEW_COPY, in)
		if (Memc[sigma] != EOS)
		    sim = immap (Memc[sigma], NEW_COPY, in)

		# Go through the input in large blocks of lines.  If the
		# block is smaller than the whole image overlap the blocks
		# so the average only has boundaries at the ends of the image.
		# However, the output is done in non-overlapping blocks with
		# the pointers are adjusted so that addresses can be in the
		# space of the input block.  CRAVERAGE does not address
		# outside of the output data block.  Set the mask values
		# based on the distances to the nearest good pixels.

		nc = IM_LEN(in,1)
		nl = IM_LEN(in,2)
		nlstep = max (1, MAXBUF / nc - nbox)

		do i = 1, nl, nlstep {
		    l1 = i
		    l2 = min (nl, i + nlstep - 1)
		    l3 = max (1, l1 - nbox / 2)
		    l4 = min (nl, l2 + nbox / 2)
		    nl1 = l4 - l3 + 1
		    inbuf = imgs2r (in, 1, nc, l3, l4)
		    if (out != NULL)
			outbuf = imps2r (out, 1, nc, l1, l2) - (l1 - l3) * nc
		    if (pm != NULL) {
			if (pmmode == READ_WRITE) {
			    pinbuf = imgs2s (pm, 1, nc, l3, l4)
			    pbuf = imps2s (pm, 1, nc, l1, l2)
			    call amovs (Mems[pinbuf+(l1-l3)*nc],
				Mems[pbuf], nc*(l2-l1+1))
			    pbuf = pbuf - (l1 - l3) * nc
			} else {
			    pinbuf = NULL
			    pbuf = imps2s (pm, 1, nc, l1, l2)
			    call aclrs (Mems[pbuf], nc*(l2-l1+1))
			    pbuf = pbuf - (l1 - l3) * nc
			}
		    }
		    if (aim != NULL)
			abuf = imps2r (aim, 1, nc, l1, l2) - (l1 - l3) * nc
		    if (sim != NULL)
			sbuf = imps2r (sim, 1, nc, l1, l2) - (l1 - l3) * nc
		    if (pinbuf == NULL)
			call craverage (inbuf, outbuf, pbuf, abuf, sbuf,
			    nc, nl1, l1-l3+1, l2-l3+1, navg, nrej, nbkg,
			    var0, var1, var2, nsig, lcrsig, hcrsig,
			    lobjsig, hobjsig, crval, objval)
		    else
			call craverage1 (inbuf, pinbuf, outbuf, pbuf, abuf,
			    sbuf, nc, nl1, l1-l3+1, l2-l3+1, navg, nrej, nbkg,
			    var0, var1, var2, nsig, lcrsig, hcrsig,
			    lobjsig, hobjsig, crval, objval)
		}

		# Grow regions if desired.  The routines are nops if the
		# grow is zero.

		if (pm != NULL) {
		    if (pmmode != READ_WRITE) {
			call imunmap (pm)
			iferr (pm = immap (Memc[crmask], READ_WRITE, 0))
			    call error (1, "Can't reopen mask for growing")
		    }

		    if (crval == objval)
			call crgrow (pm, max (crgrw, objgrw), crval, crval)
		    else {
			call crgrow (pm, crgrw, crval, crval)
			call crgrow (pm, objgrw, objval, objval)
		    }
		}
	    } then
		call erract (EA_WARN)

	    if (sim != NULL)
		call imunmap (sim)
	    if (aim != NULL)
		call imunmap (aim)
	    if (pm != NULL)
		call imunmap (pm)
	    if (out != NULL)
		call imunmap (out)
	    call imunmap (in)
	}

	call imtclose (inlist)
	call imtclose (outlist)
	call imtclose (crlist)
	call imtclose (avglist)
	call imtclose (siglist)

	call sfree (sp)
end


# CRAVERAGE -- Detect, replace, and flag cosmic rays.
# A local background is computed using moving box averages to avoid
# contaminating bad pixels.  If variance model is given then that is
# used otherwise a local sigma is computed in blocks (it is not a moving box
# for efficiency) by using a percentile point of the sorted pixel values to
# estimate the width of the distribution uncontaminated by bad pixels).  Once
# the background and sigma are known deviant pixels are found by using sigma
# threshold factors.

procedure craverage (in, out, pout, aout, sout, nc, nl, nl1, nl2,
	navg, nrej, nbkg, var0, var1, var2, nsig, lcrsig, hcrsig,
	lobjsig, hobjsig crval, objval)

pointer	in			#I Input data
pointer	out			#O Output data
pointer	pout			#O Output mask (0=good, 1=bad)
pointer	aout			#O Output averages
pointer	sout			#O Output sigmas
int	nc, nl			#I Number of columns and lines
int	nl1, nl2		#I Lines to compute
int	navg			#I Averaging box half-size
int	nrej			#I Number of high pixels to reject from average
int	nbkg			#I Median background width
real	var0			#I Variance coefficient for DN^0 term
real	var1			#I Variance coefficient for DN^1 term
real	var2			#I Variance coefficient for DN^2 term
int	nsig			#I Sigma box size
real	lcrsig, hcrsig		#I Threshold sigmas outside of object
real	lobjsig, hobjsig	#I Object threshold sigmas
int	crval			#I CR mask value
int	objval			#I Object mask value

int	i, j, c, c1, c2, c3, c4, l, l1, l2, l3, l4, n1, n2
int	navg2, nbkg2, nsig2, plsig, phsig
real	data, avg, bkg, sigma, losig, hosig
real	low, high, cravg(), amedr()
pointer	stack, avgs, bkgs, sigs, work1, work2
pointer	ptr1, ptr2, ip, op, pp, ap, sp

begin
	navg2 = (2 * navg + 1) ** 2
	nbkg2 = (2 * (navg + nbkg) + 1) ** 2 - navg2
	nsig2 = nsig * nsig

	call smark (stack)
	call salloc (avgs, nc, TY_REAL)
	call salloc (bkgs, nc, TY_REAL)
	call salloc (sigs, nc, TY_REAL)
	call salloc (work1, navg2, TY_REAL)
	call salloc (work2, max (nsig2, nbkg2), TY_REAL)

	if (var0 != 0. && var1 == 0. && var2 ==0.)
	    call amovkr (sqrt(var0), Memr[sigs], nc)

	avgs = avgs - 1
	sigs = sigs - 1
	bkgs = bkgs - 1

	plsig = nint (PLSIG*nsig2/100.-1)
	phsig = nint (PHSIG*nsig2/100.-1)
	losig = lobjsig / sqrt (real(navg2-1))
	hosig = hobjsig / sqrt (real(navg2-1))

	do l = nl1, nl2 {
	    # Compute statistics.
	    l1 = max (1, l-navg-nbkg)
	    l2 = max (1, l-navg)
	    l3 = min (nl, l+navg)
	    l4 = min (nl, l+navg+nbkg)
	    ap = aout + (l - 1) * nc
	    do c = 1, nc {
		c1 = max (1, c-navg-nbkg)
		c2 = max (1, c-navg)
		c3 = min (nc, c+navg)
		c4 = min (nc, c+navg+nbkg)
		ptr1 = work1
		ptr2 = work2
		n1 = 0
		n2 = 0
		do j = l1, l2-1 {
		    ip = in + (j - 1) * nc + c1 - 1
		    do i = c1, c4 {
			Memr[ptr2] = Memr[ip]
			n2 = n2 + 1
			ptr2 = ptr2 + 1
			ip = ip + 1
		    }
		}
		do j = l2, l3 {
		    ip = in + (j - 1) * nc + c1 - 1
		    do i = c1, c2-1 {
			Memr[ptr2] = Memr[ip]
			n2 = n2 + 1
			ptr2 = ptr2 + 1
			ip = ip + 1
		    }
		    do i = c2, c3 {
			if (j != l || i != c) {
			    Memr[ptr1] = Memr[ip]
			    n1 = n1 + 1
			    ptr1 = ptr1 + 1
			}
			ip = ip + 1
		    }
		    do i = c3+1, c4 {
			Memr[ptr2] = Memr[ip]
			n2 = n2 + 1
			ptr2 = ptr2 + 1
			ip = ip + 1
		    }
		}
		do j = l3+1, l4 {
		    ip = in + (j - 1) * nc + c1 - 1
		    do i = c1, c4 {
			Memr[ptr2] = Memr[ip]
			n2 = n2 + 1
			ptr2 = ptr2 + 1
			ip = ip + 1
		    }
		}
		avg = cravg (Memr[work1], n1, nrej)
		bkg = amedr (Memr[work2], n2)
		Memr[bkgs+c] = bkg
		Memr[avgs+c] = avg
		if (aout != NULL) {
		    Memr[ap] = avg - bkg
		    ap = ap + 1
		}
	    }

	    # Compute sigmas and output if desired.
	    if (var0 != 0. || var1 != 0. || var2 != 0.) {
		if (var1 != 0.) {
		    if (var2 != 0.) {
			do c = 1, nc {
			    data = max (0., Memr[avgs+c])
			    Memr[sigs+c] = sqrt (var0+var1*data+var2*data**2)
			}
		    } else {
			do c = 1, nc {
			    data = max (0., Memr[avgs+c])
			    Memr[sigs+c] = sqrt (var0 + var1 * data)
			}
		    }
		} else if (var2 != 0.) {
		    do c = 1, nc {
			data = max (0., Memr[avgs+c])
			Memr[sigs+c] = sqrt (var0 + var2 * data**2)
		    }
		}
	    } else {
		# Compute sigmas from percentiles.  This is done in blocks.
		if (mod (l-nl1, nsig) == 0 && l<nl-nsig+1) {
		    do c = 1, nc-nsig+1, nsig {
			ptr2 = work2
			n2 = 0
			do j = l, l+nsig-1 {
			    ip = in + (j - 1) * nc + c - 1
			    do i = 1, nsig {
				Memr[ptr2] = Memr[ip]
				n2 = n2 + 1
				ptr2 = ptr2 + 1
				ip = ip + 1
			    }
			}
			call asrtr (Memr[work2], Memr[work2], n2)
			sigma = (Memr[work2+phsig] - Memr[work2+plsig]) / 2.
			call amovkr (sigma, Memr[sigs+c], nsig)
		    }
		    call amovkr (sigma, Memr[sigs+c], nc-c+1)
		}
	    }
	    if (sout != NULL) {
		sp = sout + (l - 1) * nc
		do c = 1, nc {
		    Memr[sp] = Memr[sigs+c]
		    sp = sp + 1
		}
	    }

	    # Detect, fix, and flag cosmic rays.
	    if (pout == NULL && out == NULL)
		;
	    else if (pout == NULL) {
		ip = in + (l - 1) * nc
		op = out + (l - 1) * nc
		do c = 1, nc {
		    data = Memr[ip]
		    avg = Memr[avgs+c]
		    bkg = Memr[bkgs+c]
		    sigma = Memr[sigs+c]
		    low = bkg - losig * sigma
		    high = bkg + hosig * sigma
		    if (avg < low || avg > high) {
			Memr[op] = data
		    } else {
			low = avg - lcrsig * sigma
			high = avg + hcrsig * sigma
			if (data < low || data > high)
			    Memr[op] = avg
			else
			    Memr[op] = data
		    }
		    ip = ip + 1
		    op = op + 1
		}
	    } else if (out == NULL) {
		ip = in + (l - 1) * nc
		pp = pout + (l - 1) * nc
		do c = 1, nc {
		    data = Memr[ip]
		    avg = Memr[avgs+c]
		    bkg = Memr[bkgs+c]
		    sigma = Memr[sigs+c]
		    low = bkg - losig * sigma
		    high = bkg + hosig * sigma
		    if (avg < low || avg > high)
			Mems[pp] = objval
		    else {
			low = avg - lcrsig * sigma
			high = avg + hcrsig * sigma
			if (data < low || data > high)
			    Mems[pp] = crval
		    }
		    ip = ip + 1
		    pp = pp + 1
		}
	    } else {
		ip = in + (l - 1) * nc
		op = out + (l - 1) * nc
		pp = pout + (l - 1) * nc
		do c = 1, nc {
		    data = Memr[ip]
		    avg = Memr[avgs+c]
		    bkg = Memr[bkgs+c]
		    sigma = Memr[sigs+c]
		    low = bkg - losig * sigma
		    high = bkg + hosig * sigma
		    if (avg < low || avg > high) {
			Memr[op] = data
			Mems[pp] = objval
		    } else {
			low = avg - lcrsig * sigma
			high = avg + hcrsig * sigma
			if (data < low || data > high) {
			    Memr[op] = avg
			    Mems[pp] = crval
			} else
			    Memr[op] = data
		    }
		    ip = ip + 1
		    op = op + 1
		    pp = pp + 1
		}
	    }
	}

	call sfree (stack)
end


# CRAVERAGE1 -- Detect, replace, and flag cosmic rays checking input mask.
# A local background is computed using moving box averages to avoid
# contaminating bad pixels.  If variance model is given then that is
# used otherwise a local sigma is computed in blocks (it is not a moving box
# for efficiency) by using a percentile point of the sorted pixel values to
# estimate the width of the distribution uncontaminated by bad pixels).  Once
# the background and sigma are known deviant pixels are found by using sigma
# threshold factors.

procedure craverage1 (in, pin, out, pout, aout, sout, nc, nl, nl1, nl2,
	navg, nrej, nbkg, var0, var1, var2, nsig, lcrsig, hcrsig,
	lobjsig, hobjsig crval, objval)

pointer	in			#I Input data
pointer	pin			#I Pixel mask data
pointer	out			#O Output data
pointer	pout			#O Output mask (0=good, 1=bad)
pointer	aout			#O Output averages
pointer	sout			#O Output sigmas
int	nc, nl			#I Number of columns and lines
int	nl1, nl2		#I Lines to compute
int	navg			#I Averaging box half-size
int	nrej			#I Number of high pixels to reject from average
int	nbkg			#I Median background width
real	var0			#I Variance coefficient for DN^0 term
real	var1			#I Variance coefficient for DN^1 term
real	var2			#I Variance coefficient for DN^2 term
int	nsig			#I Sigma box size
real	lcrsig, hcrsig		#I Threshold sigmas outside of object
real	lobjsig, hobjsig	#I Object threshold sigmas
int	crval			#I CR mask value
int	objval			#I Object mask value

int	i, j, c, c1, c2, c3, c4, l, l1, l2, l3, l4, n1, n2
int	navg2, nbkg2, nsig2, plsig, phsig
real	data, avg, bkg, sigma, losig, hosig
real	low, high, cravg(), amedr()
pointer	stack, avgs, bkgs, sigs, work1, work2
pointer	ptr1, ptr2, ip, mp, op, pp, ap, sp

begin
	navg2 = (2 * navg + 1) ** 2
	nbkg2 = (2 * (navg + nbkg) + 1) ** 2 - navg2
	nsig2 = nsig * nsig

	call smark (stack)
	call salloc (avgs, nc, TY_REAL)
	call salloc (bkgs, nc, TY_REAL)
	call salloc (sigs, nc, TY_REAL)
	call salloc (work1, navg2, TY_REAL)
	call salloc (work2, max (nsig2, nbkg2), TY_REAL)

	if (var0 != 0. && var1 == 0. && var2 ==0.)
	    call amovkr (sqrt(var0), Memr[sigs], nc)

	avgs = avgs - 1
	sigs = sigs - 1
	bkgs = bkgs - 1

	losig = lobjsig / sqrt (real(navg2-1))
	hosig = hobjsig / sqrt (real(navg2-1))

	do l = nl1, nl2 {
	    # Compute statistics.
	    l1 = max (1, l-navg-nbkg)
	    l2 = max (1, l-navg)
	    l3 = min (nl, l+navg)
	    l4 = min (nl, l+navg+nbkg)
	    ap = aout + (l - 1) * nc
	    do c = 1, nc {
		c1 = max (1, c-navg-nbkg)
		c2 = max (1, c-navg)
		c3 = min (nc, c+navg)
		c4 = min (nc, c+navg+nbkg)
		ptr1 = work1
		ptr2 = work2
		n1 = 0
		n2 = 0
		do j = l1, l2-1 {
		    ip = in + (j - 1) * nc + c1 - 1
		    mp = pin + (j - 1) * nc + c1 - 1
		    do i = c1, c4 {
			if (Mems[mp] == 0) {
			    Memr[ptr2] = Memr[ip]
			    n2 = n2 + 1
			    ptr2 = ptr2 + 1
			}
			ip = ip + 1
			mp = mp + 1
		    }
		}
		do j = l2, l3 {
		    ip = in + (j - 1) * nc + c1 - 1
		    mp = pin + (j - 1) * nc + c1 - 1
		    do i = c1, c2-1 {
			if (Mems[mp] == 0) {
			    Memr[ptr2] = Memr[ip]
			    n2 = n2 + 1
			    ptr2 = ptr2 + 1
			}
			ip = ip + 1
			mp = mp + 1
		    }
		    do i = c2, c3 {
			if ((j != l || i != c) && Mems[mp] == 0) {
			    Memr[ptr1] = Memr[ip]
			    n1 = n1 + 1
			    ptr1 = ptr1 + 1
			}
			ip = ip + 1
			mp = mp + 1
		    }
		    do i = c3+1, c4 {
			if (Mems[mp] == 0) {
			    Memr[ptr2] = Memr[ip]
			    n2 = n2 + 1
			    ptr2 = ptr2 + 1
			}
			ip = ip + 1
			mp = mp + 1
		    }
		}
		do j = l3+1, l4 {
		    ip = in + (j - 1) * nc + c1 - 1
		    mp = pin + (j - 1) * nc + c1 - 1
		    do i = c1, c4 {
			if (Mems[mp] == 0) {
			    Memr[ptr2] = Memr[ip]
			    n2 = n2 + 1
			    ptr2 = ptr2 + 1
			}
			ip = ip + 1
		    }
		}
		if (n1 > 0)
		    avg = cravg (Memr[work1], n1, nrej)
		else
		    avg = INDEFR
		if (n2 > 0)
		    bkg = amedr (Memr[work2], n2)
		else
		    bkg = INDEFR
		Memr[bkgs+c] = bkg
		Memr[avgs+c] = avg
		if (aout != NULL) {
		    if (IS_INDEFR(avg) || IS_INDEFR(bkg))
			Memr[ap] = 0.
		    else
			Memr[ap] = avg - bkg
		    ap = ap + 1
		}
	    }

	    # Compute sigmas and output if desired.
	    if (var0 != 0. || var1 != 0. || var2 != 0.) {
		if (var1 != 0.) {
		    if (var2 != 0.) {
			do c = 1, nc {
			    data = max (0., Memr[avgs+c])
			    Memr[sigs+c] = sqrt (var0+var1*data+var2*data**2)
			}
		    } else {
			do c = 1, nc {
			    data = max (0., Memr[avgs+c])
			    Memr[sigs+c] = sqrt (var0 + var1 * data)
			}
		    }
		} else if (var2 != 0.) {
		    do c = 1, nc {
			data = max (0., Memr[avgs+c])
			Memr[sigs+c] = sqrt (var0 + var2 * data**2)
		    }
		}
	    } else {
		# Compute sigmas from percentiles.  This is done in blocks.
		if (mod (l-nl1, nsig) == 0 && l<nl-nsig+1) {
		    do c = 1, nc-nsig+1, nsig {
			ptr2 = work2
			n2 = 0
			do j = l, l+nsig-1 {
			    ip = in + (j - 1) * nc + c - 1
			    mp = pin + (j - 1) * nc + c - 1
			    do i = 1, nsig {
				if (Mems[mp] == 0) {
				    Memr[ptr2] = Memr[ip]
				    n2 = n2 + 1
				    ptr2 = ptr2 + 1
				}
				ip = ip + 1
				mp = mp + 1
			    }
			}
			if (n2 > 10) {
			    call asrtr (Memr[work2], Memr[work2], n2)
			    plsig = nint (PLSIG*n2/100.-1)
			    phsig = nint (PHSIG*n2/100.-1)
			    sigma = (Memr[work2+phsig]-Memr[work2+plsig])/2.
			} else
			    sigma = INDEFR
			call amovkr (sigma, Memr[sigs+c], nsig)
		    }
		    call amovkr (sigma, Memr[sigs+c], nc-c+1)
		}
	    }
	    if (sout != NULL) {
		sp = sout + (l - 1) * nc
		do c = 1, nc {
		    sigma = Memr[sigs+c]
		    if (IS_INDEFR(sigma))
			Memr[sp] = 0.
		    else
			Memr[sp] = sigma
		    sp = sp + 1
		}
	    }

	    # Detect, fix, and flag cosmic rays.
	    if (pout == NULL && out == NULL)
		;
	    if (pout == NULL) {
		ip = in + (l - 1) * nc
		mp = pin + (l - 1) * nc
		op = out + (l - 1) * nc
		do c = 1, nc {
		    data = Memr[ip]
		    avg = Memr[avgs+c]
		    bkg = Memr[bkgs+c]
		    sigma = Memr[sigs+c]
		    if (!(Mems[mp] != 0 || IS_INDEFR(avg) ||
			IS_INDEFR(bkg) || IS_INDEFR(sigma))) {
			low = bkg - losig * sigma
			high = bkg + hosig * sigma
			if (avg < low || avg > high) {
			    Memr[op] = data
			} else {
			    low = avg - lcrsig * sigma
			    high = avg + hcrsig * sigma
			    if (data < low || data > high)
				Memr[op] = avg
			    else
				Memr[op] = data
			}
		    } else
			Memr[op] = data
		    ip = ip + 1
		    mp = mp + 1
		    op = op + 1
		}
	    } else if (out == NULL) {
		ip = in + (l - 1) * nc
		mp = pin + (l - 1) * nc
		pp = pout + (l - 1) * nc
		do c = 1, nc {
		    data = Memr[ip]
		    avg = Memr[avgs+c]
		    bkg = Memr[bkgs+c]
		    sigma = Memr[sigs+c]
		    if (!(Mems[mp] != 0 || IS_INDEFR(avg) ||
			IS_INDEFR(bkg) || IS_INDEFR(sigma))) {
			low = bkg - losig * sigma
			high = bkg + hosig * sigma
			if (avg < low || avg > high)
			    Mems[pp] = objval
			else {
			    low = avg - lcrsig * sigma
			    high = avg + hcrsig * sigma
			    if (data < low || data > high)
				Mems[pp] = crval
			}
		    }
		    ip = ip + 1
		    mp = mp + 1
		    pp = pp + 1
		}
	    } else {
		ip = in + (l - 1) * nc
		mp = pin + (l - 1) * nc
		op = out + (l - 1) * nc
		pp = pout + (l - 1) * nc
		do c = 1, nc {
		    data = Memr[ip]
		    avg = Memr[avgs+c]
		    bkg = Memr[bkgs+c]
		    sigma = Memr[sigs+c]
		    if (!(Mems[mp] != 0 || IS_INDEFR(avg) ||
			IS_INDEFR(bkg) || IS_INDEFR(sigma))) {
			low = bkg - losig * sigma
			high = bkg + hosig * sigma
			if (avg < low || avg > high) {
			    Memr[op] = data
			    Mems[pp] = objval
			} else {
			    low = avg - lcrsig * sigma
			    high = avg + hcrsig * sigma
			    if (data < low || data > high) {
				Memr[op] = avg
				Mems[pp] = crval
			    } else
				Memr[op] = data
			}
		    } else
			Memr[op] = data
		    ip = ip + 1
		    mp = mp + 1
		    op = op + 1
		    pp = pp + 1
		}
	    }
	}

	call sfree (stack)
end


# CRAVG -- Compute average with the highest nrej points excluded.
# When nrej is greater than 2 the data array will be returned sorted.

real procedure cravg (data, npts, nrej)

real	data[npts]		#I Input data (will be sorted if nrej>2)
int	npts			#I Number of data points
int	nrej			#I Number of data points to reject

int	i
real	sum, max1, max2, val

begin
	if (npts <= nrej)
	    return (INDEFR)

	switch (nrej) {
	case 0:
	    sum = 0.
	    do i = 1, npts
		sum = sum + data[i]
	case 1:
	    sum = 0.
	    max1 = data[1]
	    do i = 2, npts {
		val = data[i]
		if (val > max1) {
		    sum = sum + max1
		    max1 = val
		} else
		    sum = sum + val
	    }
	case 2:
	    sum = 0.
	    max1 = min (data[1], data[2])
	    max2 = max (data[1], data[2])
	    do i = 3, npts {
		val = data[i]
		if (val > max1) {
		    sum = sum + max1
		    if (val > max2) {
			max1 = max2
			max2 = val
		    } else
			max1 = val
		} else
		    sum = sum + val
	    }
	default:
	    call asrtr (data, data, npts)
	    sum = 0.
	    do i = 1, npts-nrej
		sum = sum + data[i]
	}

	return (sum / (npts - nrej))
end
