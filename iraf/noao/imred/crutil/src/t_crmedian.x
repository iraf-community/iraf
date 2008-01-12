include	<imhdr.h>
include	<mach.h>

define	MAXBUF		500000	# Maximum pixel buffer

define	PLSIG		15.87	# Low percentile
define	PHSIG		84.13	# High percentile


# T_CRMEDIAN -- Detect, fix, and flag cosmic rays.
# Deviant pixels relative to a local median and sigma are detected and
# replaced by the median value and/or written to a cosmic ray mask.

procedure t_crmedian ()

pointer	input			# Input image
pointer	output			# Output image
pointer	crmask			# Output mask
pointer	median			# Output median
pointer	sigma			# Output sigma
pointer	residual		# Output residual
real	var0			# Variance coefficient for DN^0 term
real	var1			# Variance coefficient for DN^1 term
real	var2			# Variance coefficient for DN^2 term
real	lsig, hsig		# Threshold sigmas
int	ncmed, nlmed		# Median box size
int	ncsig, nlsig		# Sigma box size

int	i, nc, nl, nlstep, l1, l2, l3, l4, nl1
pointer	sp, extname, in, out, pm, mim, sim, rim
pointer	inbuf, outbuf, pmbuf, mbuf, sbuf, rbuf
real	clgetr()
int	clgeti(), nowhite()
pointer	immap(), imgs2r(), imps2r(), imps2s()
errchk	immap, imgs2r, imps2r, imps2s, crmedian, imgstr

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (crmask, SZ_FNAME, TY_CHAR)
	call salloc (residual, SZ_FNAME, TY_CHAR)
	call salloc (median, SZ_FNAME, TY_CHAR)
	call salloc (sigma, SZ_FNAME, TY_CHAR)
	call salloc (extname, SZ_FNAME, TY_CHAR)

	# Get parameters.
	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("crmask", Memc[crmask], SZ_FNAME)
	call clgstr ("median", Memc[median], SZ_FNAME)
	call clgstr ("sigma", Memc[sigma], SZ_FNAME)
	call clgstr ("residual", Memc[residual], SZ_FNAME)
	var0 = clgetr ("var0")
	var1 = clgetr ("var1")
	var2 = clgetr ("var2")
	lsig = clgetr ("lsigma")
	hsig = clgetr ("hsigma")
	ncmed = clgeti ("ncmed")
	nlmed = clgeti ("nlmed")
	ncsig = clgeti ("ncsig")
	nlsig = clgeti ("nlsig")

	# Map the input and output images.
	in = NULL; out = NULL; pm = NULL; mim = NULL; sim = NULL; rim = NULL
	inbuf = NULL; outbuf = NULL; pmbuf = NULL
	mbuf = NULL; sbuf=NULL; rbuf = NULL
	in = immap (Memc[input], READ_ONLY, 0)
	if (nowhite (Memc[output], Memc[output], SZ_FNAME) > 0)
	    out = immap (Memc[output], NEW_COPY, in)
	if (nowhite (Memc[crmask], Memc[crmask], SZ_FNAME) > 0) {
	    if (Memc[crmask] == '!')
		call imgstr (in, Memc[crmask+1], Memc[crmask], SZ_FNAME)
	    iferr (call imgstr (in, "extname", Memc[extname], SZ_FNAME))
		call strcpy ("pl", Memc[extname], SZ_FNAME)
	    call xt_maskname (Memc[crmask], Memc[extname], 0, Memc[crmask],
		SZ_FNAME)
	    pm = immap (Memc[crmask], NEW_COPY, in)
	}
	if (nowhite (Memc[median], Memc[median], SZ_FNAME) > 0)
	    mim = immap (Memc[median], NEW_COPY, in)
	if (nowhite (Memc[sigma], Memc[sigma], SZ_FNAME) > 0)
	    sim = immap (Memc[sigma], NEW_COPY, in)
	if (nowhite (Memc[residual], Memc[residual], SZ_FNAME) > 0)
	    rim = immap (Memc[residual], NEW_COPY, in)

	# Go through the input in large blocks of lines.  If the
	# block is smaller than the whole image overlap the blocks
	# so the median only has boundaries at the ends of the image.
	# However, the output is done in non-overlapping blocks with
	# the pointers are adjusted so that addresses can be in the space
	# of the input block.  CRMEDIAN does not address outside of
	# the output data block.  Set the mask values based on the
	# distances to the nearest good pixels.

	nc = IM_LEN(in,1)
	nl = IM_LEN(in,2)
	nlstep = max (1, MAXBUF / nc - nlmed)

	do i = 1, nl, nlstep {
	    l1 = i
	    l2 = min (nl, i + nlstep - 1)
	    l3 = max (1, l1 - nlmed / 2)
	    l4 = min (nl, l2 + nlmed / 2)
	    nl1 = l4 - l3 + 1
	    inbuf = imgs2r (in, 1, nc, l3, l4)
	    if (out != NULL)
		outbuf = imps2r (out, 1, nc, l1, l2) - (l1 - l3) * nc
	    if (pm != NULL)
		pmbuf = imps2s (pm, 1, nc, l1, l2) - (l1 - l3) * nc
	    if (mim != NULL)
		mbuf = imps2r (mim, 1, nc, l1, l2) - (l1 - l3) * nc
	    if (sim != NULL)
		sbuf = imps2r (sim, 1, nc, l1, l2) - (l1 - l3) * nc
	    if (rim != NULL)
		rbuf = imps2r (rim, 1, nc, l1, l2) - (l1 - l3) * nc
	    call crmedian (inbuf, outbuf, pmbuf, mbuf, sbuf, rbuf,
		nc, nl1, l1-l3+1, l2-l3+1, ncmed, nlmed, var0, var1, var2,
		ncsig, nlsig, lsig, hsig)
	}

	if (rim != NULL)
	    call imunmap (rim)
	if (sim != NULL)
	    call imunmap (sim)
	if (mim != NULL)
	    call imunmap (mim)
	if (pm != NULL)
	    call imunmap (pm)
	if (out != NULL)
	    call imunmap (out)
	call imunmap (in)
	call sfree (sp)
end


# CRMEDIAN -- Detect, replace, and flag cosmic rays.
# A local background is computed using moving box medians to avoid
# contaminating bad pixels.  If variance model is given then that is
# used otherwise a local sigma is computed in blocks (it is not a moving box
# for efficiency) by using a percentile point of the sorted pixel values to
# estimate the width of the distribution uncontaminated by bad pixels).  Once
# the background and sigma are known deviant pixels are found by using sigma
# threshold factors.

procedure crmedian (in, out, pout, mout, sout, rout, nc, nl, nl1, nl2,
	ncmed, nlmed, var0, var1, var2, ncsig, nlsig, lsig, hsig)

pointer	in			#I Input data
pointer	out			#O Output data
pointer	pout			#O Output mask (0=good, 1=bad)
pointer	mout			#O Output medians
pointer	sout			#O Output sigmas
pointer	rout			#O Output residuals
int	nc, nl			#I Number of columns and lines
int	nl1, nl2		#I Lines to compute
int	ncmed, nlmed		#I Median box size
real	var0			# Variance coefficient for DN^0 term
real	var1			# Variance coefficient for DN^1 term
real	var2			# Variance coefficient for DN^2 term
int	ncsig, nlsig		#I Sigma box size
real	lsig, hsig		#I Threshold sigmas

int	i, j, k, l, m, plsig, phsig
real	data, med, sigma, low, high, amedr()
pointer	stack, meds, sigs, work, ptr, ip, op, pp, mp, sp, rp

begin
	call smark (stack)
	call salloc (meds, nc, TY_REAL)
	call salloc (sigs, nc, TY_REAL)
	call salloc (work, max (ncsig*nlsig, ncmed*nlmed), TY_REAL)

	if (var0 != 0. && var1 == 0. && var2 ==0.)
	    call amovkr (sqrt(var0), Memr[sigs], nc)

	meds = meds - 1
	sigs = sigs - 1

	i = ncsig * nlsig
	plsig = nint (PLSIG*i/100.-1)
	phsig = nint (PHSIG*i/100.-1)

	do i = nl1, nl2 {

	    # Compute median and output if desired.  This is a moving median.
	    l = min (nl, i+nlmed/2)
	    l = max (1, l-nlmed+1)
	    mp = mout + (i - 1) * nc 
	    do j = 1, nc {
		k = min (nc, j+ncmed/2)
		k = max (1, k-ncmed+1)
		ptr = work
		ip = in + (l - 1) * nc + k - 1
		do m = l, l+nlmed-1 {
		    call amovr (Memr[ip], Memr[ptr], ncmed)
		    ip = ip + nc
		    ptr = ptr + ncmed
		}
		med = amedr (Memr[work], ncmed * nlmed)
		Memr[meds+j] = med
		if (mout != NULL) {
		    Memr[mp] = med
		    mp = mp + 1
		}
	    }

	    # Compute sigmas and output if desired.
	    if (var0 != 0. || var1 != 0. || var2 != 0.) {
		if (var1 != 0.) {
		    if (var2 != 0.) {
			do j = 1, nc {
			    data = max (0., Memr[meds+j])
			    Memr[sigs+j] = sqrt (var0 + var1*data + var2*data**2)
			}
		    } else {
			do j = 1, nc {
			    data = max (0., Memr[meds+j])
			    Memr[sigs+j] = sqrt (var0 + var1 * data)
			}
		    }
		} else if (var2 != 0.) {
		    do j = 1, nc {
			data = max (0., Memr[meds+j])
			Memr[sigs+j] = sqrt (var0 + var2 * data**2)
		    }
		}
	    } else {
		# Compute sigmas from percentiles.  This is done in blocks.
		if (mod (i-nl1, nlsig) == 0 && i<nl-nlsig+1) {
		    do j = 1, nc-ncsig+1, ncsig {
			ptr = work
			ip = in + (i - 1) * nc + j - 1
			do k = i, i+nlsig-1 {
			    call amovr (Memr[ip], Memr[ptr], ncsig)
			    ip = ip + nc
			    ptr = ptr + ncsig
			}
			call asrtr (Memr[work], Memr[work], ncsig*nlsig)
			sigma = (Memr[work+phsig] - Memr[work+plsig]) / 2.
			call amovkr (sigma, Memr[sigs+j], ncsig)
		    }
		    call amovkr (sigma, Memr[sigs+j], nc-j+1)
		}
	    }
	    if (sout != NULL) {
		sp = sout + (i - 1) * nc
		do j = 1, nc {
		    Memr[sp] = Memr[sigs+j]
		    sp = sp + 1
		}
	    }

	    # Detect, fix, and flag cosmic rays.
	    if (rout == NULL) {
		if (pout == NULL) {
		    ip = in + (i - 1) * nc
		    op = out + (i - 1) * nc
		    do j = 1, nc {
			data = Memr[ip]
			med = Memr[meds+j]
			sigma = Memr[sigs+j]
			low = med - lsig * sigma
			high = med + hsig * sigma
			if (data < low || data > high)
			    Memr[op] = med
			else
			    Memr[op] = data
			ip = ip + 1
			op = op + 1
		    }
		} else if (out == NULL) {
		    ip = in + (i - 1) * nc
		    pp = pout + (i - 1) * nc
		    do j = 1, nc {
			data = Memr[ip]
			med = Memr[meds+j]
			sigma = Memr[sigs+j]
			low = med - lsig * sigma
			high = med + hsig * sigma
			if (data < low || data > high)
			    Mems[pp] = 1
			else
			    Mems[pp] = 0
			ip = ip + 1
			pp = pp + 1
		    }
		} else {
		    ip = in + (i - 1) * nc
		    op = out + (i - 1) * nc
		    pp = pout + (i - 1) * nc
		    do j = 1, nc {
			data = Memr[ip]
			med = Memr[meds+j]
			sigma = Memr[sigs+j]
			low = med - lsig * sigma
			high = med + hsig * sigma
			if (data < low || data > high) {
			    Memr[op] = med
			    Mems[pp] = 1
			} else {
			    Memr[op] = data
			    Mems[pp] = 0
			}
			ip = ip + 1
			op = op + 1
			pp = pp + 1
		    }
		}
	    } else {
		if (pout == NULL && out == NULL) {
		    ip = in + (i - 1) * nc
		    rp = rout + (i - 1) * nc
		    do j = 1, nc {
			data = Memr[ip]
			med = Memr[meds+j]
			sigma = Memr[sigs+j]
			if (sigma > 0.)
			    Memr[rp] = (data - med) / sigma
			else {
			    if ((data - med) < 0.)
				Memr[rp] = -MAX_REAL
			    else
				Memr[rp] = MAX_REAL
			}
			ip = ip + 1
			rp = rp + 1
		    }
		} else if (pout == NULL) {
		    ip = in + (i - 1) * nc
		    op = out + (i - 1) * nc
		    rp = rout + (i - 1) * nc
		    do j = 1, nc {
			data = Memr[ip]
			med = Memr[meds+j]
			sigma = Memr[sigs+j]
			low = med - lsig * sigma
			high = med + hsig * sigma
			if (data < low || data > high)
			    Memr[op] = med
			else
			    Memr[op] = data
			if (sigma > 0.)
			    Memr[rp] = (data - med) / sigma
			else {
			    if ((data - med) < 0.)
				Memr[rp] = -MAX_REAL
			    else
				Memr[rp] = MAX_REAL
			}
			ip = ip + 1
			op = op + 1
			rp = rp + 1
		    }
		} else if (out == NULL) {
		    ip = in + (i - 1) * nc
		    pp = pout + (i - 1) * nc
		    rp = rout + (i - 1) * nc
		    do j = 1, nc {
			data = Memr[ip]
			med = Memr[meds+j]
			sigma = Memr[sigs+j]
			low = med - lsig * sigma
			high = med + hsig * sigma
			if (data < low || data > high)
			    Mems[pp] = 1
			else
			    Mems[pp] = 0
			if (sigma > 0.)
			    Memr[rp] = (data - med) / sigma
			else {
			    if ((data - med) < 0.)
				Memr[rp] = -MAX_REAL
			    else
				Memr[rp] = MAX_REAL
			}
			ip = ip + 1
			pp = pp + 1
			rp = rp + 1
		    }
		} else {
		    ip = in + (i - 1) * nc
		    op = out + (i - 1) * nc
		    pp = pout + (i - 1) * nc
		    rp = rout + (i - 1) * nc
		    do j = 1, nc {
			data = Memr[ip]
			med = Memr[meds+j]
			sigma = Memr[sigs+j]
			low = med - lsig * sigma
			high = med + hsig * sigma
			if (data < low || data > high) {
			    Memr[op] = med
			    Mems[pp] = 1
			} else {
			    Memr[op] = data
			    Mems[pp] = 0
			}
			if (sigma > 0.)
			    Memr[rp] = (data - med) / sigma
			else {
			    if ((data - med) < 0.)
				Memr[rp] = -MAX_REAL
			    else
				Memr[rp] = MAX_REAL
			}
			ip = ip + 1
			op = op + 1
			pp = pp + 1
			rp = rp + 1
		    }
		}
	    }
	}

	call sfree (stack)
end
