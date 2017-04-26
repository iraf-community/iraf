# AP_GMEASURE -- Procedure to measure the fluxes and effective areas of a set of
# apertures assuming a Gaussian weighting function.

procedure ap_gmeasure (im, wx, wy, c1, c2, l1, l2, aperts, sums, areas,
        naperts, sigsq, gain, varsky)

pointer	im			# pointer to image
real	wx, wy			# center of subraster
int	c1, c2			# column limits
int	l1, l2			# line limits
real	aperts[ARB]		# array of apertures
double	sums[ARB]		# array of sums
double	areas[ARB]		# aperture areas
int	naperts			# number of apertures
real	sigsq			# the profile widht squared
real	gain			# the sky value
real	varsky			# the sky variance

int	i, j, k, nx, yindex
double	fctn, weight, norm
pointer	sp, buf, sump, sumpw
real	xc, yc, apmaxsq, dy2, r2, r, prof, var
pointer	imgs2r()

begin
	# Initialize.
	call smark (sp)
	call salloc (sump, naperts, TY_DOUBLE)
	call salloc (sumpw, naperts, TY_DOUBLE)
	call aclrd (Memd[sump], naperts)
	call aclrd (Memd[sumpw], naperts)

	# Get array boundary parameters.
	nx = c2 - c1 + 1
	xc = wx - c1 + 1
	yc = wy - l1 + 1
	apmaxsq = (aperts[naperts] + 0.5) ** 2

	# Clear out the accumulaters
	call aclrd (sums, naperts)
	call aclrd (areas, naperts)

	# Loop over the pixels.
	do j = l1, l2 {
	    buf = imgs2r (im, c1, c2, j, j)
	    if (buf == EOF) {
		call sfree (sp)
		return
	    }
	    yindex = j - l1 + 1
	    dy2 = (yindex - yc) ** 2
	    do i = 1, nx {
		r2 = (i - xc) ** 2 + dy2
		if (r2 > apmaxsq)
		    next
		prof = max (exp (-r2 / sigsq), 0.0)
		if (prof <= 0.0)
		    next
		var = max (0.0, Memr[buf+i-1])
		var = var / gain + varsky
		if (var <= 0.0)
		    next
		weight = prof / var
		r = sqrt (r2) - 0.5
		do k = 1, naperts {
		    if (r > aperts[k])
			next
		    fctn = max (0.0, min (1.0, aperts[k] - r))
		    sums[k] = sums[k] + weight * fctn * Memr[buf+i-1]
		    areas[k] = areas[k] + weight * fctn
		    Memd[sump+k-1] = Memd[sump+k-1] + prof
		    Memd[sumpw+k-1] = Memd[sumpw+k-1] + weight * prof
		}
	    }
	}

	# Normalize.
	do k = 1, naperts {
	    if (Memd[sumpw+k-1] <= 0.0d0)
	        norm = 0.0d0
	    else
	        norm = Memd[sump+k-1] / Memd[sumpw+k-1]
	    sums[k] = sums[k] * norm
	    areas[k] = areas[k] * norm
	}

	call sfree (sp)
end


# AP_BGMEASURE -- Procedure to measure the fluxes and effective areas of a set
# of apertures assuming a Gaussian weighting function.

procedure ap_bgmeasure (im, wx, wy, c1, c2, l1, l2, datamin, datamax,
	aperts, sums, areas, naperts, minapert, sigsq, gain, varsky)

pointer	im			# pointer to image
real	wx, wy			# center of subraster
int	c1, c2			# column limits
int	l1, l2			# line limits
real	datamin			# minimum good data
real	datamax			# maximum good data
real	aperts[ARB]		# array of apertures
double	sums[ARB]		# array of sums
double	areas[ARB]		# aperture areas
int	naperts			# number of apertures
int	minapert		# minimum aperture fo bad pixels
real	sigsq			# the profile widht squared
real	gain			# the image gain value
real	varsky			# the sky variance

int	i, j, k, nx, yindex, kindex
double	fctn, weight, norm
pointer	sp, buf, sump, sumpw
real	xc, yc, apmaxsq, dy2, r2, r
real	pixval, prof, var
pointer	imgs2r()

begin
	# Initialize.
	call smark (sp)
	call salloc (sump, naperts, TY_DOUBLE)
	call salloc (sumpw, naperts, TY_DOUBLE)
	call aclrd (Memd[sump], naperts)
	call aclrd (Memd[sumpw], naperts)
	minapert = naperts + 1

	# Get array boundary parameters.
	nx = c2 - c1 + 1
	xc = wx - c1 + 1
	yc = wy - l1 + 1
	apmaxsq = (aperts[naperts] + 0.5) ** 2

	# Clear out the accumulaters
	call aclrd (sums, naperts)
	call aclrd (areas, naperts)

	# Loop over the pixels.
	do j = l1, l2 {
	    buf = imgs2r (im, c1, c2, j, j)
	    if (buf == EOF) {
		call sfree (sp)
		return
	    }
	    yindex = j - l1 + 1
	    dy2 = (yindex - yc) ** 2
	    do i = 1, nx {
		r2 = (i - xc) ** 2 + dy2
		if (r2 > apmaxsq)
		    next
		prof = max (exp (-r2 / sigsq), 0.0)
		if (prof <= 0.0)
		    next
		pixval = Memr[buf+i-1]
		var = max (0.0, pixval)
		var = pixval / gain + varsky
		if (var <= 0.0)
		    next
		weight = prof / var
		r = sqrt (r2) - 0.5
		kindex = naperts + 1
		do k = 1, naperts {
		    if (r > aperts[k])
			next
		    kindex = min (k, kindex)
		    fctn = max (0.0, min (1.0, aperts[k] - r))
		    sums[k] = sums[k] + weight * fctn * pixval
		    areas[k] = areas[k] + weight * fctn
		    Memd[sump+k-1] = Memd[sump+k-1] + prof
		    Memd[sumpw+k-1] = Memd[sumpw+k-1] + weight * prof
		}
		if (kindex < minapert) {
		    if (pixval < datamin || pixval > datamax)
			minapert = kindex
		}
	    }
	}

	# Normalize.
	do k = 1, naperts {
	    if (Memd[sumpw+k-1] <= 0.0d0)
	        norm = 0.0d0
	    else
	        norm = Memd[sump+k-1] / Memd[sumpw+k-1]
	    sums[k] = sums[k] * norm
	    areas[k] = areas[k] * norm
	}

	call sfree (sp)
end
