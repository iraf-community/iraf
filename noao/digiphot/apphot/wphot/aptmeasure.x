# AP_TMEASURE -- Procedure to measure the fluxes and effective areas of a set of
# apertures assuming a conical weighting function.

procedure ap_tmeasure (im, wx, wy, c1, c2, l1, l2, aperts, sums, areas,
        naperts, fwhmpsf, gain, varsky)

pointer	im			# pointer to image
real	wx, wy			# center of subraster
int	c1, c2			# column limits
int	l1, l2			# line limits
real	aperts[ARB]		# array of apertures
double	sums[ARB]		# array of sums
double	areas[ARB]		# aperture areas
int	naperts			# number of apertures
real	fwhmpsf			# width of the profile
real	gain			# the image gain
real	varsky			# the sky variance

int	i, j, k, yindex, nx
double	fctn, weight, norm
pointer	buf, sp, sump, sumpw
real	xc, yc, apmaxsq, dy2, r2, r, pixval, prof, var
pointer	imgs2r()

begin
	# Initialize.
	call smark (sp)
	call salloc (sump, naperts, TY_DOUBLE)
	call salloc (sumpw, naperts, TY_DOUBLE)
	call aclrd (Memd[sump], naperts)
	call aclrd (Memd[sumpw], naperts)

	# Set up some array boundary parameters.
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
	    if (buf == NULL) {
		call sfree (sp)
		return
	    }
	    yindex = j - l1 + 1
	    dy2 = (yindex - yc) ** 2
	    do i = 1, nx {
		r2 = (i - xc) ** 2 + dy2
		if (r2 > apmaxsq)
		    next
		r = sqrt (r2)
		prof = max (1.0 - r / fwhmpsf, 0.0)
		if (prof <= 0.0)
		    next
		pixval = Memr[buf+i-1]
		var = max (0.0, pixval)
		var = var / gain + varsky
		if (var <= 0.0)
		    next
		weight = prof / var
		r = r - 0.5
		do k = 1, naperts {
		    if (r > aperts[k])
			next
		    fctn = max (0.0, min (1.0, aperts[k] - r))
		    Memd[sump+k-1] = Memd[sump+k-1] + prof
		    Memd[sumpw+k-1] = Memd[sumpw+k-1] + prof * weight
		    sums[k] = sums[k] + weight * fctn * pixval
		    areas[k] = areas[k] + weight * fctn
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


# AP_BTMEASURE -- Procedure to measure the fluxes and effective areas of a
# set of apertures assuming a conical weighting function.

procedure ap_btmeasure (im, wx, wy, c1, c2, l1, l2, datamin, datamax,
	aperts, sums, areas, naperts, minapert, fwhmpsf, gain, varsky)

pointer	im			# pointer to image
real	wx, wy			# center of subraster
int	c1, c2			# column limits
int	l1, l2			# line limits
real	datamin			# minimum good data value
real	datamax			# maximum good data value
real	aperts[ARB]		# array of apertures
double	sums[ARB]		# array of sums
double	areas[ARB]		# aperture areas
int	naperts			# number of apertures
int	minapert		# minimum aperture
real	fwhmpsf			# width of the profile
real	gain			# the image gain
real	varsky			# the sky variance

int	i, j, k, yindex, kindex, nx
double	fctn, weight, norm
pointer	buf, sp, sump, sumpw
real	xc, yc, apmaxsq, dy2, r2, r, pixval, prof, var
pointer	imgs2r()

begin
	# Initialize.
	call smark (sp)
	call salloc (sump, naperts, TY_DOUBLE)
	call salloc (sumpw, naperts, TY_DOUBLE)
	call aclrd (Memd[sump], naperts)
	call aclrd (Memd[sumpw], naperts)
	minapert = naperts + 1

	# Set up some array boundary parameters.
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
	    if (buf == NULL) {
		call sfree (sp)
		return
	    }
	    yindex = j - l1 + 1
	    dy2 = (yindex - yc) ** 2
	    do i = 1, nx {
		r2 = (i - xc) ** 2 + dy2
		if (r2 > apmaxsq)
		    next
		r = sqrt (r2)
		prof = max (1.0 - r / fwhmpsf, 0.0)
		if (prof <= 0.0)
		    next
		pixval = Memr[buf+i-1]
		var = max (0.0, pixval)
		var = var / gain + varsky
		if (var <= 0.0)
		    next
		weight = prof / var
		r = r - 0.5
		kindex = naperts + 1
		do k = 1, naperts {
		    if (r > aperts[k])
			next
		    kindex = min (k, kindex)
		    fctn = max (0.0, min (1.0, aperts[k] - r))
		    Memd[sump+k-1] = Memd[sump+k-1] + prof
		    Memd[sumpw+k-1] = Memd[sumpw+k-1] + prof * weight
		    sums[k] = sums[k] + weight * fctn * pixval
		    areas[k] = areas[k] + weight * fctn
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
