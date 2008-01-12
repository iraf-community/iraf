define	NF		25		# Maximum number of filters
define	SZ_FILTER	7		# Maximum length of filter name
define	TOL		0.0001		# Convergence tolerance


# T_CCDTIME -- Compute the time, magnitude, and signal-to-noise for CCD
# exposures for a given telescope, detector, and filters.  The telescope
# detector, and filter data come from a text database.  The computed
# quantities fix two parameters and compute the third.

procedure t_ccdtime()

pointer	database		# Telescope, detector, filter database
pointer	telescope		# Telescope
pointer	detector		# Detector
pointer	fltr[5]			# Filters
real	time			# Target time (sec)
real	mag			# Target magnitude
real	snr			# Target SNR
int	sum			# CCD summing
real	seeing			# Seeing (arcsec)
real	phase			# Moon phase (0-28)
real	airmass			# Airmass

int	i, j, k, nf, n
real	a, b, c
real	aper, scale, trans, nread, dark, pixsize, lum, p
real	star[NF], sky[NF], t, m, s, nstar, nsky, ndark, noise, npix
real	dqe, ext, starmag, counts, sky0, sky1, sky2
pointer	sp, tdb, ddb, filter[NF], fdb[NF]

int	clgeti()
real	clgetr()
double	dbgetd()
pointer	dbopen()
errchk	dbopen

define	done_	10

begin
	call smark (sp)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (telescope, SZ_FNAME, TY_CHAR)
	call salloc (detector, SZ_FNAME, TY_CHAR)
	do i = 1, 5
	    call salloc (fltr[i], SZ_LINE, TY_CHAR)
	do i = 1, NF
	    call salloc (filter[i], SZ_FILTER, TY_CHAR)

	# Get task parameters.
	call clgstr ("database", Memc[database], SZ_FNAME)
	call clgstr ("telescope", Memc[telescope], SZ_FNAME)
	call clgstr ("detector", Memc[detector], SZ_FNAME)
	call clgstr ("f1", Memc[fltr[1]], SZ_LINE)
	call clgstr ("f2", Memc[fltr[2]], SZ_LINE)
	call clgstr ("f3", Memc[fltr[3]], SZ_LINE)
	call clgstr ("f4", Memc[fltr[4]], SZ_LINE)
	call clgstr ("f5", Memc[fltr[5]], SZ_LINE)

	time = clgetr ("time")
	mag = clgetr ("magnitude")
	snr = clgetr ("snr")
	sum = clgeti ("sum")
	seeing = clgetr ("seeing")
	phase = clgetr ("phase")
	airmass = clgetr ("airmass")

	# The input filter strings may be lists of filters and here
	# we expand them into all filters.

	nf = 1; k = 0
	do i = 1, 5 {
	    for (j = fltr[i]; Memc[j] != EOS; j = j + 1) {
	        Memc[filter[nf]+k] = Memc[j]
	        if (Memc[j] == ',') {
		    Memc[filter[nf]+k] = EOS
		    if (k > 0) {
			nf = nf + 1; k = 0
		    }
		} else
		    k = k + 1
	    }
	    Memc[filter[nf]+k] = EOS
	    if (k > 0) {
		nf = nf + 1; k = 0
	    }
	}
	nf = nf - 1

	i = 0
	if (IS_INDEFR(time))
	    i = i + 1
	 else if (time <= 0.)
	    call error (1, "Requested time must be greater than zero")
	 else if (time > 100000.)
	    call error (1, "Requested time must be less than 100,000")

	if (IS_INDEFR(mag))
	    i = i + 1
	else if (mag > 40.)
	    call error (1, "Requested magnitude must be less than 40")
	else if (mag < -40.)
	    call error (1, "Requested magnitude must be greater than -40")

	if (IS_INDEFR(snr))
	    i = i + 1
	 else if (snr <= 0.)
	    call error (1, "Requested SNR must be greater than zero")
	 else if (snr > 100000.)
	    call error (1, "Requested SNR must be less than 100,000")

	if (i > 1) {
	    call sfree (sp)
	    call error (1,
		"At least two of time, magnitude, and snr must be specified")
	}

	if (phase < 14)
	    p = phase
	else
	    p = 28 - phase

	# Open database entries.
	# If '?' this will print list and then the task will exit.
	# If an error occurs in the telescope or detector abort.
	# If an error occurs in the filter ignore the filter.

	tdb = dbopen ("", Memc[database], "telescope", Memc[telescope])
	ddb = dbopen ("", Memc[database], "detector", Memc[detector])

	n = 0
	do i = 1, nf {
	    iferr (fdb[n+1]=dbopen("",Memc[database],"filter",Memc[filter[i]]))
		next
	    if (fdb[n+1] == NULL)
		goto done_
	    n = n + 1
	    filter[n] = filter[i]
	}
	if (tdb == NULL || ddb == NULL || n == 0)
	    goto done_

	# Get star and sky rates for telescope/detector/filter combination.
	# Convert to a standard rate at 20th magnitude at the given airmass.

	do i = 1, n {
	    # Get telescope parameters.
	    aper = dbgetd (tdb, "aperture", Memc[filter[i]], Memc[detector])
	    scale = dbgetd (tdb, "scale", Memc[filter[i]], Memc[detector])
	    trans = dbgetd (tdb, "transmission", Memc[filter[i]],
		Memc[detector])

	    # Get detector parameters.
	    dqe = dbgetd (ddb, Memc[filter[i]], Memc[filter[i]],
		Memc[telescope])
	    nread = dbgetd (ddb, "rdnoise", Memc[filter[i]], Memc[telescope])
	    dark = dbgetd (ddb, "dark", Memc[filter[i]], Memc[telescope])
	    pixsize = dbgetd (ddb,"pixsize",Memc[filter[i]],Memc[telescope]) *
		(scale / 1000) * sum
	    npix  =  max (9, nint (1.4 * (seeing / pixsize)**2+0.5))

	    # Get filter parameters.
	    iferr (ext = dbgetd (fdb[i], "extinction", Memc[telescope],
		Memc[detector]))
		ext = 1
	    starmag = dbgetd (fdb[i], "mag", Memc[telescope], Memc[detector])
	    counts = dbgetd (fdb[i], "star", Memc[telescope], Memc[detector])
	    sky0 = dbgetd (fdb[i], "sky0", Memc[telescope], Memc[detector])
	    sky1 = dbgetd (fdb[i], "sky1", Memc[telescope], Memc[detector])
	    sky2 = dbgetd (fdb[i], "sky2", Memc[telescope], Memc[detector])

	    lum = 10. ** (0.4 * (starmag - 20.))
	    star[i] = counts * lum * aper ** 2 * trans *
		10 ** (0.4 * (1 - airmass) * ext)
	    star[i] = star[i] * dqe
	    sky[i] = sky0 + sky1 * p + sky2 * p * p
	    sky[i] = star[i] * pixsize ** 2 * 10. ** ((20. - sky[i]) / 2.5)
	}
	if (!IS_INDEFR(mag))
	    lum = 10. ** (0.4 * (20. - mag))

	# Print header and column labels.
	call printf ("Database: %-20s  Telescope: %-10s  Detector: %-10s\n")
	    call pargstr (Memc[database])
	    call pargstr (Memc[telescope])
	    call pargstr (Memc[detector])
	call printf ("  Sum: %-2d  Arcsec/pixel: %-4.2f  Pixels/star: %-4.1f\n")
	    call pargi (sum)
	    call pargr (pixsize)
	    call pargr (npix)
	call printf ("  Seeing: %-.3g  Airmass: %-4.2f  Phase: %-4.1f\n")
	    call pargr (seeing)
	    call pargr (airmass)
	    call pargr (phase)
	call printf ("\n%7s %7s %7s %7s %7s %7s %s\n")
	    call pargstr ("Filter")
	    call pargstr ("Time")
	    call pargstr ("Mag")
	    call pargstr ("SNR")
	    call pargstr ("Star")
	    call pargstr ("Sky/pix")
	    call pargstr ("   Noise contributions")
	call printf ("%47w %7s %7s %7s\n")
	    call pargstr ("Star")
	    call pargstr ("Sky")
	    call pargstr ("CCD")

	# Compute exposure time through each filter.

	if (!IS_INDEFR(mag) && !IS_INDEFR(snr)) {
	    call printf ("\n")
	    do i = 1, n {
		a = ((star[i] * lum) / snr) ** 2
		b = -(star[i] * lum + npix * (sky[i] + dark))
		c = -npix * nread ** 2
		t = (-b + sqrt (b**2 - 4 * a * c)) / (2 * a)

		nstar = star[i] * lum * t
		nsky  = sky[i] * t
		ndark = dark * t
		noise = sqrt(nstar + npix * (nsky + ndark + nread**2))
		s = nstar / noise
		m = mag

		call printf (
		    "%7s %7.2f %7.1f %7.1f %7.1f %7.1f %7.2f %7.2f %7.2f\n")
		    call pargstr (Memc[filter[i]])
		    call pargr (t)
		    call pargr (m)
		    call pargr (s)
		    call pargr (nstar)
		    call pargr (nsky)
		    call pargr (sqrt (nstar))
		    call pargr (sqrt (npix * nsky))
		    call pargr (sqrt (npix * (ndark + nread**2)))
	    }
	}

	# Compute magnitude through each filter.
	# Use resubstitution to iterate for SNR.

	if (!IS_INDEFR(time) && !IS_INDEFR(snr)) {
	    call printf ("\n")
	    do i = 1, n {
		m = 20
		s = 0
		do j = 1, 100 {
		    t = time
		    nstar = star[i] * 10**(0.4*(20.0-m)) * t
		    nsky  = sky[i] * t
		    ndark = dark * t
		    noise = sqrt(nstar + npix * (nsky + ndark + nread**2))
		    m = 20 - 2.5 * log10 (snr * noise / (t * star[i]))
		    s = nstar / noise
		    if (abs(1-s/snr) <= TOL)
			break
		}

		call printf (
		    "%7s %7.2f %7.1f %7.1f %7.1f %7.1f %7.2f %7.2f %7.2f\n")
		    call pargstr (Memc[filter[i]])
		    call pargr (t)
		    call pargr (m)
		    call pargr (s)
		    call pargr (nstar)
		    call pargr (nsky)
		    call pargr (sqrt (nstar))
		    call pargr (sqrt (npix * nsky))
		    call pargr (sqrt (npix * (ndark + nread**2)))
	    }
	}

	# Compute SNR through each filter.

	if (!IS_INDEFR(time) && !IS_INDEFR(mag)) {
	    call printf ("\n")
	    do i = 1, n {
		t = time
		m = mag
		nstar = star[i] * lum * t
		nsky  = sky[i] * t
		ndark = dark * t
		noise = sqrt(nstar + npix * (nsky + ndark + nread**2))
		s = nstar / noise

		call printf (
		    "%7s %7.2f %7.1f %7.1f %7.1f %7.1f %7.2f %7.2f %7.2f\n")
		    call pargstr (Memc[filter[i]])
		    call pargr (t)
		    call pargr (m)
		    call pargr (s)
		    call pargr (nstar)
		    call pargr (nsky)
		    call pargr (sqrt (nstar))
		    call pargr (sqrt (npix * nsky))
		    call pargr (sqrt (npix * (ndark + nread**2)))
	    }
	}
	call printf ("\n")

done_
	call dbclose (tdb)
	call dbclose (ddb)
	do i = 1, n
	    call dbclose (fdb[i])
	call sfree (sp)
end
