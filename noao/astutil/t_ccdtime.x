define	NF		5		# Maximum number of filters
define	SZ_FILTER	7		# Maximum length of filter name
define	TOL		0.01		# Convergence tolerance


# T_CCDTIME -- Compute the time, magnitude, and signal-to-noise for CCD
# exposures for a given telescope, detector, and filters.  The telescope
# detector, and filter data come from a text database.  The computed
# quantities fix two parameters and compute the third.

procedure t_ccdtime()

pointer	database		# Telescope, detector, filter database
pointer	telescope		# Telescope
pointer	detector		# Detector
pointer	filter[NF]		# Filters
real	time			# Target time (sec)
real	mag			# Target magnitude
real	snr			# Target SNR
int	sum			# CCD summing
real	seeing			# Seeing (arcsec)
real	phase			# Moon phase (0-28)
real	airmass			# Airmass

int	i, n
real	aper, scale, trans, nread, dark, pixsize, lum, p, t1
real	star[NF], sky[NF], t, m, s, nstar, nsky, ndark, noise, npix
pointer	sp, tdb, ddb, fdb[NF]

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
	do i = 1, NF
	    call salloc (filter[i], SZ_FILTER, TY_CHAR)

	# Get task parameters.
	call clgstr ("database", Memc[database], SZ_FNAME)
	call clgstr ("telescope", Memc[telescope], SZ_FNAME)
	call clgstr ("detector", Memc[detector], SZ_FNAME)
	call clgstr ("f1", Memc[filter[1]], SZ_FILTER)
	call clgstr ("f2", Memc[filter[2]], SZ_FILTER)
	call clgstr ("f3", Memc[filter[3]], SZ_FILTER)
	call clgstr ("f4", Memc[filter[4]], SZ_FILTER)
	call clgstr ("f5", Memc[filter[5]], SZ_FILTER)

	time = clgetr ("time")
	mag = clgetr ("magnitude")
	snr = clgetr ("snr")
	sum = clgeti ("sum")
	seeing = clgetr ("seeing")
	phase = clgetr ("phase")
	airmass = clgetr ("airmass")

	i = 0
	if (IS_INDEFR(time))
	    i = i + 1
	if (IS_INDEFR(mag))
	    i = i + 1
	if (IS_INDEFR(snr))
	    i = i + 1
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
	do i = 1, NF {
	    iferr (fdb[n+1]=dbopen("",Memc[database],"filter",Memc[filter[i]]))
		next
	    if (fdb[n+1] == NULL)
		goto done_
	    n = n + 1
	    filter[n] = filter[i]
	}
	if (tdb == NULL || ddb == NULL || n == 0)
	    goto done_

	# Get telescope and detector parameters.
	aper = dbgetd (tdb, "aperture")
	scale = dbgetd (tdb, "scale")
	trans = dbgetd (tdb, "transmission")
	nread = dbgetd (ddb, "rdnoise")
	dark = dbgetd (ddb, "dark")
	pixsize = dbgetd (ddb, "pixsize") * (scale / 1000) * sum
	npix  =  1.4 * (seeing / pixsize)**2

	# Get star and sky rates for telescope/detector/filter combination.
	# Convert to a standard rate at 20th magnitude at the given airmass.

	do i = 1, n {
	    lum = 10. ** (0.4 * (dbgetd (fdb[i], "mag") - 20.))
	    star[i] = dbgetd (fdb[i], "star") * lum * aper ** 2 * trans *
		10 ** (0.4 * (1 - airmass))
	    star[i] = star[i] * dbgetd (ddb, Memc[filter[i]])
	    sky[i] = dbgetd (fdb[i], "sky0") + dbgetd (fdb[i], "sky1") * p +
		dbgetd (fdb[i], "sky2") * p * p
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
	call printf ("  Seeing: %-3.1f  Airmass: %-4.2f  Phase: %-4.1f\n")
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
	# Use resubstitution to iterate for exposure time.

	if (!IS_INDEFR(mag) && !IS_INDEFR(snr)) {
	    call printf ("\n")
	    do i = 1, n {
		t  = 10.
		t1 = 0.0
		while (abs(t-t1) > TOL) {
		    t = t1
		    m = mag
		    nstar = star[i] * lum
		    nsky  = sky[i] * t
		    ndark = dark * t
		    noise = sqrt(nstar * t + npix * (nsky + ndark + nread**2))
		    s = nstar / noise
		    t1 = snr / s
		}
		nstar = nstar * t
		s = s * t 

		call printf (
		    "%7s %7.1f %7.1f %7.1f %7.1f %7.1f %7.2f %7.2f %7.2f\n")
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
		while (abs(snr-s) > TOL) {
		    t = time
		    nstar = star[i] * 10**(0.4*(20.0-m)) * t
		    nsky  = sky[i] * t
		    ndark = dark * t
		    noise = sqrt(nstar + npix * (nsky + ndark + nread**2))
		    m = 20 - 2.5 * log10 (snr * noise / (t * star[i]))
		    s = nstar / noise
		}

		call printf (
		    "%7s %7.1f %7.1f %7.1f %7.1f %7.1f %7.2f %7.2f %7.2f\n")
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
		    "%7s %7.1f %7.1f %7.1f %7.1f %7.1f %7.2f %7.2f %7.2f\n")
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
