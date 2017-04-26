include	<mach.h>
include	<error.h>
include <math.h>
include <gset.h>
include	<ctype.h>
include	"sptime.h"


# T_SPTIME -- Spectroscopic exposure time calculator.

procedure t_sptime ()

bool	interactive
int	i, j, nexp, niter, npix, nw, fd, outlist
real	nobj[4], nsky[4], time, minexp, maxexp, snr, sngoal
real	wave, x, x1, dx, thruput, sat, dnmax
pointer sp, str, err, st, tab, waves, counts, snrs, gp

bool	streq(), tabexists(), fp_equalr()
int	stgeti(), strdic()
int	clpopnu(), fntopnb(), fntgfnb(), nowhite(), open(), tabgeti()
real	stgetr(), tabgetr(), tabinterp1(), gr_mag(), gr_getr()
real	st_x2w()
pointer tabopen(), gopen(), un_open()
errchk	tabopen, tabgeti, tabgetr, tabinterp1
errchk	st_gtable, st_gtable1, stgeti, stgetr, st_snr, st_disperser
errchk	open, gopen

begin
	call smark (sp)
	call salloc (st, ST_LEN, TY_STRUCT)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (err, SZ_FNAME, TY_CHAR)

	# Load tables.
	ST_SEARCH(st) = clpopnu ("search")
	ST_TAB(st) = tabopen ()
	tab = ST_TAB(st)
	call st_gtable (st, "spectrograph", "")
	call st_gtable (st, "spectrum", "spectrograph")
	call st_gtable (st, "sky", "spectrograph")
	call st_gtable (st, "extinction", "spectrograph")
	call st_gtable (st, "telescope", "spectrograph")
	call st_gtable (st, "emissivity", "spectrograph")
	call st_gtable (st, "adc", "spectrograph")
	call st_gtable (st, "filter", "spectrograph")
	call st_gtable (st, "filter2", "spectrograph")
	call st_gtable (st, "aperture", "spectrograph")
	call st_gtable (st, "fiber", "spectrograph")
	call st_gtable (st, "aperture", "fiber")
	call st_gtable (st, "collimator", "spectrograph")
	call st_gtable (st, "disperser", "spectrograph")
	call st_gtable (st, "xdisperser", "spectrograph")
	call st_gtable (st, "corrector", "spectrograph")
	call st_gtable (st, "camera", "spectrograph")
	call st_gtable (st, "vignetting", "spectrograph")
	call st_gtable (st, "vignetting", "camera")
	call st_gtable (st, "detector", "spectrograph")
	call st_gtable (st, "sensfunc", "spectrograph")
	
	call st_gtable1 (st, "abjohnson", "abjohnson")

	# Set dispersion units.
	call stgstr (st, "units", "spectrograph", "Angstroms",
	    Memc[str], SZ_LINE)
	call strcpy (Memc[str], ST_DUNITS(st), ST_SZSTRING) 
	ST_DUNANG(st) = un_open ("angstroms")
	ST_DUN(st) = un_open (ST_DUNITS(st))

	# Set spectrum.
	ST_REFW(st) = stgetr (st, "refwave", "spectrum", INDEFR)
	if (!IS_INDEFR(ST_REFW(st)))
	    call un_ctranr (ST_DUN(st), ST_DUNANG(st), ST_REFW(st),
		ST_REFW(st), 1)
	ST_REFF(st) = stgetr (st, "refflux", "spectrograph", 10.)
	call stgstr (st, "funits", "spectrograph", "AB", Memc[str], SZ_LINE)
	ST_FUNITS(st) = strdic (Memc[str], Memc[str], SZ_LINE, FUNITS)
	switch (ST_SPEC(st)) {
	case SPEC_BB:
	    ST_PARAM(st) = stgetr (st, "temperature", "spectrograph", 6000.)
	case SPEC_FL, SPEC_FN:
	    ST_PARAM(st) = stgetr (st, "index", "spectrograph", 0.)
	}
	ST_RV(st) = stgetr (st, "R", "spectrum", 3.1)
	ST_AV(st) = ST_RV(st) * stgetr (st, "E", "spectrum", 0.)

	# Set observing conditions.
	ST_SEEING(st) = stgetr (st, "seeing", "spectrograph", 1.)
	ST_AIRMASS(st) = stgetr (st, "airmass", "spectrograph", 1.)
	ST_PHASE(st) = stgetr (st, "phase", "spectrograph", 0.)

	# Set thermal background.
	ST_THERMAL(st) = stgetr (st, "thermal", "telescope", 0.)

	# Set instrument.
	ST_CW(st) = stgetr (st, "wave", "spectrograph", INDEFR)
	if (!IS_INDEFR(ST_CW(st)))
	    call un_ctranr (ST_DUN(st), ST_DUNANG(st), ST_CW(st), ST_CW(st), 1)
	ST_ORDER(st,1) = stgeti (st, "order", "spectrograph", INDEFI)
	ST_ORDER(st,2) = stgeti (st, "xorder", "spectrograph", INDEFI)

	# Aperture.
	if (!tabexists (tab, "aperture")) {
	    if (tabexists (tab, "fiber"))
		call st_gtable1 (st, "aperture", "circle")
	    else if (!IS_INDEFR(stgetr(st, "diameter", "spectrograph", INDEFR)))
		call st_gtable1 (st, "aperture", "circle")
	    else
		call st_gtable1 (st, "aperture", "slit")
	}
	call stgstr (st, "aptype", "aperture", "", Memc[str], SZ_LINE)
	if (Memc[str] == EOS) {
	    iferr (call tabgstr (tab, "aperture", "", "type",
		Memc[str], SZ_LINE)) {
		if (tabgeti (tab, "aperture", "", "table.ndim") == 2)
		    call strcpy ("circular", Memc[str], SZ_LINE)
		else
		    call strcpy ("rectangular", Memc[str], SZ_LINE)
	    }
	}
	ST_APTYPE(st) = strdic (Memc[str], Memc[str], SZ_LINE, APTYPES)
	switch (ST_APTYPE(st)) {
	case CIRCULAR:
	    if (tabexists (tab, "fiber")) {
		ST_APSIZE(st,1) = stgetr (st, "diameter", "fiber", INDEFR)
		if (!IS_INDEFR(ST_APSIZE(st,1)) && ST_APSIZE(st,1) > 0.) {
		    ST_TELSCALE(st) = stgetr (st, "scale", "telescope", 10.)
		    ST_APSIZE(st,1) = ST_APSIZE(st,1) * ST_TELSCALE(st)
		}
	    }
	    if (IS_INDEFR(ST_APSIZE(st,1)))
		ST_APSIZE(st,1) = stgetr (st, "diameter", "aperture", -2.)
	    ST_APSIZE(st,2) = ST_APSIZE(st,1)
	case RECTANGULAR:
	    ST_APSIZE(st,1) = stgetr (st, "width", "aperture", -2.)
	    ST_APSIZE(st,2) = stgetr (st, "length", "aperture", -100.)
	default:
	    call sprintf (Memc[err], SZ_FNAME,
		"Unknown aperture type (%s)")
		call pargstr (Memc[str])
	    call error (1, Memc[err])
	}

	ST_INOUTA(st,1) = stgetr (st, "inoutangle", "spectrograph", INDEFR)
	ST_INOUTA(st,2) = stgetr (st, "xinoutangle", "spectrograph", INDEFR)
	ST_BIN(st,1) = stgeti (st, "xbin", "detector", 1)
	ST_BIN(st,2) = stgeti (st, "ybin", "detector", 1)
	ST_GAIN(st) = stgetr (st, "gain", "detector", 1.)
	ST_RDNOISE(st) = stgetr (st, "rdnoise", "detector", 0.)
	ST_DARK(st) = stgetr (st, "dark", "detector", 0.)

	# Set filter flag.
	ST_FILTBLK(st) = NO
	call stgstr (st, "block", "filter", "no", Memc[str], SZ_LINE) 
	if (streq (Memc[str], "yes"))
	    ST_FILTBLK (st) = YES

	# Set sky subtraction parameters.
	if (tabexists (tab, "sky") ||
	    (tabexists (tab, "emissivity") && ST_THERMAL(st) > 0.)) {
	    switch (ST_APTYPE(st)) {
	    case CIRCULAR:
		call stgstr (st, "skysub", "spectrograph", "multiap",
		    Memc[str], SZ_LINE)
		ST_NSKYAPS(st) = stgeti (st, "nskyaps", "spectrograph", 10)
	    case RECTANGULAR:
		call stgstr (st, "skysub", "spectrograph", "longslit",
		    Memc[str], SZ_LINE)
	    }
	} else
	    call stgstr (st, "skysub", "spectrograph", "none", Memc[str],
		SZ_LINE)
	    
	ST_SKYSUB(st) = strdic (Memc[str], Memc[str], SZ_LINE, SKYSUB)

	# Set calculation parameters.
	ST_MINEXP(st) = stgetr (st, "minexp", "spectrograph", MINEXP)
	ST_MAXEXP(st) = stgetr (st, "maxexp", "spectrograph", 3600.)
	if (ST_MINEXP(st) <= 0.)
	    ST_MINEXP(st) = MINEXP
	if (ST_MAXEXP(st) <= ST_MINEXP(st))
	    ST_MAXEXP(st) = ST_MINEXP(st)
	time = stgetr (st, "time", "spectrograph", INDEFR)
	sngoal = stgetr (st, "sn", "spectrograph", 25.)
	if (IS_INDEF(time) && IS_INDEF(sngoal))
	    call error (1,
		"Either an exposure time or a desired S/N must be specified")
	ST_SUBPIXELS(st) = stgeti (st, "subpixels", "spectrograph", 1)

	# Set output parameters.
	gp = NULL; fd = NULL
	call stgstr (st, "output", "spectrograph", "", Memc[str], SZ_LINE)
	outlist = fntopnb (Memc[str], NO)
	if (fntgfnb (outlist, Memc[str], SZ_LINE) != EOF) {
	    if (streq (Memc[str], "ALL")) {
		call strcpy (OUTTYPES, Memc[str], SZ_LINE)
		j = str+1
		for (i=j; Memc[i] != EOS; i=i+1) { 
		    if (IS_WHITE(Memc[i]) || Memc[i] == '\n')
			next
		    if (Memc[i] == Memc[str])
			Memc[j] = ','
		    else
			Memc[j] = Memc[i]
		    j = j + 1
		}
		Memc[j] = EOS
		call fntclsb (outlist)
		outlist = fntopnb (Memc[str+1], NO)
	    } else
		call fntrewb (outlist)
	    nw = stgeti (st, "nw", "spectrograph", 101)
	    call stgstr (st, "graphics", "spectrograph", "stdgraph",
		Memc[str], SZ_LINE)
	    if (nowhite (Memc[str], Memc[str], SZ_LINE) > 0) {
		gp = gopen (Memc[str], NEW_FILE+AW_DEFER, STDGRAPH)
		call stgstr (st, "interactive", "spectrograph", "yes",
		    Memc[str], SZ_LINE)
		interactive = streq (Memc[str], "yes")
	    }
	    call stgstr (st, "list", "spectrograph", "", Memc[str], SZ_LINE)
	    if (nowhite (Memc[str], Memc[str], SZ_LINE) > 0)
		fd = open (Memc[str], APPEND, TEXT_FILE)
	}

	# Focal lengths.
	ST_COLFL(st) = stgetr (st, "colfl", "collimator", INDEFR)
	if (IS_INDEFR(ST_COLFL(st))) {
	    iferr (ST_COLFL(st) = tabgetr (tab, "collimator", "", "fl"))
		ST_COLFL(st) = 1.
	}
	ST_CAMFL(st) = stgetr (st, "camfl", "camera", INDEFR)
	if (IS_INDEFR(ST_CAMFL(st))) {
	    iferr (ST_CAMFL(st) = tabgetr (tab, "camera", "", "fl"))
		ST_CAMFL(st) = 1.
	}

	call st_disperser (st, "disperser", 1)
	if (ST_DISPTYPE(st,1) == 0)
	    call error (1, "No disperser specified")
	call st_disperser (st, "xdisperser", 2)

	ST_AREA(st) = 10000 * stgetr (st, "area", "telescope", 1.)

	# Scales.
	ST_TELSCALE(st) = stgetr (st, "scale", "telescope", 10.)
	ST_SCALE(st,1) = ST_TELSCALE(st)
	ST_SCALE(st,2) = ST_TELSCALE(st)
	x = gr_mag (ST_GR(st,1), ST_CW(st), ST_ORDER(st,1))
	if (!IS_INDEF(x))
	    ST_SCALE(st,1) = ST_SCALE(st,1) / x
	x = gr_mag (ST_GR(st,2), ST_CW(st), ST_ORDER(st,2))
	if (!IS_INDEF(x))
	    ST_SCALE(st,2) = ST_SCALE(st,2) / x
	x = ST_COLFL(st) / ST_CAMFL(st)
	ST_SCALE(st,1) = ST_SCALE(st,1) * x
	ST_SCALE(st,2) = ST_SCALE(st,2) * x
	ST_SCALE(st,1) = ST_SCALE(st,1) *
	    stgetr (st, "apmagdisp", "spectrograph", 1.)
	ST_SCALE(st,2) = ST_SCALE(st,2) *
	    stgetr (st, "apmagxdisp", "spectrograph", 1.)
	ST_PIXSIZE(st) = stgetr (st, "pixsize", "detector", 0.02)
	ST_SCALE(st,1) = ST_SCALE(st,1) * ST_PIXSIZE(st) * ST_BIN(st,1)
	ST_SCALE(st,2) = ST_SCALE(st,2) * ST_PIXSIZE(st) * ST_BIN(st,2)

	# Convert aperture sizes to arcsec.
	if (ST_APSIZE(st,1) < 0.)
	    ST_APSIZE(st,1) = -ST_APSIZE(st,1) * ST_SCALE(st,1)
	else
	    ST_APSIZE(st,1) = ST_APSIZE(st,1)
	if (ST_APSIZE(st,2) < 0.)
	    ST_APSIZE(st,2) = -ST_APSIZE(st,2) * ST_SCALE(st,2)
	else
	    ST_APSIZE(st,2) = ST_APSIZE(st,2)

	# Set dispersion per pixel and per resolution element.
	ST_RES(st,1) = stgetr (st, "resolution", "camera", INDEFR)
	if (IS_INDEFR(ST_RES(st,1)))
	    ST_RES(st,1) = 2
	else
	    ST_RES(st,1) = ST_RES(st,1) / ST_PIXSIZE(st)
	ST_RES(st,2) = ST_RES(st,1)
	ST_DISP(st,1) = abs (gr_getr (ST_GR(st,1), "dispersion"))
	ST_DISP(st,1) = ST_DISP(st,1) * ST_PIXSIZE(st) * ST_BIN(st,1)
	x = 1 + min (ST_SEEING(st), ST_APSIZE(st,1)) / ST_SCALE(st,1)
	ST_DISP(st,2) = ST_DISP(st,1) * max (2., ST_RES(st,1), x)

	# Set number of pixels in object.
	switch (ST_APTYPE(st)) {
	case CIRCULAR:
	    x = ST_APSIZE(st,2) / ST_SCALE(st,2) + ST_RES(st,2)
	    npix = max (1, int (x + 0.999))
	    ST_NOBJPIX(st) = npix
	    ST_APLIM(st) = YES
	case RECTANGULAR:
	    x = ST_APSIZE(st,2) / ST_SCALE(st,2) + ST_RES(st,2)
	    npix = max (1, int (x + 0.999))
	    x = min (ST_APSIZE(st,2), 3*ST_SEEING(st)) / ST_SCALE(st,2) +
		ST_RES(st,2)
	    ST_NOBJPIX(st) = min (npix, int (x + 0.999))
	    if (ST_NOBJPIX(st) > npix)
		ST_APLIM(st) = NO
	    else
		ST_APLIM(st) = YES
	}

	# Set number of pixels in sky.
	switch (ST_SKYSUB(st)) {
	case SKY_NONE:
	    ST_NSKYPIX(st) = 0
	case SKY_LONGSLIT:
	    ST_NSKYPIX(st) = max (0, npix - ST_NOBJPIX(st))
	case SKY_MULTIAP:
	    ST_NSKYPIX(st) = npix * ST_NSKYAPS(st)
	case SKY_SHUFFLE:
	    ST_NSKYPIX(st) = npix
	}

	# Compute exposure time and S/N.
	if (!tabexists (tab, "spectrum")) {
	    if (IS_INDEF(ST_REFW(st)))
		ST_REFW(st) = ST_CW(st)
	    switch (ST_FUNITS(st)) {
	    case AB:
		ST_REFFL(st) = 10. ** (-0.4 *
		    (ST_REFF(st) + 5*log10(ST_REFW(st)) + 2.40))
	    case FLAMBDA:
		if (ST_REFF(st) < 0.)
		    call error (1,
			"Monochromatic flux (F-lambda) must be greater than 0")
		ST_REFFL(st) = ST_REFF(st)
	    case FNU:
		if (ST_REFF(st) < 0.)
		    call error (1,
			"Monochromatic flux (F-nu) must be greater than 0")
		ST_REFFL(st) = ST_REFF(st) * C / (ST_REFW(st) * ST_REFW(st))
	    case UMAG,BMAG,VMAG,RMAG,IMAG,JMAG,HMAG,KSMAG,KMAG,LMAG,LPMAG,MMAG:
		switch (ST_FUNITS(st)) {
		case UMAG:
		    ST_REFW(st) = 3650.
		case BMAG:
		    ST_REFW(st) = 4400.
		case VMAG:
		    ST_REFW(st) = 5500.
		case RMAG:
		    ST_REFW(st) = 7000.
		case IMAG:
		    ST_REFW(st) = 9000.
		case JMAG:
		    ST_REFW(st) = 12150.
		case HMAG:
		    ST_REFW(st) = 16540.
		case KSMAG:
		    ST_REFW(st) = 21570.
		case KMAG:
		    ST_REFW(st) = 21790.
		case LMAG:
		    ST_REFW(st) = 35470.
		case LPMAG:
		    ST_REFW(st) = 37610.
		case MMAG:
		    ST_REFW(st) = 47690.
		}

		ST_REFFL(st) = ST_REFF(st) +
		    tabinterp1 (tab, "abjohnson", ST_REFW(st))
		ST_REFFL(st) = 10. ** (-0.4 *
		    (ST_REFFL(st) + 5*log10(ST_REFW(st)) + 2.40))
	    }
	}

	# Check saturation.
	sat = stgetr (st, "saturation", "detector", MAX_REAL)
	dnmax = stgetr (st, "dnmax", "detector", MAX_REAL)

	wave = ST_CW(st)
	if (!IS_INDEF(time)) {
	    if (time <= 0.) {
		call eprintf ("Total Exposure Time must be greater than 0.\n")
		return
	    }

	    if (ST_MAXEXP(st) > 0.) {
		nexp = max (1, int (time / ST_MAXEXP(st) + 0.99))
		time = time / nexp
	    } else
		nexp = 1

	} else {
	    if (sngoal <= 0.) {
		call printf ("Desired S/N must be greater than 0.\n")
		return
	    }

	    nexp = 1
	    minexp = ST_MINEXP(st)
	    maxexp = ST_MAXEXP(st)
	    time = maxexp
	    snr = 0.

	    # Iterate to try and achieve the requested SNR.
	    do niter = 1, MAXITER {

		if (snr > 0.) {
		    x = time
		    i = nexp

		    # After the first pass we use the last calculated SNR to
		    # estimate a new time per exposure and number of exposures.
		    time = time*sngoal*sngoal/(nexp*snr*snr)

		    # Divide into multiple exposures if the time per exposure
		    # exceeds a maximum.  Note the maximum may be reset by
		    # saturation criteria.
		    if (time > maxexp) {
			nexp = nexp * max (1, int (time / maxexp + 0.99))
			time = x*sngoal*sngoal/(nexp*snr*snr)
		    } 

		    # Apply a minimum time per exposure if possible.
		    if (time < minexp && nexp > 1) {
			nexp = max (1, nexp * time / minexp)
			time = x*sngoal*sngoal/(nexp*snr*snr)
		    }

		    # New time per exposure to try.
		    time = max (minexp, min (maxexp, time))
		    if (fp_equalr (time, x) && nexp == i)
			break
		}

		# Compute SNR.
		call st_snr (st, NULL, wave, nexp, time, nobj, nsky,
		    snr, thruput)

		# Reset maximum time per exposure to avoid saturation.
		if (nobj[1]+nsky[1] > sat && time > minexp && snr < sngoal) {
		    time = time * nexp
		    snr = snr * snr * nexp
		    nexp = nexp * (1 + (nobj[1] + nsky[1]) / sat)
		    time = time / nexp
		    snr = sqrt (snr / nexp)
		    maxexp = max (minexp, time)
		    next
		}
		if ((nobj[1]+nsky[1])/ST_GAIN(st) > dnmax && time > minexp &&
			snr < sngoal) {
		    time = time * nexp
		    snr = snr * snr * nexp
		    nexp = nexp * (1 + (nobj[1] + nsky[1]) /
			(ST_GAIN(st) * dnmax))
		    time = time / nexp
		    snr = sqrt (snr / nexp)
		    maxexp = max (minexp, time)
		    next
		}

		if (abs ((sngoal-sqrt(real(nexp))*snr)/sngoal) < 0.001)
		    break
	    }
	}
	ST_NEXP(st) = nexp
	ST_TIME(st) = time

	# Output.
	npix = stgetr (st, "ndisp", "detector", 2048.)
	nw = max (1, min (nw, npix))
	x1 = npix * ST_PIXSIZE(st)

	call salloc (waves, nw, TY_REAL)
	call salloc (counts, nw, TY_REAL)
	call salloc (snrs, nw, TY_REAL)

	if (nw > 1) {
	    dx = x1 / (nw - 1)
	    x1 = -x1 / 2
	    do i = 0, nw-1 {
		x = x1 + dx * i
		Memr[waves+i] = st_x2w (st, 1, x)
	    }
	} else
	    Memr[waves] = wave

	# Output result summary.
	call st_results (st, STDOUT)
	call st_check (st, STDOUT, Memr[waves], nw)
	call st_snr (st, STDOUT, wave, nexp, time, nobj, nsky, snr, thruput)

	while (fntgfnb (outlist, Memc[str], SZ_LINE) != EOF)
	    call st_output (st, gp, fd, interactive, Memc[str], Memr[waves], nw)
	    
	# Finish up.
	call un_close (ST_DUN(st))
	call un_close (ST_DUNANG(st))
	call clpcls (ST_SEARCH(st))
	call tabclose (ST_TAB(st))
	call gr_close (ST_GR(st,1))
	call gr_close (ST_GR(st,2))
	if (gp != NULL)
	    call gclose (gp)
	if (fd != NULL)
	    call close (fd)
	call fntclsb (outlist)
	call sfree (sp)
end


# ST_RESULTS -- Print result summary.

procedure st_results (st, fd)

pointer st		#I SPECTIME structure
int	fd		#I Output file descriptor

char	eff[SZ_FNAME]
real	x, y
int	npix, order, tabgeti(), stgeti()
pointer	tab
real	gr_getr()
bool	tabexists()

begin
	tab = ST_TAB(st)

	call fprintf (fd, "\n")
	if (tabexists (tab, "spectrum"))
	    call st_description (st, fd, "Object spectrum: ", "spectitle",
		"spectrum")
	else {
	    call fprintf (fd, "Object spectrum: ")
	    switch (ST_SPEC(st)) {
	    case SPEC_BB:
		call fprintf (fd, "Blackbody spectrum of temperature %g K\n")
		    call pargr (ST_PARAM(st))
	    case SPEC_FL:
		call fprintf (fd, "F(lambda) power law of index %g\n")
		    call pargr (ST_PARAM(st))
	    case SPEC_FN:
		call fprintf (fd, "F(nu) power law of index %g\n")
		    call pargr (ST_PARAM(st))
	    }
	    if (ST_AV(st) > 0.) {
		call fprintf (fd,
		    "Reddening: E(B-V) of %g with A(V)/E(B-V) of %g\n")
		    call pargr (ST_AV(st) / ST_RV(st))
		    call pargr (ST_RV(st))
	    }
	    call un_ctranr (ST_DUNANG(st), ST_DUN(st), ST_REFW(st), x, 1)
	    call fprintf (fd, "Reference wavelength: %.4g %s\n")
		call pargr (x)
		call pargstr (ST_DUNITS(st))
	    call fprintf (fd, "Reference flux: ")
	    switch (ST_FUNITS(st)) {
	    case AB:
		call fprintf (fd, "AB = %.3g (%.3g ergs/s/cm^2/A)\n")
		    call pargr (ST_REFF(st))
		    call pargr (ST_REFFL(st))
	    case FLAMBDA:
		call fprintf (fd, "%.3g ergs/s/cm^2/A\n")
		    call pargr (ST_REFF(st))
	    case FNU:
		call fprintf (fd, "%.3g ergs/s/cm^2/Hz\n")
		    call pargr (ST_REFF(st))
	    case UMAG, BMAG, VMAG, RMAG, IMAG, JMAG:
		switch (ST_FUNITS(st)) {
		case UMAG:
		    call fprintf (fd, "U = %.3g ")
			call pargr (ST_REFF(st))
		case BMAG:
		    call fprintf (fd, "B = %.3g ")
			call pargr (ST_REFF(st))
		case VMAG:
		    call fprintf (fd, "V = %.3g ")
			call pargr (ST_REFF(st))
		case RMAG:
		    call fprintf (fd, "R = %.3g ")
			call pargr (ST_REFF(st))
		case IMAG:
		    call fprintf (fd, "I = %.3g ")
			call pargr (ST_REFF(st))
		case JMAG:
		    call fprintf (fd, "J = %.3g ")
			call pargr (ST_REFF(st))
		}
		call fprintf (fd, "(%.3g ergs/s/cm^2/A)\n")
		    call pargr (ST_REFFL(st))
	    }
	}
	if (tabexists (tab, "sky")) {
	    call st_description (st, fd, "Sky spectrum: ", "skytitle", "sky")
	    if (tabgeti (tab, "sky", "", "table.ndim") == 2) {
		call fprintf (fd, "\tMoon phase: %d\n")
		    call pargr (ST_PHASE(st))
	    }
	}
	if (ST_AIRMASS(st) > 0) {
	    call st_description (st, fd, "Extinction: ", "exttitle",
		"extinction")
	    call fprintf (fd, "\tAirmass: %.3g\n")
		call pargr (ST_AIRMASS(st))
	}
	call fprintf (fd, "Seeing: %.2g\" (FWHM)\n")
	    call pargr (ST_SEEING(st))
	if (tabexists (tab, "emissivity") && ST_THERMAL(st) > 0.) {
	    call fprintf (fd, "Thermal Background:\n")
	    call st_description (st, fd, "\tEmissivity: ",
		"emistitle", "emissivity")
	    call fprintf (fd, "\tTemperature: %.1g K\n")
		call pargr (ST_THERMAL(st))
	}

	call fprintf (fd, "\n")
	call st_description (st, fd, "Telescope: ", "teltitle", "telescope")
	call fprintf (fd, "\tArea: %.1f m^2, Scale: %.4g arcsec/mm\n")
	    call pargr (ST_AREA(st) / 10000.)
	    call pargr (ST_TELSCALE(st))
	if (tabexists (tab, "adc"))
	    call st_description (st, fd, "ADC: ", "adctitle", "adc")
	if (tabexists (tab, "spectrograph"))
	    call st_description (st, fd, "Spectrograph: ", "title",
		"spectrograph")
	call st_description (st, fd, "Collimator: ", "coltitle", "collimator")
	call fprintf (fd, "\tFocal length = %.4g m\n")
	    call pargr (ST_COLFL(st))
	if (tabexists (tab, "aperture")) {
	    call st_description (st, fd, "Apertures: ", "aptitle", "aperture")
	    switch (ST_APTYPE(st)) {
	    case CIRCULAR:
		call fprintf (fd, "\tSize: %.2f\", %.3g mm, %.1f pixels\n")
		    call pargr (ST_APSIZE(st,1))
		    call pargr (ST_APSIZE(st,1) / ST_TELSCALE(st))
		    call pargr (ST_APSIZE(st,1) / ST_SCALE(st,1))
	    case RECTANGULAR:
		call fprintf (fd,
		"\tSize: %.2f\" x %.2f\", %.3g x %.3g mm, %.1f x %.1f pixels\n")
		    call pargr (ST_APSIZE(st,1))
		    call pargr (ST_APSIZE(st,2))
		    call pargr (ST_APSIZE(st,1) / ST_TELSCALE(st))
		    call pargr (ST_APSIZE(st,2) / ST_TELSCALE(st))
		    call pargr (ST_APSIZE(st,1) / ST_SCALE(st,1))
		    call pargr (ST_APSIZE(st,2) / ST_SCALE(st,2))
	    }
	}
	if (tabexists (tab, "fiber"))
	    call st_description (st, fd, "Fibers: ", "fibtitle", "fiber")
	if (tabexists (tab, "filter"))
	    call st_description (st, fd, "Filter: ", "ftitle", "filter")
	if (tabexists (tab, "filter2"))
	    call st_description (st, fd, "Filter: ", "f2title", "filter2")
	if (ST_DISPTYPE(st,1) != 0) {
	    call st_description (st, fd, "Disperser: ", "disptitle",
		"disperser")
	    order = nint (gr_getr (ST_GR(st,1), "order"))
	    call fprintf (fd, "\tCentral order = %d\n")
		call pargi (order)
	    x = gr_getr (ST_GR(st,1), "wavelength")
	    call un_ctranr (ST_DUNANG(st), ST_DUN(st), x, x, 1)
	    call fprintf (fd, "\tCentral wavelength = %.6g %s\n")
		call pargr (x)
		call pargstr (ST_DUNITS(st))
	    x = abs(gr_getr(ST_GR(st,1), "dispersion"))
	    call un_ctranr (ST_DUNANG(st), ST_DUN(st), x, x, 1)
	    call un_ctranr (ST_DUNANG(st), ST_DUN(st), ST_DISP(st,1), y, 1)
	    call fprintf (fd,
	    "\tCentral dispersion = %.3g %s/mm, %.3g %s/pixel\n")
		call pargr (x)
		call pargstr (ST_DUNITS(st))
		call pargr (y)
		call pargstr (ST_DUNITS(st))
	    if (ST_DISPTYPE(st,1) == GRATING &&
		int(gr_getr(ST_GR(st,1),"full"))==YES) {
		call fprintf (fd, "\tRuling = %d lines/mm\n")
		    call pargr (gr_getr (ST_GR(st,1), "g"))
		call fprintf (fd, "\tBlaze = %.1f deg\n")
		    call pargr (gr_getr (ST_GR(st,1), "blaze"))
		x = gr_getr (ST_GR(st,1), "tilt")
		if (abs(x) > 0.1) {
		    call fprintf (fd, "\tGrating tilt = %.1f degrees\n")
			call pargr (gr_getr (ST_GR(st,1), "tilt"))
		    x = gr_getr (ST_GR(st,1), "mag")
		    if (abs(x) < 0.99) {
			call fprintf (fd, "\tGrating magnification = %.2f\n")
			    call pargr (x)
		    }
		}
		call sprintf (eff, SZ_FNAME, "eff%d")
		    call pargi (order)
		if (!tabexists (tab, eff)) {
		    if (order == 1 && tabexists (tab, "disperser")) {
			if (tabgeti (tab, "disperser", "", "table.ndim") != 0)
			    call strcpy ("disperser", eff, SZ_FNAME)
		    }
		}
		if (tabexists (tab, eff))
		    call fprintf (fd, "\tUsing tabulated efficiencies\n")
		else
		    call fprintf (fd, "\tUsing predicted efficiencies\n")
	    }
	}
	if (ST_DISPTYPE(st,2) != 0) {
	    call st_description (st, fd, "Crossdisperser: ", "xdisptitle",
		"xdisperser")
	    order = nint (gr_getr (ST_GR(st,2), "order"))
	    call fprintf (fd, "\tCentral order = %d\n")
		call pargi (order)
	    x = gr_getr (ST_GR(st,2), "wavelength")
	    call un_ctranr (ST_DUNANG(st), ST_DUN(st), x, x, 1)
	    call fprintf (fd, "\tCentral wavelength = %.6g %s\n")
		call pargr (x)
		call pargstr (ST_DUNITS(st))
	    x = abs(gr_getr(ST_GR(st,1), "dispersion"))
	    call un_ctranr (ST_DUNANG(st), ST_DUN(st), x, x, 1)
	    call fprintf (fd, "\tCentral dispersion = %.3g %s/mm\n")
		call pargr (x)
		call pargstr (ST_DUNITS(st))
	    if (ST_DISPTYPE(st,2) == GRATING &&
		int(gr_getr(ST_GR(st,2),"full"))==YES) {
		call fprintf (fd, "\tRuling = %d lines/mm\n")
		    call pargr (gr_getr (ST_GR(st,2), "g"))
		call fprintf (fd, "\tBlaze = %d deg\n")
		    call pargr (gr_getr (ST_GR(st,2), "blaze"))
		x = gr_getr (ST_GR(st,2), "tilt")
		if (abs(x) > 0.1) {
		    call fprintf (fd, "\tGrating tilt = %.1f degrees\n")
			call pargr (gr_getr (ST_GR(st,2), "tilt"))
		    x = gr_getr (ST_GR(st,2), "mag")
		    if (abs(x) < 0.99) {
			call fprintf (fd, "\tGrating magnification = %.2f\n")
			    call pargr (x)
		    }
		}
		call sprintf (eff, 10, "xeff%d")
		    call pargi (order)
		if (!tabexists (tab, eff)) {
		    if (order == 1 && tabexists (tab, "xdisperser")) {
			if (tabgeti (tab, "xdisperser", "", "table.ndim") != 0)
			    call strcpy ("xdisperser", eff, SZ_FNAME)
		    }
		}
		if (tabexists (tab, eff))
		    call fprintf (fd, "\tUsing tabulated efficiencies\n")
		else
		    call fprintf (fd, "\tUsing predicted efficiencies\n")
	    }
	}
	if (tabexists (tab, "corrector"))
	    call st_description (st, fd, "Corrector: ", "cortitle", "corrector")
	call st_description (st, fd, "Camera: ", "camtitle", "camera")
	call fprintf (fd, "\tFocal length = %.4g m, Resolution = %.1g pixels\n")
	    call pargr (ST_CAMFL(st))
	    call pargr (ST_RES(st,1))
	call st_description (st, fd, "Detector: ", "dettitle", "detector")
	call fprintf (fd, "\tPixel size: %d microns, %.2f\"\n")
	    call pargr (1000 * ST_PIXSIZE(st))
	    call pargr (ST_SCALE(st,1))
	if (ST_BIN(st,1) != 1 || ST_BIN(st,2) != 1) {
	    call fprintf (fd, "\tBinning: %dx%d (XxY)\n")
		call pargi (ST_BIN(st,1))
		call pargi (ST_BIN(st,2))
	}
	npix = stgeti (st, "ndisp", "detector", 2048) / ST_BIN(st,1)
	call fprintf (fd, "\tNumber of pixels = %d\n")
	    call pargi (npix)
	call fprintf (fd,
	    "\tRead noise = %.1f e-, Gain = %.1f e-/DN, Dark = %.3g e-/s\n") 
	    call pargr (ST_RDNOISE(st))
	    call pargr (ST_GAIN(st))
	    call pargr (ST_DARK(st))

	call fprintf (fd, "\n")
	call fprintf (fd,
	    "Source pixels: %d pixels")
	    call pargi (ST_NOBJPIX(st))
	if (ST_APLIM(st) == YES)
	    call fprintf (fd, " (aperture & camera resolution limited)\n")
	else
	    call fprintf (fd, " (3xFWHM seeing disk & camera resolution)\n")
	switch (ST_SKYSUB(st)) {
	case SKY_NONE:
	    call fprintf (fd,
		"Background pixels: no background subtraction done\n")
	case SKY_LONGSLIT:
	    call fprintf (fd, "Background pixels: %3d pixels\n")
		call pargi (ST_NSKYPIX(st))
	case SKY_MULTIAP:
	    call fprintf (fd,
		"Background pixels: %d apertures each with %d pixels\n")
		call pargi (ST_NSKYAPS(st))
		call pargi (ST_NSKYPIX(st) / ST_NSKYAPS(st))
	case SKY_SHUFFLE:
	    call fprintf (fd,
		"Background pixels: shuffle with %d pixels (same as source)\n")
		call pargi (ST_NSKYPIX(st))
	}
	call fprintf (fd, "\n")
end


# ST_DESCRIPTION -- Print description of table.

procedure st_description (st, fd, label, title, table)

pointer st		#I SPECTIME structure
int	fd		#I Output file descriptor
char	label[ARB]	#I Label
char	title[ARB]	#I Title parameter name
char	table[ARB]	#I Table name

pointer sp, str

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Print label and title.
	call st_gtitle (st, title, table, Memc[str], SZ_LINE)
	call fprintf (fd, "%s%s\n")
	    call pargstr (label)
	    call pargstr (Memc[str])

	# Print description if available.
	ifnoerr (call tabgstr (ST_TAB(st), table, "", "description", Memc[str],
	    SZ_LINE)) {
	    call fprintf (fd, "%s\n")
		call pargstr (Memc[str])
	}

	call sfree (sp)
end


# ST_GTITLE -- Get title.

procedure st_gtitle (st, param, table, title, maxchar)

pointer st		#I SPECTIME structure
char	param[ARB]	#I Title parameter name
char	table[ARB]	#I Table name
char	title[ARB]	#O Title
int	maxchar		#I Maximum string length

char	dummy[1]
int	nowhite()

begin
	call clgstr (param, title, maxchar)
	if (nowhite (title, dummy, 1) > 0)
	    return

	iferr (call tabgstr (ST_TAB(st), table, "spectrograph", param, title,
	    maxchar)) {
	    iferr (call tabgstr (ST_TAB(st), table, "", "title",
		title, maxchar)) {
		iferr (call tabgstr (ST_TAB(st), table, "",
		    "table.filename", title, maxchar))
		    title[1] = EOS
	    }
	}
end


# ST_SNR -- Source, sky, SNR, and thruput including all orders.

procedure st_snr (st, fd, wave, nexp, time, nobj, nsky, snr, thruput)

pointer st		#I SPECTIME structure
int	fd		#I Output descriptor
real	wave		#I Wavelengths
int	nexp		#I Number of exposures
real	time		#I Exposure time
real	nobj[4]		#O Number of object photons (1=total)
real	nsky[4]		#O Number of sky photons (1=total)
real	snr		#O S/N per pixel for total
real	thruput		#O System thruput at primary wavelength

int	i, j, order
real	w, f, st_spectrum()
errchk	st_spectrum, st_snr1

begin
	# Initialize.
	do i = 1, 4 {
	    nobj[i] = 0
	    nsky[i] = 0
	}

	# If not x-dispersed and disperser is a grating do overlapping orders.
	if (ST_DISPTYPE(st,2) == 0 && ST_FILTBLK(st) == NO &&
	    (ST_DISPTYPE(st,1) == GRATING || ST_DISPTYPE(st,1) == GRISM)) {
	    order = ST_ORDER(st,1)
	    do j = 2, 4 {
		i = order + j - 3
		if (i < 1)
		    next
		w = wave * order / i
		f = st_spectrum (st, w)
		ST_ORDER(st,1) = i
		call st_snr1 (st, NULL, w, f, nexp, time, nobj[j], nsky[j],
		    snr, thruput)
		if (i != order) {
		    nobj[1] = nobj[1] + nobj[j]
		    nsky[1] = nsky[1] + nsky[j]
		}
	    }
	    ST_ORDER(st,1) = order

	    w = wave
	    f = st_spectrum (st, w)
	    call st_snr1 (st, fd, w, f, nexp, time, nobj, nsky, snr, thruput)
	} else {
	    w = wave
	    f = st_spectrum (st, w)
	    call st_snr1 (st, fd, w, f, nexp, time, nobj, nsky, snr, thruput)
	    nobj[3] = nobj[1]
	    nsky[3] = nsky[1]
	}
end


# ST_SNR1 -- Compute and print photons and S/N for given exposure time.

procedure st_snr1 (st, fd, wave, flux, nexp, time, nobj, nbkg, snr, thruput)

pointer st		#I SPECTIME structure
int	fd		#I Output descriptor
real	wave		#I Wavelength
real	flux		#I Flux
int	nexp		#I Number of exposures
real	time		#I Exposure time
real	nobj		#U Number of object photons
real	nbkg		#U Number of bkg photons
real	snr		#O S/N per pixel
real	thruput		#O System thruput

int	i, ncols
real	tobs, n, ndark
real	w, w1, nmag, sky, thermal, bkg, dobj, dbkg, ddark, dreadout, tnoise
real	ext, tel, adc, ap, fib, inst, fltr1, fltr2, col, eff1, eff2, disp
real	cor, cam, vig, dqe, cum, sqnexp

real	tabinterp1(), tabinterp2(), st_dispeff(), st_w2dw(), st_w2x()
errchk	tabinterp1, tabinterp2, st_dispeff

begin
	# Check for reasonable wavlength and source flux.
	if (wave < 0. || flux < 0.) {
	    nobj = 0.
	    nbkg = 0.
	    snr = 0.
	    thruput = 0.
	    return
	}

	# Set observation time.
	switch (ST_SKYSUB(st)) {
	case SKY_SHUFFLE:
	    tobs = time / 2
	default:
	    tobs = time
	}

	# Compute pixel counts over subsampled pixels.
	disp = st_w2dw (st, 1, wave) * ST_PIXSIZE(st) * ST_BIN(st,1) /
	    ST_SUBPIXELS(st)
	w1 = wave - disp * (ST_SUBPIXELS(st) + 1) / 2.
	do i = 1, ST_SUBPIXELS(st) {
	    w = w1 + disp * i

	    # Atmospheric transmission.
	    iferr {
		ext = tabinterp1 (ST_TAB(st), "extinction", w)
		ext = 10 ** (-0.4 * ext * ST_AIRMASS(st))
	    } then
		ext = INDEF

	    # Telescope transmission.
	    iferr (tel = tabinterp1 (ST_TAB(st), "telescope", w))
		tel = 1

	    # ADC transmission.
	    iferr (adc = tabinterp1 (ST_TAB(st), "adc", w))
		adc = INDEF

	    # Aperture thruput.
	    iferr {
		switch (ST_APTYPE(st)) {
		case CIRCULAR:
		    ap = tabinterp1 (ST_TAB(st), "aperture",
			ST_APSIZE(st,1) / ST_SEEING(st))
		case RECTANGULAR:
		    ap = tabinterp2 (ST_TAB(st), "aperture",
			ST_APSIZE(st,1) / ST_SEEING(st),
			ST_APSIZE(st,2) / ST_SEEING(st))
		}
	    } then
		ap = 1

	    # Fiber transmission.
	    iferr (fib = tabinterp1 (ST_TAB(st), "fiber", w))
		fib = INDEF

	    # Spectrograph transmission.
	    iferr (inst = tabinterp1 (ST_TAB(st), "spectrograph", w))
		inst = INDEF

	    # Filter transmission.
	    iferr (fltr1 = tabinterp1 (ST_TAB(st), "filter", w))
		fltr1 = INDEF
	    iferr (fltr2 = tabinterp1 (ST_TAB(st), "filter2", w))
		fltr2 = INDEF

	    # Collimator transmission.
	    iferr (col = tabinterp1 (ST_TAB(st), "collimator", w))
		col = 1

	    # Disperser efficiency.
	    eff1 = st_dispeff (st, "disperser", w, ST_ORDER(st,1))
	    eff2 = st_dispeff (st, "xdisperser", w, ST_ORDER(st,2))

	    # Corrector transmission.
	    iferr (cor = tabinterp1 (ST_TAB(st), "corrector", w))
		cor = INDEF

	    # Camera transmission.
	    iferr (cam = tabinterp1 (ST_TAB(st), "camera", w))
		cam = 1

	    # Vignetting.
	    iferr (vig = tabinterp1 (ST_TAB(st), "vignetting",
		st_w2x (st, 1, w)))
		vig = INDEF

	    # Detector DQE.
	    iferr (dqe = tabinterp1 (ST_TAB(st), "detector", w))
		dqe = 1

	    # Cumulative transmission.
	    thruput = 1
	    if (!IS_INDEF(tel)) {
		tel = max (0., tel)
		thruput = thruput * tel
	    }
	    if (!IS_INDEF(adc)) {
		adc = max (0., adc)
		thruput = thruput * adc
	    }
	    if (!IS_INDEF(ap)) {
		ap = max (0., ap)
		thruput = thruput * ap
	    }
	    if (!IS_INDEF(fib)) {
		fib = max (0., fib)
		thruput = thruput * fib
	    }
	    if (!IS_INDEF(inst)) {
		inst = max (0., inst)
		thruput = thruput * inst
	    }
	    if (!IS_INDEF(fltr1)) {
		fltr1 = max (0., fltr1)
		thruput = thruput * fltr1
	    }
	    if (!IS_INDEF(fltr2)) {
		fltr2 = max (0., fltr2)
		thruput = thruput * fltr2
	    }
	    if (!IS_INDEF(eff1)) {
		eff1 = max (0., eff1)
		thruput = thruput * eff1
	    }
	    if (!IS_INDEF(eff2)) {
		eff2 = max (0., eff2)
		thruput = thruput * eff2
	    }
	    if (!IS_INDEF(cor)) {
		cor = max (0., cor)
		thruput = thruput * cor
	    }
	    if (!IS_INDEF(col)) {
		col = max (0., col)
		thruput = thruput * col
	    }
	    if (!IS_INDEF(cam)) {
		cam = max (0., cam)
		thruput = thruput * cam
	    }
	    if (!IS_INDEF(vig)) {
		vig = max (0., vig)
		thruput = thruput * vig
	    }
	    if (!IS_INDEF(dqe)) {
		dqe = max (0., dqe)
		thruput = thruput * dqe
	    }

	    # Source photons detected.
	    nmag = flux / (C * H / w)
	    n = nmag * ST_AREA(st) * thruput * tobs * disp
	    if (!IS_INDEF(ext))
		n = n * ext
	    n = max (0., n)
	    if (n < 100000.)
		n = int (n)
	    nobj = nobj + n

	    # Sky photon flux.
	    iferr (sky = tabinterp2 (ST_TAB(st), "sky", w, ST_PHASE(st)))
		iferr (sky = tabinterp1 (ST_TAB(st), "sky", w))
		    sky = 0
	    sky = sky / (C * H / w)

	    # Thermal photon flux.
	    thermal = 0.
	    if (!IS_INDEF(ST_THERMAL(st))) {
		iferr {
		    thermal = tabinterp1 (ST_TAB(st), "emissivity", w)
		    # 1.41e24 = 2 * c * (arcsec/rad)^2 * (A/cm) * (A/cm)^-4
		    thermal = thermal * 1.41e24 / (w ** 4) /
			(exp (min (30., 1.43877e8 / (w * ST_THERMAL(st)))) - 1)
		} then
		    thermal = 0.
	    }

	    # Total background.
	    bkg = sky + thermal

	    switch (ST_APTYPE(st)) {
	    case CIRCULAR:
		bkg = bkg * PI * (ST_APSIZE(st,1) / 2) ** 2
	    case RECTANGULAR:
		bkg = bkg * ST_APSIZE(st,1) * ST_SCALE(st,1) * ST_NOBJPIX(st)
	    }
	    n = bkg * ST_AREA(st) * thruput / ap * tobs * disp
	    n = max (0., n)
	    if (n < 100000.)
		n = int (n)
	    nbkg = nbkg + n
	}

	# Dark counts.
	ndark = ST_NOBJPIX(st) * ST_DARK(st) * time
	ndark = max (0., ndark)
	if (ndark <100000.)
	    ndark = int (ndark)

	# Noise.
	dobj = sqrt (nobj)
	dbkg = sqrt (nbkg)
	ddark = sqrt (ndark)
	dreadout = sqrt (ST_NOBJPIX(st) * ST_RDNOISE(st)**2)
	tnoise = nobj + nbkg + ndark + dreadout**2

	# Background subtraction statistics.
	switch (ST_SKYSUB(st)) {
	case SKY_NONE:
	    nobj = nobj + nbkg
	    nbkg = 0.
	default:
	    if (ST_NSKYPIX(st) > 0)
		tnoise = tnoise + ((nbkg + ndark) / ST_NOBJPIX(st) +
		    ST_RDNOISE(st)**2) / ST_NSKYPIX(st)
	}

	# Final S/N.
	snr = nobj
	tnoise = sqrt (tnoise)
	if (tnoise > 0.)
	    snr = snr / tnoise

	if (fd == NULL)
	    return

	# Print results.
	sqnexp = sqrt (real(nexp))
	ncols = nint (ST_DISP(st,2) / ST_DISP(st,1))

	call un_ctranr (ST_DUNANG(st), ST_DUN(st), wave, w, 1)
	if (nexp > 1) {
	    call fprintf (fd,
	    "---- Results for %d exposures of %.2fs at %.4g %s ----\n")
		call pargi (nexp)
		call pargr (time)
		call pargr (w)
		call pargstr (ST_DUNITS(st))
	} else {
	    call fprintf (fd,
	    "---- Results for %.2fs exposure at %.4g %s ----\n")
		call pargr (time)
		call pargr (w)
		call pargstr (ST_DUNITS(st))
	}

	call fprintf (fd, "\nSource flux: %.3g photons/s/cm^2/A (AB=%.3g)\n")
	    call pargr (nmag)
	    call pargr (-2.5*log10(flux) - 5*log10(wave) - 2.40)
	call fprintf (fd, "Background flux: %.3g photons/s/cm^2/A (over source pixels)\n")
	    call pargr (bkg)

	call fprintf (fd, "\nTransmision/Efficiencies:%40t%10s %10s\n")
	    call pargstr ("individual")
	    call pargstr ("cumulative")
	cum = 1
	if (!IS_INDEF(ext) && ext < 1) {
	    cum = cum * ext
	    call fprintf (fd,
		"    Atmosphere (%.2g mag/airmass)%40t%9.1f%% %9.1f%%\n")
		call pargr (-2.5 * log10 (ext) / ST_AIRMASS(st))
		call pargr (100 * ext)
		call pargr (100 * cum)
	}
	if (!IS_INDEF(tel) && tel < 1) {
	    cum = cum * tel
	    call fprintf (fd, "    Telescope%40t%9.1f%% %9.1f%%\n")
		call pargr (100 * tel)
		call pargr (100 * cum)
	}
	if (!IS_INDEF(adc) && adc < 1) {
	    cum = cum * adc
	    call fprintf (fd, "    ADC%40t%9.1f%% %9.1f%%\n")
		call pargr (100 * adc)
		call pargr (100 * cum)
	}
	if (!IS_INDEF(ap) && ap < 1) {
	    cum = cum * ap
	    call fprintf (fd,
		"    Aperture (seeing=%.2g\")%40t%9.1f%% %9.1f%%\n")
		call pargr (ST_SEEING(st))
		call pargr (100 * ap)
		call pargr (100 * cum)
	}
	if (!IS_INDEF(fib) && fib < 1) {
	    cum = cum * fib
	    call fprintf (fd, "    Fiber%40t%9.1f%% %9.1f%%\n")
		call pargr (100 * fib)
		call pargr (100 * cum)
	}
	if (!IS_INDEF(fltr1) && fltr1 < 1) {
	    cum = cum * fltr1
	    call fprintf (fd, "    Filter%40t%9.1f%% %9.1f%%\n")
		call pargr (100 * fltr1)
		call pargr (100 * cum)
	}
	if (!IS_INDEF(fltr2) && fltr2 < 1) {
	    cum = cum * fltr2
	    call fprintf (fd, "    Filter%40t%9.1f%% %9.1f%%\n")
		call pargr (100 * fltr2)
		call pargr (100 * cum)
	}
	if (!IS_INDEF(inst) && inst < 1) {
	    cum = cum * inst
	    call fprintf (fd, "    Spectrograph%40t%9.1f%% %9.1f%%\n")
		call pargr (100 * inst)
		call pargr (100 * cum)
	}
	if (!IS_INDEF(col) && col < 1) {
	    cum = cum * col
	    call fprintf (fd, "    Collimator%40t%9.1f%% %9.1f%%\n")
		call pargr (100 * col)
		call pargr (100 * cum)
	}
	if (!IS_INDEF(eff1) && eff1 < 1) {
	    cum = cum * eff1
	    call fprintf (fd, "    Disperser%40t%9.1f%% %9.1f%%\n")
		call pargr (100 * eff1)
		call pargr (100 * cum)
	}
	if (!IS_INDEF(eff2) && eff2 < 1) {
	    cum = cum * eff2
	    call fprintf (fd, "    Cross disperser%40t%9.1f%% %9.1f%%\n")
		call pargr (100 * eff2)
		call pargr (100 * cum)
	}
	if (!IS_INDEF(cor) && cor < 1) {
	    cum = cum * cor
	    call fprintf (fd, "    Corrector%40t%9.1f%% %9.1f%%\n")
		call pargr (100 * cor)
		call pargr (100 * cum)
	}
	if (!IS_INDEF(cam) && cam < 1) {
	    cum = cum * cam
	    call fprintf (fd, "    Camera%40t%9.1f%% %9.1f%%\n")
		call pargr (100 * cam)
		call pargr (100 * cum)
	}
	if (!IS_INDEF(vig) && vig < 0.999) {
	    cum = cum * vig
	    call fprintf (fd, "    Vignetting%40t%9.1f%% %9.1f%%\n")
		call pargr (100 * vig)
		call pargr (100 * vig)
	}
	if (!IS_INDEF(dqe) && dqe < 1) {
	    cum = cum * dqe
	    call fprintf (fd, "    Detector DQE%40t%9.1f%% %9.1f%%\n")
		call pargr (100 * dqe)
		call pargr (100 * cum)
	}

	call fprintf (fd, "\nStatistics per exposure per pixel:%40t%10s %10s\n")
	    call pargstr ("e-")
	    call pargstr ("sigma")
	call fprintf (fd, "    Source%40t%10d %10.1f\n")
	    call pargr (nobj)
	    call pargr (dobj)
	if (nbkg > 0.) {
	    call fprintf (fd, "    Background%40t%10d %10.1f\n")
		call pargr (nbkg)
		call pargr (dbkg)
	}
	call fprintf (fd, "    Dark%40t%10d %10.1f\n")
	    call pargr (ndark)
	    call pargr (ddark)
	call fprintf (fd, "    Readout%40t%10w %10.1f\n")
	    call pargr (dreadout)
	call fprintf (fd, "    Net%40t%10d %10.1f\n")
	    call pargr (nobj + nbkg + ndark)
	    call pargr (tnoise)

	call fprintf (fd, "\nSignal-to-Noise Statistics:\n")
	call fprintf (fd, "  %3d%7tper exposure per pixel\n")
	    call pargr (snr)
	    call pargr (ST_DISP(st,1))
	call un_ctranr (ST_DUNANG(st), ST_DUN(st), ST_DISP(st,2), w, 1)
	call fprintf (fd,
	    "  %3d%7tper exposure per %.3g %s (%d pixel) resolution element\n")
	    call pargr (snr * sqrt (real (ncols)))
	    call pargr (w)
	    call pargstr (ST_DUNITS(st))
	    call pargi (ncols)
	if (nexp > 1.) {
	    call fprintf (fd,
		"  %3d%10tper %d exposures per pixel\n")
		call pargr (snr * sqnexp)
		call pargi (nexp)
		call pargr (ST_DISP(st,1))
	    call un_ctranr (ST_DUNANG(st), ST_DUN(st), ST_DISP(st,2), w, 1)
	    call fprintf (fd,
	"  %3d%10tper %d exposures per %.3g %s (%d pixel) resolution element\n")
		call pargr (snr * sqrt (real (ncols)) * sqnexp)
		call pargi (nexp)
		call pargr (w)
		call pargstr (ST_DUNITS(st))
		call pargi (ncols)
	}

	if (nexp > 1) {
	    call fprintf (fd,
		"\nExposure time: %d exposures of %.2fs")
		call pargi (nexp)
		call pargr (time)
	    if (nexp * time > 60.) {
		call fprintf (fd, " (%.1h)")
		    call pargr (nexp * time / 3600.)
	    } else {
		call fprintf (fd, " (%.1f)")
		call pargr (nexp * time)
	    }
	} else {
	    call fprintf (fd, "\nExposure time: %.2fs")
		call pargr (time)
	    if (time > 60.) {
		call fprintf (fd, " (%.1h)")
		    call pargr (time / 3600.)
	    }
	}
	if (ST_SKYSUB(st) == SKY_SHUFFLE)
	    call fprintf (fd, " (shuffled)\n")
	else
	    call fprintf (fd, "\n")
	call fprintf (fd, "\n")
end


# ST_CHECK -- Check for possible errors such as lack of proper order blocking.

procedure st_check (st, fd, waves, npix)

pointer st		#I SPECTIME structure
int	fd		#I Output file descriptor
real	waves[ARB]	#I Wavelengths
int	npix		#I Number of pixels

int	i, n, step
real	nobj[4], nsky[4]
real	w1, w2, w, dw, snr, thruput, sat, dnmax, maxcount, maxfrac
pointer	tab
real	stgetr(), tabgetr(), gr_getr()

begin
	tab = ST_TAB(st)

	# Check for extrapolations.  This has to be done before the order
	# overlap because that may reference wavelength which are extrapolated.

	ifnoerr (w1 = tabgetr (tab, "spectrum", "", "table.xmin")) {
	    w2 = tabgetr (tab, "spectrum", "", "table.xmax")
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		    "WARNING: Spectrum table extrapolated in wavelength\n")
	}
	ifnoerr (w1 = tabgetr (tab, "sky", "", "table.xmin")) {
	    w2 = tabgetr (tab, "sky", "", "table.xmax")
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		    "WARNING: Sky table extrapolated in wavelength\n")
	    ifnoerr (w1 = tabgetr (tab, "sky", "", "table.ymin")) {
		w2 = tabgetr (tab, "sky", "", "table.ymax")
		if (w1 > ST_PHASE(st) || w2 < ST_PHASE(st))
		    call fprintf (fd,
			"WARNING: Sky table extrapolated in lunar phase\n")
	    }
	}
	ifnoerr (w1 = tabgetr (tab, "sensfunc", "", "table.xmin")) {
	    w2 = tabgetr (tab, "sensfunc", "", "table.xmax")
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		    "WARNING: Sensitivity table extrapolated in wavelength\n")
	}
	ifnoerr (w1 = tabgetr (tab, "extinction", "", "table.xmin")) {
	    w2 = tabgetr (tab, "extinction", "", "table.xmax")
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		    "WARNING: Extinction table extrapolated in wavelength\n")
	}
	ifnoerr (w1 = tabgetr (tab, "telescope", "", "table.xmin")) {
	    w2 = tabgetr (tab, "telescope", "", "table.xmax")
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		    "WARNING: Telescope table extrapolated in wavelength\n")
	}
	ifnoerr (w1 = tabgetr (tab, "adc", "", "table.xmin")) {
	    w2 = tabgetr (tab, "adc", "", "table.xmax")
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		    "WARNING: ADC table extrapolated in wavelength\n")
	}
	ifnoerr (w1 = tabgetr (tab, "filter", "", "table.xmin")) {
	    w2 = tabgetr (tab, "filter", "", "table.xmax")
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		    "WARNING: Filter table extrapolated in wavelength\n")
	}
	ifnoerr (w1 = tabgetr (tab, "filter2", "", "table.xmin")) {
	    w2 = tabgetr (tab, "filter2", "", "table.xmax")
	    if (w1 > waves[1] || w2 < waves[npix])
	    call fprintf (fd,
		"WARNING: Second filter table extrapolated in wavelength\n")
	}
	ifnoerr (w1 = tabgetr (tab, "fiber", "", "table.xmin")) {
	    w2 = tabgetr (tab, "fiber", "", "table.xmax")
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		    "WARNING: Fiber table extrapolated in wavelength\n")
	}
	ifnoerr (w1 = tabgetr (tab, "collimator", "", "table.xmin")) {
	    w2 = tabgetr (tab, "collimator", "", "table.xmax")
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		    "WARNING: Collimator table extrapolated in wavelength\n")
	}
	ifnoerr (w1 = tabgetr (tab,"disperser","","table.xmin")/ST_ORDER(st,1)) {
	    w2 = tabgetr (tab, "disperser", "", "table.xmax") / ST_ORDER(st,1)
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		    "WARNING: Disperser table extrapolated in wavelength\n")
	    ifnoerr (w1 = tabgetr (tab, "disperser", "", "table.ymin")) {
		w2 = tabgetr (tab, "disperser", "", "table.ymax")
		if (w1 > ST_ORDER(st,1) || w2 < ST_ORDER(st,1))
		    call fprintf (fd,
			"WARNING: Disperser table extrapolated in order\n")
	    }
	}
	ifnoerr (w1 = tabgetr (tab,"xdisperser", "","table.xmin")/ST_ORDER(st,2)) {
	    w2 = tabgetr (tab, "xdisperser", "", "table.xmax") / ST_ORDER(st,2)
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		"WARNING: Cross disperser table extrapolated in wavelength\n")
	    ifnoerr (w1 = tabgetr (tab, "xdisperser", "", "table.ymin")) {
		w2 = tabgetr (tab, "xdisperser", "", "table.ymax")
		if (w1 > ST_ORDER(st,2) || w2 < ST_ORDER(st,2))
		    call fprintf (fd,
		    "WARNING: Cross disperser table extrapolated in order\n")
	    }
	}
	ifnoerr (w1 = tabgetr (tab, "corrector", "", "table.xmin")) {
	    w2 = tabgetr (tab, "corrector", "", "table.xmax")
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		    "WARNING: Corrector table extrapolated in wavelength\n")
	}
	ifnoerr (w1 = tabgetr (tab, "camera", "", "table.xmin")) {
	    w2 = tabgetr (tab, "camera", "", "table.xmax")
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		    "WARNING: Camera table extrapolated in wavelength\n")
	}
	ifnoerr (w1 = tabgetr (tab, "detector", "", "table.xmin")) {
	    w2 = tabgetr (tab, "detector", "", "table.xmax")
	    if (w1 > waves[1] || w2 < waves[npix])
		call fprintf (fd,
		    "WARNING: Detector table extrapolated in wavelength\n")
	}

	# Check for saturation and DN limits.
	sat = stgetr (st, "saturation", "detector", MAX_REAL)
	dnmax = stgetr (st, "dnmax", "detector", MAX_REAL)

	# Check for insufficient sky pixels.
	switch (ST_SKYSUB(st)) {
	case SKY_LONGSLIT:
	    if (ST_NSKYPIX(st) <= 0)
		call fprintf (fd,
		    "WARNING: Slit is too short for sky subtraction\n")
	}

	# Check for order overlaps and saturation.
	if (npix > 3) {
	    step = max (1, npix / 21)
	    maxfrac = 0.
	    maxcount = 0
	    do i = 3*step, npix-3*step, step {
		w = waves[i]
		call st_snr (st, NULL, w, ST_NEXP(st), ST_TIME(st), nobj, nsky,
		    snr, thruput)
		maxcount = max (nobj[1]+nsky[3], maxcount)
		if (nobj[1] > 0.) {
		    maxfrac = max (nobj[2]/nobj[1], maxfrac)
		    maxfrac = max (nobj[4]/nobj[1], maxfrac)
		}
	    }
	} else {
	    w = waves[1]
	    call st_snr (st, NULL, w, ST_NEXP(st), ST_TIME(st), nobj, nsky,
		snr, thruput)
	    maxcount = max (nobj[1]+nsky[3], maxcount)
	    if (nobj[1] > 0.) {
		maxfrac = max (nobj[2]/nobj[1], maxfrac)
		maxfrac = max (nobj[4]/nobj[1], maxfrac)
	    }
	}

	if (maxcount > sat)
	    call fprintf (fd, "WARNING: Exposure may saturate.\n")
	if (maxcount / ST_GAIN(st) > dnmax)
	    call fprintf (fd, "WARNING: Exposure may overflow DN maximum.\n")
	if (maxfrac > 0.1)
	    call fprintf (fd,
		"WARNING: More than 10%% contribution from other orders.\n")

	if (ST_DISPTYPE(st,2) != 0 && !IS_INDEFI(ST_ORDER(st,1))) {
	    w = ST_CW(st)
	    i = ST_ORDER(st,1)
	    dw = abs (gr_getr (ST_GR(st,2), "dispersion"))
	    dw = dw * ST_PIXSIZE(st) * ST_BIN(st,2)
	    n = w * (1 - real (i) / real (i + 1)) / dw
	    if (n < ST_APSIZE(st,2) / ST_SCALE(st,2)) 
		call fprintf (fd, "WARNING: Orders overlap\n")
	}

	call fprintf (fd, "\n")
end


# ST_GTABLE -- Get table name from CL and load if a name is given.
# Otherwise get table name from another table, if specified, and load if
# a name is found.

procedure st_gtable (st, name, table)

pointer st		#I SPECTIME structure
char	name[ARB]	#I Table to load (CL parameter name and table name)
char	table[ARB]	#I Table to use if not defined by CL parameter

pointer sp, dir, path, fname
int	i, nowhite(), strdic()
bool	streq()
errchk	st_gtable1

define	done_	10

begin
	call smark (sp)
	call salloc (dir, SZ_FNAME, TY_CHAR)
	call salloc (path, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Determine table name from CL parameter or table.
	call stgstr (st, name, table, "", Memc[fname], SZ_FNAME)
	i = nowhite (Memc[fname], Memc[fname], SZ_FNAME)

	# Special cases.
	if (streq (Memc[fname], "none"))
	    goto done_
	if (streq (name, "spectrum")) {
	    ST_SPEC(st) = strdic (Memc[fname], Memc[path], SZ_FNAME, SPECTYPES)
	    if (ST_SPEC(st) != SPEC_TAB && streq (Memc[fname], Memc[path]))
		goto done_
	    ST_SPEC(st) = SPEC_TAB
	}

	# If a filename is given find it and load it.
	if (Memc[fname] != EOS)
	    call st_gtable1 (st, name, Memc[fname])

done_	call sfree (sp)
end


# ST_GTABLE1 -- Load table with search path.
# Otherwise get table name from another table, if specified, and load if
# a name is found.

procedure st_gtable1 (st, name, fname)

pointer st		#I SPECTIME structure
char	name[ARB]	#I Table name
char	fname[ARB]	#I File

pointer sp, dir, path
real	value
int	i, ctor(), strlen(), clgfil(), access()
errchk	tabload

define	done_	10

begin
	call smark (sp)
	call salloc (dir, SZ_FNAME, TY_CHAR)
	call salloc (path, SZ_FNAME, TY_CHAR)

	# Check for constant value.
	i = 1
	if (ctor (fname, i, value) == strlen (fname)) {
	    call tabload (ST_TAB(st), name, fname)
	    goto done_
	}

	# Absolute path or current directory.
	if (access (fname, READ_ONLY, 0) == YES) {
	    call tabload (ST_TAB(st), name, fname)
	    goto done_
	}

	# Search directories.
	call clprew (ST_SEARCH(st))
	while (clgfil (ST_SEARCH(st), Memc[dir], SZ_FNAME) != EOF) {
	    call sprintf (Memc[path], SZ_FNAME, "%s/%s")
		call pargstr (Memc[dir])
		call pargstr (fname)
	    if (access (Memc[path], READ_ONLY, 0) == NO)
		next
	    call tabload (ST_TAB(st), name, Memc[path])
	    goto done_
	}

	# Search sptimelib$.
	call sprintf (Memc[path], SZ_FNAME, "%s/%s")
	    call pargstr ("sptimelib$")
	    call pargstr (fname)
	if (access (Memc[path], READ_ONLY, 0) == YES) {
	    call tabload (ST_TAB(st), name, Memc[path])
	    goto done_
	}

	call sprintf (Memc[path], SZ_FNAME, "Table `%s' not found")
	    call pargstr (fname)
	call error (1, Memc[path])

done_	call sfree (sp)
end


# ST_SPECTRUM -- Return spectrum flux.

real procedure st_spectrum (st, wave)

pointer st		#I SPECTIME pointer
real	wave		#I Wavelength
real	flux		#O Flux

real	tabinterp1(), ccm()
errchk	tabinterp1, ccm

begin
	switch (ST_SPEC(st)) {
	case SPEC_TAB:
	    flux = tabinterp1 (ST_TAB(st), "spectrum", wave)
	case SPEC_BB:
	    flux = (exp (min (30., 1.4338e8/(ST_REFW(st)*ST_PARAM(st)))) - 1) /
		(exp (min (30., 1.4338e8/(wave*ST_PARAM(st)))) - 1)
	    flux = ST_REFFL(st) * (ST_REFW(st) / wave) ** 5 * flux
	case SPEC_FL:
	    flux = ST_REFFL(st) * (wave/ST_REFW(st)) ** ST_PARAM(st)
	case SPEC_FN:
	    flux = ST_REFFL(st) * (wave/ST_REFW(st)) ** (-2.-ST_PARAM(st))
	}
	flux = flux * 10. ** (0.4 * ST_AV(st) *
	    (ccm (ST_REFW(st), ST_RV(st)) - ccm (wave, ST_RV(st))))

	return (flux)
end


# ST_OUTPUT -- Output graphs and/or lists.

procedure st_output (st, gp, fd, interactive, output, w, npts)

pointer st			#I SPECTIME structure
pointer gp			#U GIO pointer
int	fd			#I FIO pointer
bool	interactive		#I Interactive?
char	output[ARB]		#I Output type
real	w[npts]			#I Wavelengths
int	npts			#I Number of points

int	i, j, ndata, type
real	nobj[4], nsky[4]
real	wx1, wx2, wy1, wy2, wmin, wmax, a, b, buf, f, snr, thruput, airmass
pointer sp, spec, name
pointer	title, xlabel, ylabel, id[4], xdata, ydata, str, tab

bool	tabexists()
int	strdic(), clgcur()
real	st_dispeff(), tabinterp1(), tabinterp2(), st_spectrum(), tabgetr()
errchk	st_snr, st_dispeff, tabinterp1, tabinterp2, st_spectrum

begin
	if (gp == NULL && fd == NULL)
	    return

	call smark (sp)
	call salloc (spec, SZ_LINE, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (xlabel, SZ_LINE, TY_CHAR)
	call salloc (ylabel, SZ_LINE, TY_CHAR)
	call salloc (id[1], SZ_LINE, TY_CHAR)
	call salloc (id[2], SZ_LINE, TY_CHAR)
	call salloc (id[3], SZ_LINE, TY_CHAR)
	call salloc (id[4], SZ_LINE, TY_CHAR)
	call salloc (xdata, npts, TY_REAL)
	call salloc (ydata, 4*npts, TY_REAL)
	call salloc (str, SZ_LINE, TY_CHAR)

	tab = ST_TAB(st)

	# Set title, data, and data limits.
	call st_gtitle (st, "title", "spectrograph", Memc[title], SZ_LINE)
	call st_gtitle (st, "spectitle", "spectrum", Memc[spec], SZ_LINE)
	if (Memc[spec] == EOS) {
	    switch (ST_SPEC(st)) {
	    case SPEC_BB:
		call sprintf (Memc[spec], SZ_LINE,
		    "Blackbody spectrum of temperature %g K")
		    call pargr (ST_PARAM(st))
	    case SPEC_FL:
		call sprintf (Memc[spec], SZ_LINE,
		    "F(lambda) power law spectrum of index %g")
		    call pargr (ST_PARAM(st))
	    case SPEC_FN:
		call sprintf (Memc[spec], SZ_LINE,
		    "F(nu) power law spectrum of index %g")
		    call pargr (ST_PARAM(st))
	    }
	}

	call sprintf (Memc[xlabel], SZ_LINE, "Dispersion (%s)")
	    call pargstr (ST_DUNITS(st))

	ndata = npts
	#call amovr (w, Memr[xdata], ndata)
	call un_ctranr (ST_DUNANG(st), ST_DUN(st), w, Memr[xdata], ndata)
	Memr[ydata] = INDEF
	Memr[ydata+npts] = INDEF
	Memr[ydata+2*npts] = INDEF
	Memr[ydata+3*npts] = INDEF
	#wx1 = INDEF; wx2 = INDEF; wy1 = -4.; wy2 = 104.
	wx1 = INDEF; wx2 = INDEF; wy1 = INDEF; wy2 = INDEF

	type = strdic (output, Memc[name], SZ_LINE, OUTTYPES)
	switch (type) {
	case OUT_RATE:
	    if (Memc[title] != EOS)
		call strcat ("\n", Memc[title], SZ_LINE)
	    call strcat (Memc[spec], Memc[title], SZ_LINE)
	    call strcpy ("Photons/s/A", Memc[ylabel], SZ_LINE)
	    call strcpy ("Object (ph/s/A)", Memc[id[1]], SZ_LINE) 
	    call strcpy ("Background (ph/s/A)", Memc[id[2]], SZ_LINE) 
	    call strcpy ("Total (ph/s/A)", Memc[id[3]], SZ_LINE) 

	    do i = 1, npts {
		call st_snr (st, NULL, w[i], ST_NEXP(st), ST_TIME(st),
		    nobj, nsky, snr, thruput)
		Memr[ydata+i-1] = nobj[1] / ST_TIME(st) / ST_DISP(st,1)
		Memr[ydata+npts+i-1] = nsky[1] / ST_TIME(st) / ST_DISP(st,1)
		Memr[ydata+2*npts+i-1] = (nobj[1]+nsky[1]) /
		    ST_TIME(st) / ST_DISP(st,1)
	    }
	    call alimr (Memr[ydata+npts], npts, a, b)
	    if (b <= 0.) {
		Memr[ydata+npts] = INDEF
		Memr[ydata+2*npts] = INDEF
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call sprintf (Memc[str], SZ_LINE, "Object photon rates\n")
		call strcat (Memc[str], Memc[title], SZ_LINE)
	    } else {
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call sprintf (Memc[str], SZ_LINE,
		    "Object, background, and total photon rates\n")
		call strcat (Memc[str], Memc[title], SZ_LINE)
	    }

	    wy1 = INDEF; wy2 = INDEF
	case OUT_OBJ:
	    if (Memc[title] != EOS)
		call strcat ("\n", Memc[title], SZ_LINE)
	    call strcat (Memc[spec], Memc[title], SZ_LINE)
	    call sprintf (Memc[str], SZ_LINE, "\nExposure time = %d seconds")
		call pargr (ST_NEXP(st) * max (ST_TIME(st),1.))
	    call strcat (Memc[str], Memc[title], SZ_LINE)
	    if (ST_NEXP(st) > 1) {
		call sprintf (Memc[str], SZ_LINE, "(%d exposures)")
		    call pargi (ST_NEXP(st))
		call strcat (Memc[str], Memc[title], SZ_LINE)
	    }
	    call strcpy ("Object DN", Memc[ylabel], SZ_LINE)
	    call strcpy ("Total Object (counts)", Memc[id[1]], SZ_LINE) 
	    call sprintf (Memc[id[2]], SZ_LINE, "Order %d (counts)")
		call pargi (ST_ORDER(st,1)-1)
	    call sprintf (Memc[id[3]], SZ_LINE, "Order %d (counts)")
		call pargi (ST_ORDER(st,1))
	    call sprintf (Memc[id[4]], SZ_LINE, "Order %d (counts)")
		call pargi (ST_ORDER(st,1)+1)

	    do i = 1, npts {
		call st_snr (st, NULL, w[i], 1, ST_NEXP(st) * ST_TIME(st),
		    nobj, nsky, snr, thruput)
		Memr[ydata+i-1] = nobj[1] / ST_GAIN(st)
		Memr[ydata+npts+i-1] = nobj[2] / ST_GAIN(st)
		Memr[ydata+2*npts+i-1] = nobj[3] / ST_GAIN(st)
		Memr[ydata+3*npts+i-1] = nobj[4] / ST_GAIN(st)
	    }
	    call alimr (Memr[ydata+npts], npts, a, b)
	    if (b <= 0.)
		Memr[ydata+npts] = INDEF
	    call alimr (Memr[ydata+3*npts], npts, a, b)
	    if (b <= 0.)
		Memr[ydata+3*npts] = INDEF
	    if (IS_INDEF(Memr[ydata+npts]) && IS_INDEF(Memr[ydata+3*npts])) {
		Memr[ydata+2*npts] = INDEF
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call sprintf (Memc[str], SZ_LINE,
		    "Object DN at gain of %.2g\n")
		    call pargr (ST_GAIN(st))
		call strcat (Memc[str], Memc[title], SZ_LINE)
	    } else {
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call sprintf (Memc[str], SZ_LINE,
		    "Object DN at gain of %.2g with order contributions\n")
		    call pargr (ST_GAIN(st))
		call strcat (Memc[str], Memc[title], SZ_LINE)
	    }

	    wy1 = INDEF; wy2 = INDEF
	case OUT_SNR:
	    if (Memc[title] != EOS)
		call strcat ("\n", Memc[title], SZ_LINE)
	    call strcat (Memc[spec], Memc[title], SZ_LINE)
	    call sprintf (Memc[str], SZ_LINE, "\nExposure time = %d seconds")
		call pargr (ST_NEXP(st) * max(ST_TIME(st),1.))
	    call strcat (Memc[str], Memc[title], SZ_LINE)
	    if (ST_NEXP(st) > 1) {
		call sprintf (Memc[str], SZ_LINE, "(%d exposures)")
		    call pargi (ST_NEXP(st))
		call strcat (Memc[str], Memc[title], SZ_LINE)
	    }
	    call strcpy ("S/N", Memc[ylabel], SZ_LINE)
	    call strcpy ("S/N", Memc[id[1]], SZ_LINE) 

	    a = sqrt (real(ST_NEXP(st)))
	    do i = 1, npts {
		call st_snr (st, NULL, w[i], ST_NEXP(st), ST_TIME(st),
		    nobj, nsky, snr, thruput)
		Memr[ydata+i-1] = a * snr
	    }
	    wy1 = INDEF; wy2 = INDEF
	case OUT_ATM:
	    call st_gtitle (st, "exttitle", "extinction", Memc[str], SZ_LINE)
	    if (Memc[str] == EOS)
		call strcpy ("Extinction", Memc[str], SZ_LINE)
	    if (Memc[title] != EOS)
		call strcat ("\n", Memc[title], SZ_LINE)
	    call strcat (Memc[str], Memc[title], SZ_LINE)
	    call sprintf (Memc[str], SZ_LINE, "\nAirmass of %g")
		call pargr (ST_AIRMASS(st))
	    call strcat (Memc[str], Memc[title], SZ_LINE)
	    call strcpy ("Transmission (%)", Memc[ylabel], SZ_LINE)
	    call strcpy ("ATM Transmission (%)", Memc[id[1]], SZ_LINE) 

	    iferr  {
		do i = 1, npts {
		    f = tabinterp1 (tab, "extinction", w[i])
		    Memr[ydata+i-1] = 100 * 10 ** (-0.4 * f * ST_AIRMASS(st))
		}
	    } then
		call amovkr (100., Memr[ydata], npts)
	case OUT_TEL:
	    call st_gtitle (st, "teltitle", "telescope", Memc[str], SZ_LINE)
	    if (Memc[str] == EOS)
		call strcpy ("Telescope", Memc[str], SZ_LINE)
	    if (Memc[title] != EOS)
		call strcat ("\n", Memc[title], SZ_LINE)
	    call strcat (Memc[str], Memc[title], SZ_LINE)
	    call strcpy ("Transmission (%)", Memc[ylabel], SZ_LINE)
	    call strcpy ("Telescope Transmission (%)", Memc[id[1]], SZ_LINE) 

	    iferr  {
		do i = 1, npts
		    Memr[ydata+i-1] = 100 *
			tabinterp1 (tab, Memc[name], w[i])
	    } then
		call amovkr (100., Memr[ydata], npts)
	case OUT_AP:
	    call st_gtitle (st, "aptitle", "aperture", Memc[str], SZ_LINE)
	    if (Memc[str] == EOS)
		call strcpy ("Aperture", Memc[str], SZ_LINE)
	    if (Memc[title] != EOS)
		call strcat ("\n", Memc[title], SZ_LINE)
	    call strcat (Memc[str], Memc[title], SZ_LINE)
	    call sprintf (Memc[str], SZ_LINE, "\nSeeing of %.2f\"")
		call pargr (ST_SEEING(st))
	    call strcat (Memc[str], Memc[title], SZ_LINE)
	    call strcpy ("Transmission (%)", Memc[ylabel], SZ_LINE)
	    call strcpy ("Aperture (%)", Memc[id[1]], SZ_LINE) 

	    iferr  {
		switch (ST_APTYPE(st)) {
		case CIRCULAR:
		    f = 100 * tabinterp1 (tab, Memc[name],
			ST_APSIZE(st,1) / ST_SEEING(st))
		case RECTANGULAR:
		    f = 100 * tabinterp2 (tab, Memc[name],
			ST_APSIZE(st,1) / ST_SEEING(st),
			ST_APSIZE(st,2) / ST_SEEING(st))
		}
	    } then
		f = 100
	    call amovkr (f, Memr[ydata], npts)
	case OUT_FIB:
	    if (tabexists (tab, Memc[name])) {
		call st_gtitle (st, "fibtitle", "fiber", Memc[str], SZ_LINE)
		if (Memc[str] == EOS)
		    call strcpy ("Fiber", Memc[str], SZ_FNAME)
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call strcat (Memc[str], Memc[title], SZ_LINE)
		call strcpy ("Transmission (%)", Memc[ylabel], SZ_LINE)

		iferr  {
		    do i = 1, npts
			Memr[ydata+i-1] = 100 *
			    tabinterp1 (tab, Memc[name], w[i])
		} then
		    call amovkr (100., Memr[ydata], npts)
	    }
	case OUT_FILT, OUT_FILT2:
	    if (tabexists (tab, Memc[name])) {
		if (type == OUT_FILT)
		    call st_gtitle (st, "ftitle", "filter", Memc[str], SZ_LINE)
		else
		    call st_gtitle (st, "f2title", "filter2", Memc[str],SZ_LINE)
		if (Memc[str] == EOS)
		    call strcpy ("Filter", Memc[str], SZ_LINE)
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call strcat (Memc[str], Memc[title], SZ_LINE)
		call strcpy ("Transmission (%)", Memc[ylabel], SZ_LINE)
		call strcpy ("Fiber (%)", Memc[id[1]], SZ_LINE) 

		iferr  {
		    do i = 1, npts
			Memr[ydata+i-1] = 100 *
			    tabinterp1 (tab, Memc[name], w[i])
		} then
		    call amovkr (100., Memr[ydata], npts)
	    }
	case OUT_COL:
	    call st_gtitle (st, "coltitle", "collimator", Memc[str], SZ_LINE)
	    if (Memc[str] == EOS)
		call strcpy ("Collimator", Memc[str], SZ_LINE)
	    if (Memc[title] != EOS)
		call strcat ("\n", Memc[title], SZ_LINE)
	    call strcat (Memc[str], Memc[title], SZ_LINE)
	    call strcpy ("Transmission (%)", Memc[ylabel], SZ_LINE)
	    call strcpy ("Collimator (%)", Memc[id[1]], SZ_LINE) 

	    iferr  {
		do i = 1, npts
		    Memr[ydata+i-1] = 100 *
			tabinterp1 (tab, Memc[name], w[i])
	    } then
		call amovkr (100., Memr[ydata], npts)
	case OUT_DISP:
	    if (ST_DISPTYPE(st,1) != 0) {
		call st_gtitle (st, "disptitle", "disperser", Memc[str],SZ_LINE)
		if (Memc[str] == EOS)
		    call strcpy ("Disperser", Memc[str], SZ_LINE)
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call strcat (Memc[str], Memc[title], SZ_LINE)
		call strcpy ("Efficiency (%)", Memc[ylabel], SZ_LINE)
		call strcpy ("Disperser (%)", Memc[id[1]], SZ_LINE) 

		do i = 1, npts
		    Memr[ydata+i-1] = 100 * st_dispeff (st, Memc[name], w[i],
			ST_ORDER(st,1))
	    }
	case OUT_XDISP:
	    if (ST_DISPTYPE(st,2) != 0) {
		call st_gtitle (st, "xdisptitle","xdisperser",Memc[str],SZ_LINE)
		if (Memc[str] == EOS)
		    call strcpy ("Crossdisperser", Memc[str], SZ_LINE)
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call strcat (Memc[str], Memc[title], SZ_LINE)
		call strcpy ("Efficiency (%)", Memc[ylabel], SZ_LINE)
		call strcpy ("Cross-disperser (%)", Memc[id[1]], SZ_LINE) 

		do i = 1, npts
		    Memr[ydata+i-1] = 100 * st_dispeff (st, Memc[name], w[i],
			ST_ORDER(st,2))
	    }
	case OUT_COR:
	    if (tabexists (tab, Memc[name])) {
		call st_gtitle (st, "cortitle", "corrector", Memc[str], SZ_LINE)
		if (Memc[str] == EOS)
		    call strcpy ("Corrector", Memc[str], SZ_LINE)
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call strcat (Memc[str], Memc[title], SZ_LINE)
		call strcpy ("Transmission (%)", Memc[ylabel], SZ_LINE)
		call strcpy ("Corrector (%)", Memc[id[1]], SZ_LINE) 

		iferr  {
		    do i = 1, npts
			Memr[ydata+i-1] = 100 *
			    tabinterp1 (tab, Memc[name], w[i])
		} then
		    call amovkr (100., Memr[ydata], npts)
	    }
	case OUT_CAM:
	    call st_gtitle (st, "camtitle", "camera", Memc[str], SZ_LINE)
	    if (Memc[str] == EOS)
		call strcpy ("Camera", Memc[str], SZ_LINE)
	    if (Memc[title] != EOS)
		call strcat ("\n", Memc[title], SZ_LINE)
	    call strcat (Memc[str], Memc[title], SZ_LINE)
	    call strcpy ("Transmission (%)", Memc[ylabel], SZ_LINE)
	    call strcpy ("Camera (%)", Memc[id[1]], SZ_LINE) 

	    iferr  {
		do i = 1, npts
		    Memr[ydata+i-1] = 100 *
			tabinterp1 (tab, Memc[name], w[i])
	    } then
		call amovkr (100., Memr[ydata], npts)
	case OUT_DET:
	    call st_gtitle (st, "dettitle", "detector", Memc[str], SZ_LINE)
	    if (Memc[str] == EOS)
		call strcpy ("Detector", Memc[str], SZ_LINE)
	    if (Memc[title] != EOS)
		call strcat ("\n", Memc[title], SZ_LINE)
	    call strcat (Memc[str], Memc[title], SZ_LINE)
	    call strcpy ("DQE (%)", Memc[ylabel], SZ_LINE)
	    call strcpy ("DQE (%)", Memc[id[1]], SZ_LINE) 

	    iferr  {
		do i = 1, npts
		    Memr[ydata+i-1] = 100 *
			tabinterp1 (tab, Memc[name], w[i])
	    } then
		call amovkr (100., Memr[ydata], npts)
	case OUT_SPEC:
	    call strcpy ("Spectrograph Other", Memc[str], SZ_LINE)
	    if (Memc[title] != EOS)
		call strcat ("\n", Memc[title], SZ_LINE)
	    call strcat (Memc[str], Memc[title], SZ_LINE)
	    call strcpy ("Transmission (%)", Memc[ylabel], SZ_LINE)
	    call strcpy ("Spectrograph Other (%s)", Memc[id[1]], SZ_LINE) 

	    iferr  {
		do i = 1, npts
		    Memr[ydata+i-1] = 100 *
			tabinterp1 (tab, Memc[name], w[i])
	    } then
		Memr[ydata] = INDEF
	case OUT_ADC:
	    if (tabexists (tab, Memc[name])) {
		call st_gtitle (st, "adctitle", "adc", Memc[str], SZ_LINE)
		if (Memc[str] == EOS)
		    call strcpy ("ADC", Memc[str], SZ_LINE)
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call strcat (Memc[str], Memc[title], SZ_LINE)
		call strcpy ("Transmission (%)", Memc[ylabel], SZ_LINE)
		call strcpy ("ADC (%)", Memc[id[1]], SZ_LINE) 

		iferr  {
		    do i = 1, npts
			Memr[ydata+i-1] = 100 *
			    tabinterp1 (tab, Memc[name], w[i])
		} then
		    call amovkr (100., Memr[ydata], npts)
	    }
	case OUT_EMIS:
	    if (tabexists (tab, Memc[name]) && ST_THERMAL(st) > 0.) {
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call st_gtitle (st, "emistitle", "emissivity",Memc[str],SZ_LINE)
		if (Memc[str] == EOS)
		    call strcpy ("Emissivity", Memc[str], SZ_LINE)
		call strcat (Memc[str], Memc[title], SZ_LINE)
		call sprintf (Memc[str], SZ_LINE, "\nTemperature  = %.1f K")
		    call pargr (ST_THERMAL(st))
		call strcat (Memc[str], Memc[title], SZ_LINE)
		call strcpy ("Emissivity (%)", Memc[ylabel], SZ_LINE)
		call strcpy ("Emissivity (%)", Memc[id[1]], SZ_LINE) 

		iferr  {
		    do i = 1, npts
			Memr[ydata+i-1] = 100 *
			    tabinterp1 (tab, Memc[name], w[i])
		} then
		    call amovkr (100., Memr[ydata], npts)
	    }
	case OUT_THRU:
	    if (Memc[title] != EOS)
		call strcat ("\n", Memc[title], SZ_LINE)
	    call strcat ("System Thruput (Telescope to Detected Photons)",
		Memc[title], SZ_LINE)
	    call strcpy ("Thruput (%)", Memc[ylabel], SZ_LINE)
	    call strcpy ("Thruput (%)", Memc[id[1]], SZ_LINE) 

	    do i = 1, npts {
		call st_snr (st, NULL, w[i], ST_NEXP(st), ST_TIME(st),
		    nobj, nsky, snr, thruput)
		Memr[ydata+i-1] = 100 * thruput
	    }
	case OUT_COUNTS:
	    if (Memc[title] != EOS)
		call strcat ("\n", Memc[title], SZ_LINE)
	    call strcat (Memc[spec], Memc[title], SZ_LINE)
	    call sprintf (Memc[str], SZ_LINE, "\nExposure time = %d seconds")
		call pargr (ST_NEXP(st) * max(ST_TIME(st),1.))
	    call strcat (Memc[str], Memc[title], SZ_LINE)
	    if (ST_NEXP(st) > 1) {
		call sprintf (Memc[str], SZ_LINE, "(%d exposures)")
		    call pargi (ST_NEXP(st))
		call strcat (Memc[str], Memc[title], SZ_LINE)
	    }
	    call strcpy ("DN", Memc[ylabel], SZ_LINE)
	    call strcpy ("Object (counts)", Memc[id[1]], SZ_LINE) 
	    call strcpy ("Background (counts)", Memc[id[2]], SZ_LINE) 
	    call strcpy ("Total (counts)", Memc[id[3]], SZ_LINE) 

	    do i = 1, npts {
		call st_snr (st, NULL, w[i], 1, ST_NEXP(st) * ST_TIME(st),
		    nobj, nsky, snr, thruput)
		Memr[ydata+i-1] = nobj[1] / ST_GAIN(st)
		Memr[ydata+npts+i-1] = nsky[1] / ST_GAIN(st)
		Memr[ydata+2*npts+i-1] = (nobj[1]+nsky[1]) / ST_GAIN(st)
	    }
	    call alimr (Memr[ydata+npts], npts, a, b)
	    if (b <= 0.) {
		Memr[ydata+npts] = INDEF
		Memr[ydata+2*npts] = INDEF
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call sprintf (Memc[str], SZ_LINE,
		    "Object DN at gain of %.2g\n")
		    call pargr (ST_GAIN(st))
		call strcat (Memc[str], Memc[title], SZ_LINE)
	    } else {
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call sprintf (Memc[str], SZ_LINE,
		    "Object, background, and total DN at gain of %.2g\n")
		    call pargr (ST_GAIN(st))
		call strcat (Memc[str], Memc[title], SZ_LINE)
	    }

	    wy1 = INDEF; wy2 = INDEF
	case OUT_SENS:
	    airmass = ST_AIRMASS(st)
	    ST_AIRMASS(st) = 0.

	    if (Memc[title] != EOS)
		call strcat ("\n", Memc[title], SZ_LINE)
	    call strcat ("Sensitivity Function", Memc[title], SZ_LINE)
	    call strcpy ("Magnitudes", Memc[ylabel], SZ_LINE)
	    call strcpy ("Sensitivity (mag)", Memc[id[1]], SZ_LINE) 

	    wy1 = MAX_REAL
	    wy2 = -MAX_REAL
	    do i = 1, npts {
		call st_snr (st, NULL, w[i], ST_NEXP(st), ST_TIME(st),
		    nobj, nsky, snr, thruput)
		a = nobj[1] / ST_GAIN(st) / ST_TIME(st) / ST_DISP(st,1)
		b = st_spectrum (st, w[i])
		if (a > 0. && b > 0.) {
		    a = 2.5 * (log10 (a) - log10 (b))
		    wy1 = min (wy1, a)
		    wy2 = max (wy2, a)
		} else
		    a = 0
		Memr[ydata+i-1] = a
	    }
	    if (wy1 < wy2) {
		buf = 0.1 * (wy2 - wy1)
		wy1 = wy1 - buf
		wy2 = wy2 + buf
	    } else {
		wy1 = INDEF
		wy2 = INDEF
	    }

	    ST_AIRMASS(st) = airmass
	case OUT_CORRECT:
	    if (tabexists (tab, "sensfunc")) {
		airmass = ST_AIRMASS(st)
		ST_AIRMASS(st) = 0.

		iferr (call tabgstr (tab, "sensfunc", "",
		    "title", Memc[str], SZ_LINE)) {
		    call tabgstr (tab, "sensfunc", "", "table.filename",
			Memc[str], SZ_LINE)
		}
		if (Memc[title] != EOS)
		    call strcat ("\n", Memc[title], SZ_LINE)
		call strcat ("Correction from: ", Memc[title], SZ_LINE)
		call strcat (Memc[str], Memc[title], SZ_LINE)
		call strcpy ("Correction factor", Memc[ylabel], SZ_LINE)
		call strcpy ("Correction factor", Memc[id[1]], SZ_LINE) 

		wmin = tabgetr (tab, "sensfunc", "", "table.xmin")
		wmax = tabgetr (tab, "sensfunc", "", "table.xmax")

		wy1 = MAX_REAL
		wy2 = -MAX_REAL
		ndata = 0
		do i = 1, npts {
		    if (w[i] < wmin || w[i] > wmax)
			next
		    call st_snr (st, NULL, w[i], ST_NEXP(st), ST_TIME(st),
			nobj, nsky, snr, thruput)
		    a = nobj[1] / ST_GAIN(st) / ST_TIME(st) / ST_DISP(st,1)
		    b = st_spectrum (st, w[i])
		    if (a <= 0. || b <= 0.)
			next
		    a = 2.5 * (log10 (a) - log10 (b))
		    b = tabinterp1 (tab, "sensfunc", w[i])
		    a = 10. ** (0.4 * (b - a))
		    wy1 = min (wy1, a)
		    wy2 = max (wy2, a)
		    Memr[xdata+ndata] = w[i]
		    Memr[ydata+ndata] = a
		    ndata = ndata + 1
		}
		if (wy1 < wy2) {
		    buf = 0.1 * max (0.1, wy2 - wy1)
		    wy1 = wy1 - buf
		    wy2 = wy2 + buf
		} else {
		    wy1 = INDEF
		    wy2 = INDEF
		}

		ST_AIRMASS(st) = airmass
	    }
	}

	# Draw graph.
	if (gp != NULL && !IS_INDEF(Memr[ydata])) {
	    call greactivate (gp, 0)
	    if (IS_INDEF(wx1) || IS_INDEF(wx2)) {
		call alimr (Memr[xdata], ndata, wx1, wx2)
		buf = 0.1 * (wx2 - wx1)
		wx1 = wx1 - buf
		wx2 = wx2 + buf
	    }
	    if (IS_INDEF(wy1) || IS_INDEF(wy2)) {
		call alimr (Memr[ydata], ndata, wy1, wy2)
		do i = 1, 3 {
		    if (!IS_INDEF(Memr[ydata+i*npts])) { 
			call alimr (Memr[ydata+i*npts], ndata, a, b)
			wy1 = min (wy1, a)
			wy2 = max (wy2, b)
		    }
		}
		buf = 0.1 * (wy2 - wy1)
		wy1 = wy1 - buf
		wy2 = wy2 + buf
	    }

	    call gclear (gp)
	    call gsview (gp, 0.15, 0.95, 0.15, 0.85)
	    call gswind (gp, wx1, wx2, wy1, wy2)
	    call glabax (gp, Memc[title], Memc[xlabel], Memc[ylabel])
	    do i = 1, 4 {
		if (!IS_INDEF(Memr[ydata+(i-1)*npts])) { 
		    call gseti (gp, G_PLTYPE, i)
		    call gseti (gp, G_PLCOLOR, i)
		    call gpline (gp, Memr[xdata], Memr[ydata+(i-1)*npts], ndata)
		}
	    }

	    if (interactive) {
		call printf (
		    "Type 'q' to quit graphs or any other key to continue")
		i = clgcur ("gcur", a, b, i, j, Memc[str], SZ_LINE)
		if (j == 'q')
		    call gclose (gp)
	    }
	    if (gp != NULL)
		call gdeactivate (gp, 0)
	}

	# Output list.
	if (fd != NULL && !IS_INDEF(Memr[ydata])) {
	    j = 0
	    do i = 0, ARB {
		if (Memc[title+i] == EOS || Memc[title+i] == '\n') {
		    if (j != 0) {
			Memc[str+j] = EOS
			call fprintf (fd, "# %s\n")
			    call pargstr (Memc[str])
		    }
		    if (Memc[title+i] == EOS)
			break
		    j = 0
		} else {
		    Memc[str+j] = Memc[title+i]
		    j = j + 1
		}
	    }
	    call fprintf (fd, "# Column 1: %s\n")
		call pargstr (Memc[xlabel])
	    do j = 1, 4 {
		if (IS_INDEF(Memr[ydata+(j-1)*npts]))
		    next
		call fprintf (fd, "# Column %d: %s\n")
		    call pargi (j+1)
		    call pargstr (Memc[id[j]])
	    }
	    do i = 1, ndata {
		call fprintf (fd, "%12.8g")
		    call pargr (Memr[xdata+i-1])
		do j = 1, 4 {
		    if (IS_INDEF(Memr[ydata+(j-1)*npts]))
			next
		    call fprintf (fd, "  %12.8g")
			call pargr (Memr[ydata+(j-1)*npts+i-1])
		}
		call fprintf (fd, "\n")
	    }
	    call fprintf (fd, "\n")
	}

	call sfree (sp)
end


# CCM -- Compute CCM Extinction Law

real procedure ccm (wavelength, rv) 

real	wavelength		# Wavelength in Angstroms
real	rv			# A(V) / E(B-V)

real	x, y, a, b

begin
	# Convert to inverse microns
	x = 10000. / wavelength
	x = max (0.3, min (10., x))

	# Compute a(x) and b(x)
	if (x < 0.3) {
	    call error (1, "Wavelength out of range of extinction function")

	} else if (x < 1.1) {
	    y = x ** 1.61
	    a = 0.574 * y
	    b = -0.527 * y

	} else if (x < 3.3) {
	    y = x - 1.82
	    a = 1 + y * (0.17699 + y * (-0.50447 + y * (-0.02427 +
		y * (0.72085 + y * (0.01979 + y * (-0.77530 + y * 0.32999))))))
	    b = y * (1.41338 + y * (2.28305 + y * (1.07233 + y * (-5.38434 +
		y * (-0.62251 + y * (5.30260 + y * (-2.09002)))))))

	} else if (x < 5.9) {
	    y = (x - 4.67) ** 2
	    a = 1.752 - 0.316 * x - 0.104 / (y + 0.341)
	    b = -3.090 + 1.825 * x + 1.206 / (y + 0.263)

	} else if (x < 8.0) {
	    y = (x - 4.67) ** 2
	    a = 1.752 - 0.316 * x - 0.104 / (y + 0.341)
	    b = -3.090 + 1.825 * x + 1.206 / (y + 0.263)

	    y = x - 5.9
	    a = a - 0.04473 * y**2 - 0.009779 * y**3
	    b = b + 0.2130 * y**2 + 0.1207 * y**3

	} else if (x <= 10.0) {
	    y = x - 8
	    a = -1.072 - 0.628 * y + 0.137 * y**2 - 0.070 * y**3
	    b = 13.670 + 4.257 * y - 0.420 * y**2 + 0.374 * y**3

	} else {
	    call error (1, "Wavelength out of range of extinction function")
	    
	}

	# Compute A(lambda)/A(V)
	y = a + b / rv
	return (y)
end


# STGETR -- Get real parameter.
# Returns INDEFR if parameter is not defined.

real procedure stgetr (st, param, table, default)

pointer st		#I SPECTIME structure
char	param[ARB]	#I Parameter name
char	table[ARB]	#I Name of table
real	default		#I Default value
real	val		#O Returned value

real	clgetr(), tabgetr()
errchk	tabgetr

begin
	# Get parameter from CL.
	val = clgetr (param)

	# If the value is INDEF get the value from the table.
	if (IS_INDEFR(val)) {
	    iferr (val = tabgetr (ST_TAB(st), table, "spectrograph", param))
		val = INDEFR
	}

	# If the value is INDEF set default.
	if (IS_INDEFR(val))
	    val = default

	return (val)
end


# STGETI -- Get integer parameter.
# Returns INDEFI if parameter is not defined.

int procedure stgeti (st, param, table, default)

pointer st		#I SPECTIME structure
char	param[ARB]	#I Parameter name
char	table[ARB]	#I Name of table
int	default		#I Default value
int	val		#O Returned value

int	clgeti(), tabgeti()
errchk	tabgeti

begin
	# Get parameter from CL.
	val = clgeti (param)

	# If the value is INDEF get the value from the table.
	if (IS_INDEFI(val)) {
	    iferr (val = tabgeti (ST_TAB(st), table, "spectrograph", param))
		val = INDEFI
	}

	# If the value is INDEF set the default.
	if (IS_INDEFI(val))
	    val = default

	return (val)
end


# STGSTR -- Get string parameter.
# Returns null string if parameter is not defined.

procedure stgstr (st, param, table, default, val, maxchar)

pointer st		#I SPECTIME structure
char	param[ARB]	#I Parameter name
char	table[ARB]	#I Name of table
char	default[ARB]	#I Default value
char	val[ARB]	#O Returned value
int	maxchar		#I Maximum string length

char	tmp[10]
int	nowhite()
errchk	tabgste

begin
	# Get parameter from CL.
	call clgstr (param, val, maxchar)

	# If the value is a null string get the value from a table.
	if (nowhite (val, tmp, 10) == 0) {
	    iferr (call tabgstr (ST_TAB(st), table, "spectrograph", param,
		val, maxchar))
		val[1] = EOS
	}

	# If the value is null set the default value.
	if (val[1] == EOS)
	    call strcpy (default, val, maxchar)
end
