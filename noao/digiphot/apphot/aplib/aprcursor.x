include "../lib/apphot.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/noise.h"
include "../lib/phot.h"
include "../lib/radprof.h"
include "../lib/fitpsf.h"


# AP_CFWHMPSF -- Read the fwhmpsf of the radial profile plot.

real procedure ap_cfwhmpsf (ap, gd, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, fwhmpsf, xjunk, yjunk
int	clgcur()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark the FWHM of the PSF on the plot.
	call printf ("Mark half-width half-maximum of the psf (%g) pixels:")
	    call pargr (apstatr (ap, FWHMPSF) * scale / 2.0)
	call gscur (gd, apstatr (ap, FWHMPSF) * scale / 2.0, (imin + imax) /
	    2.0)
	stat = clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk <= 0.0 || xjunk > rmax)
	    fwhmpsf = apstatr (ap, FWHMPSF)
	else
	    fwhmpsf = 2.0 * xjunk / scale

	call sfree (sp)

	return (fwhmpsf)
end


# AP_CCAPERT -- Read the centering aperture of the radial profile plot.

real procedure ap_ccapert (ap, gd, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, capert, xjunk, yjunk
int	clgcur()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark the centering aperture on the plot.
	call printf ("Mark centering radius (%g) pixels:")
	    call pargr (apstatr (ap, CAPERT) * scale)
	call gscur (gd, apstatr (ap, CAPERT) * scale, (imin + imax) / 2.0)
	stat = clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk <= 0.0 || xjunk > rmax)
	    capert = apstatr (ap, CAPERT)
	else
	    capert = xjunk / scale

	call sfree (sp)

	return (capert)
end


# AP_CANNULUS -- Read the sky annulus of the radial profile plot.

real procedure ap_cannulus (ap, gd, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, annulus, xjunk, yjunk
int	clgcur()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark the inner sky radius.
	call printf ("Mark inner sky radius (%g) pixels:")
	    call pargr (apstatr (ap, ANNULUS) * apstatr (ap, SCALE))
	call gscur (gd, apstatr (ap, ANNULUS) * apstatr (ap, SCALE),
	    (imin + imax) / 2.0)
	stat = clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk < 0.0 || xjunk > rmax)
	    annulus = apstatr (ap, ANNULUS)
	else
	    annulus = xjunk / scale

	call sfree (sp)

	return (annulus)
end


# AP_CDANNULUS -- Read the sky annulus width off the radial profile plot.

real procedure ap_cdannulus (ap, gd, annulus, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, annulus, dannulus, xjunk, yjunk
int	clgcur()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark the outer sky radius.
	call printf ("Mark outer sky radius (%g) pixels:")
	    call pargr (apstatr (ap, SCALE) * (apstatr (ap, ANNULUS) +
		apstatr (ap, DANNULUS)))
	call gscur (gd, apstatr (ap, SCALE) * (apstatr (ap, ANNULUS) +
	    apstatr (ap, DANNULUS)), (imin + imax) / 2.0)
	stat = clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || (xjunk / scale < annulus))
	    dannulus = apstatr (ap, DANNULUS)
	else
	    dannulus = (xjunk / scale - annulus)

	call sfree (sp)

	return (dannulus)
end


# AP_CSIGMA -- Read the sky sigma off the radial profile plot.

real procedure ap_csigma (ap, gd, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	mean, sigma3, xjunk, yjunk
int	clgcur()
int	apstati()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Estimate the mean sky.
	if (apstati (ap, POSITIVE) == YES)
	    mean = imin
	else
	    mean = imax
	call printf ("Estimate sky sigma. Mark mean sky level (%g):")
	    call pargr (mean)
	call gscur (gd, (rmin + rmax) / 2.0, mean)
	stat = clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || yjunk < imin || yjunk > imax)
	    mean = mean
	else
	    mean = yjunk 

	# Estimate the sky sigma.
	if (IS_INDEFR (apstatr (ap, SKYSIGMA)))
	    sigma3 = INDEFR
	else
	    sigma3 = 3.0 * apstatr (ap, SKYSIGMA)
	call printf ("Next mark 3 sigma sky level (%g):")
	    call pargr (sigma3)
	if (IS_INDEFR(sigma3))
	    call gscur (gd, (rmin + rmax) / 2.0, imin - 1.0)
	else
	    call gscur (gd, (rmin + rmax) / 2.0, mean + sigma3)
	stat = clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
        if (stat == EOF || yjunk < imin || yjunk > imax) {
	    sigma3 = apstatr (ap, SKYSIGMA)
	    if (! IS_INDEFR (sigma3))
		sigma3 = 3.0 * sigma3
	} else
	    sigma3 = abs (yjunk - mean)

	call sfree (sp)

	return (sigma3)
end


# AP_CCTHRESH -- Read the centering threshold off the radial profile plot.

real procedure ap_ccthresh (ap, gd, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	minsky, threshold, xjunk, yjunk
int	clgcur()
int	apstati()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Estimate the minimum (maximum) data level.
	if (apstati (ap, POSITIVE) == YES) {
	    minsky = imin
	    call printf (
	     "Estimate threshold for centering. Mark minimum data level (%g):")
	        call pargr (minsky)
	    call gscur (gd, (rmin + rmax) / 2.0, minsky)
	} else {
	    minsky = imax
	    call printf (
	     "Estimate threshold for centering. Mark maximun data level (%g):")
	        call pargr (minsky)
	    call gscur (gd, (rmin + rmax) / 2.0, minsky)
	}

	stat = clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || yjunk < imin || yjunk > imax)
	    minsky = minsky
	else
	    minsky = yjunk 

	# Mark the threshold above the data minimum.
	if (apstati (ap, POSITIVE) == YES) {
	    call printf ("Next mark threshold above data min (%g) adu:")
	        call pargr (apstatr (ap, CTHRESHOLD))
	    call gscur (gd, (rmin + rmax) / 2.0, minsky + apstatr (ap,
		CTHRESHOLD))
	} else {
	    call printf ("Next mark threshold below data max (%g) adu:")
	        call pargr (apstatr (ap, CTHRESHOLD))
	    call gscur (gd, (rmin + rmax) / 2.0, minsky - apstatr (ap,
		CTHRESHOLD))
	}

	stat = clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || yjunk < imin || yjunk > imax)
	    threshold = apstatr (ap, CTHRESHOLD)
	else
	    threshold = abs (yjunk - minsky)

	call sfree (sp)

	return (threshold)
end


# AP_CTHRESH -- Read the centering threshold off the radial profile plot.

real procedure ap_cthresh (ap, gd, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	minsky, threshold, xjunk, yjunk
int	clgcur()
int	apstati()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Estimate the minimum (maximum) data level.
	if (apstati (ap, POSITIVE) == YES) {
	    minsky = imin
	    call printf (
	     "Estimate threshold for centering. Mark minimum data level (%g):")
	        call pargr (minsky)
	    call gscur (gd, (rmin + rmax) / 2.0, minsky)
	} else {
	    minsky = imax
	    call printf (
	     "Estimate threshold for centering. Mark maximun data level (%g):")
	        call pargr (minsky)
	    call gscur (gd, (rmin + rmax) / 2.0, minsky)
	}

	stat = clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || yjunk < imin || yjunk > imax)
	    minsky = minsky
	else
	    minsky = yjunk 

	# Mark the threshold above the data minimum.
	if (apstati (ap, POSITIVE) == YES) {
	    call printf ("Next mark threshold above data min (%g) adu:")
	        call pargr (apstatr (ap, THRESHOLD))
	    call gscur (gd, (rmin + rmax) / 2.0, minsky + apstatr (ap,
		THRESHOLD))
	} else {
	    call printf ("Next mark threshold below data max (%g) adu:")
	        call pargr (apstatr (ap, THRESHOLD))
	    call gscur (gd, (rmin + rmax) / 2.0, minsky - apstatr (ap,
		THRESHOLD))
	}

	stat = clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || yjunk < imin || yjunk > imax)
	    threshold = apstatr (ap, THRESHOLD)
	else
	    threshold = abs (yjunk - minsky)

	call sfree (sp)

	return (threshold)
end


# AP_CAPER -- Read the apertures off the radial profile plot.

real procedure ap_caper (ap, gd, outstr, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
char	outstr[ARB]	# output apertures
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	i, wcs, key, naperts
pointer	sp, cmd, tstr, aperts
real	scale, xjunk, yjunk
int	clgcur()
int	apstati(), strlen()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (tstr, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Get the apertures.
	naperts = apstati (ap, NAPERTS)
	call salloc (aperts, naperts, TY_REAL)
	call aparrays (ap, APERTS, Memr[aperts])

	# Encode the old aperture string.
	outstr[1] = EOS
	do i = 1, naperts - 1 {
	    call sprintf (outstr[1+strlen(outstr)], SZ_FNAME,"%.2f,")
		call pargr (Memr[aperts+i-1] * scale)
	}
	call sprintf (outstr[1+strlen(outstr)], SZ_FNAME,"%.2f")
	    call pargr (Memr[aperts+naperts-1] * scale)

	# Type prompt string.
	call printf ("Mark apertures (%s) pixels [q=quit]:")
	    call pargstr (outstr)
	call gscur (gd, Memr[aperts] * scale, (imin + imax) / 2.0)

	# Mark the apertures.
	outstr[1] = EOS
	Memc[tstr] = EOS
	while (clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {
	    if (key == 'q')
		break
	    if (xjunk <= 0.0 || xjunk > rmax)
		next
	    call sprintf (outstr[1+strlen(outstr)], SZ_FNAME,"%.2f,")
		call pargr (xjunk / scale)
	    call sprintf (Memc[tstr+strlen(Memc[tstr])], SZ_FNAME,"%.2f,")
		call pargr (xjunk)
	    call printf ("Mark apertures (%s) pixels [q=quit]:")
	        call pargstr (Memc[tstr])
	}
	outstr[strlen(outstr)] = EOS

	call sfree (sp)
end


# AP_CRPROF -- Read the radial profile size off the radial profile plot.

real procedure ap_crprof (ap, gd, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, radius, xjunk, yjunk
int	clgcur()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Estimate the minimum (maximum) data level.
	# Mark maximum radius of the radial profile.
	call printf ("Mark maximum radius for profile (%g) pixels:")
	    call pargr (apstatr (ap, RPRADIUS) * scale)
	call gscur (gd, apstatr (ap, RPRADIUS) * scale, (imin + imax) / 2.0)
	stat = clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk < 0.0 || xjunk > rmax)
	    radius = apstatr (ap, RPRADIUS)
	else
	    radius = xjunk / scale

	call sfree (sp)
	return (radius)
end

# AP_CRPSTEP -- Read the radial profile size off the radial profile plot.

real procedure ap_crpstep (ap, gd, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, step, xjunk, yjunk
int	clgcur()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark the radial profile step size.
	call printf ("Mark step size (%g) pixels:")
	    call pargr (apstatr (ap, RPSTEP) * scale)
	call gscur (gd, apstatr (ap, RPSTEP) * scale, (imin + imax) / 2.0)
	stat = clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk < 0.0 || xjunk > rmax)
	    step = apstatr (ap, RPSTEP)
	else
	    step = xjunk / scale

	call sfree (sp)

	return (step)
end

# AP_CPAPERT -- Read the centering aperture of the radial profile plot.

real procedure ap_cpapert (ap, gd, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, psfapert, xjunk, yjunk
int	clgcur()
real	apstatr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark the centering aperture on the plot.
	call printf ("Mark centering radius (%g) pixels:")
	    call pargr (apstatr (ap, PSFAPERT) * scale)
	call gscur (gd, apstatr (ap, PSFAPERT) * scale, (imin + imax) / 2.0)
	stat = clgcur ("cursor", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk <= 0.0 || xjunk > rmax)
	    psfapert = apstatr (ap, PSFAPERT)
	else
	    psfapert = xjunk / scale

	call sfree (sp)

	return (psfapert)
end
