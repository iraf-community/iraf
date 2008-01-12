include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"


# AP_CFWHMPSF -- Read the fwhmpsf from the radial profile plot.

real procedure ap_cfwhmpsf (ap, gd, out, stid, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# output file descriptor
int	stid		# sequence number in output file
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, fwhmpsf, xjunk, yjunk
int	clgcur()
real	apstatr(), ap_vfwhmpsf()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark the FWHM of the PSF on the plot.
	call printf ("Mark half-width half-maximum of the psf (%g) pixels:")
	    call pargr (apstatr (ap, FWHMPSF) * scale / 2.0)
	call gscur (gd, apstatr (ap, FWHMPSF) * scale / 2.0, (imin + imax) /
	    2.0)
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk <= 0.0 || xjunk > rmax)
	    fwhmpsf = apstatr (ap, FWHMPSF)
	else
	    fwhmpsf = 2.0 * xjunk / scale

	# Verify the results.
	call apsetr (ap, FWHMPSF, fwhmpsf)
	fwhmpsf = ap_vfwhmpsf (ap)

	# Save the results.
	if (out != NULL && stid > 1)
	    call ap_rparam (out, KY_FWHMPSF, fwhmpsf, UN_ASCALEUNIT,
		"full width half maximum of the psf")

	call sfree (sp)

	return (fwhmpsf)
end


# AP_CDATAMIN -- Read the good data minimum off the radial profile plot.

real procedure ap_cdatamin (ap, gd, out, stid, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# output file descriptor
int	stid		# sequence number in output file
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	datamin, xjunk, yjunk
int	clgcur()
real	apstatr(), ap_vdatamin()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Mark the datamin on the plot.
	call printf ("Mark the good data minimum (%g) counts:")
	    call pargr (apstatr (ap, DATAMIN))
	if (IS_INDEFR (apstatr (ap, DATAMIN)))
	    call gscur (gd, (rmin + rmax) / 2.0, imin - 1.0)
	else
	    call gscur (gd, (rmin + rmax) / 2.0, apstatr (ap, DATAMIN))
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || yjunk < imin || yjunk > imax)
	    datamin = apstatr (ap, DATAMIN)
	else
	    datamin = yjunk

	# Verify the results.
	call apsetr (ap, DATAMIN, datamin)
	datamin = ap_vdatamin (ap)

	# Store the results.
	if (out != NULL && stid > 1)
	    call ap_rparam (out, KY_DATAMIN, datamin, UN_ACOUNTS,
	        "minimum good data value")
	call sfree (sp)

	return (datamin)
end


# AP_CDATAMAX -- Read the good data maximum off the radial profile plot.

real procedure ap_cdatamax (ap, gd, out, stid, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# output file descriptor
int	stid		# sequence number in output file
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	datamax, xjunk, yjunk
int	clgcur()
real	apstatr(), ap_vdatamax()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Mark the datamax on the plot.
	call printf ("Mark the good data maximum (%g) counts:")
	    call pargr (apstatr (ap, DATAMAX))
	if (IS_INDEFR (apstatr (ap, DATAMAX)))
	    call gscur (gd, (rmin + rmax) / 2.0, imax + 1.0)
	else
	    call gscur (gd, (rmin + rmax) / 2.0, apstatr (ap, DATAMAX))
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || yjunk < imin || yjunk > imax)
	    datamax = apstatr (ap, DATAMAX)
	else
	    datamax = yjunk

	# Verify the result.
	call apsetr (ap, DATAMAX, datamax)
	datamax = ap_vdatamax (ap)

	# Store the results.
	if (out != NULL && stid > 1)
	    call ap_rparam (out, KY_DATAMAX, datamax, UN_ACOUNTS,
	        "maximum good data value")
	call sfree (sp)

	return (datamax)
end


# AP_CCAPERT -- Read the centering aperture of the radial profile plot.

real procedure ap_ccapert (ap, gd, out, stid, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# output file descriptor
int	stid		# output file sequence number
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, capert, xjunk, yjunk
int	clgcur()
real	apstatr(), ap_vcapert()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark the centering aperture on the plot.
	call printf ("Mark centering box half width (%g) pixels:")
	    call pargr (apstatr (ap, CAPERT) * scale)
	call gscur (gd, apstatr (ap, CAPERT) * scale, (imin + imax) / 2.0)
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk <= 0.0 || xjunk > rmax)
	    capert = apstatr (ap, CAPERT)
	else
	    capert = xjunk / scale

	# Verify the results.
	call apsetr (ap, CAPERT, capert)
	capert = ap_vcapert (ap)

	# Store the results.
	if (out != NULL && stid > 1)
	    call ap_rparam (out, KY_CAPERT, 2.0 * capert, UN_CSCALEUNIT,
	        "centering box width")
	call sfree (sp)

	return (capert)
end


# AP_CRCLEAN -- Read the cleaning radius off the radial profile plot.

real procedure ap_crclean (ap, gd, out, stid, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# output file descriptor
int	stid		# sequence number in output file
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, rclean, xjunk, yjunk
int	clgcur()
real	apstatr(), ap_vrclean()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark the cleaning radius on the plot.
	call printf (
	    "Mark the centering algorithm cleaning radius (%g) pixels:")
	    call pargr (apstatr (ap, RCLEAN) * scale)
	call gscur (gd, apstatr (ap, RCLEAN) * scale, (imin + imax) /
	    2.0)
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk <= 0.0 || xjunk > rmax)
	    rclean = apstatr (ap, RCLEAN)
	else
	    rclean = xjunk / scale

	# Verify the results.
	call apsetr (ap, RCLEAN, rclean)
	rclean = ap_vrclean (ap)

	# Save the results.
	if (out != NULL && stid > 1)
	    call ap_rparam (out, KY_RCLEAN, rclean, UN_CSCALEUNIT,
		"cleaning radius")
	call sfree (sp)

	return (rclean)
end


# AP_CRCLIP -- Read the clipping radius off the radial profile plot.

real procedure ap_crclip (ap, gd, out, stid, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# output file descriptor
int	stid		# sequence number in output file
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, rclip, xjunk, yjunk
int	clgcur()
real	apstatr(), ap_vrclip()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark clipping radius on the plot.
	call printf (
	    "Mark the centering algorithm clipping radius (%g) pixels:")
	    call pargr (apstatr (ap, RCLIP) * scale)
	call gscur (gd, apstatr (ap, RCLIP) * scale, (imin + imax) /
	    2.0)
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk <= 0.0 || xjunk > rmax)
	    rclip = apstatr (ap, RCLIP)
	else
	    rclip = xjunk / scale

	# Verify the results.
	call apsetr (ap, RCLIP, rclip)
	rclip = ap_vrclip (ap)

	# Save the results.
	if (out != NULL && stid > 1)
	    call ap_rparam (out, KY_RCLIP, rclip, UN_CSCALEUNIT,
	        "clipping radius")
	call sfree (sp)

	return (rclip)
end


# AP_CANNULUS -- Read the sky annulus of the radial profile plot.

real procedure ap_cannulus (ap, gd, out, stid, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# output file descriptor
int	stid		# output file sequence number
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, annulus, xjunk, yjunk
int	clgcur()
real	apstatr(), ap_vannulus()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark the inner sky radius.
	call printf ("Mark inner sky radius (%g) pixels:")
	    call pargr (apstatr (ap, ANNULUS) * apstatr (ap, SCALE))
	call gscur (gd, apstatr (ap, ANNULUS) * apstatr (ap, SCALE),
	    (imin + imax) / 2.0)
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk < 0.0 || xjunk > rmax)
	    annulus = apstatr (ap, ANNULUS)
	else
	    annulus = xjunk / scale

	# Verify the result.
	call apsetr (ap, ANNULUS, annulus)
	annulus = ap_vannulus (ap)

	# Save the results.
	if (out != NULL && stid > 1)
	    call ap_rparam (out, KY_ANNULUS, annulus, UN_SSCALEUNIT,
		"radius of the inner sky annulus")
	call sfree (sp)

	return (annulus)
end


# AP_CRGROW -- Read the region growing radius off the radial profile plot.

real procedure ap_crgrow (ap, gd, out, stid, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# the output file descriptor
int	stid		# output file sequence number
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, rgrow, xjunk, yjunk
int	clgcur()
real	apstatr(), ap_vrgrow()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark the inner sky radius.
	call printf ("Mark region growing radius (%g) pixels:")
	    call pargr (apstatr (ap, RGROW) * apstatr (ap, SCALE))
	call gscur (gd, apstatr (ap, RGROW) * apstatr (ap, SCALE),
	    (imin + imax) / 2.0)
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk < 0.0 || xjunk > rmax)
	    rgrow = apstatr (ap, RGROW)
	else
	    rgrow = xjunk / scale

	# Verify the region growing radius.
	call apsetr (ap, RGROW, rgrow)
	rgrow = ap_vrgrow (ap) 

	# Save the results.
	if (out != NULL && stid > 1)
	    call ap_rparam (out, KY_RGROW, rgrow, UN_SSCALEUNIT,
	        "region growing radius")
	call sfree (sp)

	return (rgrow)
end


# AP_CDANNULUS -- Read the sky annulus width off the radial profile plot.

real procedure ap_cdannulus (ap, gd, out, stid, annulus, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# output file descriptor
int	stid		# output file sequence number
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, annulus, dannulus, xjunk, yjunk
int	clgcur()
real	apstatr(), ap_vdannulus()

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
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || (xjunk / scale < annulus) || xjunk > rmax)
	    dannulus = apstatr (ap, DANNULUS)
	else
	    dannulus = (xjunk / scale - annulus)

	# Verify the width of the annulus.
	call apsetr (ap, DANNULUS, dannulus)
	dannulus = ap_vdannulus (ap)

	# Save the results.
	if (out != NULL && stid > 1)
	    call ap_rparam (out, KY_DANNULUS, dannulus, UN_SSCALEUNIT,
	        "width of the sky annulus")
	call sfree (sp)

	return (dannulus)
end


# AP_CSIGMA -- Read the sky sigma from the radial profile plot.

real procedure ap_csigma (ap, gd, out, stid, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# output file descriptor
int	stid		# sequence number in output file
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	mean, sigma3, xjunk, yjunk
int	clgcur()
int	apstati()
real	apstatr(), ap_vsigma()

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
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
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
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
        if (stat == EOF || yjunk < imin || yjunk > imax) {
	    sigma3 = apstatr (ap, SKYSIGMA)
	    if (! IS_INDEFR (sigma3))
		sigma3 = 3.0 * sigma3
	} else
	    sigma3 = abs (yjunk - mean)

	# Verify the results.
	if (IS_INDEFR(sigma3))
	    call apsetr (ap, SKYSIGMA, INDEFR) 
	else
	    call apsetr (ap, SKYSIGMA, sigma3 / 3.0)
	sigma3 = ap_vsigma (ap)

	# Save the results.
	if (out != NULL && stid > 1)
	    call ap_rparam (out, KY_SKYSIGMA, sigma3, UN_NCOUNTS,
	        "standard deviation of 1 sky pixel")

	call sfree (sp)

	if (IS_INDEFR(sigma3))
	    return (sigma3)
	else
	    return (sigma3 / 3.0)
end


# AP_CAPER -- Read the apertures off the radial profile plot.

procedure ap_caper (ap, gd, out, stid, outstr, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# output file descriptor
int	stid		# output file number sequence
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
	call ap_arrayr (ap, APERTS, Memr[aperts])

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
	while (clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd],
	    SZ_LINE) != EOF) {
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

	# Verify the results.
	call apsets (ap, APERTS, outstr)
	call ap_vaperts (ap, outstr, SZ_LINE)

	# Save the results.
	if (out != NULL && stid > 1)
	    call ap_sparam (out, KY_APERTS, outstr, UN_PSCALEUNIT,
	        "list of aperture radii")

	call sfree (sp)
end
