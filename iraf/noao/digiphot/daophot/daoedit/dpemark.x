include <ctype.h>
include "daoedit.h"

# DP_MFWHMPSF -- Mark the fwhmpsf on the radial profile plot and confirm.

procedure dp_mfwhmpsf (gd)

pointer	gd		# pointer to the graphics stream

int	wcs, key, stat
pointer	sp, cmd
real	rmin, rmax, imin, imax, scale, fwhmpsf, wx, wy
int	clgcur()
real	clgetr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Determine the x and y limits of the current plot.
	call ggwind (gd, rmin, rmax, imin, imax)

	# Get the current parameters.
	scale = 1.0 / clgetr ("datapars.scale")
	fwhmpsf = clgetr ("datapars.fwhmpsf") / 2.0

	# Mark the FWHM of the PSF on the radial profile plot.
	call printf ("Mark HWHM of the psf (%g pixels):")
	    call pargr (fwhmpsf * scale)
	call gscur (gd, fwhmpsf * scale, (imin + imax) / 2.0)
	stat = clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || wx <= 0.0 || wx > rmax)
	    ;
	else
	    fwhmpsf = wx / scale

	# Store the new fwhmpsf.
	call clputr ("datapars.fwhmpsf", 2.0 * fwhmpsf)

	call sfree (sp)
end


# DP_MSIGMA -- Mark the sky sigma on the radial profile plot and confirm.

procedure dp_msigma (gd)

pointer	gd		# pointer to the grapics stream

int	wcs, key, stat
pointer	sp, cmd
real	rmin, rmax, imin, imax, mean, sigma, wx, wy
int	clgcur()
real	clgetr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Determine the range of the plot
	call ggwind (gd, rmin, rmax, imin, imax)

	# Mark the mean sky on the radial profile plot.
	call printf ("Mark mean sky background level:")
	call gscur (gd, (rmin + rmax) / 2.0, imin)
	stat = clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || wy < imin || wy > imax)
	    mean = imin
	else
	    mean = wy

	# Get the current value.
	sigma = clgetr ("datapars.sigma")
	if (! IS_INDEFR (sigma))
	    sigma = 3.0 * sigma

	# Mark the sky sigma on the radial profile plot.
	call printf ("Mark 3 sigma sky level (%g counts):")
	    call pargr (sigma)
	if (IS_INDEFR(sigma))
	    call gscur (gd, (rmin + rmax) / 2.0, imax)
	else
	    call gscur (gd, (rmin + rmax) / 2.0, mean + sigma)
	stat = clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE)
        if (stat == EOF || wy < imin || wy > imax)
	    ;
	else
	    sigma = abs (wy - mean) / 3.0

	# Store the new sky sigma.
	call clputr ("datapars.sigma", sigma)

	call sfree (sp)
end


# DP_MDMIN -- Mark the minimum good data value on the radial profile plot
# and confirm.

procedure dp_mdmin (gd)

pointer	gd		# pointer to the grapics stream

int	wcs, key, stat
pointer	sp, cmd
real	rmin, rmax, imin, imax, datamin, wx, wy
int	clgcur()
real	clgetr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Determine the limits of the plot.
	call ggwind (gd, rmin, rmax, imin, imax)

	# Get the current value.
	datamin = clgetr ("datapars.datamin")

	# Mark the threshold on the radial profile plot.
	call printf ("Mark the minimum good data level (%g counts):")
	    call pargr (datamin)
	    
	if (IS_INDEFR(datamin) || datamin < imin)
	    call gscur (gd, (rmin + rmax) / 2.0, imin)
	else
	    call gscur (gd, (rmin + rmax) / 2.0, datamin)
	stat = clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || wy < imin || wy > imax)
	    ;
	else
	    datamin = wy

	# Store the new good data minimum.
	call clputr ("datapars.datamin", datamin)

	call sfree (sp)
end


# DP_MDMAX -- Mark the maximum good data value on the radial profile plot
# and confirm.

procedure dp_mdmax (gd)

pointer	gd		# pointer to the grapics stream

int	wcs, key, stat
pointer	sp, cmd
real	rmin, rmax, imin, imax, datamax, wx, wy
int	clgcur()
real	clgetr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Determine the limits of the plot.
	call ggwind (gd, rmin, rmax, imin, imax)

	# Get the current value.
	datamax = clgetr ("datapars.datamax")

	# Mark the threshold on the radial profile plot.
	call printf ("Mark the maximum good data level (%g counts):")
	    call pargr (datamax)
	    
	if (IS_INDEFR(datamax) || datamax > imax)
	    call gscur (gd, (rmin + rmax) / 2.0, imax)
	else
	    call gscur (gd, (rmin + rmax) / 2.0, datamax)
	stat = clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || wy < imin || wy > imax)
	    ;
	else
	    datamax = wy

	# Store the new maximum good data value.
	call clputr ("datapars.datamax", datamax)

	call sfree (sp)
end


# DP_MCBOX -- Mark the centering aperture on the radial profile plot and
# confirm.

procedure dp_mcbox (gd)

pointer	gd		# pointer to the grapics stream

int	wcs, key, stat
pointer	sp, cmd
real	rmin, rmax, imin, imax, scale, capert, wx, wy
int	clgcur()
real	clgetr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Determine the x and y limits of the current plot.
	call ggwind (gd, rmin, rmax, imin, imax)

	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	capert = clgetr ("centerpars.cbox") / 2.0

	# Mark the centering aperture on the radial profile plot.
	call printf ("Mark centering box half width (%g pixels):")
	    call pargr (capert * scale)
	call gscur (gd, capert * scale, (imin + imax) / 2.0)
	stat = clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || wx <= 0.0 || wx > rmax)
	    ;
	else
	    capert = wx / scale

	# Store the new centering box.
	call clputr ("centerpars.cbox", 2.0 * capert)

	call sfree (sp)
end


# DP_MRCLEAN -- Mark the cleaning radius on the radial profile plot and
# confirm.

procedure dp_mrclean (gd)

pointer	gd		# pointer to the graphics stream

int	wcs, key, stat
pointer	sp, cmd
real	rmin, rmax, imin, imax, scale, rclean, wx, wy
int	clgcur()
real	clgetr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Get the current plot window.
	call ggwind (gd, rmin, rmax, imin, imax)

	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	rclean = clgetr ("centerpars.rclean")

	# Mark the cleaning radius on the plot.
	call printf (
	    "Mark the centering algorithm cleaning radius (%g pixels):")
	    call pargr (scale * rclean)
	call gscur (gd, scale * rclean, (imin + imax) / 2.0)
	stat = clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || wx <= 0.0 || wx > rmax)
	    ;
	else
	    rclean = wx / scale

	# Store the new cleaning radius.
	call clputr ("centerpars.rclean", rclean)

	call sfree (sp)
end


# DP_MRCLIP -- Mark the clipping radius on the radial profile plot and.
# confirm.

procedure dp_mrclip (gd)

pointer	gd		# pointer to the grapics stream

int	wcs, key, stat
pointer	sp, cmd
real	rmin, rmax, imin, imax, scale, rclip, wx, wy
int	clgcur()
real	clgetr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Get the current plot window.
	call ggwind (gd, rmin, rmax, imin, imax)

	# Get the clipping radius values.
	scale = 1.0 / clgetr ("datapars.scale")
	rclip = clgetr ("centerpars.rclip")

	# Mark clipping radius on the plot.
	call printf (
	    "Mark the centering algorithm clipping radius (%g pixels):")
	    call pargr (scale * rclip)
	call gscur (gd, scale * rclip, (imin + imax) / 2.0)
	stat = clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || wx <= 0.0 || wx > rmax)
	    ;
	else
	    rclip = wx / scale

	# Store the new clipping radius.
	call clputr ("centerpars.rclip", rclip)

	call sfree (sp)
end


# DP_MANNULUS -- Mark the sky annulus on the radial profile plot and confirm.

procedure dp_mannulus (gd)

pointer	gd		# pointer to the grapics stream

int	wcs, key, stat
pointer	sp, cmd
real	rmin, rmax, imin, imax, scale, annulus, wx, wy
int	clgcur()
real	clgetr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Get the current plot window.
	call ggwind (gd, rmin, rmax, imin, imax)

	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	annulus = clgetr ("fitskypars.annulus")

	# Mark the inner sky radius.
	call printf ("Mark inner sky radius (%g pixels):")
	    call pargr (annulus * scale)
	call gscur (gd, annulus * scale, (imin + imax) / 2.0)
	stat = clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || wx < 0.0 || wx > rmax)
	    ;
	else
	    annulus = wx / scale

	# Store the new sky annulus.
	call clputr ("fitskypars.annulus", annulus)

	call sfree (sp)

end


# DP_MDANNULUS -- Mark the sky annulus width on the radial profile plot and
# confirm.

procedure dp_mdannulus (gd)

pointer	gd		# pointer to the grapics stream

int	wcs, key, stat
pointer	sp, cmd
real	rmin, rmax, imin, imax, scale, annulus, dannulus, wx, wy
int	clgcur()
real	clgetr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Get the current plot window.
	call ggwind (gd, rmin, rmax, imin, imax)

	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	annulus = clgetr ("fitskypars.annulus")
	dannulus = clgetr ("fitskypars.dannulus")

	# Mark the outer sky radius.
	call printf ("Mark outer sky radius (%g pixels):")
	    call pargr (scale * (annulus + dannulus))
	call gscur (gd, scale * (annulus + dannulus), (imin + imax) / 2.0)
	stat = clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || (wx / scale < annulus) || wx > rmax)
	    ;
	else
	    dannulus = (wx / scale - annulus)

	# Save the new sky annulus width.
	call clputr ("fitskypars.dannulus", dannulus)

	call sfree (sp)
end


# DP_MRGROW -- Mark the regions growing radius the radial profile plot.

procedure dp_mrgrow (gd)

pointer	gd		# pointer to the grapics stream

int	wcs, key, stat
pointer	sp, cmd
real	rmin, rmax, imin, imax, scale, rgrow, wx, wy
int	clgcur()
real	clgetr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Get the current plot window.
	call ggwind (gd, rmin, rmax, imin, imax)

	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	rgrow = clgetr ("fitskypars.rgrow")

	# Mark the inner sky radius.
	call printf ("Mark region growing radius (%g pixels):")
	    call pargr (rgrow * scale)
	call gscur (gd, rgrow * scale, (imin + imax) / 2.0)
	stat = clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || wx < 0.0 || wx > rmax)
	    ;
	else
	    rgrow = wx / scale

	# Store the new sky annulus.
	call clputr ("fitskypars.rgrow", rgrow)

	call sfree (sp)

end


# DP_MAPER -- Mark the photometry apertures on the radial profile plot and
# confirm.

procedure dp_maper (gd)

pointer	gd		# pointer to the grapics stream

int	wcs, key, naperts
pointer	sp, oapstr, aperts, tapstr, apstr, cmd
real	rmin, rmax, imin, imax, scale, wx, wy
int	dp_gaperts(), clgcur(), strlen() 
real	clgetr()

begin
	call smark (sp)
	call salloc (oapstr, SZ_LINE, TY_CHAR)
	call salloc (aperts, MAX_NAPERTS, TY_REAL)
	call salloc (apstr, SZ_LINE, TY_CHAR)
	call salloc (tapstr, SZ_LINE, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Determine the current plot window.
	call ggwind (gd, rmin, rmax, imin, imax)

	# Decode the apertures.
	scale = 1.0 / clgetr ("datapars.scale")
	call clgstr ("photpars.apertures", Memc[oapstr], SZ_LINE)
	naperts = dp_gaperts (Memc[oapstr], Memr[aperts], MAX_NAPERTS)

	# Type prompt string.
	call printf ("Mark apertures (%s pixels) [q=quit]:")
	    call pargstr (Memc[oapstr])
	call gscur (gd, Memr[aperts] * scale, (imin + imax) / 2.0)

	# Mark the apertures.
	Memc[apstr] = EOS
	Memc[tapstr] = EOS
	while (clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd],
	    SZ_LINE) != EOF) {
	    if (key == 'q')
		break
	    if (wx <= 0.0 || wx > rmax)
		next
	    call sprintf (Memc[apstr+strlen(Memc[apstr])], SZ_FNAME,"%.2f,")
		call pargr (wx / scale)
	    call sprintf (Memc[tapstr+strlen(Memc[tapstr])], SZ_FNAME,"%.2f,")
		call pargr (wx)
	    call printf ("Mark apertures (%s pixels) [q=quit]:")
	        call pargstr (Memc[tapstr])
	}
	Memc[apstr+strlen(Memc[apstr])-1] = EOS

	# Save the new aperture string.
	call clpstr ("photpars.apertures", Memc[apstr])

	call sfree (sp)
end


# DP_MPSFRAD -- Mark the psf radius on the radial profile plot and confirm.

procedure dp_mpsfrad (gd)

pointer	gd		# pointer to the graphics stream

int	wcs, key, stat
pointer	sp, cmd
real	rmin, rmax, imin, imax, scale, psfrad, wx, wy
int	clgcur()
real	clgetr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Determine the x and y limits of the current plot.
	call ggwind (gd, rmin, rmax, imin, imax)

	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	psfrad = clgetr ("daopars.psfrad")

	# Mark the FWHM of the PSF on the radial profile plot.
	call printf ("Mark the PSF radius (%g pixels):")
	    call pargr (psfrad * scale)
	call gscur (gd, psfrad * scale, (imin + imax) / 2.0)
	stat = clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || wx <= 0.0 || wx > rmax)
	    ;
	else
	    psfrad = wx / scale

	# Store the new PSF radius.
	call clputr ("daopars.psfrad", psfrad)

	call sfree (sp)
end


# DP_MFITRAD -- Mark the fitting radius on the radial profile plot and confirm.

procedure dp_mfitrad (gd)

pointer	gd		# pointer to the graphics stream

int	wcs, key, stat
pointer	sp, cmd
real	rmin, rmax, imin, imax, scale, fitrad, wx, wy
int	clgcur()
real	clgetr()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Determine the x and y limits of the current plot.
	call ggwind (gd, rmin, rmax, imin, imax)

	# Get the current values.
	scale = 1.0 / clgetr ("datapars.scale")
	fitrad = clgetr ("daopars.fitrad")

	# Mark the FWHM of the PSF on the radial profile plot.
	call printf ("Mark the fitting radius (%g pixels):")
	    call pargr (fitrad * scale)
	call gscur (gd, fitrad * scale, (imin + imax) / 2.0)
	stat = clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || wx <= 0.0 || wx > rmax)
	    ;
	else
	    fitrad = wx / scale

	# Store the new fitting radius.
	call clputr ("daopars.fitrad", fitrad)

	call sfree (sp)
end


# DP_GAPERTS -- Decode the aperture string.

int procedure dp_gaperts (str, aperts, max_naperts)

char	str[ARB]		# aperture string
real	aperts[ARB]		# aperture array
int	max_naperts		# maximum number of apertures

int	naperts, ip, op, ndecode, nap
pointer	sp, outstr
real	apstart, apend, apstep
bool	fp_equalr()
int	dp_gctor()

begin
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)

	naperts = 0
	for (ip = 1; str[ip] != EOS && naperts < max_naperts;) {

	    # Initialize.
	    apstart = 0.0
	    apend = 0.0
	    apstep = 0.0
	    ndecode = 0

	    # Skip past white space and commas.
	    while (IS_WHITE(str[ip]))
		ip = ip + 1
	    if (str[ip] == ',')
		ip = ip + 1

	    # Get the starting aperture number.
	    op = 1
	    while (IS_DIGIT(str[ip]) || str[ip] == '.') {
		Memc[outstr+op-1] = str[ip]
		ip = ip + 1
		op = op + 1
	    }
	    Memc[outstr+op-1] = EOS

	    # Decode the starting aperture.
	    op = 1
	    if (dp_gctor (Memc[outstr], op, apstart) > 0) {
	        apend = apstart
	        ndecode = 1
	    } else
		apstart = 0.0

	    # Skip past white space and commas.
	    while (IS_WHITE(str[ip]))
		ip = ip + 1
	    if (str[ip] == ',')
		ip = ip + 1

	    # Search for the ending aperture.
	    if (str[ip] == ':') {
		ip = ip + 1

		# Get the ending aperture.
		op = 1
		while (IS_DIGIT(str[ip]) || str[ip] == '.') {
		    Memc[outstr+op-1] = str[ip]
		    ip = ip + 1
		    op = op + 1
		}
		Memc[outstr+op-1] = EOS

	        # Decode the ending aperture.
	        op = 1
	        if (dp_gctor (Memc[outstr], op, apend) > 0) {
	            ndecode = 2
	            apstep = apend - apstart
		}
	     }

	    # Skip past the white space.
	    while (IS_WHITE(str[ip]))
		ip = ip + 1

	    # Skip past the commas.
	    if (str[ip] == ',')
		ip = ip + 1

	    # Get the step size.
	    if (str[ip] == ':') {
		ip = ip + 1

		# Get the step size.
		op = 1
		while (IS_DIGIT(str[ip]) || str[ip] == '.') {
		    Memc[outstr+op-1] = str[ip]
		    ip = ip + 1
		    op = op + 1
		}
		Memc[outstr+op-1] = EOS

		# Decode the step size.
		op = 1
		if (dp_gctor (Memc[outstr], op, apstep) > 0) {
		    if (fp_equalr (apstep, 0.0))
			apstep = apend - apstart
		    else
			ndecode = (apend - apstart) / apstep + 1
		    if (ndecode < 0) {
			ndecode = -ndecode
			apstep = - apstep
		    }
		}
	    }

	    # Negative apertures are not permitted.
	    if (apstart <= 0.0 || apend <= 0.0)
		break

	    # Fill in the apertures.
	    if (ndecode == 0) {
		;
	    } else if (ndecode == 1) {
		naperts = naperts + 1
		aperts[naperts] = apstart
	    } else if (ndecode == 2) {
		naperts = naperts + 1
		aperts[naperts] = apstart
		if (naperts >= max_naperts)
		    break
		naperts = naperts + 1
		aperts[naperts] = apend
	    } else {
		for (nap = 1; nap <= ndecode && naperts < max_naperts;
		    nap = nap + 1) {
		    naperts = naperts + 1
		    aperts[naperts] = apstart + (nap - 1) * apstep
		}
	    }
	}

	call sfree (sp)

	return (naperts)
end


# DP_GCTOR -- Procedure to convert a character variable to a real number.
# This routine is just an interface routine to the IRAF procedure gctod.

int procedure dp_gctor (str, ip, rval)

char	str[ARB]	# string to be converted
int	ip		# pointer to the string
real	rval		# real value

double	dval
int	nchars
int	gctod()

begin
	nchars = gctod (str, ip, dval)
	rval = dval
	return (nchars)
end
