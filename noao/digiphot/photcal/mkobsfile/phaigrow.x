include <time.h>
include <mach.h>
include <gset.h>
include "../lib/apfile.h"

define	HELPFILE	"photcal$mkobsfile/apfile.key"

# PH_AIGROW - Compute the curve of growth using the data from the input
# photometry files, optional airmasses from the obsparams file, and the
# initial values of the parameters using interactive graphics.

procedure ph_aigrow (gd, apcat, magfd, logfd, mgd, imtable, id, x, y, nap,
	rap, mag, merr, naperts, params, lterms, smallap, largeap)

pointer	gd			# pointer to the graphics descriptor
int	apcat			# the aperture correction file descriptor
int	magfd			# the best magnitudes file descriptor
int	logfd			# the output log file descriptor
pointer	mgd			# pointer to the metacode graphics stream
pointer	imtable			# pointer to the image symbol table
int	id[ARB]			# the star ids
real	x[ARB]			# the star x coordinates
real	y[ARB]			# the star y coordinates
int	nap[ARB]		# the array of aperture numbers
real	rap[naperts,ARB]	# input array of aperture radii
real	mag[naperts,ARB]	# input array of magnitudes / differences
real	merr[naperts,ARB]	# input array of magnitude errors
int	naperts			# the number of input apertures
double	params[ARB]		# the initial values for the parameters
int	lterms			# the number of terms to be fit
int	smallap			# the small aperture number
int	largeap			# the large aperture number

int	k, nimages, newfit, fitok, wcs, key, isymbol, newgraph, newimage
int	graphtype, ndata
pointer	sp, sym, symbol, agr, cmd, inap
real	wx, wy
int	stnsymbols(), ph_amfit(), clgcur(), ph_audelete()
pointer	sthead(), stnext()

begin
	if (logfd != NULL)
	    call ph_logtitle (logfd, apcat)

	nimages = stnsymbols (imtable, 0)
	if (nimages <= 0) {
	    call printf ("Error: There is no input data to fit\n")
	    if (logfd != NULL)
	        call fprintf (logfd, "Error: There is no input data to fit\n")
	    return
	}

	# Allocate temporary working space
	call smark (sp)
	call salloc (sym, nimages, TY_POINTER)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Order the symbol table.
	symbol = sthead (imtable)
	ndata = 0
	do k = nimages, 1, -1 {
	    Memi[sym+k-1] = symbol
	    ndata = ndata + IMT_NENTRIES(symbol)
	    symbol = stnext (imtable, symbol)
	}

	# Allocate memory required for fitting.
	call ph_ameminit (agr, lterms, naperts)

	# Do the initial a parameter and seeing fit.
	if (ph_amfit (imtable, agr, nap, rap, mag, merr, naperts, params,
	    lterms) == ERR)
	    fitok = NO
	else
	    fitok = YES
	newfit = NO

	# Define the current image to be the first image and evaluate the fit.
	symbol = Memi[sym]
	isymbol = 1
	call ph_a1eval (agr, IMT_IMNAME(symbol), IMT_RO(symbol),
	    IMT_NXAIRMASS(symbol), nap[IMT_OFFSET(symbol)],
	    rap[1,IMT_OFFSET(symbol)], mag[1,IMT_OFFSET(symbol)],
	    merr[1,IMT_OFFSET(symbol)], naperts, IMT_NENTRIES(symbol))
	newimage = NO

	# Plot the initial graph.
	call ph_gimfit (gd, agr, IMT_IMNAME(symbol), IMT_RO(symbol),
	    IMT_XAIRMASS(symbol), nap[IMT_OFFSET(symbol)],
	    nap[IMT_OFFSET(symbol)], rap[1,IMT_OFFSET(symbol)],
	    mag[1,IMT_OFFSET(symbol)], naperts, IMT_NENTRIES(symbol), fitok)
	graphtype = AGR_FIT
	newgraph = NO

	# Print the status report.
	call ph_aeprint (agr, IMT_IMNAME(symbol), IMT_RO(symbol),
	    rap[1,IMT_OFFSET(symbol)], naperts, smallap, largeap, fitok,
	    newfit)

	# Allocate the rejection array
	call malloc (inap, ndata, TY_INT)
	call amovi (nap, Memi[inap], ndata)

	# Examine the fit.
	while (clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    switch (key) {

	    # Quit the program.
	    case 'q':
		break

	    # Print help
	    case '?':
		call gpagefile (gd, HELPFILE, "")

	    # Execute colon commands.
	    case ':':
		call ph_acolon (gd, imtable, agr, symbol, isymbol, params,
		    lterms, naperts, smallap, largeap, Memc[cmd], fitok, newfit,
		    newimage, newgraph)

	    # Compute a new fit.
	    case 'f':
	        if (newfit == YES) {
		    call ph_amemfree (agr)
		    call ph_ameminit (agr, lterms, naperts)
	        }
	        if (ph_amfit (imtable, agr, Memi[inap], rap, mag, merr, naperts,
		    params, lterms) == ERR)
	    	    fitok = NO
		else
	    	    fitok = YES
		newfit = NO
		newimage = YES
		newgraph = YES
		isymbol = 1
		symbol = Memi[sym+isymbol-1]
		

	    # Print out the id of a particular object
	    case 'c':
		call ph_pnearest (gd, graphtype, wx, wy, agr,
		    id[IMT_OFFSET(symbol)], x[IMT_OFFSET(symbol)],
		    y[IMT_OFFSET(symbol)], Memi[inap+IMT_OFFSET(symbol)-1],
		    nap[IMT_OFFSET(symbol)], rap[1,IMT_OFFSET(symbol)],
		    mag[1,IMT_OFFSET(symbol)], naperts, IMT_NENTRIES(symbol),
		    smallap, largeap)

	    # Write the answer for the current image.
	    case 'w':
		call ph_aeprint (agr, IMT_IMNAME(symbol), IMT_RO(symbol),
		    rap[1,IMT_OFFSET(symbol)], naperts, smallap, largeap,
		    fitok, newfit)

	    # Delete points.
	    case 'd':
		if (ph_audelete (gd, graphtype, wx, wy, agr,
		    x[IMT_OFFSET(symbol)], y[IMT_OFFSET(symbol)],
		    Memi[inap+IMT_OFFSET(symbol)-1], nap[IMT_OFFSET(symbol)],
		    rap[1,IMT_OFFSET(symbol)], mag[1,IMT_OFFSET(symbol)],
		    naperts, IMT_NENTRIES(symbol), smallap, largeap, YES) ==
		    YES)
		    newfit = YES

	    # Undelete points.
	    case 'u':
		if (ph_audelete (gd, graphtype, wx, wy, agr,
		    x[IMT_OFFSET(symbol)], y[IMT_OFFSET(symbol)],
		    Memi[inap+IMT_OFFSET(symbol)-1], nap[IMT_OFFSET(symbol)],
		    rap[1,IMT_OFFSET(symbol)], mag[1,IMT_OFFSET(symbol)],
		    naperts, IMT_NENTRIES(symbol), smallap, largeap, NO) ==
		    YES)
		    newfit = YES

	    # Redraw the graph.
	    case 'g':
		newgraph = YES

	    # Graph the model fit.
	    case 'm':
		if (graphtype != AGR_FIT) {
		    graphtype = AGR_FIT
		    newgraph = YES
		}

	    # Graph the resdiduals from the adopted model as a function of
	    # aperture.
	    case 'r':
		if (fitok == YES && ! IS_INDEFR(IMT_RO(symbol)) &&
		    graphtype != AGR_ARESIDUALS) {
		    graphtype = AGR_ARESIDUALS
		    newgraph = YES
		}

	    # Graph the residuals from the adopted model as a function of
	    # magnitude.
	    case 'b':
		if (fitok == YES && ! IS_INDEFR(IMT_RO(symbol)) &&
		    graphtype != AGR_BRESIDUALS) {
		    graphtype = AGR_BRESIDUALS
		    newgraph = YES
		}

	    # Graph the residuals as a fucntion of x.
	    case 'x':
		if (fitok == YES && ! IS_INDEFR(IMT_RO(symbol)) &&
		    graphtype != AGR_XRESIDUALS) {
		    graphtype = AGR_XRESIDUALS
		    newgraph = YES
		}

	    # Graph the residuals as a function of y.
	    case 'y':
		if (fitok == YES && ! IS_INDEFR(IMT_RO(symbol)) &&
		    graphtype != AGR_YRESIDUALS) {
		    graphtype = AGR_YRESIDUALS
		    newgraph = YES
		}

	    # Graph the cumulative aperture correction.
	    case 'a':
		if (fitok == YES && ! IS_INDEFR(IMT_RO(symbol)) &&
		    graphtype != AGR_CUMULATIVE) {
		    graphtype = AGR_CUMULATIVE
		    newgraph = YES
		}

	    # Move to the previous image.
	    case 'p':
		if (isymbol == 1)
		    call printf ("Already at beginning of image list\n")
		else {
		    isymbol = isymbol - 1
		    symbol = Memi[sym+isymbol-1]
		    newimage = YES
		    newgraph = YES
		}

	    # Move to the next image.
	    case 'n':
		if (isymbol == nimages)
		    call printf ("Already at end of image list\n")
		else {
		    isymbol = isymbol + 1
		    symbol = Memi[sym+isymbol-1]
		    newimage = YES
		    newgraph = YES
		}
	    }

	    if (newimage == YES) {
	        call ph_a1eval (agr, IMT_IMNAME(symbol), IMT_RO(symbol),
	            IMT_NXAIRMASS(symbol), Memi[inap+IMT_OFFSET(symbol)-1],
	    	    rap[1,IMT_OFFSET(symbol)], mag[1,IMT_OFFSET(symbol)],
	    	    merr[1,IMT_OFFSET(symbol)], naperts, IMT_NENTRIES(symbol))
		newimage = NO
	    }

	    if (newgraph == YES) {
		switch (graphtype) {
		case AGR_FIT:
		    call ph_gimfit (gd, agr, IMT_IMNAME(symbol), IMT_RO(symbol),
	                IMT_XAIRMASS(symbol), Memi[inap+IMT_OFFSET(symbol)-1],
			nap[IMT_OFFSET(symbol)], rap[1,IMT_OFFSET(symbol)],
			mag[1,IMT_OFFSET(symbol)], naperts,
			IMT_NENTRIES(symbol), fitok)
		case AGR_ARESIDUALS:
		    call ph_gaimres (gd, agr, IMT_IMNAME(symbol),
		        IMT_RO(symbol), IMT_XAIRMASS(symbol),
			Memi[inap+IMT_OFFSET(symbol)-1],
			nap[IMT_OFFSET(symbol)], rap[1,IMT_OFFSET(symbol)],
			mag[1,IMT_OFFSET(symbol)], naperts,
			IMT_NENTRIES(symbol), fitok)
		case AGR_BRESIDUALS:
		    call ph_gbimres (gd, agr, IMT_IMNAME(symbol),
		        IMT_RO(symbol), IMT_XAIRMASS(symbol),
			Memi[inap+IMT_OFFSET(symbol)-1],
			nap[IMT_OFFSET(symbol)], mag[1,IMT_OFFSET(symbol)],
			naperts, IMT_NENTRIES(symbol), fitok)
		case AGR_XRESIDUALS:
		    call ph_gaximres (gd, agr, IMT_IMNAME(symbol),
		        IMT_RO(symbol), IMT_XAIRMASS(symbol),
			Memi[inap+IMT_OFFSET(symbol)-1],
			nap[IMT_OFFSET(symbol)], x[IMT_OFFSET(symbol)],
			mag[1,IMT_OFFSET(symbol)], naperts,
			IMT_NENTRIES(symbol), fitok)
		case AGR_YRESIDUALS:
		    call ph_gayimres (gd, agr, IMT_IMNAME(symbol),
		        IMT_RO(symbol), IMT_XAIRMASS(symbol),
			Memi[inap+IMT_OFFSET(symbol)-1],
			nap[IMT_OFFSET(symbol)], y[IMT_OFFSET(symbol)],
			mag[1,IMT_OFFSET(symbol)], naperts,
			IMT_NENTRIES(symbol), fitok)
		case AGR_CUMULATIVE:
		    call ph_gacum (gd, agr, IMT_IMNAME(symbol),
		        IMT_RO(symbol), IMT_XAIRMASS(symbol),
			Memi[inap+IMT_OFFSET(symbol)-1],
			nap[IMT_OFFSET(symbol)], rap[1,IMT_OFFSET(symbol)],
			mag[1,IMT_OFFSET(symbol)], naperts,
			IMT_NENTRIES(symbol), smallap, largeap, fitok)
		default:
		    call ph_gimfit (gd, agr, IMT_IMNAME(symbol), IMT_RO(symbol),
	                IMT_XAIRMASS(symbol), Memi[inap+IMT_OFFSET(symbol)-1],
			nap[IMT_OFFSET(symbol)], rap[1,IMT_OFFSET(symbol)],
			mag[1,IMT_OFFSET(symbol)], naperts,
			IMT_NENTRIES(symbol), fitok)
		}
		call ph_aeprint (agr, IMT_IMNAME(symbol), IMT_RO(symbol),
		    rap[1,IMT_OFFSET(symbol)], naperts, smallap, largeap,
		    fitok, newfit)
		newgraph = NO
	    }
	}

	# Output the results.

	# Fit the seeing radii and the cog model parameters.
	if (fitok == NO) {
	    call printf ("Error: The cog model fit did not converge\n")
	    if (logfd != NULL) {
		call ph_ishow (logfd, params, lterms)
	        call ph_pshow (logfd, agr, lterms)
	        call ph_rshow (logfd, imtable)
	        call fprintf (logfd,
		    "Error: The cog model fit did not converge\n")
	    }
	} else {
	    if (newfit == YES) {
	        call printf ("Warning: The cog model fit is out-of-date\n")
	        if (logfd != NULL)
	            call fprintf (logfd,
		        "Warning: The cog model fit is out-of-date\n")
	    }
	    if (logfd != NULL) {
		call ph_ishow (logfd, params, lterms)
	        call ph_pshow (logfd, agr, lterms)
	        call ph_rshow (logfd, imtable)
	    }
	    call ph_aeval (imtable, agr, apcat, magfd, logfd, mgd, id, x, y,
	        Memi[inap], rap, mag, merr, naperts, smallap, largeap)
	    if (logfd != NULL)
		call ph_tfshow (logfd, agr, naperts)
	}

	# Free memory required for fitting.
	call ph_amemfree (agr)

	call mfree (inap, TY_INT)
	call sfree (sp)
end


# PH_GIMFIT -- Graph the data and fit for a particular image.

procedure ph_gimfit (gd, agr, image, r0, xairmass, inap, nap, rap, mag, naperts,
	npts, fitok)

pointer	gd			# pointer to the graphics descriptor
pointer	agr			# pointer to the fit structure
char	image[ARB]		# the image name
real	r0			# the seeing radius
real	xairmass		# the airmass of the image
int	inap[ARB]		# the array of good aperture numbers
int	nap[ARB]		# the array of aperture numbers
real	rap[naperts,ARB]	# input array of aperture radii
real	mag[naperts,ARB]	# input array of magnitudes / differences
int	naperts			# the number of input apertures
int	npts			# the number of points
int	fitok			# is the fit ok

int	i, j
pointer	sp, title
real	rmin, rmax, dr, mmin, mmax, dm
int	strlen()
real	ph_achi()

begin
	if (gd == NULL)
	    return

	# Allocate working space.
	call smark (sp)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)

	# Compute the data window in x.
	call alimr (rap, naperts * npts, rmin, rmax)
	dr = rmax - rmin
	rmin = rmin - 0.1 * dr
	rmax = rmax + 0.1 * dr

	# Compute the data window in y.
	mmin = MAX_REAL
	mmax = -MAX_REAL
	do i = 1, npts {
	    do j = 2, nap[i] {
		if (mag[j,i] < mmin)
		    mmin = mag[j,i]
		if (mag[j,i] > mmax)
		    mmax = mag[j,i]
	    }
	}
	if (mmin > mmax) {
	    mmin = -0.1
	    mmax = 0.1
	} else {
	    dm = mmax - mmin
	    mmin = mmin - 0.1 * dm
	    mmax = mmax + 0.1 * dm
	}

	# Clear the screen and set the data window.
	call gclear (gd)
	call gswind (gd, rmin, rmax, mmin, mmax)

	# Set up the title and the axis labels.
	call sysid (Memc[title], 2 * SZ_LINE)
	call sprintf (Memc[title+strlen(Memc[title])],
	    2 * SZ_LINE - strlen(Memc[title]),
	    "\nImage: %s  Ro: %.3f  X: %.3f  Rms: %.3g")
	    call pargstr (image)
	    call pargr (r0)
	    call pargr (xairmass)
	    if (fitok == YES && ! IS_INDEFR(r0))
	        call pargr (sqrt (ph_achi (Memr[AGR_WR(agr)],
		    Memr[AGR_RESID(agr)], Memr[AGR_RESSQ(agr)], naperts)))
	    else
		call pargr (INDEFR)
	if (fitok == YES && ! IS_INDEFR(r0))  {
	    call sprintf (Memc[title+strlen(Memc[title])],
	        2 * SZ_LINE - strlen (Memc[title]),
	        "\nDashed line: theoretical cog   ")
	    call sprintf (Memc[title+strlen(Memc[title])],
	        2 * SZ_LINE - strlen (Memc[title]), "Solid line: adopted cog")
	}
	call glabax (gd, Memc[title], "Aperture Radius", "Curve of Growth")

	# Draw the fitted and rejected data.
	do i = 1, npts { 
	    call gpmark (gd, Memr[AGR_RBAR(agr)+1], mag[2,i], inap[i] - 1,
	        GM_PLUS, 2.0, 2.0)
	    if (nap[i] > inap[i])
	        call gpmark (gd, Memr[AGR_RBAR(agr)+inap[i]],
		    mag[inap[i]+1,i], nap[i] - inap[i], GM_CROSS, 2.0, 2.0)
	}

	# Plot the theoretical and adopt models.
	if (fitok == YES && ! IS_INDEFR(r0)) {
	    call gseti (gd, G_PLTYPE, GL_DASHED)
	    call gpline (gd, Memr[AGR_RBAR(agr)+1], Memr[AGR_THEO(agr)+1],
		naperts - 1)
	    call gseti (gd, G_PLTYPE, GL_SOLID)
	    call gpline (gd, Memr[AGR_RBAR(agr)+1], Memr[AGR_ADOPT(agr)+1],
		naperts - 1)
	}

	call gflush (gd)
	call sfree (sp)
end


# PH_GAIMRES -- Graph the residuals from the adopted model for a particular
# image.

procedure ph_gaimres (gd, agr, image, r0, xairmass, inap, nap, rap, mag,
	naperts, npts, fitok)

pointer	gd			# pointer to the graphics descriptor
pointer	agr			# pointer to the fit structure
char	image[ARB]		# the image name
real	r0			# the seeing radius
real	xairmass		# the airmass of the image
int	inap[ARB]		# the array of good aperture numbers
int	nap[ARB]		# the array of aperture numbers
real	rap[naperts,ARB]	# input array of aperture radii
real	mag[naperts,ARB]	# input array of magnitudes / differences
int	naperts			# the number of input apertures
int	npts			# the number of points
int	fitok			# is the fit ok

int	i
pointer	sp, title, diff
real	rmin, rmax, dr, dmin, dmax, ddmin, ddmax, dd
int	strlen()
real	ph_achi()

begin
	call smark (sp)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (diff, naperts, TY_REAL)

	# Set the data window.
	call alimr (rap, naperts * npts, rmin, rmax)
	dr = rmax - rmin
	rmin = rmin - 0.1 * dr
	rmax = rmax + 0.1 * dr

	dmin = MAX_REAL
	dmax = -MAX_REAL
	do i = 1, npts {
	    call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[diff+1],
	        nap[i] - 1)
	    call alimr (Memr[diff+1], nap[i] - 1, ddmin, ddmax)
	    if (ddmin < dmin)
		dmin = ddmin
	    if (ddmax > dmax)
		dmax = ddmax
	}
	if (dmin > dmax) {
	    dmin = -0.1
	    dmax = 0.1
	} else {
	    dd = dmax - dmin
	    dmin = dmin - 0.1 * dd
	    dmax = dmax + 0.1 * dd
	}

	call gclear (gd)
	call gswind (gd, rmin, rmax, dmin, dmax)

	# Set up the axes and the axis labels.
	call sysid (Memc[title], 2 * SZ_LINE)
	call sprintf (Memc[title+strlen(Memc[title])], 2 * SZ_LINE -
	    strlen(Memc[title]), "\nImage: %s  Ro: %.3f  X: %.3f  Rms: %.3g\n")
	    call pargstr (image)
	    call pargr (r0)
	    call pargr (xairmass)
	    call pargr (sqrt (ph_achi (Memr[AGR_WR(agr)], Memr[AGR_RESID(agr)],
	        Memr[AGR_RESSQ(agr)], naperts)))
	call glabax (gd, Memc[title], "Aperture Radius", "Residuals")

	# Draw the data.
	do i = 1, npts { 
	    call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[diff+1],
	        nap[i] - 1)
	    call gpmark (gd, Memr[AGR_RBAR(agr)+1], Memr[diff+1], inap[i] - 1,
	        GM_PLUS, 2.0, 2.0)
	    if (nap[i] > inap[i])
	        call gpmark (gd, Memr[AGR_RBAR(agr)+inap[i]],
		    Memr[diff+inap[i]], nap[i] - inap[i], GM_CROSS, 2.0, 2.0)
	}

	call gseti (gd, G_PLTYPE, GL_SOLID)
	call gamove (gd, rmin, 0.0)
	call gadraw (gd, rmax, 0.0)
	call gpline (gd, Memr[AGR_RBAR(agr)+1], Memr[AGR_AVE(agr)+1],
	    naperts - 1)

	call gflush (gd)
	call sfree (sp)
end



# PH_GBIMRES -- Graph the residuals from the adopted model  for a particular
# image as a function of magnitude in the first aperture.

procedure ph_gbimres (gd, agr, image, r0, xairmass, inap, nap, mag,
	naperts, npts, fitok)

pointer	gd			# pointer to the graphics descriptor
pointer	agr			# pointer to the fit structure
char	image[ARB]		# the image name
real	r0			# the seeing radius
real	xairmass		# the airmass of the image
int	inap[ARB]		# the array of good aperture numbers
int	nap[ARB]		# the array of aperture numbers
real	mag[naperts,ARB]	# input array of magnitudes / differences
int	naperts			# the number of input apertures
int	npts			# the number of points
int	fitok			# is the fit ok

int	i, j
pointer	sp, title, diff
real	rmin, rmax, dr, dmin, dmax, ddmin, ddmax, dd
int	strlen()
real	ph_achi()

begin
	call smark (sp)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (diff, naperts, TY_REAL)

	# Set the data window.
	rmin = MAX_REAL
	rmax = -MAX_REAL
	do i = 1, npts {
	    if (mag[1,i] < rmin)
		rmin = mag[1,i]
	    if (mag[1,i] > rmax)
		rmax = mag[1,i]
	}
	if (rmin > rmax) {
	    rmin = -0.1
	    rmax = 0.1
	} else {
	    dr = rmax - rmin
	    rmin = rmin - 0.1 * dr
	    rmax = rmax + 0.1 * dr
	}

	dmin = MAX_REAL
	dmax = -MAX_REAL
	do i = 1, npts {
	    call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[diff+1],
	        nap[i] - 1)
	    call alimr (Memr[diff+1], nap[i] - 1, ddmin, ddmax)
	    if (ddmin < dmin)
		dmin = ddmin
	    if (ddmax > dmax)
		dmax = ddmax
	}
	if (dmin > dmax) {
	    dmin = -0.1
	    dmax = 0.1
	} else {
	    dd = dmax - dmin
	    dmin = dmin - 0.1 * dd
	    dmax = dmax + 0.1 * dd
	}

	call gclear (gd)
	call gswind (gd, rmin, rmax, dmin, dmax)

	# Set up the axes and the axis labels.
	call sysid (Memc[title], 2 * SZ_LINE)
	call sprintf (Memc[title+strlen(Memc[title])], 2 * SZ_LINE -
	    strlen(Memc[title]), "\nImage: %s  Ro: %.3f  X: %.3f  Rms: %.3g\n")
	    call pargstr (image)
	    call pargr (r0)
	    call pargr (xairmass)
	    call pargr (sqrt (ph_achi (Memr[AGR_WR(agr)], Memr[AGR_RESID(agr)],
	        Memr[AGR_RESSQ(agr)], naperts)))
	call glabax (gd, Memc[title], "Magnitude", "Residuals")

	# Draw the data.
	do i = 1, npts { 
	    call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[diff+1],
	        nap[i] - 1)
	    do j = 2, inap[i]
	        call gmark (gd, mag[1,i], Memr[diff+j-1], GM_PLUS, 2.0, 2.0)
	    do j = inap[i] + 1, nap[i]
	        call gmark (gd, mag[1,i], Memr[diff+j-1], GM_CROSS, 2.0, 2.0)
	}

	call gseti (gd, G_PLTYPE, GL_SOLID)
	call gamove (gd, rmin, 0.0)
	call gadraw (gd, rmax, 0.0)

	call gflush (gd)
	call sfree (sp)
end


# PH_GAXIMRES -- Graph the residuals from the adopted model  for a particular
# image.

procedure ph_gaximres (gd, agr, image, r0, xairmass, inap, nap, x, mag,
	naperts, npts, fitok)

pointer	gd			# pointer to the graphics descriptor
pointer	agr			# pointer to the fit structure
char	image[ARB]		# the image name
real	r0			# the seeing radius
real	xairmass		# the airmass of the image
int	inap[ARB]		# the array of good aperture numbers
int	nap[ARB]		# the array of aperture numbers
real	x[ARB]			# the array of x values
real	mag[naperts,ARB]	# input array of magnitudes / differences
int	naperts			# the number of input apertures
int	npts			# the number of points
int	fitok			# is the fit ok

int	i, j
pointer	sp, title, diff
real	xmin, xmax, dx, dmin, dmax, ddmin, ddmax, dd
int	strlen()
real	ph_achi()

begin
	call smark (sp)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (diff, naperts, TY_REAL)

	# Set the x data window.
	call alimr (x, npts, xmin, xmax)
	dx = xmax - xmin
	xmin = xmin - 0.1 * dx
	xmax = xmax + 0.1 * dx

	# Set the y data window.
	dmin = MAX_REAL
	dmax = -MAX_REAL
	do i = 1, npts {
	    call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[diff+1],
	        nap[i] - 1)
	    call alimr (Memr[diff+1], nap[i] - 1, ddmin, ddmax)
	    if (ddmin < dmin)
		dmin = ddmin
	    if (ddmax > dmax)
		dmax = ddmax
	}
	if (dmin > dmax) {
	    dmin = -0.1
	    dmax = 0.1
	} else {
	    dd = dmax - dmin
	    dmin = dmin - 0.1 * dd
	    dmax = dmax + 0.1 * dd
	}

	# Initialize the plot.
	call gclear (gd)
	call gswind (gd, xmin, xmax, dmin, dmax)

	# Set up the axes and the axis labels.
	call sysid (Memc[title], 2 * SZ_LINE)
	call sprintf (Memc[title+strlen(Memc[title])], 2 * SZ_LINE -
	    strlen(Memc[title]), "\nImage: %s  Ro: %.3f  X: %.3f  Rms: %.3g\n")
	    call pargstr (image)
	    call pargr (r0)
	    call pargr (xairmass)
	    call pargr (sqrt (ph_achi (Memr[AGR_WR(agr)], Memr[AGR_RESID(agr)],
	        Memr[AGR_RESSQ(agr)], naperts)))
	call glabax (gd, Memc[title], "X Coordinate", "Residuals")

	# Draw the data.
	do i = 1, npts { 
	    call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[diff+1],
	        nap[i] - 1)
	    do j = 2, inap[i]
	        call gmark (gd, x[i], Memr[diff+j-1], GM_PLUS, 2.0, 2.0)
	    do j = inap[i] + 1, nap[i]
	        call gmark (gd, x[i], Memr[diff+j-1], GM_CROSS, 2.0, 2.0)
	}

	call gseti (gd, G_PLTYPE, GL_SOLID)
	call gamove (gd, xmin, 0.0)
	call gadraw (gd, xmax, 0.0)

	call gflush (gd)
	call sfree (sp)
end


# PH_GAYIMRES -- Graph the residuals from the adopted model  for a particular
# image.

procedure ph_gayimres (gd, agr, image, r0, xairmass, inap, nap, y, mag,
	naperts, npts, fitok)

pointer	gd			# pointer to the graphics descriptor
pointer	agr			# pointer to the fit structure
char	image[ARB]		# the image name
real	r0			# the seeing radius
real	xairmass		# the airmass of the image
int	inap[ARB]		# the array of good aperture numbers
int	nap[ARB]		# the array of aperture numbers
real	y[ARB]			# the array of y values
real	mag[naperts,ARB]	# input array of magnitudes / differences
int	naperts			# the number of input apertures
int	npts			# the number of points
int	fitok			# is the fit ok

int	i, j
pointer	sp, title, diff
real	ymin, ymax, dy, dmin, dmax, ddmin, ddmax, dd
int	strlen()
real	ph_achi()

begin
	call smark (sp)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (diff, naperts, TY_REAL)

	# Set the y data window.
	call alimr (y, npts, ymin, ymax)
	dy = ymax - ymin
	ymin = ymin - 0.1 * dy
	ymax = ymax + 0.1 * dy

	# Set the y data window.
	dmin = MAX_REAL
	dmax = -MAX_REAL
	do i = 1, npts {
	    call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[diff+1],
	        nap[i] - 1)
	    call alimr (Memr[diff+1], nap[i] - 1, ddmin, ddmax)
	    if (ddmin < dmin)
		dmin = ddmin
	    if (ddmax > dmax)
		dmax = ddmax
	}
	if (dmin > dmax) {
	    dmin = -0.1
	    dmax = 0.1
	} else {
	    dd = dmax - dmin
	    dmin = dmin - 0.1 * dd
	    dmax = dmax + 0.1 * dd
	}

	# Initialize the plot.
	call gclear (gd)
	call gswind (gd, ymin, ymax, dmin, dmax)

	# Set up the axes and the axis labels.
	call sysid (Memc[title], 2 * SZ_LINE)
	call sprintf (Memc[title+strlen(Memc[title])], 2 * SZ_LINE -
	    strlen(Memc[title]), "\nImage: %s  Ro: %.3f  X: %.3f  Rms: %.3g\n")
	    call pargstr (image)
	    call pargr (r0)
	    call pargr (xairmass)
	    call pargr (sqrt (ph_achi (Memr[AGR_WR(agr)], Memr[AGR_RESID(agr)],
	        Memr[AGR_RESSQ(agr)], naperts)))
	call glabax (gd, Memc[title], "Y Coordinate", "Residuals")

	# Draw the data.
	do i = 1, npts { 
	    call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[diff+1],
	        nap[i] - 1)
	    do j = 2, inap[i]
	        call gmark (gd, y[i], Memr[diff+j-1], GM_PLUS, 2.0, 2.0)
	    do j = inap[i] + 1, nap[i]
	        call gmark (gd, y[i], Memr[diff+j-1], GM_CROSS, 2.0, 2.0)
	}

	call gseti (gd, G_PLTYPE, GL_SOLID)
	call gamove (gd, ymin, 0.0)
	call gadraw (gd, ymax, 0.0)

	call gflush (gd)
	call sfree (sp)
end


# PH_GACUM -- Plot the cumulative profile.

procedure ph_gacum (gd, agr, image, r0, xairmass, inap, nap, rap, mag, naperts,
	npts, smallap, largeap, fitok)

pointer	gd			# pointer to the graphics descriptor
pointer	agr			# pointer to the fit structure
char	image[ARB]		# the image name
real	r0			# the seeing radius
real	xairmass		# the airmass of the image
int	inap[ARB]		# the array of good number apertures
int	nap[ARB]		# the array of number of apertures
real	rap[ARB]		# input array of aperture radii
real	mag[naperts,ARB]	# the array of magnitudes
int	naperts			# the number of input apertures
int	npts			# the number of points
int	smallap			# small aperture number
int	largeap			# large aperture number
int	fitok			# is the fit ok

int	i, j
pointer	sp, title, cmags, ctheo, cadopt, caderr, cptr
real	rmin, rmax, dr, dmin, dmax, ddmin, ddmax, dd
int	strlen()
real	ph_achi()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (cmags, (naperts + 1) * npts, TY_REAL)
	call salloc (ctheo, naperts + 1, TY_REAL)
	call salloc (cadopt, naperts + 1, TY_REAL)
	call salloc (caderr, naperts + 1, TY_REAL)

	# Compute the data.
	call aclrr (Memr[cmags], (naperts + 1) * npts)
	cptr = cmags
	do j = 1, npts {
	    do i = largeap, 2, -1 {
		if (i > nap[j])
	            Memr[cptr+i-1] = Memr[AGR_ADOPT(agr)+i-1] + Memr[cptr+i]
		else
	            Memr[cptr+i-1] = mag[i,j] + Memr[cptr+i]
	    }
	    cptr = cptr + naperts + 1
	}

	# Compute the model.
	call aclrr (Memr[ctheo], naperts + 1)
	call aclrr (Memr[cadopt], naperts + 1)
	call aclrr (Memr[caderr], naperts + 1)
	do i = largeap, 2, -1 {
	    Memr[ctheo+i-1] = Memr[AGR_THEO(agr)+i-1] + Memr[ctheo+i]
	    Memr[cadopt+i-1] = Memr[AGR_ADOPT(agr)+i-1] + Memr[cadopt+i]
	    Memr[caderr+i-1] = Memr[AGR_WADO(agr)+i-1] ** 2 + Memr[caderr+i]
	}

	# Set the x data window.
	call alimr (rap, naperts, rmin, rmax)
	dr = rmax - rmin
	rmin = rmin - 0.1 * dr
	rmax = rmax + 0.1 * dr

	# Set the y data window.
	dmin = MAX_REAL
	dmax = -MAX_REAL
	call alimr (Memr[cmags], (naperts + 1) * npts, ddmin, ddmax)
	if (ddmin < dmin)
	    dmin = ddmin
	if (ddmax > dmax)
	    dmax = ddmax
	dd = dmax - dmin
	dmin = dmin - 0.1 * dd
	dmax = dmax + 0.1 * dd

	call gclear (gd)
	call gswind (gd, rmin, rmax, dmax, dmin)

	# Set up the title and the axis labels.
	call sysid (Memc[title], 2 * SZ_LINE)
	call sprintf (Memc[title+strlen(Memc[title])], 2 * SZ_LINE -
	    strlen(Memc[title]), "\nImage: %s  Ro: %.3f  X: %.3f  Rms: %.3g")
	    call pargstr (image)
	    call pargr (r0)
	    call pargr (xairmass)
	    call pargr (sqrt (ph_achi (Memr[AGR_WR(agr)], Memr[AGR_RESID(agr)],
	        Memr[AGR_RESSQ(agr)], naperts)))
	call sprintf (Memc[title+strlen(Memc[title])], 2 * SZ_LINE -
	    strlen (Memc[title]), "\nDashed line: theoretical apcor  ")
	call sprintf (Memc[title+strlen(Memc[title])], 2 * SZ_LINE -
	    strlen (Memc[title]), "Solid line: adopted apcor")
	call glabax (gd, Memc[title], "Aperture Radius", "Aperture Correction")

	# Draw the data.
	cptr = cmags
	do j = 1, npts {
	    if (largeap < inap[j])
	        call gpmark (gd, rap, Memr[cptr+1], largeap - 1, GM_PLUS,
		    2.0, 2.0)
	    else
	        call gpmark (gd, rap, Memr[cptr+1], inap[j] - 1, GM_PLUS,
		    2.0, 2.0)
	    if (largeap > inap[j])
	        call gpmark (gd, rap[inap[j]], Memr[cptr+inap[j]], 
		    largeap - inap[j], GM_CROSS, 2.0, 2.0)
	    cptr = cptr + naperts + 1
	}

	# Draw the zero correction line.
	call gamove (gd, rmin, 0.0)
	call gadraw (gd, rmax, 0.0)

	# Draw the aperture limits.
	if (smallap > rap[1]) {
	    call gseti (gd, G_PLTYPE, GL_DASHED)
	    call gamove (gd, rap[1], dmin)
	    call gadraw (gd, rap[1], dmax)
	    call gseti (gd, G_PLTYPE, GL_SOLID)
	}
	call gamove (gd, rap[smallap], dmin)
	call gadraw (gd, rap[smallap], dmax)
	call gamove (gd, rap[largeap], dmin)
	call gadraw (gd, rap[largeap], dmax)
	if (rap[naperts] > largeap) {
	    call gseti (gd, G_PLTYPE, GL_DASHED)
	    call gamove (gd, rap[naperts], dmin)
	    call gadraw (gd, rap[naperts], dmax)
	    call gseti (gd, G_PLTYPE, GL_SOLID)
	}

	# Draw the model.
	call gseti (gd, G_PLTYPE, GL_DASHED)
	call gpline (gd, rap, Memr[ctheo+1], naperts)
	call gseti (gd, G_PLTYPE, GL_SOLID)
	call gpline (gd, rap, Memr[cadopt+1], naperts)

	call gflush (gd)
	call sfree (sp)
end


# PH_PNEAREST -- Print basic information for the nearest object

procedure ph_pnearest (gd, graphtype, wx, wy, agr, id, x, y, inap, nap, rap,
	mag, naperts, npts, smallap, largeap)

pointer	gd			# pointer to the graphics stream
int	graphtype		# the current graphtype
real	wx, wy			# the coordinates of the point to be un/deleted
pointer	agr			# pointer to the fitting structure
int	id[ARB]			# array of star ids
real	x[ARB]			# the x coordinates
real	y[ARB]			# the y coordinates
int	inap[ARB]		# the number of good apertures
int	nap[ARB]		# the number of measured apertures
real	rap[naperts,ARB]	# the list of aperture radii
real	mag[naperts,ARB]	# the list of magnitude differences
int	naperts			# the number of apertures
int	npts			# the number of points
int	smallap			# the small aperture number
int	largeap			# the large aperture number

int	i, j
pointer	sp, xin, yin, iap, inpts, index
real	r2min, r2, xc, yc
real	ph_nearest()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (xin, naperts + 1, TY_REAL)
	call salloc (yin, naperts + 1, TY_REAL)

	# Initialize.
	r2min = MAX_REAL
	iap = 0
	inpts = 0

	# Find the nearest point.
	switch (graphtype) {
	case AGR_FIT:

	    do i = 1, npts {
		r2 = ph_nearest (gd, wx, wy, Memr[AGR_RBAR(agr)+1], mag[2,i],
		    nap[i] - 1, nap[i] - 1, index, YES)
		if (r2 < r2min) {
		    r2min = r2
		    iap = index + 1
		    inpts = i
		    xc = Memr[AGR_RBAR(agr)+iap-1]
		    yc = mag[iap,inpts]
		}
	    }

	case AGR_ARESIDUALS:

	    do i = 1, npts {
		call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[yin+1],
		    nap[i] - 1)
		r2 = ph_nearest (gd, wx, wy, Memr[AGR_RBAR(agr)+1], Memr[yin+1],
		    nap[i] - 1, nap[i] - 1, index, YES)
		if (r2 < r2min) {
		    r2min = r2
		    iap = index + 1
		    inpts = i
		    xc = Memr[AGR_RBAR(agr)+iap-1]
		    yc = mag[iap,inpts] - Memr[AGR_ADOPT(agr)+iap-1]
		}
	    }

	case AGR_BRESIDUALS:
	    do i = 1, npts {
		call amovkr (mag[1,i], Memr[xin+1], nap[i] - 1)
		call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[yin+1],
		    nap[i] - 1)
		r2 = ph_nearest (gd, wx, wy, Memr[xin+1], Memr[yin+1],
		    nap[i] - 1, nap[i] - 1, index, YES)
		if (r2 < r2min) {
		    r2min = r2
		    iap = index + 1
		    inpts = i
		    xc = mag[1,inpts]
		    yc = mag[iap,inpts] - Memr[AGR_ADOPT(agr)+iap-1]
		}
	    }

	case AGR_XRESIDUALS:

	    do i = 1, npts {
		call amovkr (x[i], Memr[xin+1], nap[i] - 1)
		call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[yin+1],
		    nap[i] - 1)
		r2 = ph_nearest (gd, wx, wy, Memr[xin+1], Memr[yin+1],
		    nap[i] - 1, nap[i] - 1, index, YES)
		if (r2 < r2min) {
		    r2min = r2
		    iap = index + 1
		    inpts = i
		    xc = x[inpts]
		    yc = mag[iap,inpts] - Memr[AGR_ADOPT(agr)+iap-1]
		}
	    }

	case AGR_YRESIDUALS:

	    do i = 1, npts {
		call amovkr (y[i], Memr[xin+1], nap[i] - 1)
		call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[yin+1],
		    nap[i] - 1)
		r2 = ph_nearest (gd, wx, wy, Memr[xin+1], Memr[yin+1],
		    nap[i] - 1, nap[i] - 1, index, YES)
		if (r2 < r2min) {
		    r2min = r2
		    iap = index + 1
		    inpts = i
		    xc = y[inpts]
		    yc = mag[iap,inpts] - Memr[AGR_ADOPT(agr)+iap-1]
		}
	    }

	case AGR_CUMULATIVE:

	    do i = 1, npts {
		call aclrr (Memr[yin], naperts + 1)
	        do j = largeap, 2, -1 {
		    if (j > nap[i])
	                Memr[yin+j-1] = Memr[AGR_ADOPT(agr)+j-1] + Memr[yin+j]
		    else
	                Memr[yin+j-1] = mag[j,i] + Memr[yin+j]
		}
		r2 = ph_nearest (gd, wx, wy, rap[1,i], Memr[yin+1],
		    nap[i] - 1, nap[i] - 1, index, YES)
		if (r2 < r2min) {
		    r2min = r2
		    iap = index + 1
		    inpts = i
		    xc = rap[iap,inpts]
		    yc = Memr[yin+iap-1]
		}
	    }
	}

	if (iap != 0 && inpts != 0) {
	    switch (graphtype) {
	    case AGR_FIT:
		call printf (
		    "Star: %d x: %.3f y: %.3f radius: %.3f delta mag: %.3f\n")
		    call pargi (id[inpts])
		    call pargr (x[inpts])
		    call pargr (y[inpts])
		    call pargr (xc)
		    call pargr (yc)
	    case AGR_ARESIDUALS:
		call printf (
		    "Star: %d x: %.3f y: %.3f radius: %.3f residual: %.3f\n")
		    call pargi (id[inpts])
		    call pargr (x[inpts])
		    call pargr (y[inpts])
		    call pargr (xc)
		    call pargr (yc)
	    case AGR_BRESIDUALS:
		call printf (
		    "Star: %d x: %.3f y: %.3f mag[1]: %.3f residual: %.3f\n")
		    call pargi (id[inpts])
		    call pargr (x[inpts])
		    call pargr (y[inpts])
		    call pargr (xc)
		    call pargr (yc)
	    case AGR_XRESIDUALS:
		call printf (
		    "Star: %d x: %.3f y: %.3f x: %.3f residual: %.3f\n")
		    call pargi (id[inpts])
		    call pargr (x[inpts])
		    call pargr (y[inpts])
		    call pargr (xc)
		    call pargr (yc)
	    case AGR_YRESIDUALS:
		call printf (
		    "Star: %d x: %.3f y: %.3f y: %.3f residual: %.3f\n")
		    call pargi (id[inpts])
		    call pargr (x[inpts])
		    call pargr (y[inpts])
		    call pargr (xc)
		    call pargr (yc)
	    case AGR_CUMULATIVE:
		call printf (
		    "Star: %d x: %.3f y: %.3f y: %.3f apercor: %.3f\n")
		    call pargi (id[inpts])
		    call pargr (x[inpts])
		    call pargr (y[inpts])
		    call pargr (xc)
		    call pargr (yc)
	    }
	}

	call sfree (sp)
end


# PH_AUDELETE -- Delete or undelete points from the current graph.

int procedure ph_audelete (gd, graphtype, wx, wy, agr, x, y, inap, nap, rap,
	mag, naperts, npts, smallap, largeap, delete)

pointer	gd			# pointer to the graphics stream
int	graphtype		# the current graphtype
real	wx, wy			# the coordinates of the point to be un/deleted
pointer	agr			# pointer to the fitting structure
real	x[ARB]			# the x coordinates
real	y[ARB]			# the y coordinates
int	inap[ARB]		# the number of good apertures
int	nap[ARB]		# the number of measured apertures
real	rap[naperts,ARB]	# the list of aperture radii
real	mag[naperts,ARB]	# the list of magnitude differences
int	naperts			# the number of apertures
int	npts			# the number of points
int	smallap			# the small aperture number
int	largeap			# the large aperture number
int	delete			# delete points ?, otherwise undelete

int	i, j, iap, inpts, index, stat
pointer	sp, xin, yin
real	r2min, r2
real	ph_nearest()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (xin, naperts + 1, TY_REAL)
	call salloc (yin, naperts + 1, TY_REAL)

	# Initialize.
	r2min = MAX_REAL
	iap = 0
	inpts = 0

	# Find the nearest point.
	switch (graphtype) {
	case AGR_FIT:

	    do i = 1, npts {
		r2 = ph_nearest (gd, wx, wy, Memr[AGR_RBAR(agr)+1], mag[2,i],
		    inap[i] - 1, nap[i] - 1, index, delete)
		if (r2 < r2min) {
		    r2min = r2
		    iap = index + 1
		    inpts = i
		}
	    }

	case AGR_ARESIDUALS:

	    do i = 1, npts {
		call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[yin+1],
		    nap[i] - 1)
		r2 = ph_nearest (gd, wx, wy, Memr[AGR_RBAR(agr)+1], Memr[yin+1],
		    inap[i] - 1, nap[i] - 1, index, delete)
		if (r2 < r2min) {
		    r2min = r2
		    iap = index + 1
		    inpts = i
		}
	    }

	case AGR_BRESIDUALS:
	    do i = 1, npts {
		call amovkr (mag[1,i], Memr[xin+1], nap[i] - 1)
		call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[yin+1],
		    nap[i] - 1)
		r2 = ph_nearest (gd, wx, wy, Memr[xin+1], Memr[yin+1],
		    inap[i] - 1, nap[i] - 1, index, delete)
		if (r2 < r2min) {
		    r2min = r2
		    iap = index + 1
		    inpts = i
		}
	    }

	case AGR_XRESIDUALS:

	    do i = 1, npts {
		call amovkr (x[i], Memr[xin+1], nap[i] - 1)
		call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[yin+1],
		    nap[i] - 1)
		r2 = ph_nearest (gd, wx, wy, Memr[xin+1], Memr[yin+1],
		    inap[i] - 1, nap[i] - 1, index, delete)
		if (r2 < r2min) {
		    r2min = r2
		    iap = index + 1
		    inpts = i
		}
	    }

	case AGR_YRESIDUALS:

	    do i = 1, npts {
		call amovkr (y[i], Memr[xin+1], nap[i] - 1)
		call asubr (mag[2,i], Memr[AGR_ADOPT(agr)+1], Memr[yin+1],
		    nap[i] - 1)
		r2 = ph_nearest (gd, wx, wy, Memr[xin+1], Memr[yin+1],
		    inap[i] - 1, nap[i] - 1, index, delete)
		if (r2 < r2min) {
		    r2min = r2
		    iap = index + 1
		    inpts = i
		}
	    }

	case AGR_CUMULATIVE:

	    do i = 1, npts {
		call aclrr (Memr[yin], naperts + 1)
	        do j = largeap, 2, -1 {
		    if (j > nap[i])
	                Memr[yin+j-1] = Memr[AGR_ADOPT(agr)+j-1] + Memr[yin+j]
		    else
	                Memr[yin+j-1] = mag[j,i] + Memr[yin+j]
		}
		r2 = ph_nearest (gd, wx, wy, rap[1,i], Memr[yin+1],
		    inap[i] - 1, nap[i] - 1, index, delete)
		if (r2 < r2min) {
		    r2min = r2
		    iap = index + 1
		    inpts = i
		}
	    }
	}

	# Delete or undelete the point and mark it.
	if (iap != 0 && inpts != 0) {

	    switch (graphtype) {
	    case AGR_FIT:
		call amovr (Memr[AGR_RBAR(agr)+1], Memr[xin+1], nap[inpts] - 1)
		call amovr (mag[2,inpts], Memr[yin+1], nap[inpts] - 1)
	    case AGR_ARESIDUALS:
		call amovr (Memr[AGR_RBAR(agr)+1], Memr[xin+1], nap[inpts] - 1)
		call asubr (mag[2,inpts], Memr[AGR_ADOPT(agr)+1], Memr[yin+1],
		    nap[inpts] - 1)
	    case AGR_BRESIDUALS:
		call amovkr (mag[1,inpts], Memr[xin+1], nap[inpts] - 1)
		call asubr (mag[2,inpts], Memr[AGR_ADOPT(agr)+1], Memr[yin+1],
		    nap[inpts] - 1)
	    case AGR_XRESIDUALS:
		call amovkr (x[inpts], Memr[xin+1], nap[inpts] - 1)
		call asubr (mag[2,inpts], Memr[AGR_ADOPT(agr)+1], Memr[yin+1],
		    nap[inpts] - 1)
	    case AGR_YRESIDUALS:
		call amovkr (y[inpts], Memr[xin+1], nap[inpts] - 1)
		call asubr (mag[2,inpts], Memr[AGR_ADOPT(agr)+1], Memr[yin+1],
		    nap[inpts] - 1)
	    case AGR_CUMULATIVE:
		call amovr (rap[1,inpts], Memr[xin+1], nap[inpts] - 1)
	        do j = largeap, 2, -1 {
		    if (j > nap[inpts])
	                Memr[yin+j-1] = Memr[AGR_ADOPT(agr)+j-1] + Memr[yin+j]
		    else
	                Memr[yin+j-1] = mag[j,inpts] + Memr[yin+j]
		}
	    }

	    if (delete == YES) {
		inap[inpts] = iap - 1
		do i = inap[inpts] + 1, nap[inpts] {
		    call gscur (gd, Memr[xin+i-1], Memr[yin+i-1])
		    call gmark (gd, Memr[xin+i-1], Memr[yin+i-1], GM_CROSS,
		        2.0, 2.0)
		}
	    } else {
		do i = inap[inpts]+1, iap {
		    call gscur (gd, Memr[xin+i-1], Memr[yin+i-1])
		    call gseti (gd, G_PMLTYPE, GL_CLEAR)
		    call gmark (gd, Memr[xin+i-1], Memr[yin+i-1], GM_CROSS,
		        2.0, 2.0)
		    call gseti (gd, G_PMLTYPE, GL_SOLID)
		    call gmark (gd, Memr[xin+i-1], Memr[yin+i-1], GM_PLUS,
		        2.0, 2.0)
		}
		inap[inpts] = iap
	    }

	    stat = YES

	} else
	    stat = NO

	call sfree (sp)
	return (stat)
end


# PH_AEPRINT -- Print the appropriate error message under the plot.

procedure ph_aeprint (agr, image, r0, rap, naperts, smallap, largeap, fitok,
	newfit)

pointer	agr		# pointer to the fitting structure
char	image[ARB]	# the image name
real	r0		# the seing rdius
real	rap[ARB]	# the lists of aperture radii
int	naperts		# the number of aperture radii
int	smallap		# the index of the small aperture radius
int	largeap		# the index of the large aperture radius
int	fitok		# did the fit converge ?
int	newfit		# is the fit out-of-date ?

begin
	if (fitok == NO) {
	    call printf ("Error: The cog model fit did not converge\n")
	} else if (newfit == YES) {
	    call printf ("Warning: The cog model fit is out-of-date\n")
	} else if (IS_INDEFR(r0)) {
	    call printf ("Warning: Unable to fit RO for image %s\n")
		call pargstr (image)
	} else {
	    call ph_papcor (STDOUT, image, r0, rap, Memr[AGR_ADOPT(agr)],
	        Memr[AGR_WADO(agr)], naperts, smallap, largeap)
	}
end


# PH_NEAREST -- Find the nearest data point to the coordinates.

real procedure ph_nearest (gd, wx, wy, x, y, inpts, npts, index, delete)

pointer	gd			# the graphics descriptor
real	wx, wy			# the cursor coordinates
real	x[ARB]			# the x values
real	y[ARB]			# the y values
int	inpts			# the number of good points
int	npts			# the number of points
int	index			# the index of the nearest point
int	delete			# delete or undelete points

int	i, ib, ie
real	r2min, r2, nwx, nwy, nx, ny

begin
	r2min = MAX_REAL
	index = 0
	if (delete == YES) {
	    ib = 1
	    ie = inpts
	} else {
	    ib = inpts + 1
	    ie = npts
	}
	call gctran (gd, wx, wy, nwx, nwy, 1, 0)

	do i = ib, ie {
	    call gctran (gd, x[i], y[i], nx, ny, 1, 0)
	    r2 = (nx - nwx) ** 2 + (ny - nwy) ** 2
	    if (r2 < r2min) {
		r2min = r2
		index = i
	    }
	}

	return (r2min)
end


# PH_ACOLON -- Execute the apfile colon commands.

procedure ph_acolon (gd, imtable, agr, symbol, isymbol, params, lterms,
	naperts, smallap, largeap, cmd, fitok, newfit, newimage, newgraph)

pointer	gd		# pointer to the graphics stream
pointer	imtable		# pointer to the image name symbol table
pointer	agr		# pointer to the fitting structure
pointer	symbol		# the current image symbol
int	isymbol		# the index of the current image symbol
double	params[ARB]	# the initial parameters
int	lterms		# the number of parameters to fit
int	naperts		# the number of apertures
int	smallap		# the small aperture number
int	largeap		# the large aperture number
char	cmd[ARB]	# the colon command
int	fitok		# is the fit ok
int	newfit		# compute a new fit
int	newimage	# evaluate the fit for a new image
int	newgraph	# plot a new graph

int	ncmd, ival
pointer	sp, incmd, outcmd, newsymbol
real	rval
int	strdic(), nscan(), stnsymbols()
pointer	stfind(), sthead(), stnext()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (incmd, SZ_LINE, TY_CHAR)
	call salloc (outcmd, SZ_LINE, TY_CHAR)

	# Get the command.
	call sscan (cmd)
	call gargwrd (Memc[incmd], SZ_LINE)
	if (Memc[incmd] == EOS) {
	    call sfree (sp)
	    return
	}
	ncmd = strdic (Memc[incmd], Memc[incmd], SZ_LINE, AGR_CMDS)

	switch (ncmd) {
	case AGR_CMD_SHOW:

	    call gargwrd (Memc[outcmd], SZ_LINE)
	    ncmd = strdic (Memc[outcmd], Memc[outcmd], SZ_LINE, AGR_SHOWCMDS)
	    switch (ncmd) {
	    case AGR_CMD_MODEL:
		if (fitok == NO) {
		    call printf ("Error: The cog model fit did not converge\n")
		} else {
		    if (newfit == YES)
		        call printf (
			    "Warning: The cog model fit is out-of-date\n")
		    call gdeactivate (gd, 0)
		    call ph_pshow (STDOUT, agr, lterms)
		    call greactivate (gd, 0)
		}
	    case AGR_CMD_SEEING:
		if (fitok == NO) {
		    call printf ("Error: The cog model fit did not converge\n")
		} else {
		    if (newfit == YES)
		        call printf (
			    "Warning: The cog model fit is out-of-date\n")
		    call gdeactivate (gd, 0)
		    call ph_rshow (STDOUT, imtable)
		    call greactivate (gd, 0)
		}
	    case AGR_CMD_PARAMETERS:
		call gdeactivate (gd, 0)
		call ph_ishow (STDOUT, params, lterms)
		call greactivate (gd, 0)
	    default:
		if (fitok == NO) {
		    call printf ("Error: The cog model fit did not converge\n")
		} else {
		    if (newfit == YES)
		        call printf (
			    "Warning: The cog model fit is out-of-date\n")
		    call gdeactivate (gd, 0)
		    call ph_pshow (STDOUT, agr, lterms)
		    call greactivate (gd, 0)
		}
	    }

	case AGR_CMD_IMAGE:
	    call gargwrd (Memc[outcmd], SZ_LINE)
	    if (nscan() == 1) {
		call printf ("Image: %s  R0: %7.3f  X: %5.3f\n")
		    call pargstr (IMT_IMNAME(symbol))
		    call pargr (IMT_RO(symbol))
		    call pargr (IMT_XAIRMASS(symbol))
	    } else {
		newsymbol = stfind (imtable, Memc[outcmd])
		if (newsymbol == NULL) {
		    call printf ("Image: %s not found in data\n")
		        call pargstr (Memc[outcmd])
		} else {
		    symbol = newsymbol
		    isymbol = 1
		    newsymbol = sthead (imtable)
		    while (newsymbol != NULL) {
			if (newsymbol == symbol)
			    break
			isymbol = isymbol + 1
			newsymbol = stnext (imtable, newsymbol)
		    }
		    isymbol = stnsymbols(imtable, 0) - isymbol + 1
		    newimage = YES
		    newgraph = YES
		}
	    }

	case AGR_CMD_MTERMS:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("nparams = %d\n")
		    call pargi (lterms)
	    } else {
		lterms = max (1, min (ival, MAX_MTERMS))
		newfit = YES
	    }

	case AGR_CMD_SWINGS:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("swings = %g\n")
		    call pargr (real(params[1]))
	    } else {
		params[1] = max (1.0, rval)
	    }

	case AGR_CMD_PWINGS:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("pwings = %g\n")
		    call pargr (real(params[2]))
	    } else {
		params[2] = max (0.0, min (1.0, rval))
	    }

	case AGR_CMD_PGAUSS:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("pgauss = %g\n")
		    call pargr (real(params[3]))
	    } else {
		params[3] = max (0.0, min (1.0, rval))
	    }

	case AGR_CMD_RGESCALE:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("rgescale = %g\n")
		    call pargr (real(params[4]))
	    } else {
		params[4] = max (0.0, rval)
	    }

	case AGR_CMD_XWINGS:
	    call gargr (rval)
	    if (nscan() == 1) {
		call printf ("xwings = %g\n")
		    call pargr (real(params[5]))
	    } else {
		params[5] = rval
	    }

	case AGR_CMD_SMALLAP:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("smallap = %d\n")
		    call pargi (smallap)
	    } else {
		smallap = max (1, min (ival, naperts))
	    }

	case AGR_CMD_LARGEAP:
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("largeap = %d\n")
		    call pargi (largeap)
	    } else {
		largeap = max (1, min (ival, naperts))
	    }

	default:
	    call printf ("Ambiguous or undefined command\7\n")
	}


	call sfree (sp)
end
