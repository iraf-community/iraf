include	<imhdr.h>
include	<math/curfit.h>
include	<gset.h>

define	MAX_NR_BEAMS	100	# Max number of instrument apertures

define	KEY		"noao$lib/scr/flatfit.key"
define	PROMPT		"flatfit cursor options"

# Definitions for Plotting modes
define	PLT_FIT	1		# Plot the direct fit
define	PLT_ERR	2		# Plot the errors in the fit
define	PLT_LIN	3		# Plot the fit minus the linear part

# T_FLATFIT -- Accumulate a series of flat field spectra to produce
#  a grand sum and fit a function to the sum to produce a normalized
#  flat containing the pixel-to-pixel variations.
#  User interaction via the graphics cursor is provided. The following
#  cursor commands are recognized:
#
#	? - Screen help
#	/ - Status line help
#	e - Plot in residual error mode
#	f - Plot in fit to the data mode
#	o - Change order of fit
#	l - Change lower rejection sigma
#	u - Change upper rejection sigma
#	r - Reset fit to include rejected pixels
#	s - Change upper and lower sigmas to same value
#	i - Iterate again
#	n - Iterate N times
#	q - Quit and accept current solution (also RETURN)
#

procedure t_flatfit ()

pointer	image					# Image name to be fit
pointer	images					# Image name to be fit
pointer	ofile					# Output image file name
int	function				# Fitting function
int	order					# Order of fitting function
int	recs					# Spectral record numbers
int	root, nrecs				# CL and ranges flags
real	expo					# Exposure time
real	dtime					# Deadtime
real	power					# Power law coin. correction
real	lower					# Lower rejection sigma
real	upper					# Upper threshold sigma
int	ngrow					# Rejection radius
real	div_min					# Division min for option RESP
bool	coincidence, all			# Apply coincidence correction
bool	interact				# Interactive levels
pointer	bstat					# Status of each aperture
pointer	npts					# Length of spectrum
pointer	esum					# Accumulated exposure time
pointer	accum					# Pointers to beam accumulators
pointer	title
int	ccmode, beam
int	niter

int	i
pointer	sp, str, im

int	clgeti(), clgwrd(), clpopni(), imgeti()
int	get_next_image(), decode_ranges()
real	clgetr(), imgetr()
bool	clgetb()
pointer	immap()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (images, MAX_NR_BEAMS, TY_POINTER)
	call salloc (ofile, SZ_FNAME, TY_CHAR)
	call salloc (recs, 300, TY_INT)
	call salloc (bstat, MAX_NR_BEAMS, TY_INT)
	call salloc (npts, MAX_NR_BEAMS, TY_INT)
	call salloc (esum, MAX_NR_BEAMS, TY_REAL)
	call salloc (accum, MAX_NR_BEAMS, TY_POINTER)
	call salloc (title, MAX_NR_BEAMS, TY_POINTER)
	call salloc (str, SZ_LINE, TY_CHAR)
	call amovki (NULL, Memi[images], MAX_NR_BEAMS)

	# Get task parameters.
	root = clpopni ("input")

	# Get input record numbers
	call clgstr ("records", Memc[str], SZ_LINE)
	if (decode_ranges (Memc[str], Memi[recs], 100, nrecs) == ERR)
	    call error (0, "Bad range specification")

	call clgstr ("output", Memc[ofile], SZ_LINE)

	call clgcurfit ("function", "order", function, order)

	lower = clgetr ("lower")
	upper = clgetr ("upper")
	ngrow = clgeti ("ngrow")
	div_min = clgetr ("div_min")

	# Determine desired level of activity
	interact = clgetb ("interact")
	all      = clgetb ("all_interact")

	niter = clgeti ("niter")

	# Is coincidence correction to be performed?
	coincidence = clgetb ("coincor")

	if (coincidence) {
	    ccmode = clgwrd ("ccmode", Memc[str], SZ_LINE, ",photo,iids,")
	    dtime  = clgetr ("deadtime")
	    power = clgetr ("power")
	}

	call reset_next_image ()

	# Clear all beam status flags
	call amovki (INDEFI, Memi[bstat], MAX_NR_BEAMS)
	call aclrr (Memr[esum], MAX_NR_BEAMS)

	call printf ("Accumulating spectra --\n")
	call flush (STDOUT)

10	while (get_next_image (root, Memi[recs], nrecs, Memc[image],
	    SZ_FNAME) != EOF) {
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		call eprintf ("Header info not available for [%s]\n")
		    call pargstr (Memc[image])
		goto 10
	    }

	    iferr (beam = imgeti (im, "BEAM-NUM"))
		beam = 0
	    if (beam < 0  || beam > MAX_NR_BEAMS-1)
		call error (0, "Invalid aperture number")

	    iferr (expo = imgetr (im, "EXPOSURE"))
		iferr (expo = imgetr (im, "ITIME"))
		    iferr (expo = imgetr (im, "EXPTIME"))
			expo = 1

	    # Add spectrum into accumulator
	    if (IS_INDEFI (Memi[bstat+beam])) {
	        Memi[npts+beam] = IM_LEN (im,1)
	        call salloc (Memi[accum+beam], Memi[npts+beam], TY_REAL)
	        call aclrr (Memr[Memi[accum+beam]], Memi[npts+beam])
	        Memi[bstat+beam] = 0

	        call salloc (Memi[title+beam], SZ_LINE, TY_CHAR)
	        call strcpy (IM_TITLE(im), Memc[Memi[title+beam]], SZ_LINE)
	    }

	    call ff_accum_spec (im, Memi[npts], expo, Memi[bstat], beam+1,
		Memi[accum], Memr[esum], coincidence, ccmode, dtime, power,
		Memi[title])

	    call printf ("[%s] added to aperture %1d\n")
		call pargstr (Memc[image])
		call pargi (beam)
	    call flush (STDOUT)
	    if (Memi[images+beam] == NULL)
		call salloc (Memi[images+beam], SZ_FNAME, TY_CHAR)
	    call strcpy (Memc[image], Memc[Memi[images+beam]], SZ_FNAME)

	    call imunmap (im)
	}

	# Review all apertures containing data and perform fits.
	# Act interactively if desired
	do i = 0, MAX_NR_BEAMS-1 {
	    if (!IS_INDEFI (Memi[bstat+i])) {
		call fit_spec (Memr[Memi[accum+i]], Memi[npts+i], Memr[esum+i],
		    interact, function, order, niter, lower, upper, ngrow,
		    div_min, i)
		if (interact & !all)
		    interact = false
		call wrt_fit_spec (Memc[Memi[images+i]], Memr[Memi[accum+i]],
		    Memr[esum+i], Memc[ofile], i, Memc[Memi[title+i]],
		    Memi[npts+i], order)
	    }
	}

	call sfree (sp)
	call clpcls (root)
end

# ACCUM_SPEC -- Accumulate spectra by beams

procedure ff_accum_spec (im, len, expo, beam_stat, beam, accum, expo_sum,
	coincidence, ccmode, dtime, power, title)

pointer	im, accum[ARB], title[ARB]
real	expo, expo_sum[ARB]
int	beam_stat[ARB], beam, len[ARB]
bool	coincidence
int	ccmode
real	dtime, power

int	npts, co_flag, imgeti()
pointer	pix

pointer	imgl1r()

begin
	npts = IM_LEN (im, 1)

	# Map pixels and optionally correct for coincidence
	pix = imgl1r (im)
	if (coincidence) {
	    iferr (co_flag = imgeti (im, "CO-FLAG"))
		co_flag = -1
	    if (co_flag < 1) {
		call coincor (Memr[pix], Memr[pix], npts, expo, co_flag,
		    dtime, power, ccmode)
	    }
	}

	# Add in the current data
	npts = min (npts, len[beam])

	call aaddr (Memr[pix], Memr[accum[beam]], Memr[accum[beam]], npts)

	beam_stat[beam] = beam_stat[beam] + 1
	expo_sum [beam] = expo_sum [beam] + expo
end

# WRT_FIT_SPEC -- Write out normalized spectrum

procedure wrt_fit_spec (image, accum, expo_sum, ofile, beam, title, npts, order)

char	image[SZ_FNAME]
real	accum[ARB], expo_sum
int	beam, npts, order
char	ofile[SZ_FNAME]
char	title[SZ_LINE]

char	output[SZ_FNAME], temp[SZ_LINE]
pointer	im, imnew, newpix

pointer	immap(), impl1r()
int	strlen()

begin
	im = immap (image, READ_ONLY, 0)
10	call strcpy (ofile, output, SZ_FNAME)
	call sprintf (output[strlen (output) + 1], SZ_FNAME, ".%04d")
	    call pargi (beam)

	# Create new image with a user area
	# If an error occurs, ask user for another name to try
	# since many open errors result from trying to overwrite an
	# existing image.

	iferr (imnew = immap (output, NEW_COPY, im)) {
	    call eprintf ("Cannot create [%s] -- Already exists??\07\n")
		call pargstr (output)
	    call clgstr ("output", ofile, SZ_FNAME)
	    go to 10
	}

	call strcpy ("Normalized flat:", temp, SZ_LINE)
	call sprintf (temp[strlen (temp) + 1], SZ_LINE, "%s")
	    call pargstr (title)
	call strcpy (temp, IM_TITLE (imnew), SZ_LINE)
	IM_PIXTYPE (imnew) = TY_REAL

	newpix = impl1r (imnew)
	call amovr (accum, Memr[newpix], npts)

	call imaddr (imnew, "EXPOSURE", expo_sum)
	call imaddi (imnew, "QF-FLAG", order)
	call imunmap (im)
	call imunmap (imnew)

	call printf ("Fit for aperture %1d --> [%s]\n")
	    call pargi (beam)
	    call pargstr (output)
	call flush (STDOUT)
end

# FIT_SPEC -- Fit a line through the spectrum with user interaction

procedure fit_spec (accum, npts, expo_sum, interact, function, 
	order, niter, lower, upper, ngrow, div_min, beam)

real	accum[ARB], expo_sum
bool	interact
int	function, order, niter, ngrow, npts, beam
real	lower, upper, div_min

int	cc, key, gp, plt_mode
int	i, initer, sum_niter, newgraph
real	x1, y1, sigma, temp
pointer	sp, wts, x, y, cv
bool	first
char	gtitle[SZ_LINE], command[SZ_FNAME]

int	clgcur(), clgeti()
pointer	gopen()
real	clgetr(), cveval()

data	plt_mode/PLT_FIT/

begin
	# Perform initial fit
	call smark (sp)
	call salloc (wts, npts, TY_REAL)
	call salloc (x  , npts, TY_REAL)
	call salloc (y  , npts, TY_REAL)

	first = true
	if (!interact) {
	    sum_niter = 0
	    do i = 1, niter
		call linefit (accum, npts, function, order, lower, upper, 
		    ngrow, cv, first, Memr[wts], Memr[x])
	    sum_niter = niter

	} else {
	    gp = gopen ("stdgraph", NEW_FILE, STDGRAPH)
	    call sprintf (gtitle, SZ_LINE, "Flat Field Sum - %f seconds ap:%1d")
		call pargr (expo_sum)
		call pargi (beam)

	    key = 'r'
	    repeat {
		switch (key) {
		    case 'e': # Plot errors
			plt_mode = PLT_ERR
			newgraph = YES

		    case 'f': # Plot fit
			plt_mode = PLT_FIT
			newgraph = YES

		    case 'o': # Change order
			order = clgeti ("new_order")
			# Reinstate all pixels
			first = true
			newgraph = YES

		    case 'l': # Change lower sigma
			lower = clgetr ("new_lower")
			newgraph = YES

		    case 'u': # Change upper sigma
			upper = clgetr ("new_upper")
			newgraph = YES

		    case 'r': # Reset fit parameters
			first = true
			newgraph = YES

		    case 's': # Change both rejection sigmas
			lower = clgetr ("new_lower")
			upper = lower
			call clputr ("new_upper", upper)
			newgraph = YES

		    case 'i': # Iterate again - Drop thru
			initer = 1
			newgraph = YES

		    case 'n': # Iterate n times
			initer = clgeti ("new_niter")
			newgraph = YES

		    case 'q': # Quit
			break

		    case '?': # Clear and help
			call gpagefile (gp, KEY, PROMPT)

		    case '/': # Status line help
			call ff_sts_help

		    case 'I': # Interrupt
			call fatal (0, "Interrupt")

		    default:
			call printf ("\07\n")
		}

		if (newgraph == YES) {
		    # Suppress an iteration if plot mode change requested
		    if (key != 'e' && key != 'f') {
		        if (first) {
			    sum_niter = 0
			    initer = niter
			    call cvfree (cv)
		        }
		        do i = 1, initer
			    call linefit (accum, npts, function, order, lower, 
			        upper, ngrow, cv, first, Memr[wts], Memr[x])
		        sum_niter = sum_niter + initer
		    }

		    switch (plt_mode) {
		    case PLT_FIT:
		        call plot_fit (gp, accum, cv, function, order, npts, 
			    gtitle, Memr[wts], Memr[x], Memr[y], sigma)
		    case PLT_ERR:
		        call plot_fit_er (gp, accum, cv, function, order, npts,
			    gtitle, Memr[wts], Memr[x], Memr[y], sigma)
		    }

		    newgraph = NO
	        }
	    } until (clgcur ("cursor",x1,y1,cc,key,command,SZ_FNAME) == EOF)
	    call gclose (gp)
	}

	# Replace original data with the data/fit
	do i = 1, npts {
	    temp = cveval (cv, real (i))
	    if (temp == 0.0)
		temp = max (temp, div_min)
	    accum[i] = accum[i] / temp
	}

	call cvfree (cv)
	call sfree (sp)

	# Save iteration count for next time
	niter = sum_niter
end

# LINEFIT -- Fit desired function thru data

procedure linefit (pix, npts, function, order, lower, upper, ngrow, cv,
	    first, wts, x)

real	pix[ARB]			# Data array to fit
int	npts				# Elements in array
int	function			# Type of fitting function
int	order				# Order of fitting function
real	lower				# Lower rejection threshold
real	upper				# Upper rejection threshold
int	ngrow				# Rejection growing radius
pointer	cv
real	wts[ARB]			# Array weights
real	x[ARB]
bool	first

int	ier, i, nreject

int	reject()

begin
10	if (first) {
	    do i = 1, npts {
		x[i] = i
		wts[i] = 1.0
	    }

	    # Initialize curve fitting.
	    call cvinit (cv, function, order, 1., real (npts))
	    call cvfit (cv, x, pix, wts, npts, WTS_USER, ier)
	    nreject = 0
	    first = false
	}

	# Do pixel rejection if desired.
	if ((lower > 0.) || (upper > 0.))
	        nreject = reject (cv, x, pix, wts, npts, lower, upper, ngrow)
	else
		nreject = 0

	if (nreject == ERR) {
	    call eprintf ("Cannot fit data -- too many points rejected??\n")
	    call cvfree (cv)
	    first = true
	    go to 10
	}
end

# REJECT -- Reject points with large residuals from the fit.
#
# The sigma of the input to the fit is calculated.  The rejection thresholds
# are set at -lower*sigma and upper*sigma.  Points outside the rejection
# thresholds are rejected from the fit and flaged by setting their
# weights to zero.  Finally, the remaining points are refit and a new
# fit line evaluated.  The number of points rejected is returned.

int procedure reject (cv, x, y, w, npoints, lower, upper, ngrow)

pointer	cv				# Curve descriptor
real	x[ARB]				# Input ordinates
real	y[ARB]				# Input data values
real	w[ARB]				# Weights
int	npoints				# Number of input points
real	lower				# Lower rejection sigma
real	upper				# Upper rejection sigma
int	ngrow				# Rejection radius

int	i, j, n, i_min, i_max, nreject
real	sigma, residual, resid_min, resid_max

real	cveval()

begin
	# Determine sigma of fit and set rejection limits.
	sigma = 0.
	n = 0
	do i = 1, npoints {
	    if (w[i] == 0.)
		next
	    sigma = sigma + (y[i] - cveval (cv, x[i])) ** 2
	    n = n + 1
	}

	sigma = sqrt (sigma / (n - 1))
	resid_min = -lower * sigma
	resid_max = upper * sigma

	# Reject the residuals exceeding the rejection limits.
	nreject = 0
	for (i = 1; i <= npoints; i = i + 1) {
	    if (w[i] == 0.)
		next
	    residual = y[i] - cveval (cv, x[i])
	    if ((residual < resid_min) || (residual > resid_max)) {
		i_min = max (1, i - ngrow)
		i_max = min (npoints, i + ngrow)

		# Reject points from the fit and flag them with zero weight.
		do j = i_min, i_max {
		    call cvrject (cv, x[j], y[j], w[j])
		    w[j] = 0.
		    nreject = nreject + 1
		}
		i = i_max
	    }
	}

	# Refit if points have been rejected.
	if (nreject > 0) {
	    call cvsolve (cv, i)
	    if (i != OK)
		return (ERR)
	}

	return (nreject)
end

# PLOT_FIT -- Plot the fit to the image line and data

procedure plot_fit (gp, pix, cv, function, order, npts, gtitle, wts, xfit,
	yfit, sigma)

int	gp, npts, function, order
real	pix[ARB], wts[ARB],  xfit[ARB], yfit[ARB]
pointer	cv
real	sigma
char	gtitle[SZ_LINE]

real	x1, x2
int	i

begin
	# Set up plot
	x1 = 1.0
	x2 = npts

	call gseti (gp, G_NMINOR, 0)
	call gclear (gp)
	call gsview (gp, 0.15, 0.95, 0.20, 0.9)
	call gploto (gp, pix, npts, x1, x2, gtitle)

	# Now plot the fit
	do i = 1, npts
	    xfit[i] = i

	call cvvector (cv, xfit, yfit, npts)
	call gvline (gp, yfit, npts, x1, x2)

	# Compute sigma and write it out
	call get_sigma (pix, yfit, wts, npts, sigma)
	call show_status (function, order, sigma, npts, wts)
end

# PLOT_FIT_ER -- Plot the error in the fit to the image line and data

procedure plot_fit_er (gp, pix, cv, function, order, npts, gtitle, wts, xfit,
	yfit, sigma)

int	gp, npts, function, order
real	pix[ARB], wts[ARB], xfit[ARB], yfit[ARB]
pointer	cv
real	sigma
char	gtitle[SZ_LINE]

real	x1, x2, y[2]
int	i

begin
	# Set up plot
	x1 = 1.0
	x2 = npts
	y[1] = -0.0001
	y[2] = +0.0001

	call cvvector (cv, xfit, yfit, npts)

	# Compute percentage errors
	do i = 1, npts
	    if (pix[i] != 0.0)
		yfit[i] = (pix[i] - yfit[i]) / pix[i]
	    else
		yfit[i] = 0.0

	call gseti (gp, G_NMINOR, 0)
	call gclear (gp)
	call gsview (gp, 0.15, 0.95, 0.20, 0.9)

	call gploto (gp, yfit, npts, x1, x2, 
	   "Flat field fractional error in fit")

	# Draw a zero error line
	call gline (gp, x1, y[1], x2, y[2])

	# Compute sigma
	call get_sigma0 (yfit, wts, npts, sigma)
	call show_status (function, order, sigma, npts, wts)
end

# SHOW_STATUS -- Show the fit status on status line

procedure show_status (function, order, sigma, npts, wts)

int	function, order, npts
real 	sigma, wts[ARB]

int	i, nvals

begin
	# Count non-rejected points
	nvals = 0
	do i = 1, npts
	    if (wts[i] != 0.0)
		nvals = nvals + 1

	call printf ("Fit type: %s    order: %2d    rms: %6.3f")
	switch (function) {
	    case LEGENDRE:
		call pargstr ("Legendre")
	    case CHEBYSHEV:
		call pargstr ("Chebyshev")
	    case SPLINE3:
		call pargstr ("Spline3")
	    case SPLINE1:
		call pargstr ("Spline1")
	    default:
		call pargstr ("???")
	}

	    call pargi (order)
	    call pargr (sigma)

	call printf ("   points: %d out of %d")
	    call pargi (nvals)
	    call pargi (npts)

	call flush (STDOUT)
end

# GET_SIGMA -- Compute rms error between two vectors whose average difference
#              is zero.

procedure get_sigma (y1, y2, wts, n, sigma)

real	y1[ARB], y2[ARB], wts[ARB], sigma
int	n

int	i, nval
real	sum

begin
	sum = 0.0
	nval = 0
	do i = 1, n
	    if (wts[i] != 0.0) {
		sum = sum + (y1[i] - y2[i]) ** 2
		nval = nval + 1
	    }

	sigma = sqrt (sum / (nval-1))
	return
end

# GET_SIGMA0 -- Compute rms error of a vector

procedure get_sigma0 (y1, wts, n, sigma)

real	y1[ARB], wts[ARB], sigma
int	n

int	i, nval
real	sum

begin
	sum = 0.0
	nval = 0
	do i = 1, n
	    if (wts[i] != 0.0) {
		sum = sum + y1[i]**2
		nval = nval + 1
	    }

	sigma = sqrt (sum / (nval-1))
	return
end

# FF_STS_HELP -- Status line help for Flat Fit

procedure ff_sts_help ()

int	linenr, maxline

data	linenr/1/
data	maxline/2/

begin
	switch (linenr) {
	    case 1:
	    call printf ("e=err plot  f=data plot  o=order  l=lower sigma  ")
	    call printf ("u=upper sigma  s=both sigmas")

	    case 2:
	    call printf ("r=incl reject  i=iterate  n=niterate  q=quit  ")
	    call printf ("?=help  /=linehelp  <CR>=quit")
	}

	call flush (STDOUT)

	linenr = linenr + 1
	if (linenr > maxline)
	    linenr = 1
end
