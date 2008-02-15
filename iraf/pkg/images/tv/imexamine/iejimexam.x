# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	<gset.h>
include	<mach.h>
include	"imexam.h"


# IE_JIMEXAM -- 1D profile plot and gaussian fit parameters.
# If no GIO pointer is given then only the fit parameters are printed.
# The fitting uses a Levenberg-Marquardt nonlinear chi square minimization.

procedure ie_jimexam (gp, mode, ie, x, y, axis)

pointer	gp
pointer	ie
int	mode
real	x, y
int	axis

int	navg, order, clgpseti()
bool	center, background, clgpsetb()
real	sigma, width, rplot, clgpsetr()

int	i, j, k, nx, ny, x1, x2, y1, y2, nfit, flag[5]
real	xc, yc, bkg, r, dr, fit[5], xfit, yfit, asumr(), amedr()
pointer	sp, title, avstr, im, pp, data, xs, ys, ptr
pointer	clopset(), ie_gimage(), ie_gdata()

errchk	ie_gdata, mr_solve

begin
	iferr (im = ie_gimage (ie, NO)) {
	    call erract (EA_WARN)
	    return
	}

	# Get parameters
	if (IE_PP(ie) != NULL)
	    call clcpset (IE_PP(ie))
	if (axis == 1)
	    IE_PP(ie) = clopset ("jimexam")
	else
	    IE_PP(ie) = clopset ("kimexam")
	pp = IE_PP(ie)
	navg = clgpseti (pp, "naverage")
	center = clgpsetb (pp, "center")
	background = clgpsetb (pp, "background")
	sigma = clgpsetr (pp, "sigma")
	rplot = clgpsetr (pp, "rplot")
	if (background) {
	    order = clgpsetr (pp, "xorder")
	    width = clgpsetr (pp, "width")
	}

	# If the initial center is INDEF then use the previous value.
	if (!IS_INDEF(x))
	    IE_X1(ie) = x
	if (!IS_INDEF(y))
	    IE_Y1(ie) = y

	if (axis == 1) {
	    xc = IE_X1(ie)
	    yc = IE_Y1(ie)
	} else {
	    xc = IE_Y1(ie)
	    yc = IE_X1(ie)
	}

	# Get data
	r = max (rplot, 8 * sigma + width)
	x1 = xc - r
	x2 = xc + r
	y1 = nint (yc) - (navg - 1) / 2
	y2 = nint (yc) + navg / 2
	iferr {
	    if (axis == 1)
		data = ie_gdata (im, x1, x2, y1, y2)
	    else
		data = ie_gdata (im, y1, y2, x1, x2)
	} then {
	    call erract (EA_WARN)
	    return
	}

	# Compute average vector
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	yc = (y1 + y2) / 2.

	call smark (sp)
	call salloc (xs, nx, TY_REAL)
	call salloc (ys, nx, TY_REAL)
	call salloc (title, IE_SZTITLE, TY_CHAR)
	call salloc (avstr, SZ_LINE, TY_CHAR)

	ptr = data
	if (axis == 1) {
	    call sprintf (Memc[avstr], SZ_LINE, "Lines %d-%d")
		call pargi (y1)
		call pargi (y2)
	    call amovr (Memr[ptr], Memr[ys], nx) 
	    ptr = ptr + nx
	    do i = 2, ny {
		call aaddr (Memr[ptr], Memr[ys], Memr[ys], nx)
		ptr = ptr + nx
	    }
	    call adivkr (Memr[ys], real (ny), Memr[ys], nx)
	} else {
	    call sprintf (Memc[avstr], SZ_LINE, "Columns %d-%d")
		call pargi (y1)
		call pargi (y2)
	    do i = 0, nx-1 {
		Memr[ys+i] = asumr (Memr[ptr], ny) / ny
		ptr = ptr + ny
	    }
	}

	# Set default background
	bkg = 0.
	if (background) {
	    r = 4 * sigma
	    ptr = xs
	    do i = 0, nx-1 {
		if (abs (xc - x1 - i) > r) {
		    Memr[ptr] = Memr[ys+i]
		    ptr = ptr + 1
		}
	    }
	    if (ptr > xs)
		bkg = amedr (Memr[xs], ptr-xs)
	}

	# Convert to WCS
	if (axis == 1) {
	    call ie_mwctran (ie, xc, yc, xfit, yfit)
	    call ie_mwctran (ie, xc+sigma, yc, r, yfit)
	    dr = abs (xfit - r)
	    do i = 0, nx-1
		call ie_mwctran (ie, real(x1+i), yc, Memr[xs+i], yfit)
	} else {
	    call ie_mwctran (ie, yc, xc, yfit, xfit)
	    call ie_mwctran (ie, yc, xc+sigma, yfit, r)
	    dr = abs (xfit - r)
	    do i = 0, nx-1
		call ie_mwctran (ie, yc, real(x1+i), yfit, Memr[xs+i])
	}

	# Set initial fit parameters
	k = max (0, nint (xc - x1))
	fit[1] = bkg
	fit[2] = 0.
	fit[3] = Memr[ys+k] - fit[1]
	fit[4] = xfit
	fit[5] = dr

	# Do fitting.
	nfit = 1
	flag[1] = 3

	# Add centering if desired
	if (center) {
	    nfit = nfit + 1
	    flag[nfit] = 4
	    call ie_gfit (Memr[xs], Memr[ys], nx, fit, flag, nfit)
	}

	# Add sigma
	nfit = nfit + 1
	flag[nfit] = 5
	call ie_gfit (Memr[xs], Memr[ys], nx, fit, flag, nfit)

	# Now add background if desired
	if (background) {
	    if (order == 1) {
		nfit = nfit + 1
		flag[nfit] = 1
		call ie_gfit (Memr[xs], Memr[ys], nx, fit, flag, nfit)
	    } else if (order == 2) {
		nfit = nfit + 2
		flag[nfit-1] = 1
		flag[nfit] = 2
		call ie_gfit (Memr[xs], Memr[ys], nx, fit, flag, nfit)
	    }
	}

	# Plot the profile and overplot the gaussian fit.
	call sprintf (Memc[title], IE_SZTITLE, "%s: %s\n%s")
	    call pargstr (IE_IMNAME(ie))
	    call pargstr (Memc[avstr])
	    call pargstr (IM_TITLE(im))

	j = max (0, int (xc - x1 - rplot))
	k = min (nx-1, nint (xc - x1 + rplot))
	if (axis == 1)
	    call ie_graph (gp, mode, pp, Memc[title],
		Memr[xs+j], Memr[ys+j], k-j+1, IE_XLABEL(ie), IE_XFORMAT(ie))
	else
	    call ie_graph (gp, mode, pp, Memc[title],
		Memr[xs+j], Memr[ys+j], k-j+1, IE_YLABEL(ie), IE_YFORMAT(ie))

	call gseti (gp, G_PLTYPE, 2)
	xfit = min (Memr[xs+j], Memr[xs+k])
	r = (xfit - fit[4]) / fit[5]
	dr = abs ((Memr[xs+k] - Memr[xs+j]) / (k - j))
	if (abs (r) < 7.)
	    yfit = fit[1] + fit[2] * xfit + fit[3] * exp (-r**2 / 2.)
	else
	    yfit = fit[1] + fit[2] * xfit
	call gamove (gp, xfit, yfit)
	repeat {
	    xfit = xfit + 0.2 * dr
	    r = (xfit - fit[4]) / fit[5]
	    if (abs (r) < 7.)
		yfit = fit[1] + fit[2] * xfit + fit[3] * exp (-r**2 / 2.)
	    else
		yfit = fit[1] + fit[2] * xfit
	    call gadraw (gp, xfit, yfit)
	} until (xfit >= max (Memr[xs+j], Memr[xs+k]))
	call gseti (gp, G_PLTYPE, 1)

	# Print the fit values
	call printf ("%s: center=%7g peak=%7g sigma=%7.4g fwhm=%7.4g bkg=%7g\n")
	    call pargstr (Memc[avstr])
	    call pargr (fit[4])
	    call pargr (fit[3])
	    call pargr (fit[5])
	    call pargr (2.35482*fit[5])
	    call pargr (fit[1]+fit[2]*fit[4])

	if (IE_LOGFD(ie) != NULL) {
	    call fprintf (IE_LOGFD(ie),
		"%s: center=%7g peak=%7g sigma=%5.3f fwhm=%5.3f bkg=%7g\n")
		call pargstr (Memc[avstr])
		call pargr (fit[4])
		call pargr (fit[3])
		call pargr (fit[5])
		call pargr (2.35482*fit[5])
		call pargr (fit[1]+fit[2]*fit[4])
	}

	call sfree (sp)
end


# IE_GFIT -- 1D Gaussian fit.

procedure ie_gfit (xs, ys, nx, fit, flag, nfit)

real	xs[nx], ys[nx]		# Vector to be fit
int	nx			# Number of points
real	fit[5]			# Fit parameters
int	flag[nfit]		# Flag for parameters to be fit
int	nfit			# Number of parameters to be fit

int	i
real	chi1, chi2, mr

begin
	chi2 = MAX_REAL
	mr = -1.
	i = 0
	repeat {
	    call mr_solve (xs, ys, nx, fit, flag, 5, nfit, mr, chi1)
	    if (chi2 - chi1 > 1.)
		i = 0
	    else
		i = i + 1
	    chi2 = chi1
	} until (i == 3)
	mr = 0.
	call mr_solve (xs, ys, nx, fit, flag, 5, nfit, mr, chi1)

	fit[5] = abs (fit[5])
end


# DERIVS -- Compute model and derivatives for MR_SOLVE procedure.
#
#	I(x) = A1 + A2 * x + A3 exp {-[(x - A4) / A5]**2 / 2.}
#
# where the params are A1-A5.

procedure derivs (x, a, y, dyda, na)

real	x		# X value to be evaluated
real	a[na]		# Parameters
real	y		# Function value
real	dyda[na]	# Derivatives
int	na		# Number of parameters

real	arg, ex, fac

begin
	arg = (x - a[4]) / a[5]
	if (abs (arg) < 7.)
	    ex = exp (-arg**2 / 2.)
	else
	    ex = 0.
	fac = a[3] * ex * arg

	y = a[1] + a[2] * x + a[3] * ex

	dyda[1] = 1.
	dyda[2] = x
	dyda[3] = ex
	dyda[4] = fac / a[5]
	dyda[5] = fac * arg / a[5]
end


# MR_SOLVE -- Levenberg-Marquardt nonlinear chi square minimization.
#
# Use the Levenberg-Marquardt method to minimize the chi squared of a set
# of paraemters.  The parameters being fit are indexed by the flag array.
# To initialize the Marquardt parameter, MR, is less than zero.  After that
# the parameter is adjusted as needed.  To finish set the parameter to zero
# to free memory.  This procedure requires a subroutine, DERIVS, which
# takes the derivatives of the function being fit with respect to the
# parameters.  There is no limitation on the number of parameters or
# data points.  For a description of the method see NUMERICAL RECIPES
# by Press, Flannery, Teukolsky, and Vetterling, p523.

procedure mr_solve (x, y, npts, params, flags, np, nfit, mr, chisq)

real	x[npts]			# X data array
real	y[npts]			# Y data array
int	npts			# Number of data points
real	params[np]		# Parameter array
int	flags[np]		# Flag array indexing parameters to fit
int	np			# Number of parameters
int	nfit			# Number of parameters to fit
real	mr			# MR parameter
real	chisq			# Chi square of fit

int	i
real	chisq1
pointer	new, a1, a2, delta1, delta2

errchk	mr_invert

begin
	# Allocate memory and initialize.
	if (mr < 0.) {
	    call mfree (new, TY_REAL)
	    call mfree (a1, TY_REAL)
	    call mfree (a2, TY_REAL)
	    call mfree (delta1, TY_REAL)
	    call mfree (delta2, TY_REAL)

	    call malloc (new, np, TY_REAL)
	    call malloc (a1, nfit*nfit, TY_REAL)
	    call malloc (a2, nfit*nfit, TY_REAL)
	    call malloc (delta1, nfit, TY_REAL)
	    call malloc (delta2, nfit, TY_REAL)

	    call amovr (params, Memr[new], np)
	    call mr_eval (x, y, npts, Memr[new], flags, np, Memr[a2],
	        Memr[delta2], nfit, chisq)
	    mr = 0.001
	}

	# Restore last good fit and apply the Marquardt parameter.
	call amovr (Memr[a2], Memr[a1], nfit * nfit)
	call amovr (Memr[delta2], Memr[delta1], nfit)
	do i = 1, nfit
	    Memr[a1+(i-1)*(nfit+1)] = Memr[a2+(i-1)*(nfit+1)] * (1. + mr)

	# Matrix solution.
	call mr_invert (Memr[a1], Memr[delta1], nfit)

	# Compute the new values and curvature matrix.
	do i = 1, nfit
	    Memr[new+flags[i]-1] = params[flags[i]] + Memr[delta1+i-1]
	call mr_eval (x, y, npts, Memr[new], flags, np, Memr[a1],
	    Memr[delta1], nfit, chisq1)

	# Check if chisq has improved.
	if (chisq1 < chisq) {
	    mr = max (EPSILONR, 0.1 * mr)
	    chisq = chisq1
	    call amovr (Memr[a1], Memr[a2], nfit * nfit)
	    call amovr (Memr[delta1], Memr[delta2], nfit)
	    call amovr (Memr[new], params, np)
	} else
	    mr = 10. * mr

	if (mr == 0.) {
	    call mfree (new, TY_REAL)
	    call mfree (a1,  TY_REAL)
	    call mfree (a2,  TY_REAL)
	    call mfree (delta1, TY_REAL)
	    call mfree (delta2, TY_REAL)
	}
end


# MR_EVAL -- Evaluate curvature matrix.  This calls procedure DERIVS.

procedure mr_eval (x, y, npts, params, flags, np, a, delta, nfit, chisq)

real	x[npts]			# X data array
real	y[npts]			# Y data array
int	npts			# Number of data points
real	params[np]		# Parameter array
int	flags[np]		# Flag array indexing parameters to fit
int	np			# Number of parameters
real	a[nfit,nfit]		# Curvature matrix
real	delta[nfit]		# Delta array
int	nfit			# Number of parameters to fit
real	chisq			# Chi square of fit

int	i, j, k
real	ymod, dy, dydpj, dydpk
pointer	sp, dydp

begin
	call smark (sp)
	call salloc (dydp, np, TY_REAL)

	do j = 1, nfit {
	   do k = 1, j
	       a[j,k] = 0.
	    delta[j] = 0.
	}

	chisq = 0.
	do i = 1, npts {
	    call derivs (x[i], params, ymod, Memr[dydp], np)
	    dy = y[i] - ymod
	    do j = 1, nfit {
		dydpj = Memr[dydp+flags[j]-1]
		delta[j] = delta[j] + dy * dydpj
		do k = 1, j {
		    dydpk = Memr[dydp+flags[k]-1]
		    a[j,k] = a[j,k] + dydpj * dydpk
		}
	    }
	    chisq = chisq + dy * dy
	}

	do j = 2, nfit
	    do k = 1, j-1
		a[k,j] = a[j,k]

	call sfree (sp)
end
	    

# MR_INVERT -- Solve a set of linear equations using Householder transforms.

procedure mr_invert (a, b, n)

real	a[n,n]		# Input matrix and returned inverse
real	b[n]		# Input RHS vector and returned solution
int	n		# Dimension of input matrices

int	krank
real	rnorm
pointer	sp, h, g, ip

begin
	call smark (sp)
	call salloc (h, n, TY_REAL)
	call salloc (g, n, TY_REAL)
	call salloc (ip, n, TY_INT)

	call hfti (a, n, n, n, b, n, 1, 1E-10, krank, rnorm,
	    Memr[h], Memr[g], Memi[ip])

	call sfree (sp)
end
