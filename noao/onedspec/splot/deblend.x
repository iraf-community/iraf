include	<error.h>
include	<mach.h>
include	<gset.h>

define	SQ2PI	2.5066283

# DEBLEND -- Deblend lines in a spectral region.

procedure deblend (sh, gfd, wx1, wy1, wcs, pix, n, fd1, fd2, xg, yg, sg, ng)

pointer	sh			# SHDR pointer
pointer	gfd			# GIO file descriptor
real	wx1, wy1		# Cursor position
real	wcs[n]			# Coordinates
real	pix[n]			# Spectrum data
int	n			# Number of points
int	fd1, fd2		# Output file descriptors
pointer	xg, yg, sg		# Pointers to fit parameters
int	ng			# Number of components

int	bkgfit, posfit, sigfit
int	i, j, i1, npts, nlines, maxlines, wc, key
real	w, wyc, wx, wy, wx2, wy2
real	slope, height, flux, cont, sigma, eqw, scale, chisq
bool	fit
pointer	sp, cmd, x, y, z, waves

int	clgcur(), open(), fscan(), nscan()
real	clgetr(), model()
double	shdr_wl()
errchk	dofit

define	posfit_	95
define	sigfit_	96
define	bkgfit_	97
define	done_	99

begin
	call smark (sp)
	call salloc (cmd, SZ_FNAME, TY_CHAR)

	# Input cursor is first continuum point now get second continuum point.
	call printf ("d again:")
	if (clgcur ("cursor", wx2, wy2, wc, key, Memc[cmd], SZ_FNAME) == EOF) {
	    call sfree (sp)
	    return
	}

	# Set pixel indices and determine number of points to fit.
	call fixx (sh, wx1, wx2, wy1, wy2, i1, j)
	    
	npts = j - i1 + 1
	if (npts < 3) {
	    call eprintf ("At least 3 points are required\n")
	    call sfree (sp)
	    return
	}

	# Allocate space for the points to be fit.
	call salloc (x, npts, TY_REAL)
	call salloc (y, npts, TY_REAL)
	call salloc (z, npts, TY_REAL)

	# Scale the data.
	scale = 0.
	do i = 1, npts {
	    Memr[x+i-1] = wcs[i1+i-1]
	    Memr[y+i-1] = pix[i1+i-1]
	    scale = max (scale, abs (Memr[y+i-1]))
	}
	call adivkr (Memr[y], scale, Memr[y], npts)

	# Select the lines to be fit.  If no lines return.
	fit = false
	maxlines = 5
	call malloc (waves, maxlines, TY_REAL)
	nlines = 0
	call printf (
	    "Lines ('f' for file, 'm' to mark, 't' to type, 'q' to quit):")
	while (clgcur ("cursor", wx, wy, wc, key, Memc[cmd], SZ_FNAME) != EOF) {
	    switch (key) {
	    case 'f':
		call clgstr ("linelist", Memc[cmd], SZ_FNAME)
		call printf (
		"Lines ('f' for file, 'm' to mark, 't' to type, 'q' to quit):")
		iferr (j = open (Memc[cmd], READ_ONLY, TEXT_FILE)) {
		    call erract (EA_WARN)
		    next
		}
		while (fscan (j) != EOF) {
		    call gargr (wx)
		    if (nscan() < 1)
			next
		    if (wx < min (wcs[1], wcs[n]) || wx > max (wcs[1], wcs[n]))
			next
		    for (i = 0; i < nlines && wx != Memr[waves+i]; i = i + 1)
			;
		    if (i == nlines) {
			if (nlines == maxlines) {
			    maxlines = maxlines + 5
			    call realloc (waves, maxlines, TY_REAL)
			}
			Memr[waves+i] = wx
			nlines = nlines + 1
			call gmark (gfd, wx, wy, GM_VLINE, 3., 3.)
		    }
		}
		call close (j)
		next
	    case 'm':
	    case 't':
		wx = clgetr ("wavelength")
		call printf (
		    "Lines ('m' to mark, 't' to type, 'q' to quit):")
	    case 'q':
		call printf ("\n")
		break
	    case 'I':
		call fatal (0, "Interrupt")
	    default:
		call printf (
	    "Lines ('f' for file, 'm' to mark, 't' to type, 'q' to quit):\007")
		next
	    }
	    for (i = 0; i < nlines && wx != Memr[waves+i]; i = i + 1)
		;
	    if (i == nlines) {
		if (nlines == maxlines) {
		    maxlines = maxlines + 5
		    call realloc (waves, maxlines, TY_REAL)
		}
		Memr[waves+i] = wx
		nlines = nlines + 1
		call gmark (gfd, wx, wy, GM_VLINE, 3., 3.)
	    }
	}
	if (nlines == 0)
	    goto done_

	# Allocate memory.
	if (ng == 0) {
	    call malloc (xg, nlines, TY_REAL)
	    call malloc (yg, nlines, TY_REAL)
	    call malloc (sg, nlines, TY_REAL)
	} else if (ng != nlines) {
	    call realloc (xg, nlines, TY_REAL)
	    call realloc (yg, nlines, TY_REAL)
	    call realloc (sg, nlines, TY_REAL)
	}
	ng = nlines

	# Do fits.
	repeat {
posfit_     call printf ("Fit positions (fixed, single, all, quit) ")
	    if (clgcur ("cursor", wx, wy, wc, key, Memc[cmd], SZ_FNAME) == EOF)
		break
	    switch (key) {
	    case 'f':
		posfit = 1
	    case 's':
		posfit = 2
	    case 'a':
		posfit = 3
	    case 'q':
		break
	    default:
		goto posfit_
	    }
sigfit_     call printf ("Fit sigmas (single, all, quit) ")
	    if (clgcur ("cursor", wx, wy, wc, key, Memc[cmd], SZ_FNAME) == EOF)
		break
	    switch (key) {
	    case 's':
		sigfit = 2
	    case 'a':
		sigfit = 3
	    case 'q':
		break
	    default:
		goto sigfit_
	    }
bkgfit_     call printf ("Fit background (no, yes, quit) ")
	    if (clgcur ("cursor", wx, wy, wc, key, Memc[cmd], SZ_FNAME) == EOF)
		break
	    switch (key) {
	    case 'n':
		bkgfit = 0
	    case 'y':
		bkgfit = 1
	    case 'q':
		break
	    default:
		goto bkgfit_
	    }

	    # Setup initial estimates.
	    slope = (wy2-wy1) / (wx2-wx1) / scale
	    wyc = wy1 / scale - slope * wx1
	    sigma = 0.1 * abs (Memr[x+npts-1] - Memr[x]) / nlines
	    do i = 0, nlines-1 {
		w = Memr[waves+i]
		j = max (1, min (n, nint (shdr_wl (sh, double(w)))))
	        Memr[yg+i] = pix[j] / scale - wyc - slope * w
		Memr[xg+i] = w
		Memr[sg+i] = sigma
	    }

	    iferr (call dofit (bkgfit, posfit, sigfit, Memr[x], Memr[y], npts,
		wyc, slope, Memr[xg], Memr[yg], Memr[sg], ng, chisq)) {
		call erract (EA_WARN)
		next
	    }
	    call amulkr (Memr[yg], scale, Memr[yg], ng)
	    wyc = (wyc + slope * wx1) * scale
	    slope = slope * scale

	    # Compute model spectrum with continuum and plot.
	    fit = true
	    do i = 1, npts {
		w = Memr[x+i-1]
	        Memr[z+i-1] = model (w, Memr[xg], Memr[yg], Memr[sg], ng)
	        Memr[z+i-1] = Memr[z+i-1] + wyc + slope * (w - wx1)
	    }

	    call gseti (gfd, G_PLTYPE, 2)
	    call gpline (gfd, Memr[x], Memr[z], npts)
	    call gseti (gfd, G_PLTYPE, 3)
	    call gline (gfd, wx1, wyc, wx2, wyc + slope * (wx2 - wx1))
	    call gseti (gfd, G_PLTYPE, 1)
	    call gflush (gfd)

	    # Print computed values on status line.
	    i = 1
	    key = ''
	    repeat {
	        switch (key) {
	        case '-':
		    i = i - 1
		    if (i < 1)
		        i = nlines
	        case '+':
		    i = i + 1
		    if (i > nlines)
		        i = 1
	        case 'q':
		    call printf ("\n")
		    break
	        }

	        height = Memr[yg+i-1]
	        w = Memr[xg+i-1]
	        sigma = Memr[sg+i-1]
	        flux = sigma * height * SQ2PI
	        cont = wyc + slope * (w - wx1)
	        if (cont > 0.)
		    eqw = -flux / cont
	        else
		    eqw = INDEF

		if (key == 'r') {
		    call printf ("\nrms = %8.4g")
			call pargr (scale * sqrt (chisq / npts))
		} else {
	            call printf (
	        "\n%d: center = %8.6g, flux = %8.4g, eqw = %6.4g, fwhm = %6.4g")
			call pargi (i)
	                call pargr (w)
	                call pargr (flux)
	                call pargr (eqw)
	                call pargr (2.355 * sigma)
		}

	        call printf ("  (+,-,r,q):")
	        call flush (STDOUT)
	    } until (clgcur ("cursor",
		wx, wy, wc, key, Memc[cmd], SZ_FNAME) == EOF)
	}

done_
	call printf ("Deblending done\n")
	# Log computed values
	if (fit) {
	    do i = 1, nlines {
		w = Memr[xg+i-1]
		cont = wyc + slope * (w - wx1)
		height = Memr[yg+i-1]
		sigma = Memr[sg+i-1]
		flux = sigma * height * SQ2PI
		if (cont > 0.)
		    eqw = -flux / cont
		else
		    eqw = INDEF
		if (fd1 != NULL) {
		    call fprintf (fd1,
			" %9.7g %9.7g %9.6g %9.4g %9.6g %9.4g %9.4g\n")
			call pargr (w)
			call pargr (cont)
			call pargr (flux)
			call pargr (eqw)
			call pargr (height)
			call pargr (sigma)
			call pargr (2.355 * sigma)
		}
		if (fd2 != NULL) {
		    call fprintf (fd2,
			" %9.7g %9.7g %9.6g %9.4g %9.6g %9.4g %9.4g\n")
			call pargr (w)
			call pargr (cont)
			call pargr (flux)
			call pargr (eqw)
			call pargr (height)
			call pargr (sigma)
			call pargr (2.355 * sigma)
		}
	    }
	} else {
	    call mfree (xg, TY_REAL)
	    call mfree (yg, TY_REAL)
	    call mfree (sg, TY_REAL)
	    ng = 0
	}

	call mfree (waves, TY_REAL)
	call sfree (sp)
end


# SUBBLEND -- Subtract last fit.

procedure subblend (sh, gfd, x, y, n, wx1, wy1, xg, yg, sg, ng)

pointer	sh			# SHDR pointer
pointer	gfd			# GIO file descriptor
real	wx1, wy1		# Cursor position
real	x[n]			# Spectrum data
real	y[n]			# Spectrum data
int	n			# Number of points
pointer	xg, yg, sg		# Pointers to fit parameters
int	ng			# Number of components

int	i, j, i1, npts, wc, key
real	wx2, wy2
pointer	sp, cmd

int	clgcur()
real	model()

begin
	if (ng == 0)
	    return

	call smark (sp)
	call salloc (cmd, SZ_FNAME, TY_CHAR)

	# Determine fit range
	call printf ("- again:")
	call flush (STDOUT)
	if (clgcur ("cursor", wx2, wy2, wc, key, Memc[cmd], SZ_FNAME) == EOF) {
	    call sfree (sp)
	    return
	}

	call fixx (sh, wx1, wx2, wy1, wy2, i1, j)
	npts = j - i1 + 1

	do i = 1, npts {
	    y[i1+i-1] = y[i1+i-1] -
		model (x[i1+i-1], Memr[xg], Memr[yg], Memr[sg], ng)
	}

	# Plot subtracted curve
	call gpline (gfd, x[i1], y[i1], npts)
	call gflush (gfd)

	call mfree (xg, TY_REAL)
	call mfree (yg, TY_REAL)
	call mfree (sg, TY_REAL)
	ng = 0
	call sfree (sp)
end


# DOFIT -- Fit gaussian components.  This is an interface to DOFIT1
# which puts parameters into the form required by DOFIT1 and vice-versa.
# It also implements a constrained approach to the solution.

procedure dofit (bkgfit, posfit, sigfit, x, y, npts, y1, dy, xg, yg, sg, ng,
	chisq)

int	bkgfit		# Fit background (0=no, 1=yes)
int	posfit		# Position fitting flag (1=fixed, 2=single, 3=all)
int	sigfit		# Sigma fitting flag (1=fixed, 2=single, 3=all)
real	x[npts]		# X data
real	y[npts]		# Y data
int	npts		# Number of points
real	y1		# Continuum offset
real	dy		# Continuum slope
real	xg[ng]		# Initial and final x coordinates of gaussians
real	yg[ng]		# Initial and final y coordinates of gaussians
real	sg[ng]		# Initial and final sigmas of gaussians
int	ng		# Number of gaussians
real	chisq		# Chi squared

int	i
pointer	sp, a, j
errchk	dofit1

begin
	call smark (sp)
	call salloc (a, 4 + 3 * ng, TY_REAL)

	# Convert positions and widths relative to first component.
	Memr[a] = y1
	Memr[a+1] = dy
	Memr[a+2] = xg[1]
	Memr[a+3] = sg[1]
	do i = 1, ng {
	    j = a + 3 * i + 1
	    Memr[j] = yg[i]
	    Memr[j+1] = xg[i] - Memr[a+2]
	    Memr[j+2] = sg[i] / Memr[a+3]
	}

	# Do fit.
	do i = 0, bkgfit {
	    switch (10*posfit+sigfit) {
	    case 11:
		call dofit1 (i, 1, 1, x, y, npts, Memr[a], ng, chisq)
	    case 12:
		call dofit1 (i, 1, 2, x, y, npts, Memr[a], ng, chisq)
	    case 13:
		call dofit1 (i, 1, 2, x, y, npts, Memr[a], ng, chisq)
		call dofit1 (i, 1, 3, x, y, npts, Memr[a], ng, chisq)
	    case 21:
		call dofit1 (i, 2, 1, x, y, npts, Memr[a], ng, chisq)
	    case 22:
		call dofit1 (i, 1, 2, x, y, npts, Memr[a], ng, chisq)
		call dofit1 (i, 2, 2, x, y, npts, Memr[a], ng, chisq)
	    case 23:
		call dofit1 (i, 1, 2, x, y, npts, Memr[a], ng, chisq)
		call dofit1 (i, 2, 2, x, y, npts, Memr[a], ng, chisq)
		call dofit1 (i, 2, 3, x, y, npts, Memr[a], ng, chisq)
	    case 31:
		call dofit1 (i, 2, 1, x, y, npts, Memr[a], ng, chisq)
		call dofit1 (i, 3, 1, x, y, npts, Memr[a], ng, chisq)
	    case 32:
		call dofit1 (i, 1, 2, x, y, npts, Memr[a], ng, chisq)
		call dofit1 (i, 2, 2, x, y, npts, Memr[a], ng, chisq)
		call dofit1 (i, 3, 2, x, y, npts, Memr[a], ng, chisq)
	    case 33:
		call dofit1 (i, 1, 2, x, y, npts, Memr[a], ng, chisq)
		call dofit1 (i, 2, 2, x, y, npts, Memr[a], ng, chisq)
		call dofit1 (i, 3, 2, x, y, npts, Memr[a], ng, chisq)
		call dofit1 (i, 3, 3, x, y, npts, Memr[a], ng, chisq)
	    }
	}

	y1 = Memr[a]
	dy = Memr[a+1]
	do i = 1, ng {
	    j = a + 3 * i + 1
	    yg[i] = Memr[j]
	    xg[i] = Memr[j+1] + Memr[a+2]
	    sg[i] = abs (Memr[j+2] * Memr[a+3])
	}

	call sfree (sp)
end


# MODEL -- Compute model.
#
#	I(x) = I(i) exp {-[(x-xg(i)) / sg(i)]**2 / 2.}
#
# where the params are I1, I2, xg, yg, and sg.

real procedure model (x, xg, yg, sg, ng)

real	x		# X value to be evaluated
real	xg[ng]		# X coordinates of gaussians
real	yg[ng]		# Y coordinates of gaussians
real	sg[ng]		# Sigmas of gaussians
int	ng		# Number of gaussians

int	i
real	y, arg

begin
	y = 0.
	do i = 1, ng {
	    arg = (x - xg[i]) / sg[i]
	    if (abs (arg) < 7.)
		y = y + yg[i] * exp (-arg**2 / 2.)
	}
	return (y)
end


# DOFIT1 -- Perform nonlinear iterative fit for the specified parameters.
# This uses the Levenberg-Marquardt method from NUMERICAL RECIPES.

procedure dofit1 (bkgfit, posfit, sigfit, x, y, npts, a, nlines, chisq)

int	bkgfit		# Background fit (0=no, 1=yes)
int	posfit		# Position fitting flag (1=fixed, 2=one, 3=all)
int	sigfit		# Sigma fitting flag (1=fixed, 2=one, 3=all)
real	x[npts]		# X data
real	y[npts]		# Y data
int	npts		# Number of points
real	a[ARB]		# Fitting parameters
int	nlines		# Number of lines
real	chisq		# Chi squared

int	i, np, nfit
real	mr, chi2
pointer	sp, flags, ptr
errchk	mr_solve

begin
	# Number of terms is 3 for each line plus common background, center
	# and sigma.

	np = 3 * nlines + 4

	call smark (sp)
	call salloc (flags, np, TY_INT)
	ptr = flags

	# Background.
	if (bkgfit == 1) {
	    Memi[ptr] = 1
	    Memi[ptr+1] = 2
	    ptr = ptr + 2
	}

	# Peaks are always fit.
	do i = 1, nlines {
	    Memi[ptr] = 3 * i + 2
	    ptr = ptr + 1
	}

	# Positions.
	switch (posfit) {
	case 2:
	    Memi[ptr] = 3
	    ptr = ptr + 1
	case 3:
	    Memi[ptr] = 3
	    ptr = ptr + 1
	    do i = 1, nlines {
	        Memi[ptr] = 3 * i + 3
		ptr = ptr + 1
	    }
	}

	# Sigmas.
	switch (sigfit) {
	case 2:
	    Memi[ptr] = 4
	    ptr = ptr + 1
	case 3:
	    Memi[ptr] = 4
	    ptr = ptr + 1
	    do i = 1, nlines {
	        Memi[ptr] = 3 * i + 4
		ptr = ptr + 1
	    }
	}

	nfit = ptr - flags
	mr = -1.
	i = 0
	chi2 = MAX_REAL
	repeat {
	    call mr_solve (x, y, npts, a, Memi[flags], np, nfit, mr, chisq)
	    if (chi2 - chisq > 1.)
		i = 0
	    else
		i = i + 1
	    chi2 = chisq
	} until (i == 3)

	mr = 0.
	call mr_solve (x, y, npts, a, Memi[flags], np, nfit, mr, chisq)

	call sfree (sp)
end


# DERIVS -- Compute model and derivatives for MR_SOLVE procedure.
#
#	I(x) = I1 + I2 * x + I(i) exp {-[(x-xc-dx(i)) / (sig * sig(i))]**2 / 2.}
#
# where the params are I1, I2, xc, sig, I(i), dx(i), and sig(i) (i=1,nlines).

procedure derivs (x, a, y, dyda, na)

real	x		# X value to be evaluated
real	a[na]		# Parameters
real	y		# Function value
real	dyda[na]	# Derivatives
int	na		# Number of parameters

int	i
real	sig, arg, ex, fac

begin
	y = a[1] + a[2] * x
	dyda[1] = 1.
	dyda[2] = x
	dyda[3] = 0.
	dyda[4] = 0.
	do i = 5, na, 3 {
	    sig = a[4] * a[i+2]
	    arg = (x - a[3] - a[i+1]) / sig
	    if (abs (arg) < 7.)
	        ex = exp (-arg**2 / 2.)
	    else
		ex = 0.
	    fac = a[i] * ex * arg

	    y = y + a[i] * ex
	    dyda[3] = dyda[3] + fac / sig
	    dyda[4] = dyda[4] + fac * arg / a[4]
	    dyda[i] = ex
	    dyda[i+1] = fac / sig
	    dyda[i+2] = fac * arg / a[i+2]
	}
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
