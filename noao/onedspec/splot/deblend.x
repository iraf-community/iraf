include	<math.h>
include	<mach.h>

# Profile types.
define	GAUSS		1	# Gaussian profile
define	LORENTZ		2	# Lorentzian profile
define	VOIGT		3	# Voigt profile

# Elements of fit array.
define	BKG		1	# Background
define	POS		2	# Position
define	INT		3	# Intensity
define	GAU		4	# Gaussian FWHM
define	LOR		5	# Lorentzian FWHM

# Type of constraints.
define	FIXED		1	# Fixed parameter
define	SINGLE		2	# Fit a single value for all lines
define	INDEP		3	# Fit independent values for all lines


# DOFIT -- Fit line profiles.  This is an interface to DOFIT1
# which puts parameters into the required form and vice-versa.
# It also implements a constrained approach to the solution.

procedure dofit (fit, x, y, s, npts, dx, nsub, y1, dy,
    xp, yp, gp, lp, tp, np, chisq)

int	fit[5]		# Fit constraints
real	x[npts]		# X data
real	y[npts]		# Y data
real	s[npts]		# Sigma data
int	npts		# Number of points
real	dx		# Pixel size
int	nsub		# Number of subpixels
real	y1		# Continuum offset
real	dy		# Continuum slope
real	xp[np]		# Profile positions
real	yp[np]		# Profile intensities
real	gp[np]		# Profile Gaussian FWHM
real	lp[np]		# Profile Lorentzian FWHM
int	tp[np]		# Profile type	
int	np		# Number of profiles
real	chisq		# Chi squared

int	i, j, fit1[5]
pointer	sp, a, b
errchk	dofit1

begin
	call smark (sp)
	call salloc (a, 8 + 5 * np, TY_REAL)

	# Convert positions and widths relative to first component.
	Memr[a] = dx
	Memr[a+1] = nsub
	Memr[a+2] = y1
	Memr[a+3] = dy
	Memr[a+4] = yp[1]
	Memr[a+5] = xp[1]
	Memr[a+6] = gp[1]
	Memr[a+7] = lp[1]
	do i = 1, np {
	    b = a + 5 * i + 3
	    Memr[b]   = yp[i] / Memr[a+4]
	    Memr[b+1] = xp[i] - Memr[a+5]
	    switch (tp[i]) {
	    case GAUSS:
		if (Memr[a+6] == 0.)
		    Memr[a+6] = gp[i]
		Memr[b+2] = gp[i] / Memr[a+6]
	    case LORENTZ:
		if (Memr[a+7] == 0.)
		    Memr[a+7] = lp[i]
		Memr[b+3] = lp[i] / Memr[a+7]
	    case VOIGT:
		if (Memr[a+6] == 0.)
		    Memr[a+6] = gp[i]
		Memr[b+2] = gp[i] / Memr[a+6]
		if (Memr[a+7] == 0.)
		    Memr[a+7] = lp[i]
		Memr[b+3] = lp[i] / Memr[a+7]
	    }
	    Memr[b+4] = tp[i]
	}

	# Do fit.
	fit1[INT] = fit[INT]
	do i = 1, fit[BKG] {
	    fit1[BKG] = i
	    fit1[GAU] = min (SINGLE, fit[GAU])
	    fit1[LOR] = min (SINGLE, fit[LOR])
	    do j = FIXED, fit[POS] {
		fit1[POS] = j
		if (np > 1 || j != INDEP)
		    call dofit1 (fit1, x, y, s, npts, Memr[a], np, chisq)
	    }
	    if (np > 1 && (fit[GAU] == INDEP || fit[LOR] == INDEP)) {
		fit1[GAU] = fit[GAU]
		fit1[LOR] = fit[LOR]
		call dofit1 (fit1, x, y, s, npts, Memr[a], np, chisq)
	    }
	}

	y1 = Memr[a+2]
	dy = Memr[a+3]
	do i = 1, np {
	    b = a + 5 * i + 3
	    yp[i] = Memr[b] * Memr[a+4]
	    xp[i] = Memr[b+1] + Memr[a+5]
	    switch (tp[i]) {
	    case GAUSS:
		gp[i] = abs (Memr[b+2] * Memr[a+6])
	    case LORENTZ:
		lp[i] = abs (Memr[b+3] * Memr[a+7])
	    case VOIGT:
		gp[i] = abs (Memr[b+2] * Memr[a+6])
		lp[i] = abs (Memr[b+3] * Memr[a+7])
	    }
	}

	call sfree (sp)
end


# DOREFIT -- Refit line profiles.  This assumes the input is very close
# to the final solution and minimizes the number of calls to the
# fitting routines.  This is intended for efficient use in the
# in computing bootstrap error estimates.

procedure dorefit (fit, x, y, s, npts, dx, nsub, y1, dy,
    xp, yp, gp, lp, tp, np, chisq)

int	fit[5]		# Fit constraints
real	x[npts]		# X data
real	y[npts]		# Y data
real	s[npts]		# Sigma data
int	npts		# Number of points
real	dx		# Pixel size
int	nsub		# Number of subpixels
real	y1		# Continuum offset
real	dy		# Continuum slope
real	xp[np]		# Profile positions
real	yp[np]		# Profile intensities
real	gp[np]		# Profile Gaussian FWHM
real	lp[np]		# Profile Lorentzian FWHM
int	tp[np]		# Profile type	
int	np		# Number of profiles
real	chisq		# Chi squared

int	i
pointer	sp, a, b
errchk	dofit1

begin
	call smark (sp)
	call salloc (a, 8 + 5 * np, TY_REAL)

	# Convert positions and widths relative to first component.
	Memr[a] = dx
	Memr[a+1] = nsub
	Memr[a+2] = y1
	Memr[a+3] = dy
	Memr[a+4] = yp[1]
	Memr[a+5] = xp[1]
	Memr[a+6] = gp[1]
	Memr[a+7] = lp[1]
	do i = 1, np {
	    b = a + 5 * i + 3
	    Memr[b]   = yp[i] / Memr[a+4]
	    Memr[b+1] = xp[i] - Memr[a+5]
	    switch (tp[i]) {
	    case GAUSS:
		if (Memr[a+6] == 0.)
		    Memr[a+6] = gp[i]
		Memr[b+2] = gp[i] / Memr[a+6]
	    case LORENTZ:
		if (Memr[a+7] == 0.)
		    Memr[a+7] = lp[i]
		Memr[b+3] = lp[i] / Memr[a+7]
	    case VOIGT:
		if (Memr[a+6] == 0.)
		    Memr[a+6] = gp[i]
		Memr[b+2] = gp[i] / Memr[a+6]
		if (Memr[a+7] == 0.)
		    Memr[a+7] = lp[i]
		Memr[b+3] = lp[i] / Memr[a+7]
	    }
	    Memr[b+4] = tp[i]
	}

	# Do fit.
	call dofit1 (fit, x, y, s, npts, Memr[a], np, chisq)

	y1 = Memr[a+2]
	dy = Memr[a+3]
	do i = 1, np {
	    b = a + 5 * i + 3
	    yp[i] = Memr[b] * Memr[a+4]
	    xp[i] = Memr[b+1] + Memr[a+5]
	    switch (tp[i]) {
	    case GAUSS:
		gp[i] = abs (Memr[b+2] * Memr[a+6])
	    case LORENTZ:
		lp[i] = abs (Memr[b+3] * Memr[a+7])
	    case VOIGT:
		gp[i] = abs (Memr[b+2] * Memr[a+6])
		lp[i] = abs (Memr[b+3] * Memr[a+7])
	    }
	}

	call sfree (sp)
end


# MODEL -- Compute model.

real procedure model (x, dx, nsub, xp, yp, gp, lp, tp, np)

real	x		# X value to be evaluated
real	dx		# Pixel width
int	nsub		# Number of subpixels
real	xp[np]		# Profile positions
real	yp[np]		# Profile intensities
real	gp[np]		# Profile Gaussian FWHM
real	lp[np]		# Profile Lorentzian FWHM
int	tp[np]		# Profile type	
int	np		# Number of profiles

int	i, j
real	delta, x1, y, arg1, arg2, v, v0, u

begin
	delta = dx / nsub
	x1 = x - (dx + delta) / 2
	y = 0.
	do j = 1, nsub {
	    x1 = x1 + delta
	    do i = 1, np {
		switch (tp[i]) {
		case GAUSS:
		    arg1 = 1.66511 * abs ((x1 - xp[i]) / gp[i])
		    if (arg1 < 5.)
			y = y + yp[i] * exp (-arg1**2)
		case LORENTZ:
		    arg2 = abs ((x1 - xp[i]) / (lp[i] / 2))
		    y = y + yp[i] / (1 + arg2**2)
		case VOIGT:
		    arg1 = 1.66511 * (x1 - xp[i]) / gp[i]
		    arg2 = 0.832555 * lp[i] / gp[i]
		    call voigt (0., arg2, v0, u)
		    call voigt (arg1, arg2, v, u)
		    y = y + yp[i] * v / v0
		}
	    }
	}
	y = y / nsub
	return (y)
end


# DERIVS -- Compute model and derivatives for MR_SOLVE procedure.
# This could be optimized more for the Voigt profile by reversing
# the do loops since v0 need only be computed once per line.

procedure derivs (x, a, y, dyda, na)

real	x		# X value to be evaluated
real	a[na]		# Parameters
real	y		# Function value
real	dyda[na]	# Derivatives
int	na		# Number of parameters

int	i, j, nsub
real	dx, dx1, delta, x1, wg, wl, arg1, arg2, I0, dI, c, u, v, v0

begin
	dx = a[1]
	nsub = a[2]
	delta = dx / nsub
	dx1 = .1 * delta
	x1 = x - (dx + delta) / 2
	y = 0.
	do i = 1, na
	   dyda[i] = 0.
	do j = 1, nsub {
	    x1 = x1 + delta
	    y = y + a[3] + a[4] * x1
	    dyda[3] = dyda[3] + 1.
	    dyda[4] = dyda[4] + x1
	    do i = 9, na, 5 {
		switch (a[i+4]) {
		case GAUSS:
		    I0 = a[5] * a[i]
		    wg = a[7] * a[i+2]
		    arg1 = 1.66511 * (x1 - a[6] - a[i+1]) / wg
		    if (abs (arg1) < 5.) {
			dI = exp (-arg1**2)
			c = I0 * dI * arg1
			y = y + I0 * dI
			dyda[5] = dyda[5] + a[i] * dI
			dyda[6] = dyda[6] + c / wg
			dyda[7] = dyda[7] + c * arg1 / a[7]
			dyda[i] = dyda[i] + a[5] * dI
			dyda[i+1] = dyda[i+1] + c / wg
			dyda[i+2] = dyda[i+2] + c * arg1 / a[i+2]
		    }
		case LORENTZ:
		    I0 = a[5] * a[i]
		    wl = (a[8] * a[i+3] / 2)
		    arg2 = (x1 - a[6] - a[i+1]) / wl
		    dI = 1 / (1 + arg2**2)
		    c = 2 * I0 * dI * dI * arg2
		    y = y + I0 * dI
		    dyda[5] = dyda[5] + a[i] * dI
		    dyda[6] = dyda[6] + c / wl
		    dyda[8] = dyda[8] + c * arg2 / a[8]
		    dyda[i] = dyda[i] + a[5] * dI
		    dyda[i+1] = dyda[i+1] + c / wl
		    dyda[i+3] = dyda[i+3] + c * arg2 / a[i+3]
		case VOIGT:
		    a[7] = max (dx1, abs(a[7]))
		    a[8] = max (dx1, abs(a[8]))
		    a[i+2] = max (1E-6, abs(a[i+2]))
		    a[i+3] = max (1E-6, abs(a[i+3]))

		    I0 = a[5] * a[i]
		    wg = a[7] * a[i+2]
		    wl = a[8] * a[i+3]
		    arg1 = 1.66511 * (x1 - a[6] - a[i+1]) / wg
		    arg2 = 0.832555 * wl / wg
		    call voigt (0., arg2, v0, u)
		    call voigt (arg1, arg2, v, u)
		    v = v / v0; u = u / v0
		    dI = (1 - v) / (v0 * SQRTOFPI)
		    c = 2 * I0 * arg2
		    y = y + I0 * v
		    dyda[5] = dyda[5] + a[i] * v
		    dyda[6] = dyda[6] + 2 * c * (arg1 * v - arg2 * u) / wl
		    dyda[7] = dyda[7] +
			c * (dI + arg1 * (arg1 / arg2 * v - 2 * u)) / a[7]
		    dyda[8] = dyda[8] + c * (arg1 * u - dI) / a[8]
		    dyda[i] = dyda[i] + a[5] * v
		    dyda[i+1] = dyda[i+1] + 2 * c * (arg1 * v - arg2 * u) / wl
		    dyda[i+2] = dyda[i+2] +
			c * (dI + arg1 * (arg1 / arg2 * v - 2 * u)) / a[i+2]
		    dyda[i+3] = dyda[i+3] + c * (arg1 * u - dI) / a[i+3]
		}
	    }
	}
	y = y / nsub
	do i = 1, na
	   dyda[i] = dyda[i] / nsub
end


# DOFIT1 -- Perform nonlinear iterative fit for the specified parameters.
# This uses the Levenberg-Marquardt method from NUMERICAL RECIPES.

procedure dofit1 (fit, x, y, s, npts, a, nlines, chisq)

int	fit[5]		# Fit constraints
real	x[npts]		# X data
real	y[npts]		# Y data
real	s[npts]		# Sigma data
int	npts		# Number of points
real	a[ARB]		# Fitting parameters
int	nlines		# Number of lines
real	chisq		# Chi squared

int	i, np, nfit
real	mr, chi2
pointer	sp, flags, ptr
errchk	mr_solve

begin
	# Number of terms is 5 for each line plus common background, center,
	# intensity and widths.  Also the pixel size and number of subpixels.

	np = 5 * nlines + 8

	call smark (sp)
	call salloc (flags, np, TY_INT)
	ptr = flags

	# Background.
	switch (fit[BKG]) {
	case SINGLE:
	    Memi[ptr] = 3
	    Memi[ptr+1] = 4
	    ptr = ptr + 2
	}

	# Peaks.
	switch (fit[INT]) {
	case SINGLE:
	    Memi[ptr] = 5
	    ptr = ptr + 1
	case INDEP:
	    do i = 1, nlines {
		Memi[ptr] = 5 * i + 4
		ptr = ptr + 1
	    }
	}

	# Positions.
	switch (fit[POS]) {
	case SINGLE:
	    Memi[ptr] = 6
	    ptr = ptr + 1
	case INDEP:
	    do i = 1, nlines {
		Memi[ptr] = 5 * i + 5
		ptr = ptr + 1
	    }
	}

	# Gaussian FWHM.
	switch (fit[GAU]) {
	case SINGLE:
	    Memi[ptr] = 7
	    ptr = ptr + 1
	case INDEP:
	    do i = 1, nlines {
		Memi[ptr] = 5 * i + 6
		ptr = ptr + 1
	    }
	}

	# Lorentzian FWHM.
	switch (fit[LOR]) {
	case SINGLE:
	    Memi[ptr] = 8
	    ptr = ptr + 1
	case INDEP:
	    do i = 1, nlines {
		Memi[ptr] = 5 * i + 7
		ptr = ptr + 1
	    }
	}

	nfit = ptr - flags
	mr = -1.
	i = 0
	chi2 = MAX_REAL
	repeat {
	    call mr_solve (x, y, s, npts, a, Memi[flags], np, nfit, mr, chisq)
	    if (chi2 - chisq > 0.0001)
		i = 0
	    else
		i = i + 1
	    chi2 = chisq
	} until (i == 5)

	mr = 0.
	call mr_solve (x, y, s, npts, a, Memi[flags], np, nfit, mr, chisq)

	call sfree (sp)
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

procedure mr_solve (x, y, s, npts, params, flags, np, nfit, mr, chisq)

real	x[npts]			# X data array
real	y[npts]			# Y data array
real	s[npts]			# Sigma data array
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
	    call mr_eval (x, y, s, npts, Memr[new], flags, np, Memr[a2],
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
	call mr_eval (x, y, s, npts, Memr[new], flags, np, Memr[a1],
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

procedure mr_eval (x, y, s, npts, params, flags, np, a, delta, nfit, chisq)

real	x[npts]			# X data array
real	y[npts]			# Y data array
real	s[npts]			# Sigma data array
int	npts			# Number of data points
real	params[np]		# Parameter array
int	flags[np]		# Flag array indexing parameters to fit
int	np			# Number of parameters
real	a[nfit,nfit]		# Curvature matrix
real	delta[nfit]		# Delta array
int	nfit			# Number of parameters to fit
real	chisq			# Chi square of fit

int	i, j, k
real	ymod, dy, dydpj, dydpk, sig2i
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
	    if (IS_INDEF(ymod))
		next
	    sig2i = 1. / (s[i] * s[i])
	    dy = y[i] - ymod
	    do j = 1, nfit {
		dydpj = Memr[dydp+flags[j]-1] * sig2i
		delta[j] = delta[j] + dy * dydpj
		do k = 1, j {
		    dydpk = Memr[dydp+flags[k]-1]
		    a[j,k] = a[j,k] + dydpj * dydpk
		}
	    }
	    chisq = chisq + dy * dy * sig2i
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
