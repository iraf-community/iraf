include	<mach.h>

# DOFIT -- Fit gaussian components.  This is an interface to DOFIT1
# which puts parameters into the form required by DOFIT1 and vice-versa.
# It also implements a constrained approach to the solution.

procedure dofit (bkgfit, posfit, intfit, sigfit, x, y, s, npts, dx, nsub,
	y1, dy, xg, yg, sg, ng, chisq)

int	bkgfit		# Fit background (0=no, 1=yes)
int	posfit		# Position fitting flag (1=fixed, 2=single, 3=all)
int	intfit		# Intensity fitting flag (1=fixed, 2=single, 3=all)
int	sigfit		# Sigma fitting flag (1=fixed, 2=single, 3=all)
real	x[npts]		# X data
real	y[npts]		# Y data
real	s[npts]		# Sigma data
int	npts		# Number of points
real	dx		# Pixel size
int	nsub		# Number of subpixels
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
	call salloc (a, 7 + 3 * ng, TY_REAL)

	# Convert positions and widths relative to first component.
	Memr[a] = dx
	Memr[a+1] = nsub
	Memr[a+2] = y1
	Memr[a+3] = dy
	Memr[a+4] = yg[1]
	Memr[a+5] = xg[1]
	Memr[a+6] = sg[1]
	do i = 1, ng {
	    j = a + 3 * i + 4
	    Memr[j] = yg[i] / Memr[a+4]
	    Memr[j+1] = xg[i] - Memr[a+5]
	    Memr[j+2] = sg[i] / Memr[a+6]
	}

	# Do fit.
	do i = 0, bkgfit {
	    switch (10*posfit+sigfit) {
	    case 11:
		call dofit1 (i, 1, intfit, 1, x, y, s, npts, Memr[a], ng, chisq)
	    case 12:
		call dofit1 (i, 1, intfit, 2, x, y, s, npts, Memr[a], ng, chisq)
	    case 13:
		call dofit1 (i, 1, intfit, 2, x, y, s, npts, Memr[a], ng, chisq)
		call dofit1 (i, 1, intfit, 3, x, y, s, npts, Memr[a], ng, chisq)
	    case 21:
		call dofit1 (i, 2, intfit, 1, x, y, s, npts, Memr[a], ng, chisq)
	    case 22:
		call dofit1 (i, 1, intfit, 2, x, y, s, npts, Memr[a], ng, chisq)
		call dofit1 (i, 2, intfit, 2, x, y, s, npts, Memr[a], ng, chisq)
	    case 23:
		call dofit1 (i, 1, intfit, 2, x, y, s, npts, Memr[a], ng, chisq)
		call dofit1 (i, 2, intfit, 2, x, y, s, npts, Memr[a], ng, chisq)
		call dofit1 (i, 2, intfit, 3, x, y, s, npts, Memr[a], ng, chisq)
	    case 31:
		call dofit1 (i, 2, intfit, 1, x, y, s, npts, Memr[a], ng, chisq)
		call dofit1 (i, 3, intfit, 1, x, y, s, npts, Memr[a], ng, chisq)
	    case 32:
		call dofit1 (i, 1, intfit, 2, x, y, s, npts, Memr[a], ng, chisq)
		call dofit1 (i, 2, intfit, 2, x, y, s, npts, Memr[a], ng, chisq)
		call dofit1 (i, 3, intfit, 2, x, y, s, npts, Memr[a], ng, chisq)
	    case 33:
		call dofit1 (i, 1, intfit, 2, x, y, s, npts, Memr[a], ng, chisq)
		call dofit1 (i, 2, intfit, 2, x, y, s, npts, Memr[a], ng, chisq)
		call dofit1 (i, 3, intfit, 2, x, y, s, npts, Memr[a], ng, chisq)
		call dofit1 (i, 3, intfit, 3, x, y, s, npts, Memr[a], ng, chisq)
	    }
	}

	y1 = Memr[a+2]
	dy = Memr[a+3]
	do i = 1, ng {
	    j = a + 3 * i + 4
	    yg[i] = Memr[j] * Memr[a+4]
	    xg[i] = Memr[j+1] + Memr[a+5]
	    sg[i] = abs (Memr[j+2] * Memr[a+6])
	}

	call sfree (sp)
end


# MODEL -- Compute model.
#
#	I(x) = I(i) exp {-[(x-xg(i)) / sg(i)]**2 / 2.}
#
# where the params are I1, I2, xg, yg, and sg.

real procedure model (x, dx, nsub, xg, yg, sg, ng)

real	x		# X value to be evaluated
real	dx		# Pixel width
int	nsub		# Number of subpixels
real	xg[ng]		# X coordinates of gaussians
real	yg[ng]		# Y coordinates of gaussians
real	sg[ng]		# Sigmas of gaussians
int	ng		# Number of gaussians

int	i, j
real	delta, x1, y, arg

begin
	delta = dx / nsub
	x1 = x - (dx + delta) / 2
	y = 0.
	do j = 1, nsub {
	    x1 = x1 + delta
	    do i = 1, ng {
		arg = (x1 - xg[i]) / sg[i]
		if (abs (arg) < 7.)
		    y = y + yg[i] * exp (-arg**2 / 2.)
	    }
	}
	y = y / nsub
	return (y)
end


# DOFIT1 -- Perform nonlinear iterative fit for the specified parameters.
# This uses the Levenberg-Marquardt method from NUMERICAL RECIPES.

procedure dofit1 (bkgfit, posfit, intfit, sigfit, x, y, s, npts, a, nlines,
	chisq)

int	bkgfit		# Background fit (0=no, 1=yes)
int	posfit		# Position fitting flag (1=fixed, 2=one, 3=all)
int	intfit		# Intensity fitting flag (1=fixed, 2=one, 3=all)
int	sigfit		# Sigma fitting flag (1=fixed, 2=one, 3=all)
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
	# Number of terms is 3 for each line plus common background, center,
	# intensity and sigma.  Also the pixel size and number of subpixels.

	np = 3 * nlines + 7

	call smark (sp)
	call salloc (flags, np, TY_INT)
	ptr = flags

	# Background.
	if (bkgfit == 1) {
	    Memi[ptr] = 3
	    Memi[ptr+1] = 4
	    ptr = ptr + 2
	}

	# Peaks.
	switch (intfit) {
	case 2:
	    Memi[ptr] = 5
	    ptr = ptr + 1
	case 3:
	    do i = 1, nlines {
		Memi[ptr] = 3 * i + 5
		ptr = ptr + 1
	    }
	}

	# Positions.
	switch (posfit) {
	case 2:
	    Memi[ptr] = 6
	    ptr = ptr + 1
	case 3:
	    do i = 1, nlines {
	        Memi[ptr] = 3 * i + 6
		ptr = ptr + 1
	    }
	}

	# Sigmas.
	switch (sigfit) {
	case 2:
	    Memi[ptr] = 7
	    ptr = ptr + 1
	case 3:
	    do i = 1, nlines {
	        Memi[ptr] = 3 * i + 7
		ptr = ptr + 1
	    }
	}

	nfit = ptr - flags
	mr = -1.
	i = 0
	chi2 = MAX_REAL
	repeat {
	    call mr_solve (x, y, s, npts, a, Memi[flags], np, nfit, mr, chisq)
	    if (chi2 - chisq > 1.)
		i = 0
	    else
		i = i + 1
	    chi2 = chisq
	} until (i == 3)

	mr = 0.
	call mr_solve (x, y, s, npts, a, Memi[flags], np, nfit, mr, chisq)

	call sfree (sp)
end


# DERIVS -- Compute model and derivatives for MR_SOLVE procedure.
#
#	I(x) = I1 + I2 * x + I3 * I(i) exp {-[(x-xc-dx(i))/(sig*sig(i))]**2/2.}
#
# where the params are I1, I2, I3, xc, sig, I(i), dx(i), sig(i) (i=1,nlines).

procedure derivs (x, a, y, dyda, na)

real	x		# X value to be evaluated
real	a[na]		# Parameters
real	y		# Function value
real	dyda[na]	# Derivatives
int	na		# Number of parameters

int	i, j, nsub
real	dx, delta, x1, I0, sig, arg, ex, fac

begin
	dx = a[1]
	nsub = a[2]
	delta = dx / nsub
	x1 = x - (dx + delta) / 2
	y = 0.
	do i = 1, na
	   dyda[i] = 0.
	do j = 1, nsub {
	    x1 = x1 + delta
	    y = y + a[3] + a[4] * x1
	    dyda[3] = dyda[3] + 1.
	    dyda[4] = dyda[4] + x1
	    do i = 8, na, 3 {
		I0 = a[5] * a[i]
		sig = a[7] * a[i+2]
		arg = (x1 - a[6] - a[i+1]) / sig
		if (abs (arg) < 7.)
		    ex = exp (-arg**2 / 2.)
		else
		    ex = 0.
		fac = I0 * ex * arg

		y = y + I0 * ex
		dyda[5] = dyda[5] + a[i] * ex
		dyda[6] = dyda[6] + fac / sig
		dyda[7] = dyda[7] + fac * arg / a[7]
		dyda[i] = dyda[i] + a[5] * ex
		dyda[i+1] = dyda[i+1] + fac / sig
		dyda[i+2] = dyda[i+2] + fac * arg / a[i+2]
	    }
	}
	y = y / nsub
	do i = 1, na
	   dyda[i] = dyda[i] / nsub
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
