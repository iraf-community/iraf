include	<mach.h>


# ID_MR_DOFIT -- Fit gaussian components.  This is an interface to ID_DOFIT1
# which puts parameters into the form required by ID_DOFIT1 and vice-versa.
# It also implements a constrained approach to the solution.

procedure id_mr_dofit (bkgfit, posfit, sigfit, x, y, npts, y1, dy, xg, yg, sg,
	ng, chisq)

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
errchk	id_dofit1

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
		call id_dofit1 (i, 1, 1, x, y, npts, Memr[a], ng, chisq)
	    case 12:
		call id_dofit1 (i, 1, 2, x, y, npts, Memr[a], ng, chisq)
	    case 13:
		call id_dofit1 (i, 1, 2, x, y, npts, Memr[a], ng, chisq)
		call id_dofit1 (i, 1, 3, x, y, npts, Memr[a], ng, chisq)
	    case 21:
		call id_dofit1 (i, 2, 1, x, y, npts, Memr[a], ng, chisq)
	    case 22:
		call id_dofit1 (i, 1, 2, x, y, npts, Memr[a], ng, chisq)
		call id_dofit1 (i, 2, 2, x, y, npts, Memr[a], ng, chisq)
	    case 23:
		call id_dofit1 (i, 1, 2, x, y, npts, Memr[a], ng, chisq)
		call id_dofit1 (i, 2, 2, x, y, npts, Memr[a], ng, chisq)
		call id_dofit1 (i, 2, 3, x, y, npts, Memr[a], ng, chisq)
	    case 31:
		call id_dofit1 (i, 2, 1, x, y, npts, Memr[a], ng, chisq)
		call id_dofit1 (i, 3, 1, x, y, npts, Memr[a], ng, chisq)
	    case 32:
		call id_dofit1 (i, 1, 2, x, y, npts, Memr[a], ng, chisq)
		call id_dofit1 (i, 2, 2, x, y, npts, Memr[a], ng, chisq)
		call id_dofit1 (i, 3, 2, x, y, npts, Memr[a], ng, chisq)
	    case 33:
		call id_dofit1 (i, 1, 2, x, y, npts, Memr[a], ng, chisq)
		call id_dofit1 (i, 2, 2, x, y, npts, Memr[a], ng, chisq)
		call id_dofit1 (i, 3, 2, x, y, npts, Memr[a], ng, chisq)
		call id_dofit1 (i, 3, 3, x, y, npts, Memr[a], ng, chisq)
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


# ID_MODEL -- Compute model.
#
#	I(x) = I(i) exp {-[(x-xg(i)) / sg(i)]**2 / 2.}
#
# where the params are I1, I2, xg, yg, and sg.

real procedure id_model (x, xg, yg, sg, ng)

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


# ID_DOFIT1 -- Perform nonlinear iterative fit for the specified parameters.
# This uses the Levenberg-Marquardt method from NUMERICAL RECIPES.

procedure id_dofit1 (bkgfit, posfit, sigfit, x, y, npts, a, nlines, chisq)

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
errchk	id_mr_solve

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
	    call id_mr_solve (x, y, npts, a, Memi[flags], np, nfit, mr, chisq)
	    if (chi2 - chisq > 1.)
		i = 0
	    else
		i = i + 1
	    chi2 = chisq
	} until (i == 3)

	mr = 0.
	call id_mr_solve (x, y, npts, a, Memi[flags], np, nfit, mr, chisq)

	call sfree (sp)
end


# ID_DERIVS -- Compute model and derivatives for MR_SOLVE procedure.
#
#	I(x) = I1 + I2 * x + I(i) exp {-[(x-xc-dx(i)) / (sig * sig(i))]**2 / 2.}
#
# where the params are I1, I2, xc, sig, I(i), dx(i), and sig(i) (i=1,nlines).

procedure id_derivs (x, a, y, dyda, na)

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


# ID_MR_SOLVE -- Levenberg-Marquardt nonlinear chi square minimization.
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

procedure id_mr_solve (x, y, npts, params, flags, np, nfit, mr, chisq)

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

errchk	id_mr_invert

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
	    call id_mr_eval (x, y, npts, Memr[new], flags, np, Memr[a2],
	        Memr[delta2], nfit, chisq)
	    mr = 0.001
	}

	# Restore last good fit and apply the Marquardt parameter.
	call amovr (Memr[a2], Memr[a1], nfit * nfit)
	call amovr (Memr[delta2], Memr[delta1], nfit)
	do i = 1, nfit
	    Memr[a1+(i-1)*(nfit+1)] = Memr[a2+(i-1)*(nfit+1)] * (1. + mr)

	# Matrix solution.
	call id_mr_invert (Memr[a1], Memr[delta1], nfit)

	# Compute the new values and curvature matrix.
	do i = 1, nfit
	    Memr[new+flags[i]-1] = params[flags[i]] + Memr[delta1+i-1]
	call id_mr_eval (x, y, npts, Memr[new], flags, np, Memr[a1],
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


# ID_MR_EVAL -- Evaluate curvature matrix.  This calls procedure DERIVS.

procedure id_mr_eval (x, y, npts, params, flags, np, a, delta, nfit, chisq)

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
	    call id_derivs (x[i], params, ymod, Memr[dydp], np)
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

procedure id_mr_invert (a, b, n)

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
