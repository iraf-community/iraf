# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>

# GAMMLN -- Return natural log of gamma function.
# POIDEV -- Returns Poisson deviates for a given mean.
# GASDEV -- Return a normally distributed deviate of zero mean and unit var.
# MR_SOLVE -- Levenberg-Marquardt nonlinear chi square minimization.
#     MR_EVAL -- Evaluate curvature matrix.
#     MR_INVERT -- Solve a set of linear equations using Householder transforms.


# GAMMLN -- Return natural log of gamma function.
# Argument must greater than 0.  Full accuracy is obtained for values
# greater than 1.  For 0<xx<1, the reflection formula can be used first.
#
# Based on Numerical Recipes by Press, Flannery, Teukolsky, and Vetterling.
# Used by permission of the authors.
# Copyright(c) 1986 Numerical Recipes Software.

real procedure gammln (xx)

real	xx		# Value to be evaluated

int	j
double	cof[6], stp, x, tmp, ser
data	cof, stp / 76.18009173D0, -86.50532033D0, 24.01409822D0,
		-1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/

begin
	x = xx - 1.0D0
	tmp = x + 5.5D0
	tmp = (x + 0.5D0) * log (tmp) - tmp
	ser = 1.0D0
	do j = 1, 6 {
	    x = x + 1.0D0
	    ser = ser + cof[j] / x
	}
	return (tmp + log (stp * ser))
end


# POIDEV -- Returns Poisson deviates for a given mean.
# The real value returned is an integer.
#
# Based on Numerical Recipes by Press, Flannery, Teukolsky, and Vetterling.
# Used by permission of the authors.
# Copyright(c) 1986 Numerical Recipes Software.

real procedure poidev (xm, seed)

real	xm		# Poisson mean
long	seed		# Random number seed

real	oldm, g, em, t, y, sq, alxm, gammln(), urand()
data	oldm /-1./

begin
	if (xm < 12) {
	    if (xm != oldm) {
		oldm = xm
		g = exp (-xm)
	    }
	    em = 0
	    for (t = urand (seed); t > g; t = t * urand (seed))
		em = em + 1
	} else {
	    if (xm != oldm) {
		oldm = xm
		sq = sqrt (2. * xm)
		alxm = log (xm)
		g = xm * alxm - gammln (xm+1.)
	    }
	    repeat {
		repeat {
	            y = tan (PI * urand(seed))
		    em = sq * y + xm
		} until (em >= 0.)
	        em = int (em)
	        t = 0.9 * (1 + y**2) * exp (em * alxm - gammln (em+1) - g)
	    } until (urand(seed) <= t)
	}
	return (em)
end


# GASDEV -- Return a normally distributed deviate with zero mean and unit
# variance.  The method computes two deviates simultaneously.
#
# Based on Numerical Recipes by Press, Flannery, Teukolsky, and Vetterling.
# Used by permission of the authors.
# Copyright(c) 1986 Numerical Recipes Software.

real procedure gasdev (seed)

long	seed		# Seed for random numbers

real	v1, v2, r, fac, urand()
int	iset
data	iset/0/

begin
	if (iset == 0) {
	    repeat {
	        v1 = 2 * urand (seed) - 1.
	        v2 = 2 * urand (seed) - 1.
	        r = v1 ** 2 + v2 ** 2
	    } until ((r > 0) && (r < 1))
	    fac = sqrt (-2. * log (r) / r)

	    iset = 1
	    return (v1 * fac)
	} else {
	    iset = 0
	    return (v2 * fac)
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
#
# These routines have their origin in Numerical Recipes, MRQMIN, MRQCOF,
# but have been completely redesigned.

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
	    mr = 0.1 * mr
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
# This calls a routine published in in "Solving Least Squares Problems",
# by Charles L. Lawson and Richard J. Hanson, Prentice Hall, 1974.

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

	call hfti (a, n, n, n, b, n, 1, 0.001, krank, rnorm,
	    Memr[h], Memr[g], Memi[ip])

	call sfree (sp)
end
