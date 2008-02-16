# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include	<mach.h>

# GAMMLN -- Return natural log of gamma function.
# POIDEV -- Returns Poisson deviates for a given mean.
# GASDEV -- Return a normally distributed deviate of zero mean and unit var.
# MR_SOLVE -- Levenberg-Marquardt nonlinear chi square minimization.
#     MR_EVAL -- Evaluate curvature matrix.
#     MR_INVERT -- Solve a set of linear equations using Householder transforms.
# TWOFFT -- Returns the complex FFTs of two input real arrays.
# REALFT -- Calculates the FFT of a set of 2N real valued data points.
#     FOUR1 -- Computes the forward or inverse FFT of the input array.


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
#
# Modified to return zero for input values less than or equal to zero.

real procedure poidev (xm, seed)

real	xm		# Poisson mean
long	seed		# Random number seed

real	oldm, g, em, t, y, ymin, ymax, sq, alxm, gammln(), urand(), gasdev()
data	oldm /-1./

begin
	if (xm <= 0)
	    em = 0
	else if (xm < 12) {
	    if (xm != oldm) {
		oldm = xm
		g = exp (-xm)
	    }
	    em = 0
	    for (t = urand (seed); t > g; t = t * urand (seed))
		em = em + 1
	} else if (xm < 100) {
	    if (xm != oldm) {
		oldm = xm
		sq = sqrt (2. * xm)
		ymin = -xm / sq
		ymax = (1000 - xm) / sq
		alxm = log (xm)
		g = xm * alxm - gammln (xm+1.)
	    }
	    repeat {
		repeat {
	            y = tan (PI * urand(seed))
		} until (y >= ymin)
	        em = int (sq * min (y, ymax) + xm)
	        t = 0.9 * (1 + y**2) * exp (em * alxm - gammln (em+1) - g)
	    } until (urand(seed) <= t)
	} else
	    em = xm + sqrt (xm) * gasdev (seed)
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


# TWOFFT - Given two real input arrays DATA1 and DATA2, each of length
# N, this routine calls cc_four1() and returns two complex output arrays,
# FFT1 and FFT2, each of complex length N (i.e. real length 2*N), which
# contain the discrete Fourier transforms of the respective DATAs.  As
# always, N must be an integer power of 2.
#
# Based on Numerical Recipes by Press, Flannery, Teukolsky, and Vetterling.
# Used by permission of the authors.
# Copyright(c) 1986 Numerical Recipes Software.

procedure twofft (data1, data2, fft1, fft2, N)

real	data1[ARB], data2[ARB]	# Input data arrays
real	fft1[ARB], fft2[ARB]	# Output FFT arrays
int	N			# No. of points

int	nn3, nn2, jj, j
real	rep, rem, aip, aim

begin
	nn2 = 2  +  N  +  N
	nn3 = nn2  +  1

	jj = 2
	for (j=1; j <= N; j = j  +  1) {
	    fft1[jj-1] = data1[j]	# Pack 'em into one complex array
	    fft1[jj] = data2[j]
	    jj = jj  +  2
	}

	call four1 (fft1, N, 1)		# Transform the complex array
	fft2[1] = fft1[2]
	fft2[2] = 0.0
	fft1[2] = 0.0
	for (j=3; j <= N + 1; j = j  +  2) {
	    rep = 0.5 * (fft1[j]  +  fft1[nn2-j])
	    rem = 0.5 * (fft1[j] - fft1[nn2-j])
	    aip = 0.5 * (fft1[j + 1]  +  fft1[nn3-j])
	    aim = 0.5 * (fft1[j + 1] - fft1[nn3-j])
	    fft1[j] = rep
	    fft1[j+1] = aim
	    fft1[nn2-j] = rep
	    fft1[nn3-j] = -aim
	    fft2[j] = aip
	    fft2[j+1] = -rem
	    fft2[nn2-j] = aip
	    fft2[nn3-j] = rem
	}

end


# REALFT - Calculates the Fourier Transform of a set of 2N real valued
# data points.  Replaces this data (which is stored in the array DATA) by
# the positive frequency half of it's complex Fourier Transform.  The real
# valued first and last components of the complex transform are returned
# as elements DATA(1) and DATA(2) respectively.  N must be an integer power
# of 2.  This routine also calculates the inverse transform of a complex
# array if it is the transform of real data.  (Result in this case must be
# multiplied by 1/N). A forward transform is perform for isign == 1, other-
# wise the inverse transform is computed.
#
# Based on Numerical Recipes by Press, Flannery, Teukolsky, and Vetterling.
# Used by permission of the authors.
# Copyright(c) 1986 Numerical Recipes Software.

procedure realft (data, N, isign)

real 	data[ARB]	# Input data array & output FFT
int	N		# No. of points
int 	isign		# Direction of transfer

double	wr, wi, wpr, wpi, wtemp, theta	# Local variables
real	c1, c2, h1r, h1i, h2r, h2i
real	wrs, wis
int	i, i1, i2, i3, i4 
int	N2P3

begin
					# Initialize
	theta = PI/double(N)
	c1 = 0.5
	
	if (isign == 1) {
	  c2 = -0.5
	  call four1 (data,n,1)		# Forward transform is here
	} else {
	  c2 = 0.5
	  theta = -theta
	} 

	wtemp = sin (0.5 * theta)
	wpr = -2.0d0 * wtemp * wtemp
	wpi = dsin (theta)
	wr = 1.0D0  +  wpr
	wi = wpi
	n2p3 = 2*n + 3

	for (i=2; i<=n/2; i = i  +  1) {
	  i1 = 2 * i - 1
	  i2 = i1 + 1
	  i3 = n2p3 - i2
	  i4 = i3 + 1
	  wrs = sngl (wr)
	  wis = sngl (wi)
	  			# The 2 transforms are separated out of Z
	  h1r = c1 * (data[i1] + data[i3])	
	  h1i = c1 * (data[i2] - data[i4])
	  h2r = -c2 * (data[i2] + data[i4])
	  h2i = c2 * (data[i1] - data[i3])
				# Here they are recombined to form the true
				# transform of the original real data.
	  data[i1] = h1r + wr*h2r - wi*h2i
	  data[i2] = h1i + wr*h2i + wi*h2r
	  data[i3] = h1r - wr*h2r + wi*h2i
	  data[i4] = -h1i + wr*h2i + wi*h2r

	  wtemp = wr		# The reccurrence
	  wr = wr * wpr - wi * wpi + wr
	  wi = wi * wpr + wtemp * wpi + wi
	}

	if (isign == 1) {
	  h1r = data[1]
	  data[1] = h1r + data[2]
	  data[2] = h1r - data[2]
	} else {
	  h1r = data[1]
	  data[1] = c1 * (h1r + data[2])
	  data[2] = c1 * (h1r - data[2])
	  call four1 (data,n,-1)
	}

end


# FOUR1 -  Replaces DATA by it's discrete transform, if ISIGN is input
# as 1; or replaces DATA by NN times it's inverse discrete Fourier transform
# if ISIGN is input as -1.  Data is a complex array of length NN or, equiv-
# alently, a real array of length 2*NN.  NN *must* be an integer power of
# two.
#
# Based on Numerical Recipes by Press, Flannery, Teukolsky, and Vetterling.
# Used by permission of the authors.
# Copyright(c) 1986 Numerical Recipes Software.

procedure four1 (data, nn, isign)

real	data[ARB]	# Data array (returned as FFT)
int	nn		# No. of points in data array
int	isign		# Direction of transform

double 	wr, wi, wpr, wpi		# Local variables
double	wtemp, theta
real 	tempr, tempi
int	i, j, istep
int	n, mmax, m

begin
	n = 2 * nn
	j = 1
	for (i=1; i<n; i = i  +  2) {
	  if (j > i) {				# Swap 'em
	    tempr = data[j]
	    tempi = data[j+1]
	    data[j] = data[i]
	    data[j+1] = data[i+1]
	    data[i] = tempr
	    data[i+1] = tempi
	  }
	  m = n / 2
	  while (m >= 2 && j > m) {
	    j = j - m
	    m = m / 2
	  }
	  j = j + m
	}
	mmax = 2
	while (n > mmax) {
	  istep = 2 * mmax
	  theta = TWOPI / double (isign*mmax)
	  wtemp = dsin (0.5*theta)
	  wpr = -2.d0 * wtemp * wtemp
	  wpi = dsin (theta)
	  wr = 1.d0
	  wi = 0.d0
	  for (m=1; m < mmax; m = m  +  2) {
	    for (i=m; i<=n; i = i  +  istep) {
		j = i + mmax
		tempr = real (wr) * data[j] - real (wi) * data[j+1]
		tempi = real (wr) * data[j + 1] + real (wi) * data[j]
		data[j] = data[i] - tempr
		data[j+1] = data[i+1] - tempi
		data[i] = data[i] + tempr
		data[i+1] = data[i+1] + tempi
	    }
	    wtemp = wr
	    wr = wr * wpr - wi * wpi + wr
	    wi = wi * wpr + wtemp * wpi + wi
	  } 
	  mmax = istep
	}
end


################################################################################
# LU Decomosition
################################################################################
define	TINY	(1E-20)		# Number of numerical limit

# Given an N x N matrix A, with physical dimension N, this routine
# replaces it by the LU decomposition of a rowwise permutation of
# itself.  A and N are input.  A is output, arranged as in equation
# (2.3.14) above; INDX is an output vector which records the row
# permutation effected by the partial pivioting; D is output as +/-1
# depending on whether the number of row interchanges was even or odd,
# respectively.  This routine is used in combination with LUBKSB to
# solve linear equations or invert a matrix.
#
# Based on Numerical Recipes by Press, Flannery, Teukolsky, and Vetterling.
# Used by permission of the authors.
# Copyright(c) 1986 Numerical Recipes Software.

procedure ludcmp (a, n, np, indx, d)

real	a[np,np]
int	n
int	np
int	indx[n]
real	d

int	i, j, k, imax
real	aamax, sum, dum
pointer	vv

begin
	# Allocate memory.
	call malloc (vv, n, TY_REAL)
	
	# Loop over rows to get the implict scaling information.
	d = 1.
	do i = 1, n {
	    aamax = 0.
	    do j = 1, n {
	        if (abs (a[i,j]) > aamax)
		    aamax = abs (a[i,j])
	    }
	    if (aamax == 0.) {
	    	call mfree (vv, TY_REAL)
	        call error (1, "Singular matrix")
	    }
	    Memr[vv+i-1] = 1. / aamax
	}

	# This is the loop over columns of Crout's method.
	do j = 1, n {
	    do i = 1, j-1 {
	        sum = a[i,j]
		do k = 1, i-1
		    sum = sum - a[i,k] * a[k,j]
		a[i,j] = sum
	    }

	    aamax = 0.
	    do i = j, n {
	        sum = a[i,j]
		do k = 1, j-1
		    sum = sum - a[i,k] * a[k,j]
		a[i,j] = sum
		dum = Memr[vv+i-1] * abs (sum)
		if (dum >= aamax) {
		    imax = i
		    aamax = dum
		}
	    }

	    if (j != imax) {
	        do k = 1, n {
		    dum = a[imax,k]
		    a[imax,k] = a[j,k]
		    a[j,k] = dum
		}
		d = -d
		Memr[vv+imax-1] = Memr[vv+j-1]
	    }
	    indx[j] = imax

	    # Now, finally, divide by the pivot element.
	    # If the pivot element is zero the matrix is signular (at
	    # least to the precission of the algorithm.  For some
	    # applications on singular matrices, it is desirable to
	    # substitute TINY for zero.

	    if (a[j,j] == 0.)
	        a[j,j] = TINY
	    if (j != n) {
	        dum = 1. / a[j,j]
		do i = j+1, n
		    a[i,j] = a[i,j] * dum
	    }
	}

	call mfree (vv, TY_REAL)
end


# Solves the set of N linear equations AX = B.  Here A is input, not
# as the matrix of A but rather as its LU decomposition, determined by
# the routine LUDCMP.  INDX is input as the permuation vector returned
# by LUDCMP.  B is input as the right-hand side vector B, and returns
# with the solution vector X.  A, N, NP and INDX are not modified by
# this routine and can be left in place for successive calls with
# different right-hand sides B.  This routine takes into account the
# possiblity that B will begin with many zero elements, so it is
# efficient for use in matrix inversion.
#
# Based on Numerical Recipes by Press, Flannery, Teukolsky, and Vetterling.
# Used by permission of the authors.
# Copyright(c) 1986 Numerical Recipes Software.

procedure lubksb (a, n, np, indx, b)

real	a[np,np]
int	n
int	np
int	indx[n]
real	b[n]

int	i, j, ii, ll
real	sum

begin
	ii = 0
	do i = 1, n {
	    ll = indx[i]
	    sum = b[ll]
	    b[ll] = b[i]
	    if (ii != 0) {
	        do j = ii, i-1
		    sum = sum - a[i,j] * b[j]
	    } else if (sum != 0.)
	        ii = i
	    b[i] = sum
	}

	do i = n, 1, -1 {
	    sum = b[i]
	    if (i < n) {
	        do j = i+1, n
		    sum = sum - a[i,j] * b[j]
	    }
	    b[i] = sum / a[i,i]
	}
end


# Invert a matrix using LU decomposition using A as both input and output.

procedure luminv (a, n, np)

real	a[np,np]
int	n
int	np

int	i, j
real	d
pointer	y, indx

begin
	# Allocate working memory.
	call calloc (y, n*n, TY_REAL)
	call malloc (indx, n, TY_INT)

	# Setup identify matrix.
	do i = 0, n-1
	    Memr[y+(n+1)*i] = 1.

	# Do LU decomposition.
	call ludcmp (a, n, np, Memi[indx], d)

	# Find inverse by columns.
	do j = 0, n-1
	    call lubksb (a, n, np, Memi[indx], Memr[y+n*j])

	# Return inverse in a.
	do i = 1, n
	    do j = 1, n
	        a[i,j] = Memr[y+n*(j-1)+(i-1)]

	call mfree (y, TY_REAL)
end
################################################################################
