# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include	<mach.h>


#############################################################################
#  This code was copied/derived from the 'iraf-community' repository to
#  address known licensing issues with Numerical Recipes code.
#
#  Source Repository:   https://github.com/iraf-community/iraf
#  Author:              Ole Streicher
#############################################################################


# POIDEV -- Returns Poisson deviates for a given mean.
# GASDEV -- Return a normally distributed deviate of zero mean and unit var.
# MKSIGMA -- Random number with specified sigma distribution.
# TWOFFT -- Returns the complex FFTs of two input real arrays.
# REALFT -- Calculates the FFT of a set of 2N real valued data points.


# POIDEV -- Returns Poisson deviates for a given mean.
# The real value returned is an integer.
#
# Copyright(c) 2017 Anastasia Galkin
# References:
#   for lambda <= 30: Donald E. Knuth (1969). Seminumerical
#                     Algorithms. The Art of Computer Programming,
#                     Volume 2. Addison Wesley.
#   for lambda  > 30: A. C. Atkinson (1978), The Computer Generation
#                     of Poisson Random Variables, Journal of the
#                     Royal Statistical Society Series C (Applied
#                     Statistics) Vol. 28, No. 1. (1979) (pp 29-35)

real procedure poidev (xm, seed)

real	xm		# Poisson mean
long	seed		# Random number seed

real	c, beta, alpha, k, x, p, e, n, u, lhs, rhs, v, r,y
real	urand(), log(), exp(), gammln()
begin
    if (xm < 0.) {
        return 0.0
    } else if (xm < 30.) {
	x = 0.
 	p = 1.
 	e = exp(-xm)
 	while (p > e) {
 	    r = urand(seed)
	    p = p * r
 	    x = x + 1.
	}
 	return x - 1.
    } else {
        c = 0.767 - 3.36 / xm
 	beta = PI / sqrt(3. * xm)
 	alpha = beta * xm
 	k = log(c) - xm - log(beta)
	repeat {
	    u = urand(seed)
	    x = (alpha - log((1. - u) / u)) / beta
	    n = int(x + 0.5)
	    if (n >= 0.) {
	        v = urand(seed)
		y = alpha - beta * x
		lhs = y + log(v / (1. + exp(y)) ** 2)
		rhs = k + n * log(xm) - gammln(n + 1.)
	    }
	} until (lhs <= rhs)
	return n
    }
end


# GASDEV -- Return a normally distributed deviate with zero mean and unit
# variance.  The method computes two deviates simultaneously.
#
# Copyright(c) 2017 Anastasia Galkin
# Reference: G. E. P. Box and Mervin E. Muller, A Note on the Generation of
#            Random Normal Deviates, The Annals of Mathematical Statistics
#            (1958), Vol. 29, No. 2 pp. 610–611

real procedure gasdev (seed)

long	seed

int	count
data	count/0/

real	u1, u2, x
real	urand()

begin
	if (count == 0) {
	        u1 = 1. - urand (seed)
 	        u2 = urand (seed)
		x = sqrt(-2 * log(u1)) * cos(2*PI*u2);
		count = 1
	} else {
		x = sqrt(-2 * log(u1)) * sin(2*PI*u2);
		count = 0
	}
	return (x)
end


# MKSIGMA -- A sequence of random numbers of the specified sigma and
# starting seed is generated.
#
# Copyright(c) 2017 Anastasia Galkin
# Reference: G. E. P. Box and Mervin E. Muller, A Note on the Generation of
#            Random Normal Deviates, The Annals of Mathematical Statistics
#            (1958), Vol. 29, No. 2 pp. 610–611

procedure mksigma (sigma, seed, rannums, nnums)

real	sigma		# Sigma for random numbers
long	seed		# Seed for random numbers
real	rannums[nnums]	# Random numbers
int	nnums		# Number of random numbers

int	i
real	v1, v2, u1, u2, urand(), sqrt()

begin
	if (sigma > 0.) {
	    for (i=1; i<=nnums; i=i+1) {
	        u1 = 1. - urand (seed)
 	        u2 = urand (seed)
		v1 = sqrt(-2 * log(u1)) * cos(2*PI*u2)
		rannums[i] = v1 * sigma
		if (i == nnums)
		    break
		v2 = sqrt(-2 * log(u1)) * sin(2*PI*u2)
		i = i + 1
		rannums[i] = v2 * sigma
	    }
	}
end


# TWOFFT - Given two real input arrays DATA1 and DATA2, each of length
# N, this routine calls cc_four1() and returns two complex output arrays,
# FFT1 and FFT2, each of complex length N (i.e. real length 2*N), which
# contain the discrete Fourier transforms of the respective DATAs.
#
# This routine simply calls the routines provided by FFTPACK

procedure twofft (data1, data2, fft1, fft2, N)

real	data1[ARB], data2[ARB]	# Input data arrays
real	fft1[ARB], fft2[ARB]	# Output FFT arrays
int	N			# No. of points

int j
pointer sp, wsave
begin
    call smark(sp)
    call salloc(wsave, 4*N+15, TY_REAL)
    call cfft1i(N, Memr[wsave])

    do j=1, N {
        fft1[2*j-1] = data1[j]
        fft1[2*j] = 0.0
        fft2[2*j-1] = data2[j]
        fft2[2*j] = 0.0
    }
    call cfftf(N, fft1, Memr[wsave])
    call cfftf(N, fft2, Memr[wsave])
    do j=1, N {
        fft1[2*j] = -fft1[2*j]
        fft2[2*j] = -fft2[2*j]
    }
    call sfree(sp)
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
# This routine simply calls the routines provided by FFTPACK

procedure realft (data, N, isign)

real	data[ARB]	# Input data array & output FFT
int	N		# No. of points
int	isign		# Direction of transfer

pointer sp, wsave
real last
int j
begin
    call smark(sp)
    call salloc(wsave, 4*N+15, TY_REAL)
    call rfft1i(2*N, Memr[wsave])

    if (isign == 1) {
        call rfft1f(2*N, data, Memr[wsave])
        last = data[2*N]
        do j=2*N-1,3,-2 {
            data[j+1] = -data[j]
            data[j] = data[j-1]
        }
        data[2] = last
    } else {
        data[1] = data[1]/2.0
        last = data[2]/2.0
        do j=2,2*N-2,2 {
            data[j] = data[j+1]/2.0
            data[j+1] = -data[j+2]/2.0
        }
        data[2*N] = last
        call rfft1b(2*N, data, Memr[wsave])
    }
    call sfree(sp)

end


################################################################################
# LU Decomosition
################################################################################

# Given an N x N matrix A, with physical dimension N, this routine
# replaces it by the LU decomposition of a rowwise permutation of
# itself.  A and N are input.  A is output, arranged as in equation
# (2.3.14) above; INDX is an output vector which records the row
# permutation effected by the partial pivioting; D is output as +/-1
# depending on whether the number of row interchanges was even or odd,
# respectively.  This routine is used in combination with LUBKSB to
# solve linear equations or invert a matrix.
#
# This routine simply calls the LU decomposition routine provided by LAPACK.

procedure ludcmp (a, n, np, indx, d)

real	a[np,np]	# io: input a, output decomposed a
int	n		# i: logical size of a is n x n
int	np		# i: space allocated for a
int	indx[n]		# o: index to be used by xt_lubksb
real	d		# o: +1 or -1

int	istat

begin
	d = 1.0
	call sgetrf(n, n, a, np, indx, istat)
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
# This routine simply calls the LU decomposition routine provided by LAPACK.

procedure lubksb (a, n, np, indx, b)

real	a[np,np]
int	n
int	np
int	indx[n]
real	b[n]

int	status

begin
	call sgetrs('N', n, 1, a, np, indx, b, n, status)
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
