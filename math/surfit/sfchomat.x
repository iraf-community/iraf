# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/surfit.h>
include "surfitdef.h"
include <mach.h>

# SFCHOFAC -- Routine to calculate the Cholesky factorization of a
# symmetric, positive semi-definite banded matrix. This routines was
# adapted from the bchfac.f routine described in "A Practical Guide
# to Splines", Carl de Boor (1978).

procedure sfchofac (matrix, nbands, nrows, matfac, ier)

VAR_TYPE matrix[nbands, nrows]	# data matrix
int	nbands			# number of bands
int	nrows			# number of rows
VAR_TYPE matfac[nbands, nrows]	# Cholesky factorization
int	ier			# error code

int	i, n, j, imax, jmax
VAR_TYPE ratio

begin
	if (nrows == 1) {
	    if (matrix[1,1] > 0.)
	        matfac[1,1] = 1. / matrix[1,1]
	    return
	}
		
	# copy matrix into matfac
	do n = 1, nrows {
	    do j = 1, nbands
		matfac[j,n] = matrix[j,n]
	}

	do n = 1, nrows {
	    # test to see if matrix is singular
	    if (((matfac[1,n] + matrix[1,n]) - matrix[1,n]) <= DELTA) {
		do j = 1, nbands
		    matfac[j,n] = 0.
		ier = SINGULAR
		next
	    }

	    matfac[1,n] = 1. / matfac[1,n]
	    imax = min (nbands - 1, nrows - n)
	    if (imax < 1)
		next

	    jmax = imax
	    do i = 1, imax {
		ratio = matfac[i+1,n] * matfac[1,n]
		do j = 1, jmax
		    matfac[j,n+i] = matfac[j,n+i] - matfac[j+i,n] * ratio
		jmax = jmax - 1
		matfac[i+1,n] = ratio
	    }
	}
end


# SFCHOSLV -- Solve the matrix whose Cholesky factorization was calculated in
# SFCHOFAC for the coefficients. This routine was adapted from bchslv.f
# described in "A Practical Guide to Splines", by Carl de Boor (1978).

procedure sfchoslv (matfac, nbands, nrows, vector, coeff)

VAR_TYPE matfac[nbands,nrows] 		# Cholesky factorization
int	nbands				# number of bands
int	nrows				# number of rows
VAR_TYPE vector[nrows]			# right side of matrix equation
VAR_TYPE coeff[nrows]			# coefficients

int	i, n, j, jmax, nbndm1

begin
	if (nrows == 1) {
	    coeff[1] = vector[1] * matfac[1,1]
	    return
	}

	# copy vector to coefficients
	do i = 1, nrows
	    coeff[i] = vector[i]

	# forward substitution
	nbndm1 = nbands - 1
	do n = 1, nrows {
	    jmax = min (nbndm1, nrows - n)
	    if (jmax >= 1) {
	        do j = 1, jmax
		    coeff[j+n] = coeff[j+n] - matfac[j+1,n] * coeff[n]
	    }
	}

	# back substitution
	for (n = nrows; n >= 1; n = n - 1) {
	    coeff[n] = coeff[n] * matfac[1,n]
	    jmax = min (nbndm1, nrows - n)
	    if (jmax >= 1) {
		do j = 1, jmax
		    coeff[n] = coeff[n] - matfac[j+1,n] * coeff[j+n]
	    }
	}
end
