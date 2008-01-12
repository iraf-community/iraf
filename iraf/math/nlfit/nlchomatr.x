include <mach.h>
include <math/nlfit.h>
include "nlfitdefr.h"


# NL_CHFAC -- Routine to calculate the Cholesky factorization of a
# symmetric, positive semi-definite banded matrix. This routines was
# adapted from the bchfac.f routine described in "A Practical Guide
# to Splines", Carl de Boor (1978).

procedure nl_chfacr (matrix, nbands, nrows, matfac, ier)

real   matrix[nbands, nrows]	# data matrix
int	nbands			# number of bands
int	nrows			# number of rows
real   matfac[nbands, nrows]	# Cholesky factorization
int	ier			# error code

int	i, n, j, imax, jmax
real   ratio

begin
	# Test for a single element matrix.
	if (nrows == 1) {
	    if (matrix[1,1] > real (0.0))
	        matfac[1,1] = 1. / matrix[1,1]
	    return
	}
		
	# Copy the original matrix into matfac.
	do n = 1, nrows {
	    do j = 1, nbands
		matfac[j,n] = matrix[j,n]
	}

	# Compute the factorization of the matrix.
	do n = 1, nrows {

	    # Test to see if matrix is singular.
	    if (((matfac[1,n] + matrix[1,n]) - matrix[1,n]) <= EPSILONR) {
	    #if (((matfac[1,n] + matrix[1,n]) - matrix[1,n]) <= PIXEL(0.0)) {
		do j = 1, nbands
		    matfac[j,n] = real (0.0)
		ier = SINGULAR
		next
	    }

	    matfac[1,n] = real (1.0) / matfac[1,n]
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


# NL_CHSLV -- Solve the matrix whose Cholesky factorization was calculated in
# NL_CHFAC for the coefficients. This routine was adapted from bchslv.f
# described in "A Practical Guide to Splines", by Carl de Boor (1978).

procedure nl_chslvr (matfac, nbands, nrows, vector, coeff)

real	matfac[nbands,nrows] 		# Cholesky factorization
int	nbands				# number of bands
int	nrows				# number of rows
real	vector[nrows]			# right side of matrix equation
real	coeff[nrows]			# coefficients

int	i, n, j, jmax, nbndm1

begin
	# Test for a single element matrix.
	if (nrows == 1) {
	    coeff[1] = vector[1] * matfac[1,1]
	    return
	}

	# Copy input vector to coefficients vector.
	do i = 1, nrows
	    coeff[i] = vector[i]

	# Perform forward substitution.
	nbndm1 = nbands - 1
	do n = 1, nrows {
	    jmax = min (nbndm1, nrows - n)
	    if (jmax >= 1) {
	        do j = 1, jmax
		    coeff[j+n] = coeff[j+n] - matfac[j+1,n] * coeff[n]
	    }
	}

	# Perform backward substitution.
	for (n = nrows; n >= 1; n = n - 1) {
	    coeff[n] = coeff[n] * matfac[1,n]
	    jmax = min (nbndm1, nrows - n)
	    if (jmax >= 1) {
		do j = 1, jmax
		    coeff[n] = coeff[n] - matfac[j+1,n] * coeff[j+n]
	    }
	}
end


# NL_DAMP -- Procedure to add damping to matrix

procedure nl_dampr (inmatrix, outmatrix, constant, nbands, nrows)

real	inmatrix[nbands,ARB]		# input matrix
real	outmatrix[nbands,ARB]		# output matrix
real	constant			# damping constant
int	nbands, nrows			# dimensions of matrix

int	i

begin
	do i = 1, nrows
	    outmatrix[1,i] = inmatrix[1,i] * constant
end
