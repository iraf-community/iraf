# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# II_SPLINE2D -- This procedure calculates the univariate B-spline coefficients
# for each row of data. The data are assumed to be uniformly spaced with a
# spacing of 1. The first element of each row of data is assumed to contain
# the second derivative of the data at x = 1. The nxpix + 2-th element of each
# row is assumed to contain the second derivative of the function at x = nxpix.
# Therfore if each row of data contains nxpix points, nxpix+2 B-spline
# coefficients will be calculated. The univariate B-spline coefficients
# for the i-th row of data are output to the i-th column of coeff.
# Therefore two calls to II_SPLINE2D are required to calculate the 2D B-spline
# coefficients.

procedure ii_spline2d (data, coeff, nxpix, nvectors, len_data, len_coeff)

real	data[len_data,ARB]	# input data array
real	coeff[len_coeff,ARB]	# output array of univariate coefficients in x
int	nxpix			# number of x data points
int	nvectors		# number of univariate splines to calculate
int	len_data		# row dimension of data
int	len_coeff		# row dimension of coeff

int	i, j
pointer	diag

errchk	malloc, mfree

begin
	# allocate space for off-diagonal elements
	call malloc (diag, nxpix+1, TY_REAL)

	# calculate off-diagonal elements by Gaussian elimination
	Memr[diag] = -2.
	Memr[diag+1] = 0.
	do i = 3, nxpix + 1
	    Memr[diag+i-1] = 1. / (4. - Memr[diag+i-2])

	# loop over the nvectors rows of input data
	do j = 1, nvectors {

	    # copy the j-th row of data to the j-th column of coeff
	    do i = 1, nxpix + 2
		coeff[j,i] = data[i,j]

	    # forward substitution
	    coeff[j,1] = coeff[j,1] / 6.
	    coeff[j,2] = (coeff[j,2] - coeff[j,1]) / 6.
	    do i = 3, nxpix + 1
		coeff[j,i] = Memr[diag+i-1] * (coeff[j,i] - coeff[j,i-1])

	    # back subsitution
	    coeff[j,nxpix+2] = ((Memr[diag+nxpix-1] + 2.) * coeff[j,nxpix+1] -
		coeff[j,nxpix] + coeff[j,nxpix+2] / 6.) /
		(1. + Memr[diag+nxpix] * (Memr[diag+nxpix-1] + 2.))
	    do i = nxpix + 1, 3, - 1
		coeff[j,i] = coeff[j,i] - Memr[diag+i-1] * coeff[j,i+1]
	    coeff[j,1] = coeff[j,1] + 2. * coeff[j,2] - coeff[j,3]

	}

	# free space used for off-diagonal element storage
	call mfree (diag, TY_REAL)
end
