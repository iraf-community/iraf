# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# SF_B1LEG -- Procedure to evaluate all the non-zero Legendrefunctions for
# a single point and given order.

procedure sf_b1leg (x, order, k1, k2, basis)

real	x		# array of data points
int	order		# order of polynomial, order = 1, constant
real	k1, k2		# normalizing constants
real	basis[ARB]	# basis functions

int	i
real	ri, xnorm

begin
	basis[1] = 1.
	if (order == 1)
	    return

	xnorm = (x + k1) * k2 
	basis[2] = xnorm
	if (order == 2)
	    return

	do i = 3, order {
	    ri = i
	    basis[i] = ((2. * ri - 3.) * xnorm * basis[i-1] -
		       (ri - 2.) * basis[i-2]) / (ri - 1.)	
	}
end


# SF_B1CHEB -- Procedure to evaluate all the non zero Chebyshev function
# for a given x and order.

procedure sf_b1cheb (x, order, k1, k2, basis)

real	x		# number of data points
int	order		# order of polynomial, 1 is a constant
real	k1, k2		# normalizing constants
real	basis[ARB]	# array of basis functions

int	i
real	xnorm

begin
	basis[1] = 1.
	if (order == 1)
	    return

	xnorm = (x + k1) * k2
	basis[2] = xnorm
	if (order == 2)
	    return

	do i = 3, order
	    basis[i] = 2. * xnorm * basis[i-1] - basis[i-2]
end


# SF_B1SPLINE1 -- Evaluate all the non-zero spline1 functions for a
# single point.

procedure sf_b1spline1 (x, npieces, k1, k2, basis, left)

real	x		# set of data points
int	npieces		# number of polynomial pieces minus 1
real	k1, k2		# normalizing constants
real	basis[ARB]	# basis functions
int	left		# index of the appropriate spline functions

real	xnorm

begin
	xnorm = (x + k1) * k2
	left = min (int (xnorm), npieces)

	basis[2] = xnorm - left
	basis[1] = 1. - basis[2]
end


# SF_B1SPLINE3 --  Procedure to evaluate all the non-zero basis functions
# for a cubic spline.

procedure sf_b1spline3 (x, npieces, k1, k2, basis, left)

real	x		# array of data points
int	npieces		# number of polynomial pieces
real	k1, k2		# normalizing constants
real	basis[ARB]	# array of basis functions
int	left		# array of indices for first non-zero spline

real	sx, tx

begin
	sx = (x + k1) * k2
	left = min (int (sx), npieces)

	sx = sx - left
	tx = 1. - sx

	basis[1] = tx * tx * tx
	basis[2] = 1. + tx * (3. + tx * (3. - 3. * tx))
	basis[3] = 1. + sx * (3. + sx * (3. - 3. * sx))
	basis[4] = sx * sx * sx
end
