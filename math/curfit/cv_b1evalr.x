# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CV_B1LEG -- Procedure to evaluate all the non-zero Legendrefunctions for
# a single point and given order.

procedure rcv_b1leg (x, order, k1, k2, basis)

real	x		# array of data points
int	order		# order of polynomial, order = 1, constant
real	k1, k2		# normalizing constants
real	basis[ARB]	# basis functions

int	i
real	ri, xnorm

begin
	basis[1] = real(1.0)
	if (order == 1)
	    return

	xnorm = (x + k1) * k2 
	basis[2] = xnorm
	if (order == 2)
	    return

	do i = 3, order {
	    ri = i
	    basis[i] = ((real(2.0) * ri - real(3.0)) * xnorm * basis[i-1] -
		(ri - real(2.0)) * basis[i-2]) / (ri - real(1.0))	
	}
end


# CV_B1CHEB -- Procedure to evaluate all the non zero Chebyshev function
# for a given x and order.

procedure rcv_b1cheb (x, order, k1, k2, basis)

real	x		# number of data points
int	order		# order of polynomial, 1 is a constant
real	k1, k2		# normalizing constants
real	basis[ARB]	# array of basis functions

int	i
real	xnorm

begin
	basis[1] = real(1.0)
	if (order == 1)
	    return

	xnorm = (x + k1) * k2
	basis[2] = xnorm
	if (order == 2)
	    return

	do i = 3, order
	    basis[i] = real(2.0) * xnorm * basis[i-1] - basis[i-2]
end


# CV_B1SPLINE1 -- Evaluate all the non-zero spline1 functions for a
# single point.

procedure rcv_b1spline1 (x, npieces, k1, k2, basis, left)

real	x		# set of data points
int	npieces		# number of polynomial pieces minus 1
real	k1, k2		# normalizing constants
real	basis[ARB]	# basis functions
int	left		# index of the appropriate spline functions

real	xnorm

begin
	xnorm = (x + k1) * k2
	left = min (int (xnorm), npieces)

	basis[2] = max (real(0.0), min (real(1.0), xnorm - left))
	basis[1] = max (real(0.0), min (real(1.0), real(1.0) - basis[2]))
end


# CV_B1SPLINE3 --  Procedure to evaluate all the non-zero basis functions
# for a cubic spline.

procedure rcv_b1spline3 (x, npieces, k1, k2, basis, left)

real	x		# array of data points
int	npieces		# number of polynomial pieces
real	k1, k2		# normalizing constants
real	basis[ARB]	# array of basis functions
int	left		# array of indices for first non-zero spline

real	sx, tx

begin
	sx = (x + k1) * k2
	left = min (int (sx), npieces)

	sx = max (real(0.0), min (real(1.0), sx - left))
	tx = max (real(0.0), min (real(1.0), real(1.0) - sx))

	basis[1] = tx * tx * tx
	basis[2] = real(1.0) + tx * (real(3.0) + tx * (real(3.0) -
	    real(3.0) * tx))
	basis[3] = real(1.0) + sx * (real(3.0) + sx * (real(3.0) -
	    real(3.0) * sx))
	basis[4] = sx * sx * sx
end
