# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CV_B1LEG -- Procedure to evaluate all the non-zero Legendrefunctions for
# a single point and given order.

procedure dcv_b1leg (x, order, k1, k2, basis)

double	x		# array of data points
int	order		# order of polynomial, order = 1, constant
double	k1, k2		# normalizing constants
double	basis[ARB]	# basis functions

int	i
double	ri, xnorm

begin
	basis[1] = double(1.0)
	if (order == 1)
	    return

	xnorm = (x + k1) * k2 
	basis[2] = xnorm
	if (order == 2)
	    return

	do i = 3, order {
	    ri = i
	    basis[i] = ((double(2.0) * ri - double(3.0)) * xnorm * basis[i-1] -
		(ri - double(2.0)) * basis[i-2]) / (ri - double(1.0))	
	}
end


# CV_B1CHEB -- Procedure to evaluate all the non zero Chebyshev function
# for a given x and order.

procedure dcv_b1cheb (x, order, k1, k2, basis)

double	x		# number of data points
int	order		# order of polynomial, 1 is a constant
double	k1, k2		# normalizing constants
double	basis[ARB]	# array of basis functions

int	i
double	xnorm

begin
	basis[1] = double(1.0)
	if (order == 1)
	    return

	xnorm = (x + k1) * k2
	basis[2] = xnorm
	if (order == 2)
	    return

	do i = 3, order
	    basis[i] = double(2.0) * xnorm * basis[i-1] - basis[i-2]
end


# CV_B1SPLINE1 -- Evaluate all the non-zero spline1 functions for a
# single point.

procedure dcv_b1spline1 (x, npieces, k1, k2, basis, left)

double	x		# set of data points
int	npieces		# number of polynomial pieces minus 1
double	k1, k2		# normalizing constants
double	basis[ARB]	# basis functions
int	left		# index of the appropriate spline functions

double	xnorm

begin
	xnorm = (x + k1) * k2
	left = min (int (xnorm), npieces)

	basis[2] = max (double(0.0), min (double(1.0), xnorm - left))
	basis[1] = max (double(0.0), min (double(1.0), double(1.0) - basis[2]))
end


# CV_B1SPLINE3 --  Procedure to evaluate all the non-zero basis functions
# for a cubic spline.

procedure dcv_b1spline3 (x, npieces, k1, k2, basis, left)

double	x		# array of data points
int	npieces		# number of polynomial pieces
double	k1, k2		# normalizing constants
double	basis[ARB]	# array of basis functions
int	left		# array of indices for first non-zero spline

double	sx, tx

begin
	sx = (x + k1) * k2
	left = min (int (sx), npieces)

	sx = max (double(0.0), min (double(1.0), sx - left))
	tx = max (double(0.0), min (double(1.0), double(1.0) - sx))

	basis[1] = tx * tx * tx
	basis[2] = double(1.0) + tx * (double(3.0) + tx * (double(3.0) -
	    double(3.0) * tx))
	basis[3] = double(1.0) + sx * (double(3.0) + sx * (double(3.0) -
	    double(3.0) * sx))
	basis[4] = sx * sx * sx
end
