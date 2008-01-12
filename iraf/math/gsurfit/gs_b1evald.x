# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GS_B1POL -- Procedure to evaluate all the non-zero polynomial functions
# for a single point and given order.

procedure dgs_b1pol (x, order, k1, k2, basis)

double	x		# data point
int	order		# order of polynomial, order = 1, constant
double	k1, k2		# nomalizing constants, dummy in this case
double	basis[ARB]	# basis functions

int	i

begin
	basis[1] = 1.
	if (order == 1)
	    return

	basis[2] = x
	if (order == 2)
	    return

	do i = 3, order
	    basis[i] = x * basis[i-1]

end

# GS_B1LEG -- Procedure to evaluate all the non-zero Legendre functions for
# a single point and given order.

procedure dgs_b1leg (x, order, k1, k2, basis)

double	x		# data point
int	order		# order of polynomial, order = 1, constant
double	k1, k2		# normalizing constants
double	basis[ARB]	# basis functions

int	i
double	ri, xnorm

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


# GS_B1CHEB -- Procedure to evaluate all the non zero Chebyshev function
# for a given x and order.

procedure dgs_b1cheb (x, order, k1, k2, basis)

double	x		# number of data points
int	order		# order of polynomial, 1 is a constant
double	k1, k2		# normalizing constants
double	basis[ARB]	# array of basis functions

int	i
double	xnorm

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
