include	<math.h>
include	<mach.h>

# GAUSS1D -- Procedure to compute the profile of a 1d Gaussian with a
# background value of zero.

procedure gauss1d (x, y, p, np, z)

real	x		# position coordinate
real	y		# not used
real	p[ARB]		# p[1]=amplitude p[2]=center p[3]=sigma
int	np		# number of parameters == 3
real	z		# function return

real	r

begin
	r = (x - p[2]) / (p[3] * SQRTOF2)
	if (abs (r) > 5.0)
	    z = 0.0
	else
	    z = p[1] * exp (- r ** 2)
end


# DGAUSS1D -- Procedure to compute the function value and derivatives of
# a 1-D Gaussian function sitting on a zero-valued background.

procedure dgauss1d (x, y, p, dp, np, z, der)

real	x		# position coordinate
real	y		# not used
real	p[ARB]		# p[1]=amplitude, p[2]=center, p[3]=sigma
real	dp[ARB]		# parameter derivatives
int	np		# number of parameters
real	z		# function value
real	der[ARB]	# derivatives

real	r

begin
	r = (x - p[2]) / (SQRTOF2 * p[3])
	if (abs (r) > 5.0) {
	    z = 0.0
	    der[1] = 0.0
	    der[2] = 0.0
	    der[3] = 0.0
	} else {
	    der[1] = exp (- r ** 2)
	    z = der[1] * p[1]
	    der[2] = z * r  * SQRTOF2 / p[3]
	    der[3] = der[2] * SQRTOF2 * r
	}
end
