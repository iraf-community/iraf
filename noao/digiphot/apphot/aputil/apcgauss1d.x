include	<math.h>
include	<mach.h>

# CGAUSS1D - Procedure to compute the value of a 1-D Gaussian function
# sitting on top of a constant background.

procedure cgauss1d (x, y, p, np, z)

real	x		# position coordinate
real	y		# not used
real	p[ARB]		# p[1]=amplitude p[2]=center p[3]=sigma p[4]=sky
int	np		# number of parameters np = 4
real	z		# function return

real	r2

begin
	r2 = (x - p[2]) ** 2 / (2. * p[3])
	if (abs (r2) > 25.0)
	    z = p[4]
	else
	    z = p[1] * exp (-r2) + p[4]
end


# CDGAUSS1D -- Procedure to compute a 1-D Gaussian profile and its derivatives.
# The Gaussian is assumed to sitting on top of a constant background.

procedure cdgauss1d (x, y, p, dp, np, z, der)

real	x		# position coordinate
real	y		# not used
real	p[ARB]		# p[1]=amplitude, p[2]=center, p[3]=sky p[4]=sigma
real	dp[ARB]		# parameter derivatives
int	np		# number of parameters np=4
real	z		# function value
real	der[ARB]	# derivatives

real	dx, r2

begin
	dx = x - p[2]
	r2 = dx * dx / (2.0 * p[3])
	if (abs (r2) > 25.0) {
	    z = p[4]
	    der[1] = 0.0
	    der[2] = 0.0
	    der[3] = 0.0
	    der[4] = 1.0
	} else {
	    der[1] = exp (-r2)
	    z = p[1] * der[1]
	    #der[2] = z * r  * SQRTOF2 / p[3]
	    der[2] = z * dx / p[3]
	    #der[3] = der[2] * SQRTOF2 * r
	    der[3] = z * r2 / p[3]
	    der[4] = 1.0
	    z = z + p[4]
	}
end
