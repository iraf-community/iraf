include	<math.h>
include	<mach.h>

# GAUSSKEW - Procedure to sample a 1-D skewed Gaussian profile.

procedure gausskew (x, y, p, np, z)

real	x		# position coordinate
real	y		# not used
real	p[ARB]		# p[1]=amplitude p[2]=center p[3]=sigma p[4]=skew
int	np		# number of parameters == 3
real	z		# function return

real	dx, r2, r3

begin
	dx = (x - p[2])
	r2 = dx ** 2 / (2.0 * p[3])
	r3 = r2 * dx / sqrt (2.0 * abs (p[3])) 
	if (abs (r2) > 25.0)
	    z = 0.0
	else
	    z = (1.0 + p[4] * r3) * p[1] * exp (-r2)
end


# DGAUSSKEW -- Procedure to sample a 1-D skewed Gaussian and its derivatives. 

procedure dgausskew (x, y, p, dp, np, z, der)

real	x		# position coordinate
real	y		# not used
real	p[ARB]		# p[1]=amplitude, p[2]=center, p[3]=sigma, p[4]=skew
real	dp[ARB]		# parameter derivatives
int	np		# number of parameters
real	z		# function value
real	der[ARB]	# derivatives

real	dx, d1, d2, d3, r, r2, r3, rint

begin
	dx = x - p[2]
	r2 = dx ** 2 / (2.0 * p[3])
	if (abs (r2) > 25.0) {
	    z = 0.0
	    der[1] = 0.0
	    der[2] = 0.0
	    der[3] = 0.0
	    der[4] = 0.0
	} else {
	    r = dx / sqrt (2.0 * abs (p[3]))
	    r3 = r2 * r
	    d1 = exp (-r2)
	    z = d1 * p[1]
	    d2 = z * dx / p[3]
	    d3 = z * r2 / p[3]
	    rint = 1.0 + p[4] * r3
	    der[1] = d1 * rint
	    der[2] = d2 * (rint - 1.5 * p[4] * r)
	    der[3] = d3 * (rint - 1.5 * p[4] * r)
	    der[4] = z * r3
	    z = z * rint
	}
end
