# GAUSSR -- Procedure to compute a 2-D radially symmetric Gaussian profile.
# The Gaussian is assumed to be sitting on top of a constant background.

# Parameter Allocation:
# 1	Amplitude
# 2	X-center
# 3	Y-center
# 4	Sigma
# 5	Sky

procedure gaussr (x, y, p, np, z)

real	x, y		# input coordinates
real	p[np]		# parameter vector
int	np		# number of parameters
real	z		# function return

real	dx, dy, r2

begin
	dx = x - p[2]
	dy = y - p[3]
	#r2 = (dx * dx + dy * dy) / (2.0 * p[4] ** 2)
	r2 = (dx * dx + dy * dy) / (2.0 * p[4])
	if (abs (r2) > 25.0)
	    z = p[5]
	else
	    z = p[1] * exp (- r2) + p[5]
end


# DGAUSSR -- A procedure to compute a 2-D Gaussian profile and its derivatives.
# The Gaussian profile is assumed to sit on top of constant background value.

procedure dgaussr (x, y, p, dp, np, z, der)

real	x, y		# input coordinates
real	p[np]		# parameter vector
real	dp[np]		# dummy array of parameter increments
int	np		# number of parameters
real	z		# function return
real	der[np]		# derivatives

real	dx, dy, r2

begin
	dx = x - p[2]
	dy = y - p[3]
	#r2 = (dx * dx + dy * dy) / (2.0 * p[4] ** 2)
	r2 = (dx * dx + dy * dy) / (2.0 * p[4])

	if (abs (r2) > 25.0) {
	    z = p[5]
	    der[1] = 0.0
	    der[2] = 0.0
	    der[3] = 0.0
	    der[4] = 0.0
	    der[5] = 1.0
	} else {
	    der[1] = exp (-r2)
	    z = p[1] * der[1]
	    #der[2] = z * dx / p[4] ** 2 
	    der[2] = z * dx / p[4] 
	    #der[3] = z * dy / p[4] ** 2
	    der[3] = z * dy / p[4]
	    #der[4] = z * r2 * 2.0 / p[4]
	    der[4] = z * r2 / p[4]
	    z = z + p[5]
	    der[5] = 1.0
	}
end
