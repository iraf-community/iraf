# ELGAUSS -- A procedure to compute the value of a 2-D elliptical Gaussian
# function. The Gaussian is assumed to be sitting on top of a constant
# background value.

# Parameter Allocation:
# 1	Amplitude
# 2	X-center
# 3	Y-center
# 4	Sigma-x
# 5	Sigma-y
# 6	Theta-rotation
# 7	Sky

procedure elgauss (x, y, p, np, z)

real	x, y		# input coordinates
real	p[np]		# parameter vector
int	np		# number of parameters
real	z		# function return

real	dx, dy, crot, srot, xt, yt, r2

begin
	dx = x - p[2]
	dy = y - p[3]
	crot = cos (p[6])
	srot = sin (p[6])
	#xt = (dx * crot + dy * srot) / p[4]
	xt = (dx * crot + dy * srot)
	#yt = (-dx * srot + dy * crot) / p[5]
	yt = (-dx * srot + dy * crot)
	r2 = (xt ** 2 / p[4] + yt ** 2 / p[5]) / 2.0
	if (abs (r2) > 25.0)
	    z = p[7]
	else
	    z = p[1] * exp (-r2) + p[7]
end


# DELGAUSS -- Procedure to evaluate a 2-D elliptical Gaussian and its
# derivatives.  The Gaussian is assumed to sit on top of a constant sky
# background.

procedure delgauss (x, y, p, dp, np, z, der)

real	x, y		# input coordinates
real	p[np]		# parameter vector
real	dp[np]		# delta of parameters
int	np		# number of parameters
real	z		# function value
real	der[np]		# function return

real	crot, srot, crot2, srot2, sigx2, sigy2, a, b, c
real	dx, dy, dx2, dy2, r2

begin
	crot = cos (p[6])
	srot = sin (p[6])
	crot2 = crot ** 2
	srot2 = srot ** 2
	#sigx2 = p[4] ** 2
	sigx2 = p[4]
	#sigy2 = p[5] ** 2
	sigy2 = p[5]
	a = (crot2 / sigx2 + srot2 / sigy2)
	b = 2.0 * crot * srot * (1.0 / sigx2 - 1.0 /sigy2)
	c = (srot2 / sigx2 + crot2 / sigy2)

	dx = x - p[2]
	dy = y - p[3]
	dx2 = dx ** 2
	dy2 = dy ** 2
	r2 = 0.5 * (a * dx2 + b * dx * dy + c * dy2)

	if (abs (r2) > 25.0) {
	    z = p[7]
	    der[1] = 0.0
	    der[2] = 0.0
	    der[3] = 0.0
	    der[4] = 0.0
	    der[5] = 0.0
	    der[6] = 0.0
	    der[7] = 1.0
	} else {
	    der[1] = exp (-r2)
	    z = p[1] * der[1]
	    der[2] = z * (2.0 * a * dx + b * dy)
	    der[3] = z * (b * dx + 2.0 * c * dy)
	    #der[4] = z * (crot2 * dx2 + 2.0 * crot * srot * dx * dy +
	        #srot2 * dy2) / (p[4] * sigx2)
	    der[4] = z * (crot2 * dx2 + 2.0 * crot * srot * dx * dy +
	        srot2 * dy2) / (2.0 * sigx2 * sigx2)
	    #der[5] = z * (srot2 * dx2 - 2.0 * crot * srot * dx * dy +
	        #crot2 * dy2) / (p[5] * sigy2)
	    der[5] = z * (srot2 * dx2 - 2.0 * crot * srot * dx * dy +
	        crot2 * dy2) / (2.0 * sigy2 * sigy2)
	    der[6] = z * (b * dx2 + 2.0 * (c - a) * dx * dy - b * dy2)  
	    z = z + p[7]
	    der[7] = 1.0
	}
end
