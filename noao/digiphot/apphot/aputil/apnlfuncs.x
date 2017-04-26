
include	<math.h>
include	<mach.h>

# CGAUSS1D - Compute the value of a 1-D Gaussian function  on a constant
# background.

procedure cgauss1d (x, nvars, p, np, z)

real	x[ARB]		# variables, x[1] = position coordinate
int	nvars		# the number of variables, not used
real	p[ARB]		# p[1]=amplitude p[2]=center p[3]=variance p[4]=sky
int	np		# number of parameters np = 4
real	z		# function return

real	r2

begin
	if (p[3] == 0.)
	    r2 = 36.0
	else
	    r2 = (x[1] - p[2]) ** 2 / (2. * p[3])
	if (abs (r2) > 25.0)
	    z = p[4]
	else
	    z = p[1] * exp (-r2) + p[4]
end


# CDGAUSS1D -- Compute the value a 1-D Gaussian profile on a constant
# background and its derivatives.

procedure cdgauss1d (x, nvars p, dp, np, z, der)

real	x[ARB]		# variables, x[1] = position coordinate
int	nvars		# the number of variables, not used
real	p[ARB]		# p[1]=amplitude, p[2]=center, p[3]=sky p[4]=variance
real	dp[ARB]		# parameter derivatives
int	np		# number of parameters np=4
real	z		# function value
real	der[ARB]	# derivatives

real	dx, r2

begin
	dx = x[1] - p[2]
	if (p[3] == 0.)
	    r2 = 36.0
	else
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
	    der[2] = z * dx / p[3]
	    der[3] = z * r2 / p[3]
	    der[4] = 1.0
	    z = z + p[4]
	}
end


# GAUSSR -- Compute the value of a 2-D radially symmetric Gaussian profile
# which is assumed to be sitting on a constant background.

# Parameter Allocation:
# 1	Amplitude
# 2	X-center
# 3	Y-center
# 4	Variance
# 5	Sky

procedure gaussr (x, nvars, p, np, z)

real	x[ARB]		# the input variables
int	nvars		# the number of variables
real	p[np]		# parameter vector
int	np		# number of parameters
real	z		# function return

real	dx, dy, r2

begin
	dx = x[1] - p[2]
	dy = x[2] - p[3]
	if (p[4] == 0.)
	    r2 = 36.0
	else
	    r2 = (dx * dx + dy * dy) / (2.0 * p[4])
	if (abs (r2) > 25.0)
	    z = p[5]
	else
	    z = p[1] * exp (- r2) + p[5]
end


# DGAUSSR -- Compute the value of a 2-D Gaussian profile and its derivatives
# which assumed to be sitting on top of a constant background.

procedure dgaussr (x, nvars, p, dp, np, z, der)

real	x[ARB]		# the input variables
int	nvars		# the number of variables
real	p[np]		# parameter vector
real	dp[np]		# dummy array of parameter increments
int	np		# number of parameters
real	z		# function return
real	der[np]		# derivatives

real	dx, dy, r2

begin
	dx = x[1] - p[2]
	dy = x[2] - p[3]
	if (p[4] == 0.)
	    r2 = 36.0
	else
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
	    der[2] = z * dx / p[4] 
	    der[3] = z * dy / p[4]
	    der[4] = z * r2 / p[4]
	    z = z + p[5]
	    der[5] = 1.0
	}
end


# ELGAUSS -- Compute the value of a 2-D elliptical Gaussian function which
# is assumed to be sitting on top of a constant background.

# Parameter Allocation:
# 1	Amplitude
# 2	X-center
# 3	Y-center
# 4	Variance-x
# 5	Variance-y
# 6	Theta-rotation
# 7	Sky

procedure elgauss (x, nvars, p, np, z)

real	x[ARB]		# input variables, x[1] = x, x[2] = y
int	nvars		# number of variables, not used
real	p[np]		# parameter vector
int	np		# number of parameters
real	z		# function return

real	dx, dy, crot, srot, xt, yt, r2

begin
	dx = x[1] - p[2]
	dy = x[2] - p[3]
	crot = cos (p[6])
	srot = sin (p[6])
	xt = (dx * crot + dy * srot)
	yt = (-dx * srot + dy * crot)
	if (p[4] == 0. || p[5] == 0.)
	    r2 = 36.0
	else
	    r2 = (xt ** 2 / p[4] + yt ** 2 / p[5]) / 2.0
	if (abs (r2) > 25.0)
	    z = p[7]
	else
	    z = p[1] * exp (-r2) + p[7]
end


# DELGAUSS -- Compute the value of a 2-D elliptical Gaussian assumed to
# sitting on top of a constant background and its derivatives.

procedure delgauss (x, nvars, p, dp, np, z, der)

real	x[ARB]		# input variables, x[1] = x, x[2] = y
int	nvars		# number of variables, not used
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
	sigx2 = p[4]
	sigy2 = p[5]
	if (sigx2 == 0. || sigy2 == 0.)
	    r2 = 36.0
	else {
	    a = (crot2 / sigx2 + srot2 / sigy2)
	    b = 2.0 * crot * srot * (1.0 / sigx2 - 1.0 /sigy2)
	    c = (srot2 / sigx2 + crot2 / sigy2)

	    dx = x[1] - p[2]
	    dy = x[2] - p[3]
	    dx2 = dx ** 2
	    dy2 = dy ** 2
	    r2 = 0.5 * (a * dx2 + b * dx * dy + c * dy2)
	}

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
	    der[4] = z * (crot2 * dx2 + 2.0 * crot * srot * dx * dy +
	        srot2 * dy2) / (2.0 * sigx2 * sigx2)
	    der[5] = z * (srot2 * dx2 - 2.0 * crot * srot * dx * dy +
	        crot2 * dy2) / (2.0 * sigy2 * sigy2)
	    der[6] = z * (b * dx2 + 2.0 * (c - a) * dx * dy - b * dy2)  
	    z = z + p[7]
	    der[7] = 1.0
	}
end



# GAUSS1D -- Compute the profile of a 1d Gaussian with a background value
# of zero.

procedure gauss1d (x, nvars, p, np, z)

real	x[ARB]		# list of variables, x[1] = position coordinate
int	nvars		# number of variables
real	p[ARB]		# p[1]=amplitude p[2]=center p[3]=sigma
int	np		# number of parameters == 3
real	z		# function return

real	r

begin
	if (p[3] == 0.)
	    r = 6.0
	else
	    r = (x[1] - p[2]) / (p[3] * SQRTOF2)
	if (abs (r) > 5.0)
	    z = 0.0
	else
	    z = p[1] * exp (- r ** 2)
end


# DGAUSS1D -- Compute the function value and derivatives of a 1-D Gaussian
# function with a background value of zero.

procedure dgauss1d (x, nvars, p, dp, np, z, der)

real	x[ARB]		# list of variables, x[1] = position coordinate
int	nvars		# number of variables
real	p[ARB]		# p[1]=amplitude, p[2]=center, p[3]=sigma
real	dp[ARB]		# parameter derivatives
int	np		# number of parameters
real	z		# function value
real	der[ARB]	# derivatives

real	r

begin
	if (p[3] == 0.)
	    r = 6.0
	else
	    r = (x[1] - p[2]) / (SQRTOF2 * p[3])
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


# GAUSSKEW - Compute the value of a 1-D skewed Gaussian profile.
# The background value is assumed to be zero.

procedure gausskew (x, nvars, p, np, z)

real	x[ARB]		# list of variables, x[1] = position coordinate
int	nvars		# number of variables, not used
real	p[ARB]		# p[1]=amplitude p[2]=center p[3]=variance p[4]=skew
int	np		# number of parameters == 3
real	z		# function return

real	dx, r2, r3

begin
	dx = (x[1] - p[2])
	if (p[3] == 0.)
	    r2 = 36.0
	else {
	    r2 = dx ** 2 / (2.0 * p[3])
	    r3 = r2 * dx / sqrt (2.0 * abs (p[3])) 
	}
	if (abs (r2) > 25.0)
	    z = 0.0
	else
	    z = (1.0 + p[4] * r3) * p[1] * exp (-r2)
end


# DGAUSSKEW -- Compute the value of a 1-D skewed Gaussian and its derivatives. 
# The background value is assumed to be zero.

procedure dgausskew (x, nvars, p, dp, np, z, der)

real	x[ARB]		# list of variables, x[1] = position coordinate
int	nvars		# number of variables, not used
real	p[ARB]		# p[1]=amplitude, p[2]=center, p[3]=variance, p[4]=skew
real	dp[ARB]		# parameter derivatives
int	np		# number of parameters
real	z		# function value
real	der[ARB]	# derivatives

real	dx, d1, d2, d3, r, r2, r3, rint

begin
	dx = x[1] - p[2]
	if (p[3] == 0.)
	    r2 = 36.0
	else
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
