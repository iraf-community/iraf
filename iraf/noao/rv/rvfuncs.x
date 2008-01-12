include	<math.h>
include	<mach.h>

# FUNCS - File to contain all of the functional models and derivatives.
# Models currently supported are:
#	- Gaussian on a constant background
#	- N-th order polynomial
#       - Lorentzian on a constant background

# CGAUSS1D - Procedure to compute the value of a 1-D Gaussian function
# sitting on top of a constant background.

procedure cgauss1d (x, nvars, p, np, z)

real	x		# position coordinate
int	nvars		# number of variables
real	p[ARB]		# p[1]=amplitude p[2]=center p[3]=sigma p[4]=background
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

procedure cdgauss1d (x, nvars, p, dp, np, z, der)

real	x		# position coordinate
int	nvars		# number of variables
real	p[ARB]		# p[1]=amplitude, p[2]=center, p[3]=sky, p[4]=sigma
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
	    der[2] = z * dx / p[3]
	    der[3] = z * r2 / p[3]
	    der[4] = 1.0
	    z = z + p[4]
	}
end


# D_CGAUSS1D - Procedure to compute the value of a 1-D Gaussian function
# sitting on top of a constant background.

procedure d_cgauss1d (x, nvars, p, np, z)

double	x		# position coordinate
int	nvars		# number of variables
double	p[ARB]		# p[1]=amplitude p[2]=center p[3]=sigma p[4]=background
int	np		# number of parameters np = 4
double	z		# function return

double	r2

begin
	r2 = (x - p[2]) ** 2 / (2. * p[3])
	if (abs (r2) > 25.0)
	    z = p[4]
	else
	    z = p[1] * exp (-r2) + p[4]
end


# D_CDGAUSS1D -- Procedure to compute a 1-D Gaussian profile and its deriv-
# atives.  The Gaussian is assumed to sitting on top of a constant background.

procedure d_cdgauss1d (x, nvars, p, dp, np, z, der)

double	x		# position coordinate
int	nvars		# number of variables
double	p[ARB]		# p[1]=amplitude, p[2]=center, p[3]=sky, p[4]=sigma
double	dp[ARB]		# parameter derivatives
int	np		# number of parameters np=4
double	z		# function value
double	der[ARB]	# derivatives

double	dx, r2

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
	    der[2] = z * dx / p[3]
	    der[3] = z * r2 / p[3]
	    der[4] = 1.0
	    z = z + p[4]
	}
end


# POLYFIT -- Procedure to compute the fit of an N-order polynomial

procedure polyfit (x, nvars, p, np, z)

real	x		# position coordinate
int	nvars		# number of variables
real	p[ARB]		# coefficients of polynomial
int	np		# number of parameters 
real	z		# function return

int	i
real	r

begin
	r = 0.0
	do i = 2, np 
	    r = r + x**(i-1) * p[i]
	z = p[1] + r
end


# DPOLYFIT -- Procedure to compute the function value and derivatives of
# a N-order polynomial.

procedure dpolyfit (x, nvars, p, dp, np, z, der)

real	x		# position coordinate
int	nvars		# number of variables
real	p[ARB]		# p[1]=amplitude, p[2]=center, p[3]=sigma
real	dp[ARB]		# parameter derivatives
int	np		# number of parameters
real	z		# function value
real	der[ARB]	# derivatives

int	i

begin
	der[1] = 1.0
	z = 0.0
	do i = 2, np {
	    der[i] = x ** (i-1)
	    z = z + x**(i-1) * p[i]  
	}
	z = p[1] + z
end


# LORENTZ -- Procedure to compute a Lorentzian profile

procedure lorentz (x, nvars, p, np, z)

real	x		# position coordinate
int	nvars		# number of variables
real	p[ARB]		# p[1]=amplitude p[2]=center p[3]=fwhm p[4]=background
int	np		# number of parameters np = 4
real	z		# function return

real	r2

begin
	r2 = (x - p[2])**2 + (p[3] / 2.0)**2
	if (r2 != 0.0)
	    z = p[1] * ((p[3]/2.0) / r2) + p[4]
	else
	    z = p[4]
end


# DLORENTZ -- Procedure to compute the function value and derivatives of
# a Lorentzian profile

procedure dlorentz (x, nvars, p, dp, np, z, der)

real	x		# position coordinate
int	nvars		# number of variables
real	p[ARB]		# p[1]=amplitude, p[2]=center, p[3]=fwhm, [4]=background
real	dp[ARB]		# parameter derivatives
int	np		# number of parameters
real	z		# function value
real	der[ARB]	# derivatives

real	dl, dr, d2

begin
	dl = (x - p[2]) * (x - p[2])
	dr = (0.5 * p[3]) * (0.5 * p[3])
	d2 = dl + dr
	if (d2 == 0.0) {
	    der[1] = 0.0
	    der[2] = 0.0
	    der[3] = 0.0
	    der[4] = 1.0
	    z = p[4]
	} else {
	    der[1] = ((p[3]/2.0) / d2)
	    der[2] = (p[1]*p[3]/2.0) * (2.0 * (x - p[2])) / (d2 * d2)
	    der[3] = ((p[1] / (2.0 * d2)) - (((p[1]*p[3]*p[3])/2.0)/(d2*d2)))
	    der[4] = 1.0
	    z = p[1] * ((p[3]/2.0) / d2) + p[4]
	}
end


# LORENTZ -- Procedure to compute a Lorentzian profile

procedure lorentz_old (x, nvars, p, np, z)

real	x		# position coordinate
int	nvars		# number of variables
real	p[ARB]		# p[1]=amplitude p[2]=center p[3]=fwhm p[4]=background
int	np		# number of parameters np = 4
real	z		# function return

begin
	if (p[3] != 0.0)
	    z = p[1] / (1. + ((x-p[2])/p[3])**2) + p[4]
	else
	    z = p[4]
end


# DLORENTZ -- Procedure to compute the function value and derivatives of
# a Lorentzian profile

procedure dlorentz_old (x, nvars, p, dp, np, z, der)

real	x		# position coordinate
int	nvars		# number of variables
real	p[ARB]		# p[1]=amplitude, p[2]=center, p[3]=fwhm, [4]=background
real	dp[ARB]		# parameter derivatives
int	np		# number of parameters
real	z		# function value
real	der[ARB]	# derivatives

real	dx, D

begin
	#dx = (x - p[2]) / p[3]					# Frank's derivs
	dx = (x - p[2])
	D = 1. + (dx/p[3])**2
	if (p[3] == 0.0) {
	    der[1] = 0.0
	    der[2] = 0.0
	    der[3] = 0.0
	    der[4] = 1.0
	    z = p[4]
	} else {
	    der[1] = 1. / D
	    der[2] = p[1] / D**2 * (2. * dx / p[3]**2)
	    der[3] = p[1] / D**2 * (2. * dx * dx / p[3]**3)
	    #der[2] = p[1] / D**2 * (2. * dx / p[3])		# Frank's derivs
	    #der[3] = -p[1] / D**2 * (2. * dx * dx / p[3])
	    der[4] = 1.0
	    if (p[3] != 0.0)
	        z = p[1] / (1. + ((x-p[2])/p[3])**2) + p[4]
	    else
	        z = p[4]
	}
end
