# ERF -- Numerically integrate a Gaussian function from xin-0.5 to xin+0.5
# using Simpson's 1/3 rule. Also provide a first derivative of the integral
# with respect to x0 and beta. The Gaussian function is shown below.
#
#      erf = exp (-0.5 *((x - x0) / beta) ** 2))
#
# The number of intervals required to end up with an error less than alpha
# is greater than
#
#      fourth root of ((4th derivative of erf w.r.t. x) / (180. * alpha))
#
# Here alpha = 0.00005. N, the number of intervals, must be an even number
# since the number of nodes, which equals the number of intervals plus one,
# must be odd.

real procedure erf (xin, x0, beta, dfdx0, dfdbet)

real	xin		# the input value
real	x0		# position of Gaussian peak
real	beta		# width of the Gaussian
real	dfdx0		# derivative of Gaussian wrt x0
real	dfdbet		# derivative of Gaussian wrt beta

int	i, n
real	betasq, x, f, dx, deltax, dxsq, erfval, fwt

begin
	# Compute some constants.
	betasq = beta ** 2

	# Estimate the number of intervals required by evaluating the fourth
	# derivative of the Gaussian at xin.

	x = ((xin - x0) / beta) ** 2
	f = exp (-0.5 * x)
	n = max (2, int (3.247 * ((f * abs (x * (x - 6.) + 3. )) ** 0.25) /
	    beta) + 1)
	if (mod (n, 2) != 0)
	    n = n + 1
	dx = 1. / real (n)

	# Star with the lower endpoint.

	deltax = xin - x0 - 0.5
	dxsq = deltax ** 2
	f = exp (-0.5 * dxsq / betasq)
	erfval = f
	dfdx0 = f * deltax
	dfdbet = f * dxsq

	# Now include the end points of each subinterval except the last one.
	# If it is an add-numbered subinterval, weight = 4. If even, weight
	# = 2.0.

	do i = 1, n - 1 {
	    deltax = deltax + dx
	    dxsq = deltax ** 2
	    f = exp (-0.5 * dxsq / betasq)
	    fwt = f * 2. * real (1 + mod (i, 2))
	    erfval = erfval + fwt
	    dfdx0 = dfdx0 + deltax * fwt
	    dfdbet = dfdbet + dxsq * fwt
	}

	# Now add the upper end point (weight = 1.0) and multiply by dx/3.

	deltax = deltax + dx
	dxsq = deltax ** 2
	f = exp (-0.5 * dxsq / betasq)
	dx = dx / 3.
	erfval = dx * (erfval + f)
	if (erfval < 1.e-19)
	    erfval = 0.0
	dfdx0 = dx * (dfdx0 + deltax * f) / betasq
	if (abs (dfdx0) < 1.e-19)
	    dfdx0 = 0.0
	dfdbet = dx * (dfdbet + f * dxsq) / (betasq * beta)
	if (abs (dfdbet) < 1.e-19)
	    dfdbet = 0.0

	return (erfval)
end
