define	NGL	4

# DAOERF -- Numerically integrate a Gaussian function from xin-0.5 to xin+0.5
# using 4 point Gauss-Legendre integration. Beta is the half-width at
# half-maximum which is equal to 1.17741 * sigma. The Gaussian function is
# shown below.
#
#      erf = exp (-0.5 *((x - x0) / beta) ** 2))
#
#                          or
#
#      erf = exp (-0.6931472 * [(x - xo) / beta] ** 2)
#
# Also provide the first derivative of the integral with respect to xo and beta.

real procedure daoerf (xin, x0, beta, dfdx0, dfdbet)

real	xin		# the input value
real	x0		# position of Gaussian peak
real	beta		# sigma of the Gaussian
real	dfdx0		# derivative of Gaussian wrt x0
real	dfdbet		# derivative of Gaussian wrt beta

int	i, npt
real	betasq, deltax, erfval, xsq, f, x, wf
real	dx[NGL,NGL], wt[NGL,NGL]
data	dx / 0.0,         0.0,        0.0,        0.0,
            -0.28867513,  0.28867513, 0.0,        0.0,
	    -0.38729833,  0.0,        0.38729833, 0.0,
            -0.43056816, -0.16999052, 0.16999052, 0.43056816 /
data	wt / 1.0,         0.0,        0.0,        0.0,
	     0.5,         0.5,        0.0,        0.0,
	     0.27777778,  0.44444444, 0.27777778, 0.0,
             0.17392742,  0.32607258, 0.32607258, 0.17392742 /

begin
	# Compute some constants.
	betasq = beta ** 2
	deltax = xin - x0

	# Initialize.
	erfval = 0.0
	dfdx0 = 0.0
	dfdbet = 0.0

	xsq = deltax ** 2
	f = xsq / betasq
	if (f > 34.0)
	    return (erfval)

	f = exp (-0.6931472 * f)
	if (f >= 0.046) {
	    npt = 4
	} else if (f >= 0.0022) {
	    npt = 3
	} else if (f >= 0.0001) {
	    npt = 2
	} else if (f >= 1.0e-10) {
	    erfval = f
	    dfdx0 = 1.3862944 * deltax * f / betasq
	    dfdbet = 1.3862944 * xsq * f / (betasq * beta)
	    return (erfval)
	} else {
	    return (erfval)
	}

	do i = 1, npt {
	    x = deltax + dx[i,npt]
	    xsq = x ** 2
	    f = exp (-0.6931472 * xsq / betasq)
	    wf = wt[i,npt] * f
	    erfval = erfval + wf
	    dfdx0 = dfdx0 + x * wf
	    dfdbet = dfdbet + xsq * wf
	}

	dfdx0 = 1.3862944 * dfdx0 / betasq
	dfdbet = 1.3862944 * dfdbet / (betasq * beta)

	return (erfval)
end
