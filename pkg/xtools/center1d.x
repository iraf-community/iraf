# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math/iminterp.h>
include	<pkg/center1d.h>

define	MIN_WIDTH	3.		# Minimum centering width
define	EPSILON		0.001		# Accuracy of centering
define	EPSILON1	0.005		# Tolerance for convergence check
define	ITERATIONS	100		# Maximum number of iterations
define	MAX_DXCHECK	3		# Look back for failed convergence
define	INTERPTYPE	II_SPLINE3	# Image interpolation type


# CENTER1D -- Locate the center of a one dimensional feature.
# A value of INDEF is returned in the centering fails for any reason.
# This procedure just sets up the data and adjusts for emission or
# absorption features.  The actual centering is done by C1D_CENTER.
# If twidth <= 1 return the nearest minima or maxima.

real procedure center1d (x, data, npts, width, type, radius, threshold)

real	x				# Initial guess
int	npts				# Number of data points
real	data[npts]			# Data points
real	width				# Feature width
int	type				# Feature type
real	radius				# Centering radius
real	threshold			# Minimum range in feature

real	xc				# Center

int	x1, x2, nx
real	a, b, rad, wid
pointer	sp, data1

real	c1d_center()

begin

	# Check starting value.
	if (IS_INDEF(x) || (x < 1) || (x > npts))
	    return (INDEF)

	# Set parameters.  The minimum in the error radius
	# is for defining the data window.  The user error radius is used to
	# check for an error in the derived center at the end of the centering.

	call c1d_params (INDEFI, INDEFR)
	wid = max (width, MIN_WIDTH)
	rad = max (2., radius)

	# Determine the pixel value range around the initial center, including
	# the width and error radius buffer.  Check for a minimum range.

	x1 = max (1., x - wid / 2 - rad - wid)
	x2 = min (real (npts), x + wid / 2 + rad + wid + 1)
	nx = x2 - x1 + 1
	call alimr (data[x1], nx, a, b)
	if (b - a < threshold)
	    return (INDEF)

	# Allocate memory for the continuum subtracted data vector.  The X
	# range is just large enough to include the error radius and the
	# half width.

	x1 = max (1., x - wid / 2 - rad)
	x2 = min (real (npts), x + wid / 2 + rad + 1)
	nx = x2 - x1 + 1

	call smark (sp)
	call salloc (data1, nx, TY_REAL)
	call amovr (data[x1], Memr[data1], nx)

	# Make the centering data positive, subtract the continuum, and
	# apply a threshold to eliminate noise spikes.

	switch (type) {
	case EMISSION:
	    a = min (0., a)
	    call asubkr (data[x1], a + threshold, Memr[data1], nx)
	    call amaxkr (Memr[data1], 0., Memr[data1], nx)
	case ABSORPTION:
	    call anegr (data[x1], Memr[data1], nx)
	    call asubkr (Memr[data1], threshold - b, Memr[data1], nx)
	    call amaxkr (Memr[data1], 0., Memr[data1], nx)
	default:
	    call error (0, "Unknown feature type")
	}

	# Determine the center.
	xc = c1d_center (x - x1 + 1, Memr[data1], nx, width)

	# Check user centering error radius.
	if (!IS_INDEF(xc)) {
	    xc = xc + x1 - 1
	    if (abs (x - xc) > radius)
		xc = INDEF
	}

	# Free memory and return the center position.
	call sfree (sp)
	return (xc)
end


# C1D_PARAMS -- Set parameters.

procedure c1d_params (interp, eps)

int	interp		# Interpolation type
real	eps		# Accuracy of centering

int	first
data	first /YES/

int	interptype
real	epsilon
common	/c1d_common/ interptype, epsilon

begin
	if (!IS_INDEFI(interp))
	    interptype = interp
	else if (first == YES)
	    interptype = INTERPTYPE

	if (!IS_INDEFR(eps))
	    epsilon = eps
	else if (first == YES)
	    epsilon = EPSILON

	first = NO
end


# C1D_CENTER -- One dimensional centering algorithm.
# If the width is <= 1. return the nearest local maximum.

real procedure c1d_center (x, data, npts, width)

real	x				# Starting guess
int	npts				# Number of points in data vector
real	data[npts]			# Data vector
real	width				# Centering width

int	i, j, iteration, dxcheck
real	xc, wid, hwidth, dx, dxabs, dxlast
real	a, b, sum1, sum2, intgrl1, intgrl2
pointer	asi1, asi2, sp, data1

real	asigrl()

int	interptype
real	epsilon
common	/c1d_common/ interptype, epsilon

define	done_	99

begin
	# Find the nearest local maxima as the starting point.
	# This is required because the threshold limit may have set
	# large regions of the data to zero and without a gradient
	# the centering will fail.

	for (i=x+.5; (i<npts) && (data[i]<=data[i+1]); i=i+1)
	    ;
	for (; (i>1) && (data[i]<=data[i-1]); i=i-1)
	    ;
	for (j=x+.5; (j>1) && (data[j]<=data[j-1]); j=j-1)
	    ;
	for (; (j<npts) && (data[j]<=data[j+1]); j=j+1)
	    ;

	if (abs(i-x) < abs(x-j))
	    xc = i 
	else
	    xc = j

	if (width <= 1.)
	    return (xc)

	wid = max (width, MIN_WIDTH)

	# Check data range.
	hwidth = wid / 2
	if ((xc - hwidth < 1) || (xc + hwidth > npts))
	    return (INDEF)

	# Set interpolation functions.
	call asiinit (asi1, interptype)
	call asiinit (asi2, interptype)
	call asifit (asi1, data, npts)

	# Allocate, compute, and interpolate the x*y values.
	call smark (sp)
	call salloc (data1, npts, TY_REAL)
	do i = 1, npts
	    Memr[data1+i-1] = data[i] * i
	call asifit (asi2, Memr[data1], npts)
	call sfree (sp)

	# Iterate to find center.  This loop exits when 1) the maximum
	# number of iterations is reached, 2) the delta is less than
	# the required accuracy (criterion for finding a center), 3)
	# there is a problem in the computation, 4) successive steps
	# continue to exceed the minimum delta.

	dxlast = npts
	do iteration = 1, ITERATIONS {
	    # Ramp centering function.
	    # a = xc - hwidth
	    # b = xc + hwidth
	    # intgrl1 = asigrl (asi1, a, b)
	    # intgrl2 = asigrl (asi2, a, b)
	    # sum1 = intgrl2 - xc * intgrl1
	    # sum2 = intgrl1

	    # Triangle centering function.
	    a = xc - hwidth
	    b = xc - hwidth / 2
	    intgrl1 = asigrl (asi1, a, b)
	    intgrl2 = asigrl (asi2, a, b)
	    sum1 = (xc - hwidth) * intgrl1 - intgrl2
	    sum2 = -intgrl1
	    a = b
	    b = xc + hwidth / 2
	    intgrl1 = asigrl (asi1, a, b)
	    intgrl2 = asigrl (asi2, a, b)
	    sum1 = sum1 - xc * intgrl1 + intgrl2
	    sum2 = sum2 + intgrl1
	    a = b
	    b = xc + hwidth
	    intgrl1 = asigrl (asi1, a, b)
	    intgrl2 = asigrl (asi2, a, b)
	    sum1 = sum1 + (xc + hwidth) * intgrl1 - intgrl2
	    sum2 = sum2 - intgrl1

	    # Return no center if sum2 is zero.
	    if (sum2 == 0.)
		break

	    # Limit dx change in one iteration to 1 pixel.
	    dx = sum1 / abs (sum2)
	    dxabs = abs (dx)
	    xc = xc + max (-1., min (1., dx))

	    # Check data range.  Return no center if at edge of data.
	    if ((xc - hwidth < 1) || (xc + hwidth > npts))
		break

	    # Convergence tests.
	    if (dxabs < epsilon)
		goto done_
	    if (dxabs > dxlast + EPSILON1) {
		dxcheck = dxcheck + 1
		if (dxcheck > MAX_DXCHECK)
		    break
	    } else if (dxabs > dxlast - EPSILON1) {
		xc = xc - max (-1., min (1., dx)) / 2
		dxcheck = 0
	    } else {
		dxcheck = 0
	        dxlast = dxabs
	    }
	}

	# If we get here then no center was found.
	xc = INDEF

done_	call asifree (asi1)
	call asifree (asi2)
	return (xc)
end
