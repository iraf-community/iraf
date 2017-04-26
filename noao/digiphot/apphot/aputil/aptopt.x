include	<mach.h>
include	<math.h>

# APTOPT - One-dimensional centering routine using repeated convolutions to
# locate image center.

define	MAX_SEARCH	3	# Max initial search steps

int procedure aptopt (data, npix, center, sigma, tol, maxiter, ortho)

real	data[ARB]		# initial data
int	npix			# number of pixels
real	center			# initial guess at center
real	sigma			# sigma of Gaussian
real	tol			# gap tolerance for sigma
int	maxiter			# maximum number of iterations
int	ortho			# orthogonalize weighting vector

int	i, iter
pointer	sp, wgt
real	newx, x[3], news, s[3], delx
real	adotr(), apqzero()

begin
	if (sigma <= 0.0)
	    return (-1)

	# Allocate working space.
	call smark (sp)
	call salloc (wgt, npix, TY_REAL)

	# Initialize.
	x[1] = center
	call mkt_prof_derv (Memr[wgt], npix, x[1], sigma, ortho)
	s[1] = adotr (Memr[wgt], data, npix)
	#if (abs (s[1]) <= EPSILONR) {
	if (s[1] == 0.0) {
	    center = x[1]
	    call sfree (sp)
	    return (0)
	} else
	    s[3] = s[1]

	# Search for the correct interval.
	for (i = 1; (s[3] * s[1] >= 0.0) && (i <= MAX_SEARCH); i = i + 1) {
	    s[3] = s[1]
	    x[3] = x[1]
	    x[1] = x[3] + sign (sigma, s[3])
	    call mkt_prof_derv (Memr[wgt], npix, x[1], sigma, ortho)
	    s[1] = adotr (Memr[wgt], data, npix)
	    #if (abs (s[1]) <= EPSILONR) {
	    if (s[1] == 0.0) {
		center = x[1]
		call sfree (sp)
		return (0)
	    }
	}

	# Location not bracketed.
	if (s[3] * s[1] > 0.0) {
	    call sfree (sp)
	    return (-1)
	}

	# Intialize the quadratic search.
	delx = x[1] - x[3]
	x[2] = x[3] - s[3] * delx / (s[1] - s[3])
	call mkt_prof_derv (Memr[wgt], npix, x[2], sigma, ortho)
	s[2] = adotr (Memr[wgt], data, npix)
	#if (abs (s[2]) <= EPSILONR) {
	if (s[2] == 0.0) {
	    center = x[2]
	    call sfree (sp)
	    return (1)
	}

	# Search quadratically.
	for (iter = 2; iter <= maxiter; iter = iter + 1)  {

	    # Check for completion.
	    #if (abs (s[2]) <= EPSILONR)
	    if (s[2] == 0.0)
		break
	    if (abs (x[2] - x[1]) <= tol)
		break
	    if (abs (x[3] - x[2]) <= tol)
		break

	    # Compute new intermediate value.
	    newx = x[1] + apqzero (x, s)
	    call mkt_prof_derv (Memr[wgt], npix, newx, sigma, ortho)
	    news = adotr (Memr[wgt], data, npix)

	    if (s[1] * s[2] > 0.0) {
		s[1] = s[2]
		x[1] = x[2]
		s[2] = news
		x[2] = newx
	    } else {
		s[3] = s[2]
		x[3] = x[2]
		s[2] = news
		x[2] = newx
	    }
	}

	# Evaluate the center.
	center = x[2]
	call sfree (sp)
	return (iter)
end


# AP_TPROFDER -- Procedure to estimate the approximating triangle function
# and its derivatives.

procedure ap_tprofder (data, der, npix, center, sigma, ampl)

real	data[ARB]		# input data
real	der[ARB]		# derivatives
int	npix			# number of pixels
real	center			# center of input Gaussian function
real	sigma			# sigma of input Gaussian function
real	ampl			# amplitude

int	i
real	x, xabs, width

begin
	width = sigma * 2.35
	do i = 1, npix {
	    x = (i - center) / width
	    xabs = abs (x)
	    if (xabs <= 1.0) {
		data[i] = ampl * (1.0 - xabs)
		der[i] = x * data[i]
	    } else {
		data[i] = 0.0
		der[i] = 0.0
	    }
	}
end


# MKT_PROF_DERV - Make orthogonal profile derivative vector.

procedure mkt_prof_derv (weight, npix, center, sigma, norm)

real	weight[ARB]	# input weight
int	npix		# number of pixels
real	center		# center
real	sigma		# center
int	norm		# orthogonalise weight

pointer	sp, der
real	coef
real	asumr(),  adotr()

begin
	call smark (sp)
	call salloc (der, npix, TY_REAL)

	# Fetch the weighting function and derivatives.
	call ap_tprofder (Memr[der], weight, npix, center, sigma, 1.0)

	if (norm == YES) {

	    # Make orthogonal to level background.
	    coef = -asumr (weight, npix) / npix
	    call aaddkr (weight, coef, weight, npix)
	    coef = -asumr (Memr[der], npix) / npix
	    call aaddkr (Memr[der], coef, Memr[der], npix)

	    # Make orthogonal to profile vector.
	    coef = adotr (Memr[der], Memr[der], npix)
	    if (coef <= 0.0)
		coef = 1.0
	    else
	        coef = adotr (weight, Memr[der], npix) / coef
	    call amulkr (Memr[der], coef, Memr[der], npix)
	    call asubr (weight, Memr[der], weight, npix)

	    # Normalize the final vector.
	    coef = adotr (weight, weight, npix)
	    if (coef <= 0.0)
		coef = 1.0
	    else
	        coef = sqrt (1.0 / coef)
	    call amulkr (weight, coef, weight, npix)
	}

	call sfree (sp)
end

define	QTOL	.125

# APQZERO - Solve for the root of a quadratic function defined by three
# points.

real procedure apqzero (x, y)

real	x[3]
real	y[3]

real	a, b, c, det, dx
real	x2, x3, y2, y3

begin
	# Compute the determinant.
	x2 = x[2] - x[1]
	x3 = x[3] - x[1]
	y2 = y[2] - y[1]
	y3 = y[3] - y[1]
	det = x2 * x3 * (x2 - x3)

	# Compute the shift in x.
	#if (abs (det) > 100.0 * EPSILONR) {
	if (abs (det) > 0.0) {
	    a = (x3 * y2 - x2 * y3) / det
	    b = - (x3 * x3 * y2 - x2 * x2 * y3) / det
	    c =  a * y[1] / (b * b)
	    if (abs (c) > QTOL)
		dx = (-b / (2.0 * a)) * (1.0 - sqrt (1.0 - 4.0 * c))
	    else
		dx = - (y[1] / b) * (1.0 + c)
	    return (dx)
	#} else if (abs (y3) > EPSILONR)
	} else if (abs (y3) > 0.0)
	    return (-y[1] * x3 / y3)
	else
	    return (0.0)
end
