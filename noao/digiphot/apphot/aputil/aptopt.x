include	<mach.h>
include	<math.h>

# APTOPT - One-dimensional centering routine using repeated
# convolutions to locate image center.

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
	# Allocate working space.
	call smark (sp)
	call salloc (wgt, npix, TY_REAL)

	# Initialize.
	x[1] = center
	call mkt_prof_derv (Memr[wgt], npix, x[1], sigma, ortho)
	s[1] = adotr (Memr[wgt], data, npix)
	if (abs (s[1]) <= EPSILONR) {
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
	    if (abs (s[1]) <= EPSILONR) {
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
	if (abs (s[2]) <= EPSILONR) {
	    center = x[2]
	    call sfree (sp)
	    return (1)
	}

	# Search quadratically.
	for (iter = 2; iter <= maxiter; iter = iter + 1)  {

	    # Check for completion.
	    if (abs (s[2]) <= EPSILONR)
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


# AP_TPROFDER -- Procedure to estimate the triangle functions and its
# derivatives

procedure ap_tprofder (data, der, npix, center, sigma, ampl)

real	data[ARB]		# input data
real	der[ARB]		# derivatives
int	npix			# number of pixels
real	center			# center of Gaussian
real	sigma			# sigma of Gaussian
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
# Vector is output of GAUSS_DERV with Gramm-Schmidt orthogonalization applied.

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
	# Fetch the weighting function and derivatives.
	call smark (sp)
	call salloc (der, npix, TY_REAL)
	call ap_tprofder (Memr[der], weight, npix, center, sigma, 1.0)

	if (norm == YES) {

	    # Make orthogonal to level background.
	    coef = -asumr (weight, npix) / npix
	    call aaddkr (weight, coef, weight, npix)
	    coef = -asumr (Memr[der], npix) / npix
	    call aaddkr (Memr[der], coef, Memr[der], npix)

	    # Make orthogonal to profile vector.
	    coef = adotr (weight, Memr[der], npix) /
		adotr (Memr[der], Memr[der], npix)
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
