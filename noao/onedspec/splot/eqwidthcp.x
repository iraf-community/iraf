# EQWIDTH_CP -- Equivalent width following algorithm provided by
#               Caty Pilachowski. This assumes a Gaussian line profile
#               and fits to the specified core level, the width at the
#		specified flux level above the core, and the specified
#		continuum.  The line position is found by searching
#               near the vertical cursor for the nearest minimum.

define	LEFT	1	# Fit to left edge
define	RIGHT	2	# Fit to right edge
define	BOTH	3	# Fit to both edges

procedure eqwidth_cp (gfd, npts, center, cont, ylevel, x1, x2, dx, pix,
    key, ans)

int	gfd, npts
real	center, cont, ylevel, x1, x2, dx
real	pix[ARB]
int	key
char	ans[2*SZ_LINE]

int	i, i1, i2, isrch, icore, edge
real	xleft, xright, delta, rcore, rinter, y, sigma, flux, eqw, w, w1, w2
real	pi, xpara[3], coefs[3], xcore, pixcore

# Initialize reasonable values
#	isrch -- nr of pixels on either side of cursor to search for min

data	isrch /3/
data	pi /3.1415926/

begin
	# Check continuum.
	if (cont <= 0.) {
	    call eprintf ("Continuum cannot be less than zero.\n")
	    return
	}

	# Determine which edges of the line to use.
	switch (key) {
	case 'a', 'l':
	    edge = LEFT
	case 'b', 'r':
	    edge = RIGHT
	default:
	    edge = BOTH
	}

	# Search for local minimum or maximum
	call pixind (x1, x2, dx, center, i)
	icore = i
	i1 = max (1, i-isrch)
	i2 = min (npts, i+isrch)

	# If half lines is selected, restrict the search
	if (edge == LEFT)
	    i2 = max (i2-2, icore+1)
	if (edge == RIGHT)
	    i1 = min (i1+2, icore-1)

	# Search for core.
	# Someday it may be desirable to use parabolic interpolation
	# to locate an estimated minimum or maximum for the region
	do i = i1, i2 {
	    if (abs (pix[i] - cont) > abs (pix[icore] - cont))
		icore = i
	}

	# Fit parabola to three points around minimum pixel
	xpara[1] = icore - 1
	xpara[2] = icore
	xpara[3] = icore + 1

	call para (xpara, pix[icore-1], coefs)

	# Compute pixel value at minimum
	xcore = -coefs[2] / 2.0 / coefs[3]
	pixcore = coefs[1] + coefs[2] * xcore + coefs[3] * xcore**2

	# Locate left and right line edges.  If the ylevel is INDEF then use
	# the half flux point.
	if (IS_INDEF (ylevel))
	    y = (cont + pixcore) / 2.
	else
	    y = ylevel

	rcore = abs (pixcore - cont)
	rinter = abs (y - cont)

	if (rcore <= rinter) {
	    call eprintf (
		"Y cursor must be between the continuum and the line core\n")
	    return
	}

	# Bound flux level of interest
	if ((edge == LEFT) || (edge == BOTH)) {
	    for (i=icore; i >= 1; i=i-1)
		if (abs (pix[i] - cont) < rinter)
		    break

	    if (i < 1) {
		call eprintf ("Can't find left edge of line\n")
		return
	    }

	    xleft = float (i) + (y - pix[i]) / (pix[i+1] - pix[i])
	    if (edge == LEFT)
	        xright = xcore + (xcore - xleft)
	}

	# Now bound the right side
	if ((edge == RIGHT) || (edge == BOTH)) {
	    for (i=icore; i <= npts; i=i+1)
		if (abs (pix[i] - cont) < rinter)
		    break

	    if (i > npts) {
		call eprintf ("Can't find right edge of line\n")
		return
	    }

	    xright = float (i) - (y - pix[i]) / (pix[i-1] - pix[i])
	    if (edge == RIGHT)
	        xleft  = xcore - (xright - xcore)
	}

	# Compute width in Angstroms
	delta = (xright - xleft) * dx

	# And apply Gaussian model
	sigma = delta / 2. / sqrt (2. * log (rcore/rinter))
	rcore = pixcore - cont
	flux = rcore * sigma * sqrt (2. * pi)
	eqw = abs (flux / cont)

	w = x1 + (xcore-1) * dx
	call printf (
	    "center = %9.7g, eqw = %9.4g, sigma = %9.4g, fwhm = %9.4g\n")
	    call pargr (w)
	    call pargr (eqw)
	    call pargr (sigma)
	    call pargr (2.355 * sigma)

	call sprintf (ans, 2*SZ_LINE,
	    " %9.7g %9.7g %9.6g %9.4g %9.6g %9.4g %9.4g\n")
	    call pargr (w)
	    call pargr (cont)
	    call pargr (flux)
	    call pargr (eqw)
	    call pargr (pixcore - cont)
	    call pargr (sigma)
	    call pargr (2.355 * sigma)

	# Mark line computed
	w1 = x1 + (xleft - 1) * dx
	w2 = x1 + (xright- 1) * dx
	call gline (gfd, w, cont, w, pixcore)
	call gline (gfd, w1, y, w2, y)

	w1 = w - 3 * sigma
	w2 = cont + rcore * exp (-0.5*((w1-w)/sigma)**2)
	call gamove (gfd, w1, w2)
	for (; w1 <= w+3*sigma; w1=w1+0.1*sigma) {
	    w2 = cont + rcore * exp (-0.5*((w1-w)/sigma)**2)
	    call gadraw (gfd, w1, w2)
	}
end

# PARA -- Fit a parabola to three points

procedure para (x, y, c)

real	x[ARB], y[ARB], c[3]

begin
	c[3] = (y[1]-y[2]) * (x[2]-x[3]) / (x[1]-x[2]) - (y[2]-y[3])
	c[3] = c[3] / ((x[1]**2-x[2]**2) * (x[2]-x[3]) / (x[1]-x[2]) -
		(x[2]**2-x[3]**2))

	c[2] = (y[1] - y[2]) - c[3] * (x[1]**2 - x[2]**2)
	c[2] = c[2] / (x[1] - x[2])

	c[1] = y[1] - c[2] * x[1] - c[3] * x[1]**2
end
