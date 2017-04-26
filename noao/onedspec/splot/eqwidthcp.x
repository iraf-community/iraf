include	<gset.h>


# EQWIDTH_CP -- Equivalent width following algorithm provided by
#               Caty Pilachowski. This assumes a Gaussian line profile
#               and fits to the specified core level, the width at the
#		specified flux level above the core, and the specified
#		continuum.  The line position is found by searching
#               near the vertical cursor for the nearest minimum.

define	LEFT	1	# Fit to left edge
define	RIGHT	2	# Fit to right edge
define	BOTH	3	# Fit to both edges

procedure eqwidth_cp (sh, gfd, center, cont, ylevel, y, n, key, fd1, fd2,
	xg, yg, sg, lg, pg, ng)

pointer	sh
int	gfd
real	center, cont, ylevel
real	y[n]
int	n
int	key
int	fd1, fd2
pointer	xg, yg, sg, lg, pg	# Pointers to fit parameters
int	ng			# Number of components

int	i, i1, i2, isrch, icore, edge
double	xleft, xright, rcore, rinter, yl, gfwhm, lfwhm, flux, eqw, w, w1, w2
double	xpara[3], ypara[3], coefs[3], xcore, ycore
double	shdr_lw(), shdr_wl()

# Initialize reasonable values
#	isrch -- nr of pixels on either side of cursor to search for min

data	isrch /3/

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
	icore = max (1, min (n, nint (shdr_wl (sh, double(center)))))
	i1 = max (1, icore-isrch)
	i2 = min (n, icore+isrch)

	# If half lines is selected, restrict the search
	if (edge == LEFT)
	    i2 = max (i2-2, icore+1)
	if (edge == RIGHT)
	    i1 = min (i1+2, icore-1)

	# Search for core.
	# Someday it may be desirable to use parabolic interpolation
	# to locate an estimated minimum or maximum for the region
	do i = i1, i2 {
	    if (abs (y[i] - cont) > abs (y[icore] - cont))
		icore = i
	}

	# Fit parabola to three points around minimum pixel
	xpara[1] = icore - 1
	xpara[2] = icore
	xpara[3] = icore + 1
	ypara[1] = y[icore-1]
	ypara[2] = y[icore]
	ypara[3] = y[icore+1]

	call para (xpara, ypara, coefs)

	# Compute pixel value at minimum
	xcore = -coefs[2] / 2.0 / coefs[3]
	ycore = coefs[1] + coefs[2] * xcore + coefs[3] * xcore**2

	# Locate left and right line edges.  If the ylevel is INDEF then use
	# the half flux point.
	if (IS_INDEF (ylevel))
	    yl = (cont + ycore) / 2.
	else
	    yl = ylevel

	rcore = abs (ycore - cont)
	rinter = abs (yl - cont)

	if (rcore <= rinter) {
	    call eprintf (
		"Y cursor must be between the continuum and the line core\n")
	    return
	}

	# Bound flux level of interest
	if ((edge == LEFT) || (edge == BOTH)) {
	    for (i=icore; i >= 1; i=i-1)
		if (abs (y[i] - cont) < rinter)
		    break

	    if (i < 1) {
		call eprintf ("Can't find left edge of line\n")
		return
	    }

	    xleft = float (i) + (yl - y[i]) / (y[i+1] - y[i])
	    if (edge == LEFT)
	        xright = xcore + (xcore - xleft)
	}

	# Now bound the right side
	if ((edge == RIGHT) || (edge == BOTH)) {
	    for (i=icore; i <= n; i=i+1)
		if (abs (y[i] - cont) < rinter)
		    break

	    if (i > n) {
		call eprintf ("Can't find right edge of line\n")
		return
	    }

	    xright = float (i) - (yl - y[i]) / (y[i-1] - y[i])
	    if (edge == RIGHT)
	        xleft  = xcore - (xright - xcore)
	}

	# Compute in wavelength
	w = shdr_lw (sh, double(xcore))
	w1 = shdr_lw (sh, double(xleft))
	w2 = shdr_lw (sh, double(xright))

	# Apply Gaussian model
	gfwhm = 1.665109 * abs (w2 - w1) / 2. / sqrt (log (rcore/rinter))
	lfwhm = 0.
	rcore = ycore - cont
	flux = 1.064467 * rcore * gfwhm
	eqw = -flux / cont

	call printf (
	    "center = %9.7g, eqw = %9.4g, gfwhm = %9.4g\n")
	    call pargd (w)
	    call pargd (eqw)
	    call pargd (gfwhm)

	if (fd1 != NULL) {
	    call fprintf (fd1, " %9.7g %9.7g %9.6g %9.4g %9.6g %9.4g %9.4g\n")
		call pargd (w)
		call pargr (cont)
		call pargd (flux)
		call pargd (eqw)
		call pargd (ycore - cont)
		call pargd (gfwhm)
		call pargd (lfwhm)
	}
	if (fd2 != NULL) {
	    call fprintf (fd2, " %9.7g %9.7g %9.6g %9.4g %9.6g %9.4g %9.4g\n")
		call pargd (w)
		call pargr (cont)
		call pargd (flux)
		call pargd (eqw)
		call pargd (ycore - cont)
		call pargd (gfwhm)
		call pargd (lfwhm)
	}

	# Mark line computed
	call gline (gfd, real(w), cont, real(w), real(ycore))
	call gline (gfd, real(w1), real(yl), real(w2), real(yl))

	w1 = w - 2 * gfwhm
	w2 = cont + rcore * exp (-(1.665109*(w1-w)/gfwhm)**2)
	call gseti (gfd, G_PLTYPE, 2)
	call gseti (gfd, G_PLCOLOR, 2)
	call gamove (gfd, real(w1), real(w2))
	for (; w1 <= w+2*gfwhm; w1=w1+0.05*gfwhm) {
	    w2 = cont + rcore * exp (-(1.665109*(w1-w)/gfwhm)**2)
	    call gadraw (gfd, real(w1), real(w2))
	}
	call gseti (gfd, G_PLTYPE, 1)
	call gseti (gfd, G_PLCOLOR, 1)

	# Save fit parameters
	if (ng == 0) {
	    call malloc (xg, 1, TY_REAL)
	    call malloc (yg, 1, TY_REAL)
	    call malloc (sg, 1, TY_REAL)
	    call malloc (lg, 1, TY_REAL)
	    call malloc (pg, 1, TY_INT)
	} else if (ng != 1) {
	    call realloc (xg, 1, TY_REAL)
	    call realloc (yg, 1, TY_REAL)
	    call realloc (sg, 1, TY_REAL)
	    call realloc (lg, 1, TY_REAL)
	    call realloc (pg, 1, TY_INT)
	}
	Memr[xg] = w
	Memr[yg] = rcore
	Memr[sg] = gfwhm
	Memr[lg] = lfwhm
	Memi[pg] = 1
	ng = 1
end

# PARA -- Fit a parabola to three points

procedure para (x, y, c)

double	x[3], y[3], c[3]
double  x12, x13, x23, x213, x223, y13, y23

begin
        x12 = x[1] - x[2]
        x13 = x[1] - x[3]
        x23 = x[2] - x[3]

	if (x12 == 0. || x13 == 0. || x23 == 0.)
	    call error (1, "X points are not distinct")

	# Compute relative to an origin at x[3]
        x213 = x13 * x13
        x223 = x23 * x23
        y13 = y[1] - y[3]
        y23 = y[2] - y[3]
        c[3] = (y13 - y23 * x13 / x23) / (x213 - x223 * x13 / x23)
        c[2] = (y23 - c[3] * x223) / x23
        c[1] = y[3]

	# Compute relative to an origin at 0.
	c[1] = c[1] - x[3] * (c[2] - c[3] * x[3])
	c[2] = c[2] - 2 * c[3] * x[3]
end
