include <math.h>
include <math/curfit.h>
include <math/iminterp.h>


# ST_XYUNIFORM -- Compute a set of x and y values uniformly distributed in x and
# y between xmin, xmax, ymin and ymax.

procedure st_xyuniform (x, y, nstars, xmin, xmax, ymin, ymax, seed)

real	x[ARB]		# output array of x values
real	y[ARB]		# output array of y values
int	nstars		# number of stars
real	xmin, xmax	# x coordinate limits
real	ymin, ymax	# y coordinate limits
long	seed		# seed for random number generator

int	i
real	urand()

begin
	# Compute x and y values between 0 and 1.
	do i = 1, nstars {
	    x[i] = urand (seed)
	    y[i] = urand (seed)
	}

	# Map these values into the data range.
	call amapr (x, x, nstars, 0.0, 1.0, xmin, xmax)
	call amapr (y, y, nstars, 0.0, 1.0, ymin, ymax)
end


# ST_HBSAMPLE -- Compute a set of x and y values with a Hubble density
# distribution.

procedure st_hbsample (x, y, nstars, core, base, xc, yc, xmin, xmax, ymin, ymax,
        nsample, order, seed)

real	x[ARB]			# output array of x values
real	y[ARB]			# output array of y values
int	nstars			# number of stars
real	core			# Hubble core radius
real	base			# baseline density
real	xc, yc			# x and y center coordinates
real	xmin, xmax		# x range
real	ymin, ymax		# y range
int	nsample			# number of sample points
int	order			# order of spline fit
long	seed			# seed for random number generator

int	i, ier
pointer	sp, rad, prob, w, cv
real	r1, r2, r3, r4, rmin, rmax, rval, dr, theta
real	urand(), cveval()

begin
	# Allocate space for the fits.
	call smark (sp)
	call salloc (rad, nsample, TY_REAL)
	call salloc (prob, nsample, TY_REAL)
	call salloc (w, nsample, TY_REAL)

	# Compute the maximum radial distance from the center and
	# the sampling interval.

	r1 = (xmin - xc) ** 2 + (ymin - yc) ** 2
	r2 = (xmax - xc) ** 2 + (ymin - yc) ** 2
	r3 = (xmax - xc) ** 2 + (ymax - yc) ** 2
	r4 = (xmin - xc) ** 2 + (ymax - yc) ** 2
	if (xc >= xmin && xc <= xmax && yc >= ymin && yc <= ymax)
	    rmin = 0.0
	else if (yc >= ymin && yc <= ymax)
	    rmin = min (abs (xmin - xc), abs (xmax - xc))
	else if (xc >= xmin && xc <= xmax)
	    rmin = min (abs (ymin - yc), abs (ymax - yc))
	else
	    rmin = sqrt (min (r1, r2, r3, r4))
	rmax = sqrt (max (r1, r2, r3, r4))
	dr = (rmax - rmin) / (nsample - 1)

	# Compute the integral of the sampling function.
	r1 = core ** 2
	rval = rmin
	do i = 1, nsample {
	    Memr[rad+i-1] = rval
	    r2 = (core + rval) / core
	    Memr[prob+i-1] = r1 * (log (r2) + 1.0 / r2 - 1.0) +
	        base * rval ** 2 / 2.0
	    rval = rval + dr
	}

	# Normalize the probability function.
	call alimr (Memr[prob], nsample, rmin, rmax)
	call amapr (Memr[prob], Memr[prob], nsample, rmin, rmax, 0.0, 1.0)

	# Fit the inverse of the integral of the probability function
	call cvinit (cv, SPLINE3, order, 0.0, 1.0)
	call cvfit (cv, Memr[prob], Memr[rad], Memr[w], nsample, WTS_UNIFORM,
	    ier)

	# Sample the computed function.
	if (ier == OK) {
	    i = 0
	    repeat {
	        rval = cveval (cv, urand (seed))
	        theta = DEGTORAD (360.0 * urand (seed))
	        x[i+1] = rval * cos (theta) + xc
	        y[i+1] = rval * sin (theta) + yc
	        if (x[i+1] >= xmin && x[i+1] <= xmax && y[i+1] >= ymin &&
	            y[i+1] <= ymax)
		    i = i + 1
	    } until (i >= nstars)
	} else {
	    call amovkr ((xmin + xmax) / 2.0, x, nstars)
	    call amovkr ((ymin + ymax) / 2.0, y, nstars)
	    call printf ("Error computing the spatial probability function.\n")
	}

	# Free up the space.
	call cvfree (cv)
	call sfree (sp)
end


# ST_SFSAMPLE -- Compute a sample of x and y coordinate values based
# on a user supplied spatial density function.

procedure st_sfsample (r, rprob, nsf, x, y, nstars, nsample, order, xc, yc,
	xmin, xmax, ymin, ymax, seed)

real	r[ARB]			# input array of radii
real	rprob[ARB]		# input array of relative probabilities
int	nsf			# number of input points
real	x[ARB]			# output x coordinate array
real	y[ARB]			# output y coordinate array
int	nstars			# number of stars
int	nsample			# number of sample points
int	order			# order of the spline fit
real	xc, yc			# x and y center coordiantes
real	xmin, xmax		# min and max x values
real	ymin, ymax		# min and max y values
long	seed			# value of the seed

int	itemp, i, ier
pointer	sp, w, rad, iprob, cv, asi
real	rfmin, rfmax, dr, rval, theta, imin, imax
real	cveval(), asigrl(), urand()

begin
	# Allocate space for fitting.
	itemp = max (nsf, nsample)
	call smark (sp)
	call salloc (rad, nsample, TY_REAL)
	call salloc (iprob, nsample, TY_REAL)
	call salloc (w, itemp, TY_REAL)

	# Smooth the relative probability function function.
	call alimr (r, nsf, rfmin, rfmax)
	itemp = min (order, max (1, nsf / 4))
	call cvinit (cv, SPLINE3, itemp, rfmin, rfmax)
	call cvfit (cv, r, rprob, Memr[w], nsf, WTS_UNIFORM, ier) 

	# Evaluate the smoothed function at equal intervals in r,
	# Multiplying by r to prepare for the area integration.
	if (ier == OK) {
	    rval = rfmin
	    dr = (rfmax - rfmin) / (nsample - 1)
	    do i = 1, nsample {
		Memr[rad+i-1] = rval
	        Memr[iprob+i-1] = rval * cveval (cv, rval)
	        rval = rval + dr
	    }
	    call cvfree (cv)
	} else {
	    call printf ("Error smoothing the user spatial density function.\n")
	    call amovkr ((xmin + xmax) / 2.0, x, nstars)
	    call amovkr ((ymin + ymax) / 2.0, y, nstars)
	    call cvfree (cv)
	    call sfree (sp)
	    return
	}


	# Evaluate the integral.
	call asiinit (asi, II_SPLINE3)
	call asifit (asi, Memr[iprob], nsample)
	Memr[iprob] = 0.0
	do i = 2, nsample
	    Memr[iprob+i-1] = Memr[iprob+i-2] + asigrl (asi, real (i - 1),
		real (i))
	call alimr (Memr[iprob], nsample, imin, imax)
	call amapr (Memr[iprob], Memr[iprob], nsample, imin, imax, 0.0, 1.0)
	call asifree (asi)

	# Fit the inverse of the integral of the probability function.
	call cvinit (cv, SPLINE3, order, 0.0, 1.0)
	call cvfit (cv, Memr[iprob], Memr[rad], Memr[w], nsample, WTS_UNIFORM,
	    ier)

	# Sample the computed function.
	if (ier == OK) {
	    i = 0
	    repeat {
	        rval = cveval (cv, urand (seed))
	        theta = DEGTORAD (360.0 * urand (seed))
	        x[i+1] = rval * cos (theta) + xc
	        y[i+1] = rval * sin (theta) + yc
	        if (x[i+1] >= xmin && x[i+1] <= xmax && y[i+1] >= ymin &&
	            y[i+1] <= ymax)
		    i = i + 1
	    } until (i >= nstars)
	} else {
	    call printf (
	    "Error fitting the spatial probability function.\n")
	    call amovkr ((xmin + xmax) / 2.0, x, nstars)
	    call amovkr ((ymin + ymax) / 2.0, y, nstars)
	}
	call cvfree (cv)

	# Free space.
	call sfree (sp)
end


define	BUFSIZE		200

# ST_GFETCHXY -- Fetch two real values from a text file.

int procedure st_gfetchxy (sf, x, y)

int	sf			# input text file descriptor
pointer	x			# pointer to the x array
pointer	y			# pointer to the y array

int	bufsize, npts
int	fscan(), nscan()

begin
	call seek (sf, BOF)

	call malloc (x, BUFSIZE, TY_REAL)
	call malloc (y, BUFSIZE, TY_REAL)
	bufsize = BUFSIZE

	npts = 0
	while (fscan (sf) != EOF) {
	    call gargr (Memr[x+npts])
	    call gargr (Memr[y+npts])
	    if (nscan () != 2)
		next
	    npts = npts + 1
	    if (npts < bufsize)
		next
	    bufsize = bufsize + BUFSIZE
	    call realloc (x, bufsize, TY_REAL)
	    call realloc (y, bufsize, TY_REAL)
	}

	call realloc (x, npts, TY_REAL)
	call realloc (y, npts, TY_REAL)

	return (npts)
end
