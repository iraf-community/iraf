include <mach.h>
include <math.h>
include <math/curfit.h>
include <math/iminterp.h>


# ST_MAGUNIFORM -- Compute a set of magnitude values which are uniformly
# distributed between minmag and maxmag.

procedure st_maguniform (mag, nstars, minmag, maxmag, seed)

real	mag[ARB]	# output array of magnitudes
int	nstars		# number of stars
real	minmag, maxmag	# minimum and maximum magnitude values
long	seed		# seed for random number generator

int	i
real	urand()

begin
	# Get values between 0 and 1.
	do i = 1, nstars
	    mag[i] = urand (seed)

	# Map values into data range.
	call amapr (mag, mag, nstars, 0.0, 1.0, minmag, maxmag)
end


# ST_POWER -- Compute a set of magnitude values which are power law
# distributed between minmag and maxmag.

procedure st_power (mag, nstars, power, minmag, maxmag, seed)

real	mag[ARB]	# output array of magnitudes
int	nstars		# number of stars
real	power		# power law exponent
real	minmag, maxmag	# minimum and maximum magnitude values
long	seed		# seed for random number generator

int	i
real	a, urand()

begin
	# Get values between 0 and 1.
	a = 10. ** (power * (maxmag - minmag)) - 1
	do i = 1, nstars
	    mag[i] = minmag + log10 (a * urand (seed) + 1) / power
end


define	MIN_BANDS	-6.
define	MAX_BANDS	19.
define	MID_BANDS	15.

# ST_BANDS -- Compute the Bahcall and Soneira luminosity function.

procedure st_bands (mag, nstars, alpha, beta, delta, mstar, minmag, maxmag,
	mzero, nsample, order, seed)

real	mag[ARB]		# array of output magnitudes
int	nstars			# number of stars
real	alpha, beta		# Bahcall and Soneira parameters
real	delta, mstar		# Bahcall and Soneira parameters
real	minmag, maxmag		# minimum and maximum magnitude values
real	mzero			# zero point between relative and absolute mags.
int	nsample			# number of points in sampling function
int	order			# order of the spline fit
long	seed			# value of the seed

int	i, ier
pointer	sp, m, iprob, w, cv, asi
real	dmag, magval, mtemp, temp1, temp2, imin, imax
real	cveval(), asigrl(), urand()

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (m, nsample, TY_REAL)
	call salloc (iprob, nsample, TY_REAL)
	call salloc (w, nsample, TY_REAL)

	# Compute the probability function.
	magval = max (minmag - mzero, MIN_BANDS)
	dmag = (min (maxmag - mzero, MAX_BANDS) - magval) / (nsample - 1)
	do i = 1, nsample {
	    Memr[m+i-1] = magval
	    if (magval > MID_BANDS)
		mtemp = MID_BANDS
	    else
	        mtemp = magval - mstar
	    temp1 = 10.0 ** (beta * mtemp)
	    temp2 = (1.0 + 10.0 ** ((beta - alpha) * delta * mtemp)) **
	            (1.0 / delta)
	    Memr[iprob+i-1] = temp1 / temp2
	    magval = magval + dmag
	}

	# Integrate the probablity function.
	call asiinit (asi, II_SPLINE3)
	call asifit (asi, Memr[iprob], nsample)
	Memr[iprob] = 0.0
	do i = 2, nsample
	    Memr[iprob+i-1] = Memr[iprob+i-2] + asigrl (asi, real (i-1),
	        real (i))
	call alimr (Memr[iprob], nsample, imin, imax)
	call amapr (Memr[iprob], Memr[iprob], nsample, imin, imax, 0.0, 1.0)
	call asifree (asi)

	# Fit the inverse of the integral of the probability function.
	call cvinit (cv, SPLINE3, order, 0.0, 1.0)
	call cvfit (cv, Memr[iprob], Memr[m], Memr[w], nsample, WTS_UNIFORM,
	    ier)

	# Compute the magnitudes.
	if (ier == OK) {
	    do i = 1, nstars
	        mag[i] = cveval (cv, urand (seed)) + mzero
	} else {
	    call printf ("Error computing the bands luminosity function.\n")
	    call amovkr ((minmag + maxmag) / 2.0, mag, nstars)
	}
	call cvfree (cv)

	# Free space.
	call sfree (sp)
end


define	MIN_SALPETER	-4.0
define	MAX_SALPETER	16.0

# ST_SALPETER  -- Compute the Salpter luminosity function.

procedure st_salpeter (mag, nstars, minmag, maxmag, mzero, nsample, order, seed)

real	mag[ARB]		# array of output magnitudes
int	nstars			# number of stars
real	minmag, maxmag		# minimum and maximum magnitude values
real	mzero			# zero point between relative and absolute mags.
int	nsample			# number of points in sampling function
int	order			# order of the spline fit
long	seed			# value of the seed

int	i, ier
pointer	sp, m, iprob, w, cv, asi
real	dmag, magval, imin, imax
real	cveval(), asigrl(), urand()

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (m, nsample, TY_REAL)
	call salloc (iprob, nsample, TY_REAL)
	call salloc (w, nsample, TY_REAL)

	# Compute the probability function.
	magval = max (minmag - mzero, MIN_SALPETER)
	dmag = (min (maxmag - mzero, MAX_SALPETER) - magval) / (nsample - 1)
	do i = 1, nsample {
	    Memr[m+i-1] = magval
	    Memr[iprob+i-1] = 10.0 ** (-3.158375 + 1.550629e-1 * magval -
	        5.19388e-3 * magval ** 2)
	    magval = magval + dmag
	}

	# Integrate the probablity function.
	call asiinit (asi, II_SPLINE3)
	call asifit (asi, Memr[iprob], nsample)
	Memr[iprob] = 0.0
	do i = 2, nsample
	    Memr[iprob+i-1] = Memr[iprob+i-2] + asigrl (asi, real (i-1),
	        real (i))
	call alimr (Memr[iprob], nsample, imin, imax)
	call amapr (Memr[iprob], Memr[iprob], nsample, imin, imax, 0.0, 1.0)
	call asifree (asi)

	# Fit the inverse of the integral of the probability function.
	call cvinit (cv, SPLINE3, order, 0.0, 1.0)
	call cvfit (cv, Memr[iprob], Memr[m], Memr[w], nsample, WTS_UNIFORM,
	    ier)

	# Compute the magnitudes.
	if (ier == OK) {
	    do i = 1, nstars
	        mag[i] = cveval (cv, urand (seed)) + mzero
	} else {
	    call printf ("Error computing the Salpeter luminosity function.\n")
	    call amovkr ((minmag + maxmag) / 2.0, mag, nstars)
	}
	call cvfree (cv)

	# Free space.
	call sfree (sp)
end


# ST_SCHECTER -- Compute the Schecter luminosity function.

procedure st_schecter (mag, nstars, alpha, mstar, minmag, maxmag, mzero,
	nsample, order, seed)

real	mag[ARB]		# array of output magnitudes
int	nstars			# number of stars
real	alpha, mstar		# Schecter luminosity function parameters
real	minmag, maxmag		# minimum and maximum magnitude values
real	mzero			# zero point between relative and absolute mags.
int	nsample			# number of points in the sampling function
int	order			# order of the spline fit
long	seed			# value of the seed

int	i, ier
pointer	sp, m, iprob, w, cv, asi
real	dmag, magval, temp, imin, imax
real	cveval(), asigrl(), urand()

begin
	# Allocate space for fitting.
	call smark (sp)
	call salloc (m, nsample, TY_REAL)
	call salloc (iprob, nsample, TY_REAL)
	call salloc (w, nsample, TY_REAL)

	# Sample the luminosity function.
	magval = minmag - mzero
	dmag = (maxmag - minmag) / (nsample - 1)
	do i = 1, nsample {
	    Memr[m+i-1] = magval
	    temp = 0.4 * (mstar - magval)
	    Memr[iprob+i-1] = 10.0 ** ((alpha + 1) * temp) *
	        exp (- 10.0 ** temp)
	    magval = magval + dmag
	}

	# Integrate the sampling function.
	call asiinit (asi, II_SPLINE3)
	call asifit (asi, Memr[iprob], nsample)
	Memr[iprob] = 0.0
	do i = 2, nsample
	    Memr[iprob+i-1] = Memr[iprob+i-2] + asigrl (asi, real (i-1),
	        real(i))
	call alimr (Memr[iprob],nsample, imin, imax)
	call amapr (Memr[iprob], Memr[iprob], nsample, imin, imax, 0.0, 1.0)
	call asifree (asi)

	# Fit the inverse of the integral of the probability function.
	call cvinit (cv, SPLINE3, order, 0.0, 1.0)
	call cvfit (cv, Memr[iprob], Memr[m], Memr[w], nsample, WTS_UNIFORM,
	    ier)
	if (ier == OK) {
	    do i = 1, nstars
	        mag[i] = cveval (cv, urand (seed)) + mzero
	} else {
	    call printf ("Error fitting the Schecter luminosity function.\n")
	    call amovkr ((minmag + maxmag) / 2.0, mag, nstars)
	}
	call cvfree (cv)

	# Free space.
	call sfree (sp)
end


# ST_LFSAMPLE -- Compute the luminosity function using a user supplied
# function.

procedure st_lfsample (smag, mprob, nlf, mag, nstars, minmag, maxmag, nsample,
	order, seed)

real	smag[ARB]		# input array of magnitudes
real	mprob[ARB]		# input array of relative probabilities
int	nlf			# number of input points
real	mag[ARB]		# output magnitude array
int	nstars			# number of stars
real	minmag, maxmag		# minimum and maximum magnitude values
int	nsample			# number of sample points
int	order			# order of the spline fit
long	seed			# value of the seed

int	npts, i, ier
pointer	sp, m, w, iprob, cv, asi
real	mval, dm, sfmin, sfmax, imin, imax
real	cveval(), asigrl(), urand()

begin
	# Allocate space for fitting.
	npts = max (nlf, nsample)
	call smark (sp)
	call salloc (m, nsample, TY_REAL)
	call salloc (iprob, nsample, TY_REAL)
	call salloc (w, npts, TY_REAL)

	# Smooth the relative probability function.
	call alimr (smag, nlf, sfmin, sfmax)
	call cvinit (cv, SPLINE3, max (1, nlf / 4), sfmin, sfmax)
	call cvfit (cv, smag, mprob, Memr[w], nlf, WTS_UNIFORM, ier) 

	# Evaluate the smoothed function at equal intervals in r.
	if (ier == OK) {
	    mval = max (minmag, sfmin)
	    dm = (min (maxmag, sfmax) - mval) / (nsample - 1)
	    do i = 1, nsample {
		Memr[m+i-1] = mval
	        Memr[iprob+i-1] = cveval (cv, mval)
	        mval = mval + dm
	    }
	    call cvfree (cv)
	} else {
	    call printf ("Error smoothing user luminosity function.\n")
	    call amovkr ((minmag + maxmag) / 2.0, mag, nstars)
	    call cvfree (cv)
	    call sfree (sp)
	}

	# Evaluate the integral.
	call asiinit (asi, II_SPLINE3)
	call asifit (asi, Memr[iprob], nsample)
	Memr[iprob] = 0.0
	do i = 2, nsample
	    Memr[iprob+i-1] = Memr[iprob+i-2] + asigrl (asi, real(i-1), real(i))
	call alimr (Memr[iprob], nsample, imin, imax)
	call amapr (Memr[iprob], Memr[iprob], nsample, imin, imax, 0.0, 1.0)
	call asifree (asi)

	# Fit the inverse of the integral of the probability function.
	call cvinit (cv, SPLINE3, order, 0.0, 1.0)
	call cvfit (cv, Memr[iprob], Memr[m], Memr[w], nsample, WTS_UNIFORM,
	    ier)

	# Sample the computed function.
	if (ier == OK) {
	    do i = 1, nstars
	        mag[i] = cveval (cv, urand (seed))
	} else {
	    call printf ("Error computing the user luminosity function.\n")
	    call amovkr ((minmag + maxmag) / 2.0, mag, nstars)
	}
	call cvfree (cv)

	# Free space.
	call sfree (sp)
end
