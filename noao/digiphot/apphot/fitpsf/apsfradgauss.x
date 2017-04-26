include <math/nlfit.h>
include "../lib/noise.h"
include "../lib/fitpsf.h"

define	NPARAMETERS	5
define	TOL		0.001

# APSFRADGAUSS -- Fit a radial Gaussian function to the data.

int procedure apsfradgauss (ctrpix, nx, ny, emission, fwhmpsf, datamin,
	datamax, noise, gain, sigma, maxiter, k2, nreject, par, perr, npar)

real	ctrpix[nx, ny]		# object to be centered
int	nx, ny			# dimensions of subarray
int	emission		# emission or absorption version
real	fwhmpsf			# full width half max of the psf
real	datamin			# minimum good data value
real	datamax			# maximum good data value
int	noise			# noise model to be used
real	gain			# the gain in the data
real	sigma			# sigma of constant noise term
int	maxiter			# maximum number of iterations
real	k2			# k-sigma rejection criterion
int	nreject			# maximum number of rejection cycles
real	par[ARB]		# parameters
real	perr[ARB]		# errors in parameters
int	npar			# number of parameters

extern	gaussr, dgaussr
int	i, j, npts, list, imin, imax, fier
pointer	sp, x, w, zfit, nl, ptr
real	sumw, dummy, chisqr, locut, hicut
int	locpr(), apreject()
real	asumr(), apwssqr()

begin
	# Initialize.
	npts = nx * ny
	if (npts < NPARAMETERS)
	    return (AP_NPSF_TOO_SMALL)

	call smark (sp)
	call salloc (x, 2 * npts, TY_REAL)
	call salloc (w, npts, TY_REAL)
	call salloc (zfit, npts, TY_REAL)
	call salloc (list, NPARAMETERS, TY_INT)

	# Define the active parameters.
	do i = 1, NPARAMETERS
	    Memi[list+i-1] = i

	# Set variables array.
	ptr = x
	do j = 1, ny {
	    do i = 1, nx {
		Memr[ptr] = i
		Memr[ptr+1] = j
		ptr = ptr + 2
	    }
	}

	# Define the weight array.
	switch (noise) {
	case AP_NCONSTANT:
	    call amovkr (1.0, Memr[w], npts)
	case AP_NPOISSON:
	    call amaxkr (ctrpix, 0.0, Memr[w], npts)
	    if (gain  > 0.0)
		call adivkr (Memr[w], gain, Memr[w], npts)
	    if (! IS_INDEFR(sigma))
	        call aaddkr (Memr[w], sigma ** 2, Memr[w], npts)
	    call apreciprocal (Memr[w], Memr[w], npts, 1.0)
	default:
	    call amovkr (1.0, Memr[w], npts)
	}

	# Make an initial guess at the parameters.
	if (emission == YES)
	    call ap_wlimr (ctrpix, Memr[w], npts, datamin, datamax,
	        par[5], par[1], imin, imax)
	else
	    call ap_wlimr (ctrpix, Memr[w], npts, datamin, datamax,
	        par[1], par[5], imax, imin)
	par[1] = par[1] - par[5]
	if (mod (imax, nx) == 0)
	    imin = imax / nx
	else
	    imin = imax / nx + 1
	par[3] = imin
	imin = imax - (imin - 1) * nx
	par[2] = imin
	par[4] = (fwhmpsf ** 2 / 4.0)

	# Fit the function and the errors.
	call nlinitr (nl, locpr (gaussr), locpr (dgaussr), par, perr,
	    NPARAMETERS, Memi[list], NPARAMETERS, TOL, maxiter)
	call nlfitr (nl, Memr[x], ctrpix, Memr[w], npts, 2, WTS_USER, fier)

	# Perform the rejection cycle.
	if ((nreject > 0) && (k2 > 0.0)) {
	    do i = 1, nreject {
		call nlvectorr (nl, Memr[x], Memr[zfit], npts, 2)
		call asubr (ctrpix, Memr[zfit], Memr[zfit], npts)
		chisqr = apwssqr (Memr[zfit], Memr[w], npts)
		sumw = asumr (Memr[w], npts)
		if (sumw <= 0.0)
		    break
		else if (chisqr <= 0.0)
		    break
		else
		    chisqr = sqrt (chisqr / sumw)
		locut = - k2 * chisqr
		hicut = k2 * chisqr
		if (apreject (Memr[zfit], Memr[w], npts, locut, hicut) == 0) 
		    break
		call nlpgetr (nl, par, npar)
		call nlfreer (nl)
		call nlinitr (nl, locpr (gaussr), locpr (dgaussr), par, perr,
		    NPARAMETERS, Memi[list], NPARAMETERS, TOL, maxiter)
		call nlfitr (nl, Memr[x], ctrpix, Memr[w], npts, 2, WTS_USER,
		    fier)
	    }
	}

	# Get the parameters.
	call nlpgetr (nl, par, npar)
	par[4] = sqrt (abs(par[4]))

	# Get the errors.
	call nlvectorr (nl, Memr[x], Memr[zfit], npts, 2)
	call nlerrorsr (nl, ctrpix, Memr[zfit], Memr[w], npts, dummy,
	    chisqr, perr)
	perr[4] = sqrt (abs(perr[4]))

	# Compute the mean errors in the parameters.
	dummy = 0.0
	do i = 1, npts {
	    if (Memr[w+i-1] > 0.0)
		dummy = dummy + 1.0
	}
	dummy = sqrt (dummy)
	if (dummy > 0.0)
	    call adivkr (perr, dummy, perr, npar)

	call nlfreer (nl)

	call sfree (sp)

	# Return the appropriate error code.
	if (fier == NO_DEG_FREEDOM) {
	    return (AP_NPSF_TOO_SMALL)
	} else if (fier == SINGULAR) {
	    return (AP_PSF_SINGULAR)
	} else if (fier == NOT_DONE) {
	    return (AP_PSF_NOCONVERGE)
	} else {
	    return (AP_OK)
	}
end


# APREJECT -- Reject points outside of the specified intensity limits by
# setting their weights to zero.

int procedure apreject (pix, w, npts, locut, hicut)

real	pix[ARB]		# data
real	w[ARB]			# weights
int	npts			# number of data points
real	locut, hicut		# data limits

int	i, nreject

begin
	nreject = 0
	do i = 1, npts {
	    if ((pix[i] < locut || pix[i] > hicut) && w[i] > 0.0) {
		w[i] = 0.0
		nreject = nreject + 1
	    }
	}
	return (nreject)
end
