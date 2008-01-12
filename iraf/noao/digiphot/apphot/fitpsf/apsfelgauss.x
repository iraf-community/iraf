include <math.h>
include <math/nlfit.h>
include "../lib/noise.h"
include "../lib/fitpsf.h"

define	NPARAMETERS	7
define	TOL		0.001

# APSFELGAUSS -- Procedure to fit an elliptical Gaussian function to the
# stellar data.

int procedure apsfelgauss (ctrpix, nx, ny, emission, fwhmpsf, datamin,
	datamax, noise, gain, sigma, maxiter, k2, nreject, par, perr, npar)

real	ctrpix[nx,ny]		# object to be centered
int	nx, ny			# dimensions of subarray
int	emission		# emission or absorption object
real	fwhmpsf			# full width half max of the psf
real	datamin			# minimum good data value
real	datamax			# maximum good data value
int	noise			# noise model
real	gain			# the gain parameter
real	sigma			# constant term to noise
int	maxiter			# maximum number of iterations
real	k2			# k-sigma rejection criterion
int	nreject			# maximum number of rejection cycles
real	par[ARB]		# parameters
real	perr[ARB]		# errors in parameters
int	npar			# number of parameters

extern	elgauss, delgauss
int	i, j, npts, fier, imin,imax
pointer	sp, x, w, list, zfit, nl, ptr
real	sumw, dummy, chisqr, locut, hicut, ptemp
int	locpr(), apreject()
real	asumr(), apwssqr()

begin
	# Initialize.
	npts = nx * ny
	if (npts < NPARAMETERS)
	    return (AP_NPSF_TOO_SMALL)

	# Allocate working space.
	call smark (sp)
	call salloc (x, 2 * npts, TY_REAL)
	call salloc (w, npts, TY_REAL)
	call salloc (zfit, npts, TY_REAL)
	call salloc (list, NPARAMETERS, TY_INT)

	# Define the active parameters.
	do i = 1, NPARAMETERS
	    Memi[list+i-1] = i

	# Set up the varaibles array.
	ptr = x
	do j = 1, ny {
	    do i = 1, nx {
		Memr[ptr] = i
		Memr[ptr+1] = j
		ptr = ptr + 2
	    }
	}

	# Set up the weight array.
	switch (noise) {
	case AP_NCONSTANT:
	    call amovkr (1.0, Memr[w], npts)
	case AP_NPOISSON:
	    call amaxkr (ctrpix, 0.0, Memr[w], npts)
	    if (gain > 0.0)
		call adivkr (Memr[w], gain, Memr[w], npts)
	    if (! IS_INDEFR(sigma))
		call aaddkr (Memr[w], sigma ** 2, Memr[w], npts)
	    call apreciprocal (Memr[w], Memr[w], npts, 1.0)
	default:
	    call amovkr (1.0, Memr[w], npts)
	}

	# Make an initial guess at the fitting parameters.
	if (emission == YES)
	    call ap_wlimr (ctrpix, Memr[w], nx * ny, datamin, datamax,
	        par[7], par[1], imin, imax)
	else
	    call ap_wlimr (ctrpix, Memr[w], nx * ny, datamin, datamax,
	        par[7], par[1], imax, imin)
	par[1] = par[1] - par[7]
	if (mod (imax, nx) == 0)
	    imin = imax / nx
	else
	    imin = imax / nx + 1
	par[3] = imin
	imin = imax - (imin - 1) * nx
	par[2] = imin
	par[4] = (fwhmpsf ** 2 / 4.0)
	par[5] = (fwhmpsf ** 2 / 4.0)
	par[6] = 0.0

	# Get the centers and errors.
	call nlinitr (nl, locpr (elgauss), locpr (delgauss), par, perr,
	    NPARAMETERS, Memi[list], NPARAMETERS, TOL, maxiter)
	call nlfitr (nl, Memr[x], ctrpix, Memr[w], npts, 2, WTS_USER, fier)

	# Perform the rejection cycle.
	if (nreject > 0 && k2 > 0.0) {
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
		call nlinitr (nl, locpr (elgauss), locpr (delgauss), par,
		    perr, NPARAMETERS, Memi[list], NPARAMETERS, TOL, maxiter)
		call nlfitr (nl, Memr[x], ctrpix, Memr[w], npts, 2, WTS_USER,
		    fier)
	    }
	}

	# Fetch the parameters.
	call nlvectorr (nl, Memr[x], Memr[zfit], npts, 2)
	call nlpgetr (nl, par, npar)
	par[4] = sqrt (abs(par[4]))
	par[5] = sqrt (abs(par[5]))

	# Fetch the errors.
	call nlerrorsr (nl, ctrpix, Memr[zfit], Memr[w], npts, dummy,
	    chisqr, perr)
	perr[4] = sqrt (perr[4])
	perr[5] = sqrt (perr[5])

	# Compute the mean errors.
	dummy = 0.0
	do i = 1, npts {
	    if (Memr[w+i-1] > 0.0)
		dummy = dummy + 1.0
	}
	dummy = sqrt (dummy)
	if (dummy > 0.0)
	    call adivkr (perr, dummy, perr, npar)

	# Transform the parameters.
	par[6] = mod (RADTODEG(par[6]), 360.0)
	if (par[6] < 0.0)
	    par[6] = 360.0 + par[6]
	if (par[6] > 90.0 && par[6] <= 270.0)
	    par[6] = par[6] - 180.0
	else if (par[6] > 270.0 && par[6] <= 360.0)
	    par[6] = par[6] - 360.0
	if (par[5] > par[4]) {
	    if (par[6] > 0.0)
		par[6] = par[6] - 90.0
	    else if (par[6] < 0.0)
		par[6] = par[6] + 90.0
	    ptemp = par[4]
	    par[4] = par[5]
	    par[5] = ptemp
	}
	perr[6] = mod (RADTODEG(perr[6]), 360.0)

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
