include "../lib/nlfit.h"
include "../lib/noise.h"
include "../lib/fitpsf.h"

define	NPARAMETERS	5
define	TOL		0.001

# APSFRADGAUSS -- Procedure to fit a radial Gaussian to the data.

int procedure apsfradgauss (ctrpix, nx, ny, fwhmpsf, datamin, datamax, noise,
	gain, sigma, maxiter, k2, nreject, par, perr, npar)

real	ctrpix[nx, ny]		# object to be centered
int	nx, ny			# dimensions of subarray
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
int	i, list, npts, fier
pointer	sp, x, y, w, zfit, nl
real	sumw, chisqr, locut, hicut
int	apreject()
real	asumr(), apwssqr()

begin
	# Initialize.
	npts = nx * ny
	if (npts < NPARAMETERS)
	    return (AP_NPSF_TOO_SMALL)

	call smark (sp)
	call salloc (x, nx, TY_REAL)
	call salloc (y, ny, TY_REAL)
	call salloc (w, npts, TY_REAL)
	call salloc (list, NPARAMETERS, TY_INT)

	# Define the active parameters.
	do i = 1, NPARAMETERS
	    Memi[list+i-1] = i

	# Set up x and y arrays.
	do i = 1, nx
	    Memr[x+i-1] = i
	do i = 1, ny
	    Memr[y+i-1] = i

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
	call ap_wlimr (ctrpix, Memr[w], npts, datamin, datamax, par[5], par[1])
	par[1] = par[1] - par[5]
	par[2] = (1. + nx) / 2.
	par[3] = (1. + ny) / 2.
	par[4] = (fwhmpsf ** 2 / 4.0)

	# Fit the centers and errors.
	call nlinit (nl, gaussr, dgaussr, par, perr, NPARAMETERS, Memi[list],
	    NPARAMETERS, TOL, maxiter)
	call nlfit (nl, Memr[x], Memr[y], ctrpix, Memr[w], nx, ny, ny,
	    WTS_USER, fier)

	# Perform the rejection cycle.
	if (nreject > 0 && k2 > 0.0) {
	    call salloc (zfit, npts, TY_REAL)
	    do i = 1, nreject {
		call nlvector (nl, Memr[x], Memr[y], Memr[zfit], nx, ny, ny)
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
		call nlpget (nl, par, npar)
		call nlfree (nl)
		call nlinit (nl, gaussr, dgaussr, par, perr, NPARAMETERS,
		    Memi[list], NPARAMETERS, TOL, maxiter)
		call nlfit (nl, Memr[x], Memr[y], ctrpix, Memr[w], nx, ny, ny,
	    	    WTS_USER, fier)
	    }
	}

	# Get the parameters and their errors.
	call nlpget (nl, par, npar)
	call nlerrors (nl, WTS_UNIFORM, chisqr, perr)

	# Return the appropriate error code.
	call nlfree (nl)
	call sfree (sp)
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


# APREJECT -- Procedure to reject points outside of bounds

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
