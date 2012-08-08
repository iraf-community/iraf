include <math.h>
include "../lib/center.h"

define	TOL	0.001		# tolerance for fitting algorithm

# AP_LGCTR1D -- Procedure to compute the center from the 1D marginal
# distributions using a simplified version of the optimal filtering
# technique and addopting a Gaussian model for the fit. The method
# is streamlined by replacing the Gaussian with a simple triangle
# following L.Goad.

int procedure ap_lgctr1d (ctrpix, nx, ny, cx, cy, sigma, maxiter, norm,
	skysigma, xc, yc, xerr, yerr)

real	ctrpix[nx, ny]		# object to be centered
int	nx, ny			# dimensions of subarray
real	cx, cy			# center in subraster coordinates
real	sigma			# sigma of PSF
int	maxiter			# maximum number of iterations
real	norm			# the normalization factor
real	skysigma		# standard deviation of the pixels
real	xc, yc			# computed centers
real	xerr, yerr		# first guess at errors

int	nxiter, nyiter
pointer	sp, xm, ym
real	ratio, constant

int	aptopt()
real	asumr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (xm, nx, TY_REAL)
	call salloc (ym, ny, TY_REAL)

	# Compute the marginal distributions.
	call ap_mkmarg (ctrpix, Memr[xm], Memr[ym], nx, ny)
	xerr = asumr (Memr[xm], nx)
	yerr = asumr (Memr[ym], ny)
	call adivkr (Memr[xm], real (nx), Memr[xm], nx)
	call adivkr (Memr[ym], real (ny), Memr[ym], ny)

	# Compute the x center and error.
	xc = cx
	nxiter = aptopt (Memr[xm], nx, xc, sigma, TOL, maxiter, YES)
	if (xerr <= 0.0)
	    xerr = INDEFR
	else {
	    if (IS_INDEFR(skysigma))
	        constant = 0.0
	    else
	        constant = 4.0 * SQRTOFPI * sigma * skysigma ** 2 
	    ratio = constant / xerr
	    xerr = sigma ** 2 / (xerr * norm)
	    xerr = sqrt (max (xerr, ratio * xerr))
	    if (xerr > real (nx))
		xerr = INDEFR
	}

	# Compute the y center and error.
	yc = cy
	nyiter = aptopt (Memr[ym], ny, yc, sigma, TOL, maxiter, YES)
	if (yerr <= 0.0)
	    yerr = INDEFR
	else {
	    if (IS_INDEFR(skysigma))
	        constant = 0.0
	    else
	        constant = 4.0 * SQRTOFPI * sigma * skysigma ** 2 
	    ratio = constant / yerr
	    yerr = sigma ** 2 / (yerr * norm)
	    yerr = sqrt (max (yerr, ratio * yerr))
	    if (yerr > real (ny))
		yerr = INDEFR
	}

	# Return appropriate error code.
	call sfree (sp)
	if (nxiter < 0 || nyiter < 0)
	    return (AP_CTR_SINGULAR)
	else if (nxiter > maxiter || nyiter > maxiter)
	    return (AP_CTR_NOCONVERGE)
	else
	    return (AP_OK)
end
