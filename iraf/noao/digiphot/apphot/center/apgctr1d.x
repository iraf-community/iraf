include <math/nlfit.h>
include "../lib/center.h"

define	NPARS	4		# the total number of parameters
define	NAPARS	3		# the total number of active parameters
define	TOL	0.001		# the tolerance for convergence

# AP_GCTR1D -- Procedure to compute the x and y centers from the 1D marginal
# distributions using 1D Gaussian fits. Three parameters are fit for each
# marginal, the amplitude, the center of the Gaussian function itself
# and a constant background value. The sigma is set by the user and is
# assumed to be fixed.

int procedure ap_gctr1d (ctrpix, nx, ny, sigma, maxiter, xc, yc, xerr, yerr)

real	ctrpix[nx, ny]		# data subarray
size_t	nx, ny			# dimensions of data subarray
real	sigma			# sigma of PSF
int	maxiter			# maximum number of iterations
real	xc, yc			# computed centers
real	xerr, yerr		# estimate of centering error

size_t	sz_val0, sz_val1
extern	cgauss1d, cdgauss1d
long	i, minel, maxel
int	xier, yier
size_t	npts, npar
pointer	sp, x, xm, ym, w, fit, list, nl
real	chisqr, variance, p[NPARS], dp[NPARS]
pointer	locpr()

begin
	# Check the number of points.
	if (nx < NAPARS || ny < NAPARS)
	    return (AP_CTR_NTOO_SMALL)
	npts = max (nx, ny)

	call smark (sp)
	sz_val0 = NAPARS
	call salloc (list, sz_val0, TY_LONG)
	sz_val0 = nx
	call salloc (xm, sz_val0, TY_REAL)
	sz_val0 = ny
	call salloc (ym, sz_val0, TY_REAL)
	sz_val0 = npts
	call salloc (x, sz_val0, TY_REAL)
	call salloc (w, sz_val0, TY_REAL)
	call salloc (fit, sz_val0, TY_REAL)
	do i = 1, npts
	    Memr[x+i-1] = i

	# Compute the marginal distributions.
	call ap_mkmarg (ctrpix, Memr[xm], Memr[ym], nx, ny)
	call adivkr (Memr[xm], real (nx), Memr[xm], nx)
	call adivkr (Memr[ym], real (ny), Memr[ym], ny)

	# Specify which parameters are to be fit.
	Meml[list] = 1
	Meml[list+1] = 2
	Meml[list+2] = 4

	# Initialize the x fit parameters.
	call ap_alimr (Memr[xm], nx, p[4], p[1], minel, maxel)
	p[1] = p[1] - p[4]
	p[2] = maxel
	p[3] = sigma ** 2

	# Compute the x center and error.
	sz_val0 = NPARS
	sz_val1 = NAPARS
	call nlinitr (nl, locpr (cgauss1d), locpr (cdgauss1d), p, dp, sz_val0,
		      Meml[list], sz_val1, TOL, maxiter)
	call nlfitr (nl, Memr[x], Memr[xm], Memr[w], nx, 1, WTS_UNIFORM, xier)
	call nlvectorr (nl, Memr[x], Memr[fit], nx, 1)
	call nlpgetr (nl, p, npar)
	call nlerrorsr (nl, Memr[xm], Memr[fit], Memr[w], nx, variance,
	    chisqr, dp)
	call nlfreer (nl)
	xc = p[2]
	xerr = dp[2] / sqrt (real (nx))
	if (xerr > real (nx))
	    xerr = INDEFR

	# Initialize the y fit parameters.
	call ap_alimr (Memr[ym], ny, p[4], p[1], minel, maxel)
	p[1] = p[1] - p[4]
	p[2] = maxel
	p[3] = sigma ** 2

	# Fit the y marginal.
	sz_val0 = NPARS
	sz_val1 = NAPARS
	call nlinitr (nl, locpr (cgauss1d), locpr (cdgauss1d), p, dp, sz_val0,
		      Meml[list], sz_val1, TOL, maxiter)
	call nlfitr (nl, Memr[x], Memr[ym], Memr[w], ny, 1, WTS_UNIFORM, yier)
	call nlvectorr (nl, Memr[x], Memr[fit], ny, 1)
	call nlpgetr (nl, p, npar)
	call nlerrorsr (nl, Memr[ym], Memr[fit], Memr[w], ny, variance,
	    chisqr, dp)
	call nlfreer (nl)
	yc = p[2]
	yerr = dp[2] / sqrt (real (ny))
	if (yerr > real (ny))
	    yerr = INDEFR

	# Return the appropriate error code.
	call sfree (sp)
	if (xier == NO_DEG_FREEDOM || yier == NO_DEG_FREEDOM)
	    return (AP_CTR_NTOO_SMALL)
	else if (xier == SINGULAR || yier == SINGULAR)
	    return (AP_CTR_SINGULAR)
	else if (xier == NOT_DONE || yier == NOT_DONE)
	    return (AP_CTR_NOCONVERGE)
	else
	    return (AP_OK)
end
