include <mach.h>
include "../lib/apphotdef.h"
include "../lib/noisedef.h"
include "../lib/centerdef.h"
include "../lib/center.h"

define	CONVERT		.424660900	# conversion from fwhmpsf to sigma

# APFITCENTER -- Procedure to fit the centers using either 1) the intensity
# weighted mean of the marginal distributions, 2) a Gaussian fit to the
# marginals assuming a fixed sigma or 3) a simplified version of the optimal
# filtering method using a Gaussian of fixed sigma to model the profile.

int procedure apfitcenter (ap, im, wx, wy)

pointer	ap		# pointer to the apphot structure
pointer	im		# pointer to the IRAF image
real	wx, wy		# object coordinates

int	niter, ier, fier, lowsnr
pointer	ctr, nse
real	datamin, datamax, owx, owy, xshift, yshift
int	apctrbuf(), ap_ctr1d(), ap_mctr1d(), ap_gctr1d(), ap_lgctr1d()
real	ap_csnratio()

begin
	ctr = AP_PCENTER(ap)
	nse = AP_NOISE(ap)
	ier = AP_OK

	# Initialize.
	AP_CXCUR(ctr) = wx
	AP_CYCUR(ctr) = wy
	AP_XCENTER(ctr) = INDEFR
	AP_YCENTER(ctr) = INDEFR
	AP_XSHIFT(ctr) = 0.0
	AP_YSHIFT(ctr) = 0.0
	AP_XERR(ctr) = INDEFR
	AP_YERR(ctr) = INDEFR

	# Return input coordinates if centering is disabled.
	if (IS_INDEFR(wx) || IS_INDEFR(wy))
	    return (AP_CTR_NOAREA)
	else if (AP_CENTERFUNCTION(ctr) == AP_NONE) {
	    AP_XCENTER(ctr) = wx
	    AP_YCENTER(ctr) = wy
	    return (AP_OK)
	}

	# Choose which centering functions is to be used.
	switch (AP_CENTERFUNCTION(ctr)) {

	case AP_CENTROID1D:

	    # Intialize.
	    owx = wx
	    owy = wy 
	    niter = 0

	    repeat {

		# Set initial cursor position.
		AP_CXCUR(ctr) = owx
	        AP_CYCUR(ctr) = owy

	        # Get centering pixels.
	        ier = apctrbuf (ap, im, owx, owy)
		if (ier == AP_CTR_NOAREA) {
	    	    AP_XCENTER(ctr) = wx
	    	    AP_YCENTER(ctr) = wy
	    	    AP_XSHIFT(ctr) = 0.0
	    	    AP_YSHIFT(ctr) = 0.0
	    	    AP_XERR(ctr) = INDEFR
	    	    AP_YERR(ctr) = INDEFR
	    	    return (ier)
		}

		# Clean the subraster.
		if (AP_CLEAN(ctr) == YES)
	    	    call apclean (ap, Memr[AP_CTRPIX(ctr)], AP_CNX(ctr),
		        AP_CNY(ctr), AP_CXC(ctr), AP_CYC(ctr))

		# Apply threshold and check for positive or negative features.
		if (AP_POSITIVE(ap) == YES) {
		    call apsetr (ap, CDATALIMIT, datamin)
		    call asubkr (Memr[AP_CTRPIX(ctr)], datamin +
		        AP_CTHRESHOLD(nse), Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) *
			AP_CNY(ctr))
		    call amaxkr (Memr[AP_CTRPIX(ctr)], 0.0,
			Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) * AP_CNY(ctr))
		} else {
		    call apsetr (ap, CDATALIMIT, datamax)
		    call anegr (Memr[AP_CTRPIX(ctr)], Memr[AP_CTRPIX(ctr)],
		        AP_CNX(ctr) * AP_CNY(ctr))
		    call aaddkr (Memr[AP_CTRPIX(ctr)], datamax -
		        AP_CTHRESHOLD(nse), Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) *
			AP_CNY(ctr))
		    call amaxkr (Memr[AP_CTRPIX(ctr)], 0.0,
			Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) * AP_CNY(ctr))
		}

		# Test signal to noise ratio.
		if (ap_csnratio (ap, Memr[AP_CTRPIX(ctr)], AP_CNX(ctr),
		    AP_CNY(ctr), 0.0) < AP_MINSNRATIO(ctr))
	    	    lowsnr = YES
		else
		    lowsnr = NO

	        # Compute the x and y centers.
		if (AP_CTHRESHOLD(nse) > 0.0) {
	            fier = ap_ctr1d (Memr[AP_CTRPIX(ctr)], AP_CNX(ctr),
		        AP_CNY(ctr), AP_XCENTER(ctr), AP_YCENTER(ctr),
			AP_XERR(ctr), AP_YERR(ctr))
		    if (IS_INDEFR (AP_XERR(ctr)))
			AP_XCENTER(ctr) = AP_CXC(ctr)
		    if (IS_INDEFR (AP_YERR(ctr)))
			AP_YCENTER(ctr) = AP_CYC(ctr)
		} else {
	            fier = ap_mctr1d (Memr[AP_CTRPIX(ctr)], AP_CNX(ctr),
		        AP_CNY(ctr), AP_XCENTER(ctr), AP_YCENTER(ctr),
			AP_XERR(ctr), AP_YERR(ctr))
		    if (IS_INDEFR (AP_XERR(ctr)))
			AP_XCENTER(ctr) = AP_CXC(ctr)
		    if (IS_INDEFR (AP_YERR(ctr)))
			AP_YCENTER(ctr) = AP_CYC(ctr)
		}

		xshift = AP_XCENTER(ctr) - AP_CXC(ctr)
		yshift = AP_YCENTER(ctr) - AP_CYC(ctr)
	        AP_XCENTER(ctr) = xshift + owx
	        AP_YCENTER(ctr) = yshift + owy
		AP_XSHIFT(ctr) = AP_XCENTER(ctr) - wx
		AP_YSHIFT(ctr) = AP_YCENTER(ctr) - wy

		# Setup for next iteration.
		niter = niter + 1
		owx = AP_XCENTER(ctr)
		owy = AP_YCENTER(ctr)

	    } until (niter >= AP_CMAXITER(ctr) || (abs (xshift) < 1.0 &&
	        abs (yshift) < 1.0))

	case AP_GAUSS1D:

	    # Get centering buffer of pixels.
	    ier = apctrbuf (ap, im, wx, wy)
	    if (ier == AP_CTR_NOAREA) {
	        AP_XCENTER(ctr) = wx
	    	AP_YCENTER(ctr) = wy
	    	AP_XSHIFT(ctr) = 0.0
	    	AP_YSHIFT(ctr) = 0.0
	    	AP_XERR(ctr) = INDEFR
	    	AP_YERR(ctr) = INDEFR
	        return (ier)
	    }

	    # Clean the subraster.
	    if (AP_CLEAN(ctr) == YES)
	        call apclean (ap, Memr[AP_CTRPIX(ctr)], AP_CNX(ctr),
		    AP_CNY(ctr), AP_CXC(ctr), AP_CYC(ctr))

	    # Apply threshold and check for positive or negative features.
	    if (AP_POSITIVE(ap) == YES) {
		call apsetr (ap, CDATALIMIT, datamin)
		call asubkr (Memr[AP_CTRPIX(ctr)], datamin + AP_CTHRESHOLD(nse),
		    Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) * AP_CNY(ctr))
		call amaxkr (Memr[AP_CTRPIX(ctr)], 0.0,
		    Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) * AP_CNY(ctr))
	    } else {
		call apsetr (ap, CDATALIMIT, datamax)
		call anegr (Memr[AP_CTRPIX(ctr)], Memr[AP_CTRPIX(ctr)],
		    AP_CNX(ctr) * AP_CNY(ctr))
		call aaddkr (Memr[AP_CTRPIX(ctr)], datamax - AP_CTHRESHOLD(nse),
		    Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) * AP_CNY(ctr))
		call amaxkr (Memr[AP_CTRPIX(ctr)], 0.0,
		    Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) * AP_CNY(ctr))
	    }

	    # Test the signal to noise ratio.
	    if (ap_csnratio (ap, Memr[AP_CTRPIX(ctr)], AP_CNX(ctr),
	        AP_CNY(ctr), 0.0) < AP_MINSNRATIO(ctr))
	    	lowsnr = YES
	    else
		lowsnr = NO

	    # Compute the x and y centers.
	    fier = ap_gctr1d (Memr[AP_CTRPIX(ctr)], AP_CNX(ctr), AP_CNY(ctr),
		CONVERT * AP_FWHMPSF(ap) * AP_SCALE(ap), AP_CMAXITER(ctr),
		AP_XCENTER(ctr), AP_YCENTER(ctr), AP_XERR(ctr), AP_YERR(ctr))
	    AP_XCENTER(ctr) = AP_XCENTER(ctr) + wx - AP_CXC(ctr)
	    AP_YCENTER(ctr) = AP_YCENTER(ctr) + wy - AP_CYC(ctr)
	    AP_XSHIFT(ctr) = AP_XCENTER(ctr) - wx
	    AP_YSHIFT(ctr) = AP_YCENTER(ctr) - wy

	case AP_OFILT1D:

	    # Get the centering buffer of pixels.
	    ier = apctrbuf (ap, im, wx, wy)
	    if (ier == AP_CTR_NOAREA) {
	        AP_XCENTER(ctr) = wx
	    	AP_YCENTER(ctr) = wy
	    	AP_XSHIFT(ctr) = 0.0
	    	AP_YSHIFT(ctr) = 0.0
	    	AP_XERR(ctr) = INDEFR
	    	AP_YERR(ctr) = INDEFR
	        return (ier)
	    }

	    # Clean the subraster.
	    if (AP_CLEAN(ctr) == YES)
	        call apclean (ap, Memr[AP_CTRPIX(ctr)], AP_CNX(ctr),
		    AP_CNY(ctr), AP_CXC(ctr), AP_CYC(ctr))

	    # Apply threshold and check for positive or negative features.
	    if (AP_POSITIVE(ap) == YES) {
		call apsetr (ap, CDATALIMIT, datamin)
		call asubkr (Memr[AP_CTRPIX(ctr)], datamin + AP_CTHRESHOLD(nse),
		    Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) * AP_CNY(ctr))
		call amaxkr (Memr[AP_CTRPIX(ctr)], 0.0,
		    Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) * AP_CNY(ctr))
	    } else {
		call apsetr (ap, CDATALIMIT, datamax)
		call anegr (Memr[AP_CTRPIX(ctr)], Memr[AP_CTRPIX(ctr)],
		    AP_CNX(ctr) * AP_CNY(ctr))
		call aaddkr (Memr[AP_CTRPIX(ctr)], datamax - AP_CTHRESHOLD(nse),
		    Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) * AP_CNY(ctr))
		call amaxkr (Memr[AP_CTRPIX(ctr)], 0.0,
		    Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) * AP_CNY(ctr))
	    }

	    # Test the signal to noise ratio.
	    if (ap_csnratio (ap, Memr[AP_CTRPIX(ctr)], AP_CNX(ctr),
	        AP_CNY(ctr), 0.0) < AP_MINSNRATIO(ctr))
		lowsnr = YES
	    else
		lowsnr = NO

	    # Compute the new centers.
	    fier = ap_lgctr1d (Memr[AP_CTRPIX(ctr)], AP_CNX(ctr), AP_CNY(ctr),
		AP_CXC(ctr), AP_CYC(ctr), CONVERT * AP_FWHMPSF(ap) *
		AP_SCALE(ap), AP_CMAXITER(ctr), AP_SKYSIGMA(nse),
		AP_XCENTER(ctr), AP_YCENTER(ctr), AP_XERR(ctr), AP_YERR(ctr))
	    AP_XCENTER(ctr) = AP_XCENTER(ctr) + wx - AP_CXC(ctr)
	    AP_YCENTER(ctr) = AP_YCENTER(ctr) + wy - AP_CYC(ctr)
	    AP_XSHIFT(ctr) = AP_XCENTER(ctr) - wx
	    AP_YSHIFT(ctr) = AP_YCENTER(ctr) - wy

	default:

	    # do nothing gracefully
        }

	# Return appropriate error code.
	if (ier == AP_CTR_BADDATA) {
	    return (AP_CTR_BADDATA)
	} else if (fier != AP_OK) {
	    if (fier == AP_CTR_NTOO_SMALL) {
	        AP_XCENTER(ctr) = wx
	        AP_YCENTER(ctr) = wy
	        AP_XSHIFT(ctr) = 0.0
	        AP_YSHIFT(ctr) = 0.0
	        AP_XERR(ctr) = INDEFR
	        AP_YERR(ctr) = INDEFR
	        return (AP_CTR_NTOO_SMALL)
	    } else
		return (fier)
	} else if (lowsnr == YES) {
	    return (AP_CTR_LOWSNRATIO)
	} else if (abs (AP_XSHIFT(ctr)) > (AP_MAXSHIFT(ctr) * AP_SCALE(ap))) {
	    return (AP_CTR_BADSHIFT)
	} else if (abs (AP_YSHIFT(ctr)) > (AP_MAXSHIFT(ctr) * AP_SCALE(ap))) {
	    return (AP_CTR_BADSHIFT)
	} else if (ier == AP_CTR_OUTOFBOUNDS) {
	    return (AP_CTR_OUTOFBOUNDS)
	} else
	    return (AP_OK)
end
