include <mach.h>
include "../lib/apphotdef.h"
include "../lib/apphot.h"
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

int	i, niter, ier, fier, lowsnr
pointer	ctr, nse
real	cthreshold, datamin, datamax, owx, owy, xshift, yshift, med
int	apctrbuf(), ap_ctr1d(), ap_mctr1d(), ap_gctr1d(), ap_lgctr1d()
real	ap_csnratio(), amedr()

begin
	ctr = AP_PCENTER(ap)
	nse = AP_NOISE(ap)
	ier = AP_OK

	# Initialize.
	AP_CXCUR(ctr) = wx
	AP_CYCUR(ctr) = wy
	AP_OXINIT(ctr) = INDEFR
	AP_OYINIT(ctr) = INDEFR
	AP_XCENTER(ctr) = INDEFR
	AP_YCENTER(ctr) = INDEFR
	AP_OXCENTER(ctr) = INDEFR
	AP_OYCENTER(ctr) = INDEFR
	AP_XSHIFT(ctr) = 0.0
	AP_YSHIFT(ctr) = 0.0
	AP_OXSHIFT(ctr) = 0.0
	AP_OYSHIFT(ctr) = 0.0
	AP_XERR(ctr) = INDEFR
	AP_YERR(ctr) = INDEFR

	# Return input coordinates if centering is disabled.
	if (IS_INDEFR(wx) || IS_INDEFR(wy)) {
	    return (AP_CTR_NOAREA)
	} else if (AP_CENTERFUNCTION(ctr) == AP_NONE) {
	    AP_XCENTER(ctr) = wx
	    AP_YCENTER(ctr) = wy
	    switch (AP_WCSOUT(ap)) {
	    case WCS_WORLD, WCS_PHYSICAL:
		call ap_ltoo (ap, wx, wy, AP_OXINIT(ctr), AP_OYINIT(ctr), 1)
		call ap_ltoo (ap, wx, wy, AP_OXCENTER(ctr), AP_OYCENTER(ctr), 1)
	    case WCS_TV:
		call ap_ltov (im, wx, wy, AP_OXINIT(ctr), AP_OYINIT(ctr), 1)
		call ap_ltov (im, wx, wy, AP_OXCENTER(ctr), AP_OYCENTER(ctr), 1)
	    default:
		AP_OXINIT(ctr) = wx
		AP_OYINIT(ctr) = wy
		AP_OXCENTER(ctr) = wx
		AP_OYCENTER(ctr) = wy
	    }
	    return (AP_OK)
	}

	# Intialize.
	owx = wx
	owy = wy 
	niter = 0
	if (IS_INDEFR(AP_SKYSIGMA(nse)) || IS_INDEFR(AP_CTHRESHOLD(ctr)) ||
	    AP_CTHRESHOLD(ctr) <= 0.0)
	    cthreshold = 0.0
	else
	    cthreshold = AP_CTHRESHOLD(ctr) * AP_SKYSIGMA(nse) 

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
	        switch (AP_WCSOUT(ap)) {
	        case WCS_WORLD, WCS_PHYSICAL:
		    call ap_ltoo (ap, wx, wy, AP_OXINIT(ctr), AP_OYINIT(ctr), 1)
		    call ap_ltoo (ap, wx, wy, AP_OXCENTER(ctr),
		        AP_OYCENTER(ctr), 1)
	        case WCS_TV:
		    call ap_ltov (im, wx, wy, AP_OXINIT(ctr), AP_OYINIT(ctr), 1)
		    call ap_ltov (im, wx, wy, AP_OXCENTER(ctr),
		        AP_OYCENTER(ctr), 1)
	        default:
		    AP_OXINIT(ctr) = wx
		    AP_OYINIT(ctr) = wy
		    AP_OXCENTER(ctr) = wx
		    AP_OYCENTER(ctr) = wy
	        }
	    	AP_OXSHIFT(ctr) = 0.0
	    	AP_OYSHIFT(ctr) = 0.0
	    	return (ier)
	    }

	    # Clean the subraster.
	    if (AP_CLEAN(ctr) == YES)
	        call apclean (ap, Memr[AP_CTRPIX(ctr)], AP_CNX(ctr),
		    AP_CNY(ctr), AP_CXC(ctr), AP_CYC(ctr))

	    # Compute the datalimits.
	    if (cthreshold <= 0.0)
	        call alimr (Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) * AP_CNY(ctr),
		    datamin, datamax)
	    else {
		datamin = MAX_REAL
		datamax = -MAX_REAL
		do i = 1, AP_CNY(ctr) {
		    med = amedr (Memr[AP_CTRPIX(ctr)+(i-1)*AP_CNX(ctr)],
		        AP_CNX(ctr))
		    if (med < datamin)
			datamin = med
		    if (med > datamax)
			datamax = med
		}
	    }

	    # Apply threshold and check for positive or negative features.
	    if (AP_POSITIVE(ap) == YES) {
		call apsetr (ap, CDATALIMIT, datamin)
		call asubkr (Memr[AP_CTRPIX(ctr)], datamin +
		    cthreshold, Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) *
		    AP_CNY(ctr))
		call amaxkr (Memr[AP_CTRPIX(ctr)], 0.0, Memr[AP_CTRPIX(ctr)],
		    AP_CNX(ctr) * AP_CNY(ctr))
	    } else {
		call apsetr (ap, CDATALIMIT, datamax)
		call anegr (Memr[AP_CTRPIX(ctr)], Memr[AP_CTRPIX(ctr)],
		    AP_CNX(ctr) * AP_CNY(ctr))
		call aaddkr (Memr[AP_CTRPIX(ctr)], datamax -
		    cthreshold, Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) *
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
	    switch (AP_CENTERFUNCTION(ctr)) {

	    case AP_CENTROID1D:
		if (AP_CTHRESHOLD(ctr) <= 0.0) {
	            fier = ap_mctr1d (Memr[AP_CTRPIX(ctr)], AP_CNX(ctr),
		        AP_CNY(ctr), AP_EPADU(nse), AP_XCENTER(ctr),
			AP_YCENTER(ctr), AP_XERR(ctr), AP_YERR(ctr))
		    if (IS_INDEFR (AP_XERR(ctr)))
			AP_XCENTER(ctr) = AP_CXC(ctr)
		    if (IS_INDEFR (AP_YERR(ctr)))
			AP_YCENTER(ctr) = AP_CYC(ctr)
		} else {
	            fier = ap_ctr1d (Memr[AP_CTRPIX(ctr)], AP_CNX(ctr),
		        AP_CNY(ctr), AP_EPADU(nse), AP_XCENTER(ctr),
			AP_YCENTER(ctr), AP_XERR(ctr), AP_YERR(ctr))
		    if (IS_INDEFR (AP_XERR(ctr)))
			AP_XCENTER(ctr) = AP_CXC(ctr)
		    if (IS_INDEFR (AP_YERR(ctr)))
			AP_YCENTER(ctr) = AP_CYC(ctr)
		}

	    case AP_GAUSS1D:

	        fier = ap_gctr1d (Memr[AP_CTRPIX(ctr)], AP_CNX(ctr),
		    AP_CNY(ctr), CONVERT * AP_FWHMPSF(ap) * AP_SCALE(ap),
		    AP_CMAXITER(ctr), AP_XCENTER(ctr), AP_YCENTER(ctr),
		    AP_XERR(ctr), AP_YERR(ctr))

	    case AP_OFILT1D:

	        fier = ap_lgctr1d (Memr[AP_CTRPIX(ctr)], AP_CNX(ctr),
		    AP_CNY(ctr), AP_CXC(ctr), AP_CYC(ctr), CONVERT *
		    AP_FWHMPSF(ap) * AP_SCALE(ap), AP_CMAXITER(ctr),
		    AP_EPADU(nse), AP_SKYSIGMA(nse), AP_XCENTER(ctr),
		    AP_YCENTER(ctr), AP_XERR(ctr), AP_YERR(ctr))

	    default:
		# do nothing gracefully
	    }

	    # Confine the next x center to the data box.
	    AP_XCENTER(ctr) = max (0.5, min (AP_CNX(ctr) + 0.5,
	        AP_XCENTER(ctr)))
	    xshift = AP_XCENTER(ctr) - AP_CXC(ctr)
	    AP_XCENTER(ctr) = xshift + owx
	    AP_XSHIFT(ctr) = AP_XCENTER(ctr) - wx

	    # Confine the next y center to the data box.
	    AP_YCENTER(ctr) = max (0.5, min (AP_CNY(ctr) + 0.5,
	        AP_YCENTER(ctr)))
	    yshift = AP_YCENTER(ctr) - AP_CYC(ctr)
	    AP_YCENTER(ctr) = yshift + owy
	    AP_YSHIFT(ctr) = AP_YCENTER(ctr) - wy

	    switch (AP_WCSOUT(ap)) {
	    case WCS_PHYSICAL, WCS_WORLD:
		call ap_ltoo (ap, AP_XCENTER(ctr), AP_YCENTER(ctr),
		    AP_OXCENTER(ctr), AP_OYCENTER(ctr), 1)
		call ap_ltoo (ap, AP_XCENTER(ctr) - AP_XSHIFT(ctr),
		    AP_YCENTER(ctr) - AP_YSHIFT(ctr), AP_OXINIT(ctr),
		    AP_OYINIT(ctr), 1)
		AP_OXSHIFT(ctr) = AP_OXCENTER(ctr) - AP_OXINIT(ctr)
		AP_OYSHIFT(ctr) = AP_OYCENTER(ctr) - AP_OYINIT(ctr)
	    case WCS_TV:
		call ap_ltov (im, AP_XCENTER(ctr), AP_YCENTER(ctr),
		    AP_OXCENTER(ctr), AP_OYCENTER(ctr), 1)
		call ap_ltov (im, AP_XCENTER(ctr) - AP_XSHIFT(ctr),
		    AP_YCENTER(ctr) - AP_YSHIFT(ctr), AP_OXINIT(ctr),
		    AP_OYINIT(ctr), 1)
		AP_OXSHIFT(ctr) = AP_OXCENTER(ctr) - AP_OXINIT(ctr)
		AP_OYSHIFT(ctr) = AP_OYCENTER(ctr) - AP_OYINIT(ctr)
	    default:
		AP_OXINIT(ctr) = AP_XCENTER(ctr) - AP_XSHIFT(ctr)
		AP_OYINIT(ctr) = AP_YCENTER(ctr) - AP_YSHIFT(ctr)
		AP_OXCENTER(ctr) = AP_XCENTER(ctr)
		AP_OYCENTER(ctr) = AP_YCENTER(ctr)
		AP_OXSHIFT(ctr) = AP_XSHIFT(ctr)
		AP_OYSHIFT(ctr) = AP_YSHIFT(ctr)
		
	    }

	    # Setup for next iteration.
	    niter = niter + 1
	    owx = AP_XCENTER(ctr)
	    owy = AP_YCENTER(ctr)

	} until ((fier != AP_OK && fier != AP_CTR_NOCONVERGE) ||
	    (niter >= AP_CMAXITER(ctr)) || (abs (xshift) < 1.0 &&
	    abs (yshift) < 1.0))

	# Return appropriate error code.
	if (fier != AP_OK) {
	    AP_XCENTER(ctr) = wx
	    AP_YCENTER(ctr) = wy
	    AP_XSHIFT(ctr) = 0.0
	    AP_YSHIFT(ctr) = 0.0
	    AP_XERR(ctr) = INDEFR
	    AP_YERR(ctr) = INDEFR
	    switch (AP_WCSOUT(ap)) {
	    case WCS_WORLD, WCS_PHYSICAL:
		call ap_ltoo (ap, wx, wy, AP_OXINIT(ctr), AP_OYINIT(ctr), 1)
		call ap_ltoo (ap, wx, wy, AP_OXCENTER(ctr), AP_OYCENTER(ctr), 1)
	    case WCS_TV:
		call ap_ltov (im, wx, wy, AP_OXINIT(ctr), AP_OYINIT(ctr), 1)
		call ap_ltov (im, wx, wy, AP_OXCENTER(ctr), AP_OYCENTER(ctr), 1)
	    default:
		AP_OXINIT(ctr) = wx
		AP_OYINIT(ctr) = wy
		AP_OXCENTER(ctr) = wx
		AP_OYCENTER(ctr) = wy
	    }
	    AP_OXSHIFT(ctr) = 0.0
	    AP_OYSHIFT(ctr) = 0.0
	    return (fier)
	} else if (ier == AP_CTR_BADDATA) {
	    return (AP_CTR_BADDATA)
	} else if (lowsnr == YES) {
	    return (AP_CTR_LOWSNRATIO)
	} else if (abs (AP_XSHIFT(ctr)) > (AP_MAXSHIFT(ctr) * AP_SCALE(ap))) {
	    return (AP_CTR_BADSHIFT)
	} else if (abs (AP_YSHIFT(ctr)) > (AP_MAXSHIFT(ctr) * AP_SCALE(ap))) {
	    return (AP_CTR_BADSHIFT)
	} else if (ier == AP_CTR_OUTOFBOUNDS) {
	    return (AP_CTR_OUTOFBOUNDS)
	} else {
	    return (AP_OK)
	}
end
