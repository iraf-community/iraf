include <mach.h>
include "../lib/apphotdef.h"
include "../lib/noisedef.h"
include "../lib/centerdef.h"
include "../lib/center.h"

define	CONVERT	.424660900	# conversion factor from fwhmpsf to sigma

# APREFITCENTER -- Procedure to refit the centers assuming that the appropriate
# pixel buffer is in memory. See apfitcenter for further information.

int procedure aprefitcenter (ap, ier)

pointer	ap		# pointer to the apphot structure
int	ier		# previous error code

int	fier
pointer	cen, nse
int	ap_ctr1d(), ap_mctr1d(), ap_gctr1d(), ap_lgctr1d()

begin
	cen = AP_PCENTER(ap)
	nse = AP_NOISE(ap)

	# Initialize
        AP_XCENTER(cen) = AP_CXCUR(cen)
	AP_YCENTER(cen) = AP_CYCUR(cen)
	AP_XSHIFT(cen) = 0.0
	AP_YSHIFT(cen) = 0.0
	AP_XERR(cen) = INDEFR
	AP_YERR(cen) = INDEFR

	# Return input coordinates if no center fitting.
	if (IS_INDEFR(AP_CXCUR(cen)) || IS_INDEFR(AP_CYCUR(cen)))
	    return (AP_CTR_NOAREA)
	else if (AP_CENTERFUNCTION(cen) == AP_NONE)
	    return (AP_OK)

	# Choose the centering algorithm.
	switch (AP_CENTERFUNCTION(cen)) {

	case AP_CENTROID1D:

	    # Compute the x and y centroids.
	    if (AP_CTHRESHOLD(cen) <= 0.0) {
	        fier = ap_mctr1d (Memr[AP_CTRPIX(cen)], AP_CNX(cen),
		    AP_CNY(cen), AP_EPADU(nse), AP_XCENTER(cen),
		    AP_YCENTER(cen), AP_XERR(cen), AP_YERR(cen))
		if (IS_INDEFR(AP_XERR(cen)))
		    AP_XCENTER(cen) = AP_CXC(cen)
		if (IS_INDEFR(AP_YERR(cen)))
		    AP_YCENTER(cen) = AP_CYC(cen)
	    } else {
	        fier = ap_ctr1d (Memr[AP_CTRPIX(cen)], AP_CNX(cen),
		    AP_CNY(cen), AP_EPADU(nse), AP_XCENTER(cen),
		    AP_YCENTER(cen), AP_XERR(cen), AP_YERR(cen))
		if (IS_INDEFR(AP_XERR(cen)))
		    AP_XCENTER(cen) = AP_CXC(cen)
		if (IS_INDEFR(AP_YERR(cen)))
		    AP_YCENTER(cen) = AP_CYC(cen)
	    }
	    AP_XCENTER(cen) = AP_XCENTER(cen) + AP_CXCUR(cen) - AP_CXC(cen)
	    AP_YCENTER(cen) = AP_YCENTER(cen) + AP_CYCUR(cen) - AP_CYC(cen)
	    AP_XSHIFT(cen) = AP_XCENTER(cen) - AP_CXCUR(cen)
	    AP_YSHIFT(cen) = AP_YCENTER(cen) - AP_CYCUR(cen)

	case AP_GAUSS1D:

	    # Compute the 1D Gaussian x and y centers.
	    fier = ap_gctr1d (Memr[AP_CTRPIX(cen)], AP_CNX(cen), AP_CNY(cen),
		CONVERT * AP_FWHMPSF(ap) * AP_SCALE(ap), AP_CMAXITER(cen),
		AP_XCENTER(cen), AP_YCENTER(cen), AP_XERR(cen), AP_YERR(cen))
	    AP_XCENTER(cen) = AP_XCENTER(cen) + AP_CXCUR(cen) - AP_CXC(cen)
	    AP_YCENTER(cen) = AP_YCENTER(cen) + AP_CYCUR(cen) - AP_CYC(cen)
	    AP_XSHIFT(cen) = AP_XCENTER(cen) - AP_CXCUR(cen)
	    AP_YSHIFT(cen) = AP_YCENTER(cen) - AP_CYCUR(cen)

	case AP_OFILT1D:

	    # Compute the Goad 1D x and y centers.
	    fier = ap_lgctr1d (Memr[AP_CTRPIX(cen)], AP_CNX(cen), AP_CNY(cen),
		 AP_CXC(cen), AP_CYC(cen), CONVERT * AP_FWHMPSF(ap) *
		 AP_SCALE(ap), AP_CMAXITER(cen), AP_EPADU(nse),
		 AP_SKYSIGMA(nse), AP_XCENTER(cen), AP_YCENTER(cen),
		 AP_XERR(cen), AP_YERR(cen))
	    AP_XCENTER(cen) = AP_XCENTER(cen) + AP_CXCUR(cen) - AP_CXC(cen)
	    AP_YCENTER(cen) = AP_YCENTER(cen) + AP_CYCUR(cen) - AP_CYC(cen)
	    AP_XSHIFT(cen) = AP_XCENTER(cen) - AP_CXCUR(cen)
	    AP_YSHIFT(cen) = AP_YCENTER(cen) - AP_CYCUR(cen)

	default:

	    # do nothing gracefully
        }

	# Return appropriate error code.
	if (fier != AP_OK) {
	    AP_XCENTER(cen) = AP_CXCUR(cen)
	    AP_YCENTER(cen) = AP_CYCUR(cen)
	    AP_XSHIFT(cen) = 0.0
	    AP_YSHIFT(cen) = 0.0
	    AP_XERR(cen) = INDEFR
	    AP_YERR(cen) = INDEFR
	    return (fier)
	} else if (ier == AP_CTR_BADDATA) {
	    return (AP_CTR_BADDATA)
	} else if (ier == AP_CTR_LOWSNRATIO) {
	    return (AP_CTR_LOWSNRATIO)
	} else if (abs (AP_XSHIFT(cen)) > (AP_MAXSHIFT(cen) * AP_SCALE(ap))) {
	    return (AP_CTR_BADSHIFT)
	} else if (abs (AP_YSHIFT(cen)) > (AP_MAXSHIFT(cen) * AP_SCALE(ap))) {
	    return (AP_CTR_BADSHIFT)
	} else if (ier == AP_CTR_OUTOFBOUNDS) {
	    return (AP_CTR_OUTOFBOUNDS)
	} else
	    return (AP_OK)
end
