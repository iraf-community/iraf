include <imhdr.h>
include "../lib/apphotdef.h"
include "../lib/photdef.h"
include "../lib/phot.h"

# APMAGBUF -- Procedure to determine the mapping of the aperture list
# into the input image.

int procedure apmagbuf (ap, im, wx, wy, c1, c2, l1, l2)

pointer	ap		# pointer to apphot structure
pointer	im		# pointer to the IRAF image
real	wx, wy		# center coordinates
int	c1, c2		# column limits
int	l1, l2		# line limits

int	i
pointer	phot
real	rbuf
int	ap_photpix()

begin
	# Check for 0 radius aperture.
	phot = AP_PPHOT(ap)
	if (Memr[AP_APERTS(phot)] <= 0.0)
	    return (AP_APERT_NOAPERT)

	# Compute the maximum aperture size
	AP_APIX(phot) = NULL
	for (i = AP_NAPERTS(phot); AP_APIX(phot) == NULL && i >= 1; i = i - 1) {
	    rbuf =  2. * Memr[AP_APERTS(phot)+i-1] * AP_SCALE(ap)
	    AP_APIX(phot) = ap_photpix (im, wx, wy, rbuf, c1, c2, l1, l2)
	    AP_AXC(phot) = wx - c1 + 1 
	    AP_AYC(phot) = wy - l1 + 1
	    AP_ANX(phot) = c2 - c1 + 1
	    AP_ANY(phot) = l2 - l1 + 1
	    AP_NMAXAP(phot) = i
	}

	# Return the appropriate error code.
	if (AP_APIX(phot) == NULL) {
	    return (AP_APERT_NOAPERT)
	} else if (AP_NMAXAP(phot) < AP_NAPERTS(phot)) {
	    return (AP_APERT_OUTOFBOUNDS)
	} else {
	    return (AP_OK)
	}
end


# AP_PHOTPIX -- Procedure to determine the line and column limits of the
# required subraster.

int procedure ap_photpix (im, wx, wy, papert, c1, c2, l1, l2)

pointer	im		# pointer to IRAF image
real	wx, wy		# center of centering subraster annulus
real	papert		# centering radius
int	c1, c2		# column limits
int	l1, l2		# line limits

int	ncols, nlines
real	half_papert, xc1, xc2, xl1, xl2

begin
	# Check for 0 radius aperture.
	half_papert = papert / 2.
	if (half_papert <= 0)
	    return (0)
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	# Test for an out of bounds aperture.
	xc1 = wx - half_papert
	xc2 = wx + half_papert
	xl1 = wy - half_papert
	xl2 = wy + half_papert
	if ((xc1 < 0.5) || (xc2 > (real (ncols) + 0.5)) ||
	    (xl1 < 0.5) || (xl2 > (real (nlines) + 0.5)))
	    return (0)

	# Get the column and line limits, dimensions and center of the subraster
	# to be extracted.
	c1 = max (1.0, min (real (ncols), xc1))
	c2 = min (real (ncols), max (1.0, xc2 + 0.5))
	l1 = max (1.0, min (real (nlines), xl1))
	l2 = min (real (nlines), max (1.0, xl2 + 0.5))
	return ((c2 - c1 + 1) * (l2 - l1 + 1))
end
