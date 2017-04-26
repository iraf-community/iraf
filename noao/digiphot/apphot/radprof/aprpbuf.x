include <imhdr.h>
include "../lib/apphotdef.h"
include "../lib/radprofdef.h"
include "../lib/radprof.h"

# AP_RPBUF -- Procedure to determine the mapping of the of the radial
# profile size into the apertures.

int procedure ap_rpbuf (ap, im, wx, wy)

pointer	ap		# pointer to apphot structure
pointer	im		# pointer to the IRAF image
real	wx, wy		# center coordinates

int	c1, c2, l1, l2
pointer	rprof
real	rbuf
pointer ap_rppix()

begin
	# Check for 0 radius aperture.
	rprof = AP_RPROF(ap)
	if (AP_RPRADIUS(rprof) <= 0.0)
	    return (AP_RP_NOPROFILE)

	# Compute the maximum aperture size
	rbuf =  2. * AP_RPRADIUS(rprof) * AP_SCALE(ap) + 1.
	AP_RPIX(rprof) = ap_rppix (im, wx, wy, rbuf, c1, c2, l1, l2)
	AP_RPXC(rprof) = wx - c1 + 1 
	AP_RPYC(rprof) = wy - l1 + 1
	AP_RPNX(rprof) = c2 - c1 + 1
	AP_RPNY(rprof) = l2 - l1 + 1

	# Return the appropriate error code.
	if (AP_RPIX(rprof) == NULL) {
	    return (AP_RP_NOPROFILE)
	} else if (AP_RPNX(rprof) < rbuf || AP_RPNY(rprof) < rbuf) {
	    return (AP_RP_OUTOFBOUNDS)
	} else {
	    return (AP_OK)
	}
end


# AP_RPPIX -- Procedure to read in the aperture pixels

pointer procedure ap_rppix (im, wx, wy, papert, c1, c2, l1, l2)

pointer	im		# pointer to IRAF image
real	wx, wy		# center of centering subraster annulus
real	papert		# centering radius
int	c1, c2		# column limits
int	l1, l2		# line limits

int	ncols, nlines
real	half_papert, xc1, xc2, xl1, xl2
pointer	imgs2r()

begin
	# Check for 0 radius aperture.
	half_papert = papert / 2.
	if (half_papert <= 0.)
	    return (NULL)
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	# Test for an out of bounds aperture.
	xc1 = wx - half_papert
	xc2 = wx + half_papert
	xl1 = wy - half_papert
	xl2 = wy + half_papert
	if (xc1 > real (ncols) || xc2 < 1.0 || xl1 > real (nlines) || xl2 < 1.0)
	    return (NULL)

	# Get the column and line limits, dimensions and center of the subraster
	# to be extracted.
	c1 = max (1.0, min (real (ncols), xc1))
	c2 = min (real (ncols), max (1.0, xc2))
	l1 = max (1.0, min (real (nlines), xl1))
	l2 = min (real (nlines), max (1.0, xl2))
	return (imgs2r (im, c1, c2, l1, l2))
end
