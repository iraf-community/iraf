include <imhdr.h>
include "../lib/apphotdef.h"
include "../lib/fitpsfdef.h"
include "../lib/fitpsf.h"

# APFBUF -- Procedure to fetch the subraster of pixels to be fitted given the
# pointer to the IRAF image, the coordinates of the center and the apphot
# structure.

int procedure apfbuf (ap, im, wx, wy)

pointer	ap		# pointer to apphot structure
pointer	im		# pointer to the IRAF image
real	wx, wy		# center coordinates

int	ippix
pointer	psf
real	ppix
pointer	ap_psfpix()

begin
	# Check for 0 sized aperture.
	psf = AP_PPSF(ap)
	if (AP_PSFAPERT(psf) <= 0.0)
	    return (AP_NOPSFAREA)

	# Get the PSF pixels.
	ppix = max (1.0, AP_PSFAPERT(psf) * AP_SCALE(ap))
	ippix = 2 * int (ppix) + 1
	AP_PSFPIX(psf) = ap_psfpix (im, wx, wy, ippix, AP_PXC(psf), AP_PYC(psf),
	    AP_PNX(psf), AP_PNY(psf))
	if (AP_PSFPIX(psf) == NULL)
	    return (AP_NOPSFAREA)
	else if (AP_PNX(psf) < ippix || AP_PNY(psf) < ippix)
	    return (AP_PSF_OUTOFBOUNDS)
	else
	    return (AP_OK)
end


# AP_PSFPIX -- Procedure to fetch the pixels to be used for centering.

pointer procedure ap_psfpix (im, wx, wy, papert, xc, yc, nx, ny)

pointer	im		# pointer to IRAF image
real	wx, wy		# center of subraster to be extracted
int	papert		# width of subraster to be extracted
real	xc, yc		# center of extracted subraster
int	nx, ny		# dimensions of extracted subraster

int	ncols, nlines, c1, c2, l1, l2, half_papert
pointer	buf
real	xc1, xc2, xl1, xl2
pointer	imgs2r()

begin
	# Check for nonsensical input.
	half_papert = (papert - 1) / 2
	if (half_papert <= 0)
	    return (NULL)

	# Test for out of bounds pixels
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	xc1 = wx - half_papert
	xc2 = wx + half_papert
	xl1 = wy - half_papert
	xl2 = wy + half_papert
	if (xc1 > real (ncols) || xc2 < 1.0 || xl1 > real (nlines) || xl2 < 1.0)
	    return (NULL)

	# Get column and line limits, dimensions and center of subraster.
	c1 = max (1.0, min (real (ncols), xc1)) + 0.5
	c2 = min (real (ncols), max (1.0, xc2)) + 0.5
	l1 = max (1.0, min (real (nlines), xl1)) + 0.5
	l2 = min (real (nlines), max (1.0, xl2)) + 0.5
	nx = c2 - c1 + 1
	ny = l2 - l1 + 1
	xc = wx - c1 + 1
	yc = wy - l1 + 1

	# Get pixels and return.
	buf = imgs2r (im, c1, c2, l1, l2)
	if (buf == EOF)
	    return (NULL)
	else
	    return (buf)
end
