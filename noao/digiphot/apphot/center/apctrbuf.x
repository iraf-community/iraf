include <imhdr.h>
include <math.h>
include <mach.h>
include "../lib/apphotdef.h"
include "../lib/centerdef.h"
include "../lib/center.h"

# APCTRBUF -- Procedure to fetch the center pixels given the pointer to the
# IRAF image, the coordinates of the initial center and the width of the APPHOT
# centering box.

int procedure apctrbuf (ap, im, wx, wy)

pointer	ap		# pointer to apphot structure
pointer	im		# pointer to the IRAF image
real	wx, wy		# center coordinates

int	icpix
pointer	ctr
real	cpix, gdatamin, gdatamax, datamin, datamax
pointer	ap_ctrpix()

begin
	# Get pointer to centering structure.
	ctr = AP_PCENTER(ap)

	# Check for 0 sized aperture.
	if (AP_CAPERT(ctr) <= 0.0)
	    return (AP_CTR_NOAREA)

	# Get the centering buffer of pixels.
	cpix = max (1.0, AP_CAPERT(ctr) * AP_SCALE(ap))
	icpix = 2 * int (cpix) + 1
	if (AP_CTRPIX(ctr) != NULL)
	    call mfree (AP_CTRPIX(ctr), TY_REAL)
	AP_CTRPIX(ctr) = ap_ctrpix (im, wx, wy, icpix,
	    AP_CXC(ctr), AP_CYC(ctr), AP_CNX(ctr), AP_CNY(ctr))
	if (AP_CTRPIX(ctr) == NULL)
	    return (AP_CTR_NOAREA)

	# Compute the data limits.
	if (IS_INDEFR(AP_DATAMIN(ap)))
	    gdatamin = -MAX_REAL
	else
	    gdatamin = AP_DATAMIN(ap)
	if (IS_INDEFR(AP_DATAMAX(ap)))
	    gdatamax = MAX_REAL
	else
	    gdatamax = AP_DATAMAX(ap)
	call alimr (Memr[AP_CTRPIX(ctr)], AP_CNX(ctr) * AP_CNY(ctr),
	    datamin, datamax)

	if (datamin < gdatamin || datamax > gdatamax)
	    return (AP_CTR_BADDATA)
	else if (AP_CNX(ctr) < icpix || AP_CNY(ctr) < icpix)
	    return (AP_CTR_OUTOFBOUNDS)
	else
	    return (AP_OK)
end


# AP_CTRPIX -- Procedure to fetch the pixels to be used for centering.

pointer procedure ap_ctrpix (im, wx, wy, capert, xc, yc, nx, ny)

pointer	im		# pointer to IRAF image
real	wx, wy		# center of subraster to be extracted
int	capert		# width of subraster to be extracted
real	xc, yc		# center of extracted subraster
int	nx, ny		# dimensions of extracted subraster

int	i, ncols, nlines, c1, c2, l1, l2, half_capert
pointer	buf, lbuf
real	xc1, xc2, xl1, xl2
pointer	imgs2r()

begin
	# Check for nonsensical input.
	half_capert = (capert - 1) / 2
	if (half_capert <= 0)
	    return (NULL)

	# Test for out of bounds pixels
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	xc1 = wx - half_capert
	xc2 = wx + half_capert
	xl1 = wy - half_capert
	xl2 = wy + half_capert
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
	if (nx < 1 && ny < 1)
	    return (NULL)
	else {
	    call malloc (buf, nx * ny, TY_REAL)
	    lbuf = buf
	    do i = l1, l2 {
	        call amovr (Memr[imgs2r (im, c1, c2, i, i)], Memr[lbuf], nx)
	        lbuf = lbuf + nx
	    }
	    return (buf)
	}
end
