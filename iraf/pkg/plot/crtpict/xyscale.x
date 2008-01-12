# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <imhdr.h>
include	<error.h>
include	"wdes.h"
include	"crtpict.h"

# CRT_XY_SCALE -- Calculate the spatial transformation parameters and store
# them in the "wdes" window descriptor.  These are used to map the image
# to the graphics device.

procedure crt_xy_scale (gp, cl, im, wdes)

pointer	gp
pointer	cl
pointer	im
pointer	wdes

int	w1, w0, ndev_rows, ndev_cols
real	xcenter, ycenter, xsize, ysize, pxsize, pysize, xscale, yscale
real	ystart, yend
int	ggeti()
errchk	ggeti

begin
	ndev_rows = ggeti (gp, "yr")
	ndev_cols = ggeti (gp, "xr")

	# Determine the maximum display window available for mapping the
	# image.  This is a function of the user specified "fractions";
	# the outer 6% of the user specified image area is used for annotation.

	xcenter = (CRT_XE + CRT_XS) / 2.0
	xsize   = CRT_XE - CRT_XS
	ystart  = CRT_YS + (CRT_YE - CRT_YS) * GRAPHICS_FRACTION(cl)
	yend    = ystart + (CRT_YE - CRT_YS) * IMAGE_FRACTION(cl)
	ycenter = (yend + ystart) / 2.0
	ysize   = yend - ystart

	# Set device window limits in normalized device coordinates.
	# World coord system 0 is used for the device window.

	w0 = W_WC(wdes,0)
	W_XS(w0) = xcenter - (xsize / 2.0)
	W_XE(w0) = xcenter + (xsize / 2.0)
	W_YS(w0) = ycenter - (ysize / 2.0)
	W_YE(w0) = ycenter + (ysize / 2.0)

	# Determine X and Y scaling ratios required to map the image into the
	# normalized display window.  

	if (FILL(cl) == YES) {
	    # Compute scale in units of NDC viewport coords per image pixel.
	    xscale = xsize / max (1, (IM_LEN(im,1) - 1))
	    yscale = ysize / max (1, (IM_LEN(im,2) - 1))

	    # Image scaled by same factor in x and y with FILL = YES.
	    if (xscale < yscale)
		yscale = xscale
	    else
		xscale = yscale

	} else {
	    # From the user specified magnification factors, compute the scale
	    # in units of NDC coords per "effective" display pixels.

	    xscale = 1.0 / ((ndev_cols - 1) / XMAG(cl))
	    yscale = 1.0 / ((ndev_rows - 1) / YMAG(cl))
	}

	# Determine the image pixels that map to the available device
	# viewport.  The starting and ending pixels calculated from
	# xscale and yscale may be in the image interior or reference 
	# beyond the bounds of the image.  These endpoints are tuned
	# further in procedure transform_image.

	w1 = W_WC(wdes,1)
	pxsize = xsize / xscale
	pysize = ysize / yscale

	W_XS(w1) = (IM_LEN(im,1) - 1) / 2.0 + 1 - (pxsize / 2.0)
	W_XE(w1) = W_XS(w1) + pxsize
	W_YS(w1) = (IM_LEN(im,2) - 1) / 2.0 + 1 - (pysize / 2.0)
	W_YE(w1) = W_YS(w1) + pysize

	# All spatial transformations are linear.
	W_XT(w1) = W_LINEAR
	W_YT(w1) = W_LINEAR
end
