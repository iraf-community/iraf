# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>
include	<gset.h>
include	"wdes.h"
include	"crtpict.h"

# CRT_TRANSFORM_IMAGE -- Map an image into the output device.  In general this
# involves independent linear transformations in the X, Y, and Z (greyscale)
# dimensions.  If a spatial dimension is larger than the display window then
# the image is block averaged.  If a spatial dimension or a block averaged
# dimension is smaller than the display window then linear interpolation is
# used to expand the image.  The input image is accessed via IMIO; the 
# transformed image is output to a greyscale device via GIO.    

procedure crt_transform_image (gp, im, wdes, cl)

pointer	gp			# graphics descriptor for output
pointer	im			# input image
pointer	wdes			# graphics window descriptor
pointer	cl

real	ndc_xs,ndc_xe,ndc_ys,ndc_ye	# NDC of output device window
real	px1, px2, py1, py2	# image coords in fractional image pixels
real	pxsize, pysize		# size of image section in fractional pixels
real	wxcenter, wycenter	# center of device window in frac device pixels
real	xmag, ymag		# x,y magnification ratios
pointer	w0, w1			# world coord systems 0 (NDC) and 1 (pixel)
int	ndev_cols, ndev_rows, nx_output, ny_output
int	ggeti()
errchk	ggeti, gseti, gsview, gswind, draw_perimeter
errchk	crt_map_image, crt_replicate_image

begin
	# Compute pointers to WCS 0 and 1.
	w0 = W_WC(wdes,0)
	w1 = W_WC(wdes,1)

	call printf ("%s: %s\n")
	    call pargstr (W_IMSECT(wdes))
	    call pargstr (IM_TITLE(im))

	ndev_cols = ggeti (gp, "xr")
	ndev_rows = ggeti (gp, "yr")

	# Compute X and Y magnification ratios required to map image into
	# the device window.

	xmag = ((W_XE(w0) - W_XS(w0)) * ndev_cols) / (W_XE(w1) - W_XS(w1))
	ymag = ((W_YE(w0) - W_YS(w0)) * ndev_rows) / (W_YE(w1) - W_YS(w1))

	# Compute the coordinates of the image section to be displayed.
	# This is not necessarily the same as WCS 1 since the WCS coords
	# need not be inbounds, but they are integral pixels.

	px1 = max (1.0,		        real (int (W_XS(w1) + 0.5)))
	px2 = min (real (IM_LEN(im,1)), real (int (W_XE(w1) + 0.5)))
	py1 = max (1.0,		        real (int (W_YS(w1) + 0.5)))
	py2 = min (real (IM_LEN(im,2)), real (int (W_YE(w1) + 0.5)))

	# The extent of the output image in NDC coords
	pxsize = ((px2 - px1) + 1.0) / ndev_cols
	pysize = ((py2 - py1) + 1.0) / ndev_rows

	# The NDC coordinates that map to the central image pixel output
	wxcenter = (W_XE(w0) + W_XS(w0)) / 2.0
	wycenter = (W_YE(w0) + W_YS(w0)) / 2.0

	# Now compute the NDC coordinates of the output device that the
	# image will occupy.  
	ndc_xs = max (W_XS(w0), wxcenter - (pxsize / 2.0 * xmag))
	ndc_xe = max (ndc_xs, min (W_XE(w0), ndc_xs + (pxsize * xmag)))
	ndc_ys = max (W_YS(w0), wycenter - (pysize / 2.0 * ymag))
	ndc_ye = max (ndc_ys, min (W_YE(w0), ndc_ys + (pysize * ymag)))

	# To avoid possible truncation errors down the line, make sure
	# the ndc coordinates passed to the output procedures represent
	# integer pixels.
	ndc_xs = real (int (ndc_xs * ndev_cols)) / ndev_cols
	ndc_xe = real (int (ndc_xe * ndev_cols)) / ndev_cols
	ndc_ys = real (int (ndc_ys * ndev_rows)) / ndev_rows
	ndc_ye = real (int (ndc_ye * ndev_rows)) / ndev_rows

	# Output the image data in WCS 0.  The number of image pixels that
	# will be put out across the device is calculated first.  

	call gseti (gp, G_WCS, 0)
	if (REPLICATE(cl) == YES) {
	    nx_output = (int(px2) - int(px1)) + 1
	    ny_output = (int(py2) - int(py1)) + 1
	} else {
	    # Image pixels will be scaled to number of device pixels 
	    nx_output = ((ndc_xe * ndev_cols) - (ndc_xs * ndev_cols)) + 1
	    ny_output = ((ndc_ye * ndev_rows) - (ndc_ys * ndev_rows)) + 1
	    nx_output = nx_output / X_BA(cl)
	    ny_output = ny_output / Y_BA(cl)
	}

	# Tweak the ndc coordinates to insure integer replication factors.
	# This may change the ndc coordinates.
	call crt_tweak_ndc (nx_output, ndc_xs, ndc_xe, ndev_cols)
	call crt_tweak_ndc (ny_output, ndc_ys, ndc_ye, ndev_rows)

	# Call routine that actually puts out the pixels row by row
	call crt_map_image (im, gp, px1,px2,py1,py2, ndc_xs,ndc_xe,
	       ndc_ys,ndc_ye, nx_output, ny_output, W_ZS(w1),W_ZE(w1),W_ZT(w1),
	           cl)

	# Change to pixel coordinates and draw perimeter axes if requested
	call gseti  (gp, G_WCS, 2)
	call gsview (gp, ndc_xs, ndc_xe, ndc_ys, ndc_ye)
	call gswind (gp, px1, px2, py1, py2)

	if (PERIM(cl) == YES)
	    call draw_perimeter (gp)
end
