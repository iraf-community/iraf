# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gset.h>

define	XCEN	0.5
define	YCEN	0.52
define	EDGE1	0.1
define	EDGE2	0.93

# PL_MAP_VIEWPORT -- set device viewport for contour and hafton plots.  If not
# specified by user, a default viewport centered on the device is used.
#
# The value of "extreme" (ext) axes ratios changed from 1/4 to 1/16 
# (ShJ 6-10-88)

procedure pl_map_viewport (gp, ncols, nlines, ux1, ux2, uy1, uy2, fill, perim)

pointer	gp			#I pointer to graphics descriptor
int	ncols, nlines		#I size of image area, after block reduction
real	ux1, ux2, uy1, uy2	#I NDC coordinates of requested viewort
bool	fill			#I fill viewport (vs preserve aspect ratio)
bool	perim			#I draw perimeter

real	xcen, ycen
real	ncolsr, nlinesr, ratio, aspect_ratio
real	x1, x2, y1, y2, ext, xdis, ydis
data    ext /0.0625/
bool	fp_equalr()
real	ggetr()

begin
	ncolsr = real (ncols)
	nlinesr = real (nlines)

	if (fp_equalr (ux1, 0.0) && fp_equalr (ux2, 0.0) && 
	    fp_equalr (uy1, 0.0) && fp_equalr (uy2, 0.0)) {

	    if (fill && !perim) {
		x1 = 0.0;  x2 = 1.0
		y1 = 0.0;  y2 = 1.0
		xcen = 0.5;  ycen = 0.5
	    } else {
		x1 = EDGE1;  x2 = EDGE2
		y1 = EDGE1;  y2 = EDGE2
		xcen = XCEN;  ycen = YCEN
	    }

	    # Calculate optimum viewport, as in NCAR's conrec, hafton.
	    if (!fill) {
		ratio = min (ncolsr, nlinesr) / max (ncolsr, nlinesr)
		if (ratio >= ext) {
		    if (ncols > nlines) 
			y2 = (y2 - y1) * nlinesr / ncolsr + y1
		    else 
			x2 = (x2 - x1) * ncolsr / nlinesr + x1
		}
	    }

	    xdis = x2 - x1
	    ydis = y2 - y1

	    # So far, the viewport has been calculated so that equal numbers of
	    # image pixels map to equal distances in NDC space, regardless of 
	    # the aspect ratio of the device.  If the parameter "fill" has been
	    # set to no, the user wants to compensate for a non-unity aspect 
	    # ratio and make equal numbers of image pixels map to into the same 
	    # physical distance on the device, not the same NDC distance.

	    if (!fill) {
	        aspect_ratio = ggetr (gp, "ar")

	        if (fp_equalr (aspect_ratio, 0.0))
	            aspect_ratio = 1.0

		if (aspect_ratio < 1.0)
		    # Landscape
	            xdis = xdis * aspect_ratio
		else if (aspect_ratio > 1.0)
		    # Portrait
		    ydis = ydis / aspect_ratio
	    }

	    ux1 = xcen - (xdis / 2.0)
	    ux2 = xcen + (xdis / 2.0)
	    uy1 = ycen - (ydis / 2.0)
	    uy2 = ycen + (ydis / 2.0)
	}

	# Set window and viewport for WCS 1
	call gseti  (gp, G_WCS, 1)
	call gsview (gp, ux1, ux2, uy1, uy2)
	call gswind (gp, 1.0, ncolsr, 1.0, nlinesr)
	call set (ux1, ux2, uy1, uy2, 1.0, ncolsr, 1.0, nlinesr, 1)
end
