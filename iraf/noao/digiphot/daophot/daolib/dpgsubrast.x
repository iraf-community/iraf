include <imhdr.h>

# DP_GSUBRAST -- Extract a sub-raster around a specified position such that
# all pixels within the specified radius are included.

pointer	procedure dp_gsubrast (im, x, y, radius, lowx, lowy, nxpix, nypix)

pointer	im			# image descriptor
real	x, y			# position
real	radius			# radius
long	lowx, lowy		# lower boundaries of subraster
size_t	nxpix, nypix		# number of pixels in subraster

long	lint()
pointer	imgs2r()

begin
	# Determine the boundaries of the area.
	lowx = lint (x - radius) + 1
	lowy = lint (y - radius) + 1
	lowx = max (1, lowx)
	lowy = max (1, lowy)
	
	# Compute the size of the area.
	nxpix = min (lint (x + radius), IM_LEN(im, 1)) - lowx + 1
	nypix = min (lint (y + radius), IM_LEN(im, 2)) - lowy + 1

	# Return a pointer to the pixel buffer.
	if ((nxpix < 1) || (nypix < 1)) 
	    return (NULL)
	else
	    return (imgs2r (im, lowx, lowx + nxpix - 1, lowy, lowy + nypix - 1))
end
