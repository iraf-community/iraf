include <mach.h>
include "vt.h"
include "numeric.h"

# NUMERIC -- calculate some of the necessary information, including
# the partial derivitives of latitude and longitude with respect
# to x and y, that we need to do the projection.

procedure numeric (bzero, el, outputrow, pixel, xpixcenter, ypixcenter, num)

real	bzero				# latitude of subearth point
real	el[LEN_ELSTRUCT]		# ellipse parameters data structure
int	outputrow			# which output row are we working on
int	pixel				# which pixel in that output row
real	xpixcenter, ypixcenter		# coordinates of center of pixel
pointer	num				# numeric structure pointer

real	dlatdy, dlongdx			# partial derivitives
real	lat_top, lat_bot		# latitude of top and bottom of pix
real	long_left, long_rite		# longitude of left and right of pix
real	lat_mid, long_mid		# latitude and longitude of middle
real	lat1, long1, lat3, long3, lat4, long4, lat5, long5
real	x1, y1, x3, y3, x4, y4, x5, y5
bool	skip

begin
	skip = false

	# First calculate lats, longs for this pixel.
	lat_top   = 180./3.1415926*asin(real(outputrow - 90)/90.)
	lat_bot   = 180./3.1415926*asin(real(outputrow - 91)/90.)
	long_left = real(pixel - 1) - 90.
	long_rite = real(pixel) - 90.
	lat_mid   = .5 * (lat_top + lat_bot)
	long_mid  = .5 * (long_left + long_rite)

	# Check the proximity of this pixel to the image boundary, if its
	# too close, set that output pixel to zero.

	if (abs(abs(lat_mid) - 90.0) < abs(bzero)) {
	    if (abs(abs(lat_top) - 90.0) >= abs(bzero)) {
		lat_bot = -90.0 + bzero
		lat_mid = .5 * (lat_top + lat_bot)
	    } else {
		if (abs(abs(lat_bot) - 90.0) >= abs(bzero)) {
		    lat_top = 90.0 + bzero
		    lat_mid = .5 * (lat_top + lat_bot)
		} else {
	    	    # Nothing to map!
		    # Flag to pixelmap marking zero pixel.
		    VT_LATTOP(num) = 10000.
	    	    return
		}
	    }
	} else {
	    if (abs(abs(lat_top) - 90.0) < abs(bzero))
		lat_top = 90.0 + bzero
	    else
		if (abs(abs(lat_bot) - 90.0) < abs(bzero))
		    lat_bot = -90.0 + bzero
	}

	# Now that we have the pixel we want defined, calculate the partial
	# derivitives we need numerically.  First calculate the latitude and
	# longitude of the centers of the 4 adjacent pixels.

	lat1 = lat_mid
	if (pixel == 1)
	    long1 = long_mid
	else
	    long1 = long_mid - 1.0

	lat3 = lat_mid
	if (pixel == 180)
	    long3 = long_mid
	else
	    long3 = long_mid + 1.0

	long5 = long_mid
	if (outputrow == 1)
	    lat5 = lat_mid
	else
	    lat5 = 180./3.1415926*((asin(real(outputrow - 92)/90.) +
		    asin(real(outputrow - 91)/90.))/2.)

	long4 = long_mid
	if (outputrow == 180)
	    lat4 = lat_mid
	else
	    lat4 = 180./3.1415926*((asin(real(outputrow - 89)/90.) +
	            asin(real(outputrow - 90)/90.))/2.)

	# Given these latitudes and longitudes, find out where in xy coords
	# they are. Get xpixcenter and ypixcenter then the x#s and y#s.

	call getxy (lat_mid, long_mid, bzero, el, xpixcenter, ypixcenter, skip)
	if (skip) {

	    # Off the limb or behind the sun.
	    # Flag to pixelmap marking zero pixel.
	    VT_LATTOP(num) = 10000.
	    return
	}

	call getxy (lat1, long1, bzero, el, x1, y1, skip)
	call getxy (lat3, long3, bzero, el, x3, y3, skip)
	call getxy (lat4, long4, bzero, el, x4, y4, skip)
	call getxy (lat5, long5, bzero, el, x5, y5, skip)

	# Calculate the partials.
	if (x3 == x1)
	    dlongdx = 9999.
	else
	    dlongdx = (long3 - long1) / (x3 - x1)

	if (y4 == y5)
	    dlatdy = 9999.
	else
	    dlatdy = (lat4 - lat5) / (y4 - y5)

	VT_DLODX(num) = dlongdx 
	VT_DLATDY(num) = dlatdy 
	VT_LATTOP(num) = lat_top 
	VT_LATBOT(num) = lat_bot 
	VT_LOLEFT(num) = long_left 
	VT_LORITE(num) = long_rite 
	VT_LATMID(num) = lat_mid 
	VT_LOMID(num) = long_mid 
end


# GETXY -- Given the latitude and longitude of a point and the image
# parameters, return the x and y position of that point.

procedure getxy (lat, lml0, b0, el, x, y, skip)

real	lat			# latitude of point on image
real	lml0			# distance in longitude from disk center
real	b0			# latitude of sub earth point
real	el[LEN_ELSTRUCT]	# ellipse parameters data structure
real	x, y			# returned position
bool	skip			# skip flag

real	sinlat, coslat, sinbzero, cosbzero, sinlminusl0, coslminusl0
real	cosrho, sinrho, sinpminustheta, cospminustheta 
real	latitude, lminusl0, bzero

begin
	skip = false
	lminusl0 = lml0*3.1415926/180.
	bzero = b0*3.1415926/180.
	latitude = lat*3.1415926/180.
	sinlat = sin(latitude)
	coslat = cos(latitude)
	sinbzero = sin(bzero)
	cosbzero = cos(bzero)
	sinlminusl0 = sin(lminusl0)
	coslminusl0 = cos(lminusl0)
	cosrho = sinbzero * sinlat + cosbzero * coslat * coslminusl0

	# If we are behind limb return skip = true.
	if (cosrho <= 0.00) skip = true
	sinrho = (1. - cosrho**2)**.5
	if (sinrho >= EPSILONR) {
	    sinpminustheta = (coslat/sinrho) * sinlminusl0
	    cospminustheta = (coslat/sinrho) * (cosbzero * tan(latitude) -
	        sinbzero * coslminusl0)
	} else {
	    sinpminustheta = 0.000001
	    cospminustheta = 0.000001
	}

	x = E_XSEMIDIAMETER(el) * sinrho * sinpminustheta
	y = E_YSEMIDIAMETER(el) * sinrho * cospminustheta
	x = x + real(E_XCENTER(el))
	y = y + real(E_YCENTER(el))
end
