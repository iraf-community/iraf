include	<math.h>

define	DIRS	"|ang2row|row2ang|"
define	ANG2PIX	1
define	PIX2ANG	2

define	CUNITS	"|hourdegree|degrees|radians|"
define	H	1
define	D	2
define	R	3

define	MTYPES	"|nest|ring|"
define	NEST	1
define	RING	2


# T_HPCTRAN -- Convert between HEALPix rows and spherical coordinates.
#
# It is up to the user to know the coordinate and map type; e.g.
# galactic/nested, equatorial/ring.  However, the use can use
# whatever units for the coordinate type; e.g. hours/degrees, radians.
#
# The HEALPix row is 1 indexed to be consistent with IRAF conventions.
# This row can be used to access the map data with TTOOLS tasks.

procedure t_hpctran ()

int	dir			# Direction (ang2row|row2ang)
int	row			# HEALpix map row (1 indexed)
double	lng			# RA/longitude
double	lat			# DEC/latitude
int	nside			# Resolution parameter
int	cunits			# Coordinate units
int	mtype			# HEALpix map type

char	str[10]

int	clgeti(), clgwrd()
double	clgetd()
errchk	ang2row, row2ang

begin
	# Get parameters.
	dir = clgwrd ("direction", str, 10, DIRS)
	nside = clgeti ("nside")
	cunits = clgwrd ("cunits", str, 10, CUNITS)
	mtype = clgwrd ("maptype", str, 10, MTYPES)

	switch (dir) {
	case ANG2PIX:
	    lng = clgetd ("lng")
	    lat = clgetd ("lat")
	    switch (cunits) {
	    case 0:
		call error (1, "Unknown coordinate units")
	    case H:
		lng = lng * 15D0
	    case R:
		lng = RADTODEG(lng)
		lat = RADTODEG(lat)
	    }

	    call ang2row (row, lng, lat, mtype, nside)

	    call clputi ("row", row)
	case PIX2ANG:
	    row = clgeti ("row")

	    call row2ang (row, lng, lat, mtype, nside)

	    switch (cunits) {
	    case 0:
		call error (1, "Unknown coordinate units")
	    case H:
		lng = lng / 15D0
	    case R:
		lng = DEGTORAD(lng)
		lat = DEGTORAD(lat)
	    }

	    call clputd ("lng", lng)
	    call clputd ("lat", lat)
	}

	# Output the map row.
	call printf ("%d %g %g\n")
	    call pargi (row)
	    call pargd (lng)
	    call pargd (lat)
end


# TEST_HEALPIX2 -- Test routine as in the HEALPix distribution.

procedure test_healpix2 ()

double	theta, phi
int	nside
int	ipix, npix, dpix, ip1

begin

	call printf("Starting C Healpix pixel routines test\n")

	nside = 1024
	dpix = 23

	# Find the number of pixels in the full map
	npix = 12*nside*nside
	call printf("Number of pixels in full map: %d\n")
	    call pargi (npix)

	call printf("dpix: %d\n")
	    call pargi (dpix)
	call printf("Nest -> ang -> Ring -> ang -> Nest\n")
#	call printf("Nest -> ang -> Nest\n")
#	call printf("Ring -> ang -> Ring\n")
	for (ipix = 0; ipix < npix; ipix = ipix + dpix) {
	    call pix2ang_nest(nside, ipix, theta, phi)
	    call ang2pix_ring(nside, theta, phi, ip1)
	    call pix2ang_ring(nside, ip1, theta, phi)
	    call ang2pix_nest(nside, theta, phi, ip1)
#	    call pix2ang_ring(nside, ipix, theta, phi)
#	    call ang2pix_ring(nside, theta, phi, ip1)
	    if (ip1 != ipix) {
	        call printf("Error: %d %d %d\n")
		    call pargi (nside)
		    call pargi (ipix)
		    call pargi (ip1)
	    }
	}

	call printf("%d\n")
	    call pargi (nside)
	call printf("test completed\n\n")
end
