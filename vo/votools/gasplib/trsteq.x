define  ARCSEC_PER_RADIAN       206264.8062470964d0

# TRSTEQ -- procedure to compute the RA and Dec from Standard coordinates,
# given the plate centre.
#
# In rectangular coordinates the vector (1, xi, eta) points toward
# the object; the origin is the observer's location, the x-axis points
# toward the reference pixel (plate centre), the y-axis is in the direction
# of increasing right ascension, and the z-axis is in the direction of
# increasing declination.  The coordinate system is then rotated by the
# declination so the x-axis passes through the equator at the RA of the
# reference pixel; the components of the vector in this coordinate system
# are used to compute (RA - reference_RA) and declination.
#
# original, written by ???
# Phil Hodge, 15-Nov-1999  Rewrite, based on tables$lib/stxtools/xtwcs.x.

procedure trsteq (ra0, dec0, xi, eta, ra, dec)

double	ra0, dec0	# i: RA & Dec at plate centre (radians)
double	xi, eta		# i: standard coordinates (arcsec)
double	ra, dec		# o: right ascension & declination (radians)
#--
double	xi_r, eta_r	# xi & eta in radians
double	x, y, z		# vector (not unit length) pointing toward object
double	dra		# RA at (xi,eta) - RA at plate centre

begin
	xi_r = xi / ARCSEC_PER_RADIAN
	eta_r = eta / ARCSEC_PER_RADIAN

	# Rotate the rectangular coordinate system of the vector (1, xi, eta)
	# by the declination so the x-axis will pass through the equator.
	x = cos (dec0) - eta_r * sin (dec0)
	y = xi_r
	z = sin (dec0) + eta_r * cos (dec0)

	if (x == 0.d0 && y == 0.d0)
	    dra = 0.d0
	else
	    dra = atan2 (y, x)
	ra = ra0 + dra

	dec = atan2 (z, sqrt (x*x + y*y))
end
