include	<math.h>

# AST_VROTATE -- Radial velocity component of the observer relative to
# the center of the Earth due to the Earth's rotation.

procedure ast_vrotate (ra, dec, epoch, latitude, longitude, altitude, v)

double	ra		# Right Ascension of observation (hours)
double	dec		# Declination of observation (degrees)
double	epoch		# Epoch of observation (Julian epoch)
double	latitude	# Latitude (degrees)
double	longitude	# Latitude (degrees)
double	altitude	# Altitude (meters)
double	v		# Velocity (km / s)

double	lat, dlat, r, vc, lmst, ast_mst()

begin
	# LAT is the latitude in radians.
	lat = DEGTORAD (latitude) 

	# Reduction of geodetic latitude to geocentric latitude (radians).
	# Dlat is in arcseconds.

	dlat = -(11. * 60. + 32.743000d0) * sin (2 * lat) +
		1.163300d0 * sin (4 * lat) -0.002600d0 * sin (6 * lat)
	lat = lat + DEGTORAD (dlat / 3600.)

	# R is the radius vector from the Earth's center to the observer
	# (meters).  Vc is the corresponding circular velocity
	# (meters/sidereal day converted to km / sec).
	# (sidereal day = 23.934469591229 hours (1986))

	r = 6378160.0d0 * (0.998327073d0 + 0.00167643800d0 * cos (2 * lat) -
	    0.00000351d0 * cos (4 * lat) + 0.000000008d0 * cos (6 * lat)) +
	    altitude
	vc = TWOPI * (r / 1000.)  / (23.934469591229d0 * 3600.)

	# Project the velocity onto the line of sight to the star.
	lmst = ast_mst (epoch, longitude)
	v = vc * cos (lat) * cos (DEGTORAD (dec)) *
	    sin (DEGTORAD ((ra - lmst) * 15.))
end
