include	<math.h>

# AST_VORBIT -- Radial velocity component of the Earth-Moon barycenter
# relative to the Sun.

procedure ast_vorbit (ra, dec, epoch, v)

double	ra		# Right ascension of observation (hours)
double	dec		# Declination of observation (degrees)
double	epoch		# Julian epoch of observation
double	v		# Component of orbital velocity (km/s)

double	t, manom, lperi, oblq, eccen, tanom, slong, r, d, l, b, vorb
double	ast_julday()

begin
	# T is the number of Julian centuries since J1900.
	t = (ast_julday (epoch) - 2415020) / 36525.

	# MANOM is the mean anomaly of the Earth's orbit (degrees)
	# LPERI is the mean longitude of perihelion (degrees)
	# OBLQ is the mean obliquity of the ecliptic (degrees)
	# ECCEN is the eccentricity of the Earth's orbit (dimensionless)

	manom = 358.47583 + t * (35999.04975 - t * (0.000150 + t * 0.000003))
	lperi = 101.22083 + t * (1.7191733 + t * (0.000453 + t * 0.000003))
	oblq = 23.452294 - t * (0.0130125 + t * (0.00000164 - t * 0.000000503))
	eccen = 0.01675104 - t * (0.00004180 + t * 0.000000126)

	# Convert to principle angles
	manom = mod (manom, 360.0D0)
	lperi = mod (lperi, 360.0D0)

	# Convert to radians
	r = DEGTORAD (ra * 15)
	d = DEGTORAD (dec)
	manom = DEGTORAD (manom)
	lperi = DEGTORAD (lperi)
	oblq = DEGTORAD (oblq)

	# TANOM is the true anomaly (approximate formula) (radians)
	tanom = manom + (2 * eccen - 0.25 * eccen**3) * sin (manom) +
	    1.25 * eccen**2 * sin (2 * manom) +
	    13./12. * eccen**3 * sin (3 * manom)

	# SLONG is the true longitude of the Sun seen from the Earth (radians)
	slong = lperi + tanom + PI

	# L and B are the longitude and latitude of the star in the orbital
	# plane of the Earth (radians)

	call ast_coord (double (0.), double (0.), double (-HALFPI),
	    HALFPI - oblq, r, d, l, b)

	# VORB is the component of the Earth's orbital velocity perpendicular
	# to the radius vector (km/s) where the Earth's semi-major axis is
	# 149598500 km and the year is 365.2564 days.

	vorb = ((TWOPI / 365.2564) * 149598500. / sqrt (1. - eccen**2)) / 86400.

	# V is the projection onto the line of sight to the observation of
	# the velocity of the Earth-Moon barycenter with respect to the
	# Sun (km/s).

	v = vorb * cos (b) * (sin (slong - l) - eccen * sin (lperi - l))
end
