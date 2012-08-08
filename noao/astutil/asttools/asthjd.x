include	<math.h>

# AST_HJD -- Helocentric Julian Day from Epoch

procedure ast_hjd (ra, dec, epoch, lt, hjd)

double	ra		# Right ascension of observation (hours)
double	dec		# Declination of observation (degrees)
double	epoch		# Julian epoch of observation
double	lt		# Light travel time in seconds
double	hjd		# Helocentric Julian Day

double	ast_julday()

begin
	call ast_jd_to_hjd (ra, dec, ast_julday(epoch), lt, hjd)
end


# AST_JD_TO_HJD -- Helocentric Julian Day from UT Julian date

procedure ast_jd_to_hjd (ra, dec, jd, lt, hjd)

double	ra		# Right ascension of observation (hours)
double	dec		# Declination of observation (degrees)
double	jd		# Geocentric Julian date of observation
double	lt		# Light travel time in seconds
double	hjd		# Helocentric Julian Day

double	t, manom, lperi, oblq, eccen, tanom, slong, r, d, l, b, rsun

begin
	# JD is the geocentric Julian date.
	# T is the number of Julian centuries since J1900.

	t = (jd - 2415020d0) / 36525d0

	# MANOM is the mean anomaly of the Earth's orbit (degrees)
	# LPERI is the mean longitude of perihelion (degrees)
	# OBLQ is the mean obliquity of the ecliptic (degrees)
	# ECCEN is the eccentricity of the Earth's orbit (dimensionless)

	manom = 358.47583d0 +
	    t * (35999.04975d0 - t * (0.000150d0 + t * 0.000003d0))
	lperi = 101.22083d0 +
	    t * (1.7191733d0 + t * (0.000453d0 + t * 0.000003d0))
	oblq = 23.452294d0 -
	    t * (0.0130125d0 + t * (0.00000164d0 - t * 0.000000503d0))
	eccen = 0.01675104d0 - t * (0.00004180d0 + t * 0.000000126d0)

	# Convert to principle angles
	manom = mod (manom, 360.0D0)
	lperi = mod (lperi, 360.0D0)

	# Convert to radians
	r = DEGTORAD(ra * 15)
	d = DEGTORAD(dec)
	manom = DEGTORAD(manom)
	lperi = DEGTORAD(lperi)
	oblq = DEGTORAD(oblq)

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

	# R is the distance to the Sun.
	rsun = (1. - eccen**2) / (1. + eccen * cos (tanom))

	# LT is the light travel difference to the Sun.
	lt = -0.005770d0 * rsun * cos (b) * cos (l - slong)
	hjd = jd + lt
end
