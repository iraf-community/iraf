include	<math.h>

# AST_VBARY -- Radial velocity component of center of the Earth relative to
# to the barycenter of the Earth-Moon system.

procedure ast_vbary (ra, dec, epoch, v)

double	ra		# Right ascension of observation (hours)
double	dec		# Declination of observation (degrees)
double	epoch		# Julian epoch of observation
double	v		# Component of orbital velocity (km/s)

double	t, oblq, omega, llong, lperi, inclin, em, anom, vmoon
double	r, d, l, b, lm, bm, ast_julday()

begin
	# T is the number of Julian centuries since J1900.
	t = (ast_julday (epoch) - 2415020) / 36525.

	# OBLQ is the mean obliquity of the ecliptic
	# OMEGA is the longitude of the mean ascending node
	# LLONG is the mean lunar longitude (should be 13.1763965268)
	# LPERI is the mean lunar longitude of perigee
	# INCLIN is the inclination of the lunar orbit to the ecliptic
	# EM is the eccentricity of the lunar orbit (dimensionless)
	# All quantities except the eccentricity are in degrees.

	oblq = 23.452294 - t * (0.0130125 + t * (0.00000164 - t * 0.000000503))
	omega = 259.183275 - t * (1934.142008 + t * (0.002078 + t * 0.000002))
	llong = 270.434164 +
	    t * (481267.88315 + t * (-0.001133 + t * 0.0000019)) - omega
	lperi = 334.329556 +
	    t * (4069.034029 - t * (0.010325 + t * 0.000012)) - omega
	em = 0.054900489
	inclin = 5.1453964

	# Determine true longitude.  Compute mean anomaly, convert to true
	# anomaly (approximate formula), and convert back to longitude.
	# The mean anomaly is only approximate because LPERI should
	# be the true rather than the mean longitude of lunar perigee.

	lperi = DEGTORAD (lperi)
	llong = DEGTORAD (llong)
	anom = llong - lperi
	anom = anom + (2. * em - 0.25 * em**3) * sin (anom) + 1.25 * em**2 *
	    sin (2. * anom) + 13./12. * em**3 * sin (3. * anom)
	llong = anom + lperi

	# L and B are the ecliptic longitude and latitude of the observation.
	# LM and BM are the lunar longitude and latitude of the observation
	# in the lunar orbital plane relative to the ascending node.

	r = DEGTORAD (ra * 15)
	d = DEGTORAD (dec)
	omega = DEGTORAD (omega)
	oblq = DEGTORAD (oblq)
	inclin = DEGTORAD (inclin)

	call ast_coord (double (0.), double (0.), double (-HALFPI),
	    HALFPI - oblq, r, d, l, b)
	call ast_coord (omega, double (0.), omega-HALFPI, HALFPI-inclin,
	    l, b, lm, bm)

	# VMOON is the component of the lunar velocity perpendicular to the
	# radius vector.  V is the projection onto the line of sight to the
	# observation of the velocity of the Earth's center with respect to
	# the Earth-Moon barycenter.  The 81.53 is the ratio of the Earth's
	# mass to the Moon's mass.

	vmoon = (TWOPI / 27.321661) * 384403.12040 / sqrt (1. - em**2) / 86400.
	v = vmoon * cos (bm) * (sin (llong - lm) - em * sin (lperi - lm))
	v = v / 81.53
end
