include	<math.h>

# AST_VR -- Project a velocity vector in radial velocity along line of sight.

procedure ast_vr (ra1, dec1, v1, ra2, dec2, v2) 

double	ra1		# Right ascension of velocity vector (hours)
double	dec1		# Declination of velocity vector (degrees)
double	v1		# Magnitude of velocity vector
double	ra2		# Right ascension of observation (hours)
double	dec2		# Declination of observation (degrees)
double	v2		# Radial velocity along direction of observation

double	vx, vy, vz, cc, cs, s

begin
	# Cartisian velocity components of the velocity vector.
	vx = v1 * cos (DEGTORAD (15. * ra1)) * cos (DEGTORAD (dec1))
	vy = v1 * sin (DEGTORAD (15. * ra1)) * cos (DEGTORAD (dec1))
	vz = v1 * sin (DEGTORAD (dec1))

	# Direction cosines along the direction of observation.
	cc = cos (DEGTORAD (dec2)) * cos (DEGTORAD (15. * ra2))
	cs = cos (DEGTORAD (dec2)) * sin (DEGTORAD (15. * ra2))
	s  = sin (DEGTORAD (dec2))

	# Project velocity vector along the direction of observation.
	v2 = (vx * cc + vy * cs + vz * s)
end
