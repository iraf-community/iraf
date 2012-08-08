include	<math.h>

# AST_ARCSEP -- Arc distance (arcsec) between two spherical coordinates
# (hours,deg).

double procedure ast_arcsep (ra1, dec1, ra2, dec2) 

double	ra1		# Right ascension (hours)
double	dec1		# Declination (degrees)
double	ra2		# Right ascension (hours)
double	dec2		# Declination (degrees)

double	a1, b1, c1, a2, b2, c2

begin
	# Direction cosines
	a1 = cos (DEGTORAD (15D0 * ra1)) * cos (DEGTORAD (dec1))
	b1 = sin (DEGTORAD (15D0 * ra1)) * cos (DEGTORAD (dec1))
	c1 = sin (DEGTORAD (dec1))

	a2 = cos (DEGTORAD (15D0 * ra2)) * cos (DEGTORAD (dec2))
	b2 = sin (DEGTORAD (15D0 * ra2)) * cos (DEGTORAD (dec2))
	c2 = sin (DEGTORAD (dec2))

	# Modulus squared of half the difference vector.
	a1 = ((a1-a2)**2 + (b1-b2)**2 + (c1-c2)**2) / 4

	# Angle
	a1 = 2D0 * atan2 (sqrt (a1), sqrt (max (0D0, 1D0-a1)))
	a1 = RADTODEG (a1) * 3600D0

	return (a1)
end
