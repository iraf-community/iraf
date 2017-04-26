include	<math.h>

# Definition of system
define	LP		123.00d0		# Longtitude of pole
define	BP		27.40d0			# Latitude of pole
define	LO		97.7422d0		# Longitude of origin
define	BO		-60.1810d0		# Latitude of origin
define	GEPOCH		1950.0d0		# Epoch of definition

# AST_GALTOEQ -- Convert galactic coordinates (1950) to equatorial coordinates.

procedure ast_galtoeq (lii, bii, ra, dec, epoch)

double	lii		# Galactic longitude (degrees)
double	bii		# Galactic latitude (degrees)
double	ra		# Right ascension (hours)
double	dec		# Declination (degrees)
double	epoch		# Epoch of coordinates

double	ao, bo, ap, bp, a1, b1, a2, b2

begin
	ao = DEGTORAD (LO)
	bo = DEGTORAD (BO)
	ap = DEGTORAD (LP)
	bp = DEGTORAD (BP)
	a1 = DEGTORAD (lii)
	b1 = DEGTORAD (bii)

	call ast_coord (ao, bo, ap, bp, a1, b1, a2, b2)

	a2 = mod (24.0d0 + RADTODEG(a2) / 15.0d0, 24.0d0)
	b2 = RADTODEG (b2)

	# Precess the coordinates
	call ast_precess (a2, b2, GEPOCH, ra, dec, epoch)
end
