include <mach.h>
include	<math.h>

# Definition of system
define	LONGNCP		123.00d0		# Longtitude of NCP
define	RAGPOLE		192.25d0		# RA Galactic Pole (12:49)
define	DECGPOLE	27.4d0			# DEC Galactic Pole (27:24)
define	GEPOCH		1950.0d0		# Epoch of definition

# AST_GALACTIC -- Convert equatorial coordinates to galactic coordinates.
# Input coordinates and epoch are changed to epoch of galactic coordinates.

procedure ast_galactic (ra, dec, epoch, lii, bii)

double	ra		# Right ascension (hours)
double	dec		# Declination (degrees)
double	epoch		# Epoch of coordinates
double	lii		# Galactic longitude (degrees)
double	bii		# Galactic latitude (degrees)

double	rar, decr, drar, cosdecg, sindecg, cosdecr, x, y, z, r, temp

begin
	# Precess the coordinates to 1950.0
	call ast_precess (ra, dec, epoch, rar, decr, double (GEPOCH))

	# Precompute the necessary constants.
	drar = DEGTORAD (15.0d0 * rar - RAGPOLE) 
	cosdecg = cos (DEGTORAD (DECGPOLE))
	sindecg = sin (DEGTORAD(DECGPOLE))
	cosdecr = cos (DEGTORAD (decr))

	# Compute the tansformation equations
	x = cosdecr * cos (drar)
	y =  cosdecr * sin (drar)
	z = sin (DEGTORAD (decr))
	temp = z * cosdecg - x * sindecg 
	z = z * sindecg + x * cosdecg 
	x = temp
	r = sqrt (x * x + y * y)

	# Compute lii and bii and convert to degrees.
	if (r < EPSILOND)
	    lii = 0.0d0
	else
	    lii = DEGTORAD (LONGNCP)  + atan2 (-y, x)
	if (lii < 0.0d0)
	    lii = lii + TWOPI
	bii = atan2 (z, r)
	lii = RADTODEG (lii)
	bii = RADTODEG (bii)
end
