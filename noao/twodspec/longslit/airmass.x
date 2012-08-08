include	<math.h>

# IMG_AIRMASS -- Get or compute the image airmass from the image header.
# If the airmass cannot be determined from header then INDEF is returned.
#
# Airmass formulation from Allen "Astrophysical Quantities" 1973 p.125,133
# and John Ball's book on Algorithms for the HP-45.

real procedure img_airmass (im)

pointer	im			# IMIO pointer

real	airmass, zd, ha, ra, dec, st, latitude, coszd, scale, x

int	imaccf()
real	imgetr()
errchk	imgetr()

data	scale/750.0/		# Atmospheric scale height approx

begin
	# If the airmass is in the header return its value.

	if (imaccf (im, "airmass") == YES)
	    return (imgetr (im, "airmass"))

	# Compute zenith distance if not defined.

	iferr (zd = imgetr (im, "zd")) {
	    
	    # Compute hour angle if not defined.

	    iferr (ha = imgetr (im, "ha")) {
		st = imgetr (im, "st")
		ra = imgetr (im, "ra")
		ha = st - ra
		call imaddr (im, "ha", ha)
	    }
	
	    dec = imgetr (im, "dec")
	    latitude = imgetr (im, "latitude")

	    ha = DEGTORAD (ha) * 15
	    dec = DEGTORAD (dec)
	    latitude = DEGTORAD (latitude)
	    coszd = sin (latitude) * sin (dec) +
		cos (latitude) * cos (dec) * cos (ha)
	    zd = RADTODEG (acos (coszd))
	    call imaddr (im, "zd", zd)
	}

	# Compute airmass from zenith distance.

	zd = DEGTORAD (zd)
	x = scale * cos (zd)
	airmass = sqrt (x ** 2 + 2 * scale + 1) - x
	call imaddr (im, "airmass", airmass)

	return (airmass)
end
