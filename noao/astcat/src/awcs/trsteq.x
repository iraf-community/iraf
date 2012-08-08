# This routine was copied from stsdas$pkg/asnalysis/gasp/gasplib/. See
# stsdas$copyright.stsdas  for copyright restrictions.
# 
include <math.h>

define  ARCSEC_PER_RADIAN       206264.8062470964d0

# TRSTEQ -- Procedure to compute the RA and DEC from the standard coordinates
# given the plate centre.

procedure trsteq (plate_centre_ra, plate_centre_dec, xi, eta, ra, dec)

double		plate_centre_ra		#I plate center ra (radians)
double		plate_centre_dec	#I plate center dec (radians)
double		xi			#I xi standard coordinate (arcsec)
double		eta			#I eta standard coordinate (arcsec)
double		ra			#O ra (radians)
double		dec			#O dec (radians)

#double		object_xi, object_eta, numerator, denominator
double		object_xi, object_eta, x, y, z

begin
	# Convert from arcseconds to radians.
	object_xi = xi/ARCSEC_PER_RADIAN
	object_eta = eta/ARCSEC_PER_RADIAN

	# Convert to RA and Dec
	x = cos (plate_centre_dec) - object_eta * sin (plate_centre_dec)
	y = object_xi
	z = sin (plate_centre_dec) + object_eta * cos (plate_centre_dec)

	if (x == 0.0d0 && y == 0.0d0)
	    ra = 0.0d0
	else
	    ra = atan2 (y, x)
	dec = atan2 (z, sqrt (x * x + y * y))
	ra = ra + plate_centre_ra
	if (ra < 0.0d0)
	    ra = ra + DTWOPI
	else if (ra > DTWOPI)
	    ra = ra - DTWOPI

##	numerator = object_xi / dcos(plate_centre_dec)
#	numerator = object_xi
#
##	denominator = 1.0d0 - object_eta * dtan(plate_centre_dec)
#	denominator = cos (plate_centre_dec) - 
#	    object_eta * sin (plate_centre_dec)
#	ra = atan2 (numerator,denominator) + plate_centre_ra
#	if (ra < 0.0d0)
#	    ra = ra + DTWOPI
#	else if (ra > DTWOPI)
#	    ra = ra - DTWOPI
#
##	numerator = dcos(ra-plate_centre_ra) *
##     	    (object_eta + dtan(plate_centre_dec))
#	numerator = cos (ra - plate_centre_ra) *
#     	    (cos (plate_centre_dec) * object_eta + sin (plate_centre_dec))
##	denominator = 1.0d0 - object_eta * dtan(plate_centre_dec)
#	denominator = cos (plate_centre_dec) - object_eta *
#	    sin (plate_centre_dec)
#	dec = atan2 (numerator, denominator)
end
