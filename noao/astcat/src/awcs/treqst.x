# This routine was copied from the stsdas$pkg/analysis/gasp/gasplib/
# directory. See stsdas$copyright.stsdas  for copyright restrictions.
# 

define	ARCSEC_PER_RADIAN	206264.8062470964d0

# TREQST -- Procedure to convert RA and Dec to standard coordinates
# given the plate centre.

procedure  treqst (plate_centre_ra, plate_centre_dec, object_ra, object_dec,
	xi_object, eta_object)

double	plate_centre_ra	      #I plate ra center (radians)	
double	plate_centre_dec      #I plate dec center (radians)	
double	object_ra             #I object ra center (radians)
double	object_dec            #I object dec center (radians)		
double	xi_object	      #O object xi standard coordinate (arcsecs)
double	eta_object	      #O object eta standard coordinate (arcsecs)	

#double		div	
double	ra, cosra, sinra, cosdec, sindec, cosd0, sind0, cosdist

begin
	ra = object_ra - plate_centre_ra
	cosra = cos (ra)
	sinra = sin (ra)
	cosdec = cos (object_dec)
	sindec = sin (object_dec)
	cosd0 = cos (plate_centre_dec)
	sind0 = sin (plate_centre_dec)
	cosdist = sindec * sind0 + cosdec * cosd0 * cosra
	xi_object = cosdec * sinra * ARCSEC_PER_RADIAN / cosdist
	eta_object = (sindec * cosd0 - cosdec * sind0 * cosra) *
	    ARCSEC_PER_RADIAN / cosdist

#	# Find the divisor.
#	div = (sin(object_dec) * sin(plate_centre_dec) +
#     	       cos(object_dec) * cos(plate_centre_dec) *
#     	       cos(object_ra -plate_centre_ra))
#
#	# Compute standard coords and convert to arcsec
#	xi_object = cos(object_dec) * sin(object_ra-plate_centre_ra) *
#     	    ARCSEC_PER_RADIAN/div
#	eta_object = (sin(object_dec) * cos(plate_centre_dec) -
#     	    cos(object_dec) * dsin(plate_centre_dec) *
#     	    cos(object_ra - plate_centre_ra)) *
#     	    ARCSEC_PER_RADIAN/div

end
