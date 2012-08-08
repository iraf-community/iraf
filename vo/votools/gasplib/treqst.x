define	ARCSEC_PER_RADIAN	206264.8062470964d0
# TREQST -- Procedure to convert RA and Dec to standard coordinates
#	    given the plate centre.

procedure  treqst (plate_centre_ra, plate_centre_dec,
     		   object_ra, object_dec, xi_object, eta_object)

double		plate_centre_ra	      #i: (radians)	
double		plate_centre_dec      #i: (radians)	
double		object_ra             #i: (radians)
double		object_dec            #i: (radians)		
double		xi_object	      #o: standard coordinate (rad)
double		eta_object	      #o: standard coordinate (rad)	

double		div	

begin
	# Find divisor
	div=(dsin(object_dec)*dsin(plate_centre_dec)+
     	     dcos(object_dec)*dcos(plate_centre_dec)*
     	     dcos(object_ra-plate_centre_ra))

	# Compute standard coords and convert to arcsec

	xi_object = dcos(object_dec)*dsin(object_ra-plate_centre_ra)*
     		  ARCSEC_PER_RADIAN/div

	eta_object = (dsin(object_dec)*dcos(plate_centre_dec)-
     		    dcos(object_dec)*dsin(plate_centre_dec)*
     		    dcos(object_ra-plate_centre_ra))*
     		    ARCSEC_PER_RADIAN/div

end
