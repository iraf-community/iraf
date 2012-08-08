include	<math.h>


# T_AIRMASS -- Compute the airmass at a given elevation above the horizon.
# Airmass formulation from Allen "Astrophysical Quantities" 1973 p.125,133.

procedure t_airmass()

real	elevation, airmass, scale
real	x, radians_per_degree
bool	clgetb()
real	clgetr()
data	radians_per_degree /57.29577951D0/

begin
	# Get elevation in either degrees or radians and the scale factor
	# for the Earth's atmosphere.
	elevation = clgetr ("elevation")
	if (!clgetb ("radians"))
	    elevation = elevation / radians_per_degree
	scale = clgetr ("scale")

	x  = scale * sin (elevation)
	airmass = sqrt (x**2 + 2*scale + 1) - x

	call printf ("airmass %.5g at an elevation of ")
	    call pargr (airmass)
	call printf ("%.5g degrees (%.5g radians) above horizon\n")
	    call pargr (elevation * radians_per_degree)
	    call pargr (elevation)

	# Store airmass back in a parameter so that it can be accessed from
	# the CL.
	call clputr ("airmass", airmass)
end


# AIRMASS -- Compute airmass from DEC, LATITUDE and HA

# Airmass formulation from Allen "Astrophysical Quantities" 1973 p.125,133.
# and John Ball's book on Algorithms for the HP-45

double procedure airmass (ha, dec, lat)

double  ha, dec, lat, cos_zd, x

define  SCALE   750.0d0                 # Atmospheric scale height

begin
        if (IS_INDEFD (ha) || IS_INDEFD (dec) || IS_INDEFD (lat))
            call error (1, "Can't determine airmass")

        cos_zd = sin(DEGTORAD(lat)) * sin(DEGTORAD(dec)) +
                 cos(DEGTORAD(lat)) * cos(DEGTORAD(dec)) * cos(DEGTORAD(ha*15.))

        x  = SCALE * cos_zd

        return (sqrt (x**2 + 2*SCALE + 1) - x)
end


# AIRMASSX -- Compute airmass from DEC, LATITUDE, HA, and SCALE

# Airmass formulation from Allen "Astrophysical Quantities" 1973 p.125,133.
# and John Ball's book on Algorithms for the HP-45

double procedure airmassx (ha, dec, lat, scale)

double	ha			#I the input hour angle in hours
double	dec			#I the input declination in degrees
double	lat			#I the input latitude in degrees
double	scale			#I the atmospheric scale height

double cos_zd, x

#define	SCALE	750.0d0			# Atmospheric scale height

begin
	if (IS_INDEFD (ha) || IS_INDEFD (dec) || IS_INDEFD (lat))
	    call error (1, "Can't determine airmass")

	cos_zd = sin(DEGTORAD(lat)) * sin(DEGTORAD(dec)) +
		 cos(DEGTORAD(lat)) * cos(DEGTORAD(dec)) * cos(DEGTORAD(ha*15.))

	x  = scale * cos_zd

	return (sqrt (x**2 + 2.0d0 * scale + 1.0d0) - x)
end
