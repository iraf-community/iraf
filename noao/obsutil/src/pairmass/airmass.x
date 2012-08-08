include	<math.h>

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
