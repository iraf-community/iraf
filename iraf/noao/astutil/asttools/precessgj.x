include	<math.h>

# PRECESSGJ -- Precess astronomical coordinates from epoch1 to epoch2.
# Original IRAF/FORTRAN by G. Jacoby (NOAO).
# Modified by by F. Valdes for IRAF/SPP (NOAO), March 1986

procedure precessgj (ra1, dec1, epoch1, ra2, dec2, epoch2)

double	ra1, dec1, epoch1	# Input coordinates
double	ra2, dec2, epoch2	# Output coordinates

double	t, tau, theta, zeta, z, ra, dec, a, ap, test
bool	fp_equald()

begin
	if (fp_equald (epoch1, epoch2)) {
	    ra2 = ra1
	    dec2 = dec1
	    return
	}

	t = (epoch2 - epoch1) / 100.
	tau = (epoch1 - 1850.) / 100.

	theta = t * ((2005.11d0 - 0.85 * tau) - t * (0.43 + t * 0.041)) / 3600.
	zeta  = t * ((2303.55d0 + 1.40 * tau) + t * (0.30 + t * 0.017)) / 3600.
	z = zeta + t * t * 0.79 / 3600.

	ra = DEGTORAD (ra1 * 15.0)
	dec = DEGTORAD (dec1)
	theta = DEGTORAD (theta)
	zeta = DEGTORAD (zeta)
	z = DEGTORAD (z)

	a = ra + zeta
	dec2 = asin (cos(dec) * cos(a) * sin(theta) + sin(dec) * cos(theta))
	ap = asin (cos(dec) * sin(a) / cos(dec2))
	test = (cos(dec)*cos(a)*cos(theta) - sin(dec)*sin(theta)) / cos(dec2)

	if (test < 0.)
	    ap = PI - ap
	ra2 = ap + z
	if (ra2 < 0.)
	    ra2 = ra2 + TWOPI

	ra2 = RADTODEG (ra2) / 15.0
	dec2 = RADTODEG (dec2)
end
