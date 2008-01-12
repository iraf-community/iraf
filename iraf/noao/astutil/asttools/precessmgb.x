include	<math.h>

# PRECESSMGB -- Calculate the apparent change in right ascension and
# declination between two epochs.  Corrections are made for precession,
# aberration, and some terms of nutation.
#
# Based on the work of R. N. Manchester, M. A. Gordon, J. A. Ball (NRAO)
#     January 1970 (DOPSET, IBM360) 
# Modified by E. Anderson (NOAO) April 1985 (DOPSET, VMS/VAX)
# Converted to IRAF by F. Valdes (NOAO) March 1986.

procedure precessmgb (ra1, dec1, epoch1, ra2, dec2, epoch2)

double	ra1, dec1, epoch1	# First epoch coordinates
double	ra2, dec2, epoch2	# Second epoch coordinates

double	ra, dec, epoch, epoch3, dc
bool	fp_equald()

begin
	# Return if the epochs are the same.

	if (fp_equald (epoch1, epoch2)) {
	    ra2 = ra1
	    dec2 = dec1
	    return
	}

	ra = ra1
	dec = dec1
	epoch = epoch1

	# Check if epoch1 is the beginning of a year.  If not make correction
	# to the beginning of the year.

	if (epoch - int (epoch) > 1E-6) {
	    epoch3 = epoch
	    epoch = int (epoch)
	    call mgb_precess (ra, dec, epoch, ra2, dec2, epoch3, dc)
	    ra = ra - (ra2 - ra)
	    dec = dec - (dec2 - dec)
	}

	# Precess to epoch2.

	call mgb_precess (ra, dec, epoch, ra2, dec2, epoch2, dc)
end


# MGB_PRECESS -- Calculate the apparent change in right ascension and
# declination between two epochs.  The first epoch is assumed to be the
# beginning of a year (eg 1950.0).  Also calculate the equation of the
# equinoxes (in minutes of time) which may be added to the mean siderial
# time to give the apparent siderial time (AENA-469).  Corrections are
# made for precession, aberration, and some terms of nutation.
#    AENA - The American Ephemeris and Nautical Almanac (the blue book)
#    ESE  - The explanatory suplement to AENA (the green book)

procedure mgb_precess (ra1, dec1, epoch1, ra2, dec2, epoch2, dc)

double	ra1, dec1, epoch1	# First epoch coordinates
double	ra2, dec2, epoch2	# Second epoch coordinates
double	dc			# Siderial time correction

double	ra, dec, delr, deld
double	t1, t2, nday2
double	theta, zeta, z, am, an, al
double	csr, snr, csd, snd, tnd, csl, snl
double	omega, dlong, doblq
bool	fp_equald()

begin
	# Check if epochs are the same.

	if (fp_equald (epoch1, epoch2)) {
	    ra2 = ra1
	    dec2 = dec1
	    return
	}

	# Convert input coordinates to radians.

	ra = DEGTORAD (ra1 * 15.)
	dec = DEGTORAD (dec1)

	# Compute sines, cosines, and tangents.

	csr = cos (ra)
	snr = sin (ra)
	csd = cos (dec)
	snd = sin (dec)
	tnd = snd / csd

	# T1 is the time from 1900 to epoch1 (centuries),
	# t2 is the time from epoch1 to epoch2 (centuries), and
	# nday2 is the number of days since the beginning of the year
	# for epoch2.  The number of ephemeris days in a tropical
	# year is 365.2421988.

	t1 = (epoch1 - 1900.) / 100.
	t2 = (epoch2 - epoch1) / 100.
	nday2 = (epoch2 - int (epoch2)) * 365.2421988

	# Theta, zeta, and z are precessional angles from ESE-29 (arcseconds).

	theta = t2 * ((2004.682 - 0.853 * t1) - t2 * (0.426 + t2 * 0.042))
	zeta = t2 * ((2304.250 + 1.396 * t1) + t2 * (0.302 + t2 * 0.018))
	z = zeta + 0.791 * t2 ** 2

	# am and an are the M and N precessional numbers (see AENA-50, 474)
	# (radians) and alam is an approximate mean longitude for the sun
	# (AENA-50) (radians)

	am = DEGTORAD ((zeta + z) / 3600.)
	an = DEGTORAD (theta / 3600.)
	al = DEGTORAD (0.985647 * nday2 + 278.5)

	snl = sin (al)
	csl = cos (al)

	# Delr and deld are the annual aberation term in ra and dec (radians)
	# (ESE-47,48)  (0.91745051   cos (obliquity of ecliptic))
	# (0.39784993   sin (obliquity of ecliptic))
	# (-9.92413605E-5   K   20.47 ARCSECONDS   constant of aberration)
	# (ESE) plus precession terms (see AENA-50 and ESE-39).

	delr = -9.92413605e-5 * (snl * snr + 0.91745051 * csl * csr) / csd +
		am + an * snr * tnd
	deld = -9.92413605e-5 * (snl * csr * snd - 0.91745051 * csl * snr *
		snd + 0.39784993 * csl * csd) + an * csr

	# The following calculates the nutation (approximately) (ESE-41,45)
	# Omega is the angle of the first term of nutation (ESE-44)
	# (approximate formula) (radians).
	# Dlong is the nutation in longitude (delta-psi) (radians)
	# Doblq is the nutation in obliquity (delta-epsilon) (radians)

	omega = DEGTORAD (259.183275 - 1934.142 * (t1 + t2))
	dlong = -8.3597e-5 * sin (omega)
	doblq =  4.4678e-5 * cos (omega)

	# Add nutation into delr and deld (ESE-43).

	delr = delr + dlong * (0.91745051 + 0.39784993 * snr * tnd) -
		csr * tnd * doblq
	deld = deld + 0.39784993 * csr * dlong + snr * doblq

	# Compute new position and the equation of the equinoxes
	# (dc in minutes of tim, ESE-43)

	ra2 = ra1 + RADTODEG (delr / 15.)
	dec2 = dec1 + RADTODEG (deld)
	dc = dlong * 210.264169
end
