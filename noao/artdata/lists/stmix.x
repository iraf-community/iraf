include	<math.h>
include "starlist.h"

# ST_ESMIX -- Compute the percentage of elliptical galaxies.

procedure st_esmix (egal, nstar, esmix, seed)

int	egal[ARB]		# array of types
int	nstar			# number of objects
real	esmix			# fraction of elliptical galaxies
long	seed			# seed for random number generator

int	i
real	fraction
real	urand()

begin
	do i = 1, nstar {
	    fraction = urand (seed)
	    if (fraction <= esmix)
	        egal[i] = ST_DEVAUC
	    else
	        egal[i] = ST_EXP
	}
end


# ST_ROUND -- Compute an array of roundness parameters.
# For ellipical models use a uniform distribution of axial ratios.
# For spiral models use a uniform inclination distribution.  Compute
# the axial ratio with a .1 flatness parameter from Holmberg (Stars
# and Stellar Systems).  Then add cosecant absorption factor to the
# the magnitudes with a limit of 10 in the cosecant.

procedure st_round (egal, mag, round, nstars, ar, alpha, seed)

int	egal[ARB]		# array of galaxy types
real	mag[ARB]		# magnitudes
real	round[ARB]		# array of roundness values
int	nstars			# number of stars
real	ar			# minimum roundness value
real	alpha			# absorption coefficent
long	seed			# seed for the random number generator

int	i
real	dr, s, urand()

begin
	dr = (1. - ar)
	do i = 1, nstars {
	    if (egal[i] == ST_DEVAUC)
	        round[i] = ar + dr * urand (seed)
	    else {
		s = sin (HALFPI * urand (seed))
		round[i] = sqrt (s**2 * .99 + .01)
		mag[i] = mag[i] + alpha * (1 / max (0.1, s) - 1) / 9.
	    }
	}

end


# ST_PHI -- Compute an array of position angles.

procedure st_phi (phi, nstars, seed)

real	phi[ARB]		# array of position angles
int	nstars			# number of stars
long	seed			# seed for random number generator

int	i
real	urand()

begin
	do i = 1, nstars
	    phi[i] = urand (seed)
	call amapr (phi, phi, nstars, 0.0, 1.0, 0.0, 360.0)
end


# ST_DIAMETERS -- Compute the effective diameters of the galaxies based
# on their magnitudes.  The relation used is from Holmberg (Stars and
# Stellar Systems).  A uniform dispersion of 20% is added.

procedure st_diameters (mag, egal, axis, nstars, minmag, maxmag, eradius,
	sradius, seed)

real	mag[ARB]		# input array of magnitudes
int	egal[ARB]		# array of galaxy types
real	axis[ARB]		# output array of diameters
int	nstars			# number of stars
real	minmag			# minimum magnitude
real	maxmag			# maximum magnitude
real	eradius			# maximum elliptical radius
real	sradius			# maximum spiral radius
long	seed			# seed for random number generator

int	i
real	urand()

begin
	do i = 1, nstars {
	    if (egal[i] == ST_DEVAUC)
	        axis[i] = eradius * 10.0 ** ((minmag - mag[i]) / 6.)
	    else
	        axis[i] = sradius * eradius * 10.0 ** ((minmag - mag[i]) / 6.)
	    axis[i] = axis[i] * (0.8 + 0.4 * urand (seed))
	}
end


# ST_ZDIAMETERS -- Compute the effective diameters of the galaxies based
# on their magnitudes and redshift. The relation used is that the redshift
# is proportional to the luminousity and the diameters include the
# factor of (1+z)**2.  A uniform dispersion of 50% is added.

procedure st_zdiameters (mag, egal, axis, nstars, minmag, maxmag,
	z, eradius, sradius, seed)

real	mag[ARB]		# input array of magnitudes
int	egal[ARB]		# array of galaxy types
real	axis[ARB]		# output array of diameters
int	nstars			# number of stars
real	minmag			# minimum magnitude
real	maxmag			# maximum magnitude
real	z			# minumum redshift
real	eradius			# maximum elliptical radius
real	sradius			# maximum spiral radius
long	seed			# seed for random number generator

int	i
real	z0, z1, urand()

begin
	z0 = z / (1 + z) ** 2
	do i = 1, nstars {
	    z1 = z * 10.0 ** (-0.2 * (minmag - mag[i]))
	    if (egal[i] == ST_DEVAUC)
	        axis[i] = eradius * z0 * (1 + z1) ** 2 / z1
	    else
	        axis[i] = sradius * eradius * z0 * (1 + z1) ** 2 / z1
	    axis[i] = axis[i] * (0.5 + urand (seed))
	}
end
