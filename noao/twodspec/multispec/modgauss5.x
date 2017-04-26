include "ms.h"

# MOD_GAUSS5 -- Set GAUSS5 model profiles and ranges.
#
# This routine can be speeded up with look up tables for a and exp(-z).

define	ZMIN	0		# Issue warning if z < ZMIN
define	ZMAX	10		# The profile values are zero for z > ZMAX

procedure mod_gauss5 (ms, lower, profiles, ranges, len_profile, nspectra)

pointer	ms					# MULTISPEC data structure
real	lower					# Lower limit of profiles
real	profiles[len_profile, nspectra, ARB]	# The profiles to be set
						# The third dim must be >= 5
real	ranges[nspectra, LEN_RANGES]		# The ranges to be set
int	len_profile				# The length of each profile
int	nspectra				# The number of spectra

int	i, j, warn
real	dx, dx2, y, z
real	x1, a, s, s0, s1, s2, s3, profile
real	dIdx0, dIdI0, dIds0, dIds1, dIds2
real	dydx0, dzdx0

begin
	# First set the ranges array.
	call set_ranges (ms, lower, ranges, nspectra)

	# The model quantity x1 is set to 1/4 the profile length.
	# This could someday become a model parameter.
	x1 = len_profile / 4

	# For each spectrum and each point in the profile set the
	# profile/derivative values for the 5 Gauss5 parameters.

	warn = YES
	do i = 1, nspectra {
	    s0 = PARAMETER(ms, S0, i)
	    s1 = PARAMETER(ms, S1, i)
	    s2 = PARAMETER(ms, S2, i)
	    do j = 1, len_profile {
	        dx = ranges[i, DX_START] + j - 1
	        dx2 = dx * dx
	        a = 1 / sqrt (dx2 + x1 ** 2)
	        y = a * dx
		if (y < 0)
		    s3 = s2 - s1
		else
		    s3 = s2 + s1
	        s = s0 + y * s3
	        z = s * dx2
	        if (z < ZMIN) {
		    # Issue warning only once.
		    if (warn == YES) {
		        call printf ("WARNING: mod_gauss5 error.\n")
			warn = NO
		    }
		}
	        if (z < ZMAX) {
		    profile = exp(-z)
		    dydx0 = -(a ** 3) * (x1 ** 2)
		    dzdx0 = -2 * s * dx + dydx0 * s3 * dx2
		    dIdI0 = profile
		    dIdx0 = -dzdx0 * profile
		    dIds0 = -dx2 * profile
		    dIds1 = -dx2 * y * profile
		    dIds2 = dIds1
		    if (y < 0)
			dIds1 = -dIds1

		    profiles[j,i,I0_INDEX] = dIdI0
		    profiles[j,i,X0_INDEX] = dIdx0
		    profiles[j,i,S0_INDEX] = dIds0
		    profiles[j,i,S1_INDEX] = dIds1
		    profiles[j,i,S2_INDEX] = dIds2
		} else {
		    profiles[j,i,I0_INDEX] = 0.
		    profiles[j,i,X0_INDEX] = 0.
		    profiles[j,i,S0_INDEX] = 0.
		    profiles[j,i,S1_INDEX] = 0.
		    profiles[j,i,S2_INDEX] = 0.
		}
	    }
	}
end

# CONSTRAIN_GAUSS5 -- Apply constraints to the solution vector for GAUSS5.
#
# The constraints are:
#
#    DI0 > -I0/2, abs(DX0) < MAX_DX0, DS0 > -S0/2,
#    (S0+DS0)+-(S1+DS1)+(S2+DS2) > 0.
#
# where DI0, DX0, DS0, DS1, DS2 are the solution corrections and I0, S0,
# S1, and S2 are the original parameter values.  The constraints on DI0,
# and DS0 insure that I0 and S0 remain positive and the last constraint
# insures that (S0+-S1+S2) always remains positive so that the profiles
# always decrease from the center.

define	MAX_DX0		1.		# Maximum change in position

procedure constrain_gauss5 (ms, solution, nspectra, nparams)

pointer	ms
real	solution[nspectra, nparams]
int	nspectra
int	nparams

int	i
real	max_delta
real	sa, sb, dsa, dsb, scalea, scaleb, scale

begin
	do i = 1, nspectra {

	    # Limit any decrease in I0 to 1/2 I0.  This insures I0 > 0.
	    if (solution[i, I0_INDEX] != 0.) {
		max_delta = PARAMETER(ms, I0, i) / 2.
		solution[i, I0_INDEX] = max (solution[i, I0_INDEX], -max_delta)
	    }

	    # Limit the correction for X0 to MAX_DX0.
	    # Set the position to INDEF if it falls outside the image.
	    if (solution[i, X0_INDEX] != 0.) {
		max_delta = MAX_DX0
		solution[i, X0_INDEX] = max (solution[i, X0_INDEX], -max_delta)
		solution[i, X0_INDEX] = min (solution[i, X0_INDEX], max_delta)
	    }

	    # Limit any decrease in S0 to 1/2 of S0. This insures S0 > 0.
	    if (solution[i, S0_INDEX] != 0.) {
		max_delta = PARAMETER(ms, S0, i) / 2.
		solution[i, S0_INDEX] = max (solution[i, S0_INDEX], -max_delta)
	    }

	    # Limit the final S0+-S1+S2 to be positive.  If the value would be
	    # negative scale the correction vector (ds0, ds1, ds2) to make
	    # the final S0+-S1+S2 be 1/2 the old value.
	    if ((solution[i,S0_INDEX] != 0.) || (solution[i,S1_INDEX] != 0.) ||
		(solution[i,S2_INDEX] != 0.)) {
		sa = PARAMETER(ms, S0, i) + PARAMETER(ms, S1, i) +
		    PARAMETER(ms, S2, i)
		sb = PARAMETER(ms, S0, i) - PARAMETER(ms, S1, i) +
		    PARAMETER(ms, S2, i)
		dsa = solution[i, S0_INDEX] + solution[i, S1_INDEX] +
		    solution[i, S2_INDEX]
		dsb = solution[i, S0_INDEX] - solution[i, S1_INDEX] +
		    solution[i, S2_INDEX]
		if (sa + dsa < 0.)
		    scalea = -sa / 2 / dsa
		else
		    scalea = 1.
		if (sb + dsb < 0.)
		    scaleb = -sb / 2 / dsb
		else
		    scaleb = 1.
		scale = min (scalea, scaleb)
		solution[i, S0_INDEX] = scale * solution[i, S0_INDEX]
		solution[i, S1_INDEX] = scale * solution[i, S1_INDEX]
		solution[i, S2_INDEX] = scale * solution[i, S2_INDEX]
	    }
	}
end
