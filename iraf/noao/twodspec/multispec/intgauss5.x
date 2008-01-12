include "ms.h"

# INT_GAUSS5 -- Interpolate the GAUSS5 profiles between sample lines.
#
# Because calculation of the model profiles from parameter values interpolated
# from the sample lines is very slow the profiles at the sample lines are
# calculated (only when needed) and the profiles are then interpolated.

procedure int_gauss5 (ms, lower, profiles, ranges, len_profile, nspectra,
    nparams, line)

pointer	ms				# MULTISPEC data structure
real	lower				# Lower limit of profiles rel. to center
real	profiles[len_profile, nspectra, nparams, 3] # Model profiles
real	ranges[nspectra, LEN_RANGES, 3]	# Range array for profiles
int	len_profile			# Length of each profile
int	nspectra			# Number of spectra
int	nparams				# Number of parameters
int	line				# Image line to be interpolated to

real	f, x
int	i, a, b, line1, line2

real	cveval()

begin
	# The values of the static variables are used in successive calls
	# to record the state of the interpolation endpoints.  To initialize
	# this routine the value of the first element of the ranges array
	# is checked for the flag INDEFR.  The profiles array must be
	# dimensioned to have three sets of profiles (each set consisting
	# of nspectra * nparams profiles).  The first set is the interpolated
	# profiles, profiles[*,*,*,1], and the other two sets hold the
	# latest profiles from the interpolation endpoint sample lines,
	# profiles[*,*,*,2] and profiles[*,*,*,3].

	# If there is only one sample line then calculate the profiles
	# only once (when the ranges array has been flagged) and return
	# the same profiles for every image line.
	if (MS_NSAMPLES(ms) == 1) {
	    if (IS_INDEFR (ranges[1,1,1])) {
	        call msggauss5 (ms, line1)
	        call mod_gauss5 (ms, lower, profiles, ranges, len_profile,
		    nspectra)
	    }
	    return
	}

	# If there is more than one sample line then interpolation makes
	# sense.  Initialize the interpolation algorithm if the ranges array
	# has been flagged.

	if (IS_INDEFR (ranges[1,1,1])) {
	    call msgparam (ms, I0, 1)
	    call msgparam (ms, X0, 1)
	    call msgfits (ms, X0_FIT)
	    a = 1
	    line1 = 0
	    line2 = 0
	}

	# Find the nearest sample line which is less than the desired
	# image line and is not the last sample line and mark this as
	# endpoint sample line a.  Start from the last endpoint for efficiency.
	# Search forward if the desired image line is greater than the
	# endpoint sample line and backwards otherwise.

	if (line > LINE(ms, a)) {
	    do i = a + 1, MS_NSAMPLES(ms) - 1 {
		if (line > LINE(ms, i))
		    a = i
		else
		    break
	    }
	} else {
	    do i = a, 1, -1 {
		if (line <= LINE(ms, a))
		    a = i
		else
		    break
	    }
	}

	# Since endpoint a is not allowed to be the last sample line then
	# the upper interpolation endpoint is the next sample line.
	b = a + 1

	# Check to see if the new endpoints are different than the previous
	# endpoints.  If so then read the model parameters from the database
	# for the endpoints and evaluate the model profiles.
	if ((line1 == a) && (line2 == b))
	    ;					# Endpoints are the same.
	else if ((line1 == b) && (line2 == a))
	    ;					# Endpoints are the same.
	else if ((line1 == a) && (line2 != b)) {
	    line2 = b				# One endpoint is different.
	    call msggauss5 (ms, line2)
	    call mod_gauss5 (ms, lower, profiles[1,1,1,3], ranges[1,1,3],
		len_profile, nspectra)
	} else if ((line1 == b) && (line2 != a)) {
	    line2 = a				# One endpoint is different.
	    call msggauss5 (ms, line2)
	    call mod_gauss5 (ms, lower, profiles[1,1,1,3], ranges[1,1,3],
		len_profile, nspectra)
	} else if ((line1 != b) && (line2 == a)) {
	    line1 = b				# One endpoint is different.
	    call msggauss5 (ms, line1)
	    call mod_gauss5 (ms, lower, profiles[1,1,1,2], ranges[1,1,2],
		len_profile, nspectra)
	} else if ((line1 != a) && (line2 == b)) {
	    line1 = a				# One endpoint is different.
	    call msggauss5 (ms, line1)
	    call mod_gauss5 (ms, lower, profiles[1,1,1,2], ranges[1,1,2],
		len_profile, nspectra)
	} else {
	    line1 = a				# Both endpoints are different.
	    call msggauss5 (ms, line1)
	    call mod_gauss5 (ms, lower, profiles[1,1,1,2], ranges[1,1,2],
		len_profile, nspectra)
	    line2 = b
	    call msggauss5 (ms, line2)
	    call mod_gauss5 (ms, lower, profiles[1,1,1,3], ranges[1,1,3],
		len_profile, nspectra)
	}

	# Calculate the ranges for the interpolated range array to the
	# interpolated spectra position.
	f = real (line)
	do i = 1, nspectra {
	    x = cveval (CV(ms, X0_FIT, i), f)
	    ranges[i, X_START, 1] = int(x) + lower
	    ranges[i, DX_START, 1] = ranges[i, X_START, 1] - x
	}

	# Do the profile interpolation.
	f = float (line - LINE(ms, line1)) /
	    (LINE(ms, line2) - LINE(ms, line1))
	call profile_interpolation (f, len_profile, nspectra, nparams,
	    profiles, ranges)
end
