include	<imhdr.h>
include	"extract.h"
include	"apertures.h"
include	"exio.h"

# EX_MVSUM1 -- Compute a moving sum of aperture profiles centered on the
# specified line or column (except at the ends of the image).  This done
# using a working array which maintains all profiles in the sum
# indexed by a modulus.  This procedure is called sequentially through
# the image so the first line or column initializs the sum, each image
# line or column is accessed only once, and the next sum is found by
# subtracting the first profiles in the sum (previously stored) and
# adding the next profiles in the sum.  The profiles are extracted from
# the image and centered by the procedure EX_GPROFS.

procedure ex_mvsum1 (ex, im, vec, aps, naps, pstart, pcen, pend, profiles,
	bckgrnd, len_profs)

pointer	ex				# Pointer to extraction parameters
pointer	im				# EXIO pointer
int	vec				# Image vec
pointer	aps[naps]			# Apertures
int	naps				# Number of apertures
int	pstart[naps]			# Starting profile index
real	pcen[naps]			# Center of data profiles
int	pend[naps]			# Ending profile index
real	profiles[len_profs, ARB]	# Average profiles for vec
real	bckgrnd[len_profs]		# Background profiles
int	len_profs			# Length of profiles

int	i, j, first, last

begin
	if (vec == 1) {
	    first = 1
	    last = EX_NAVG(ex) - 1
	    call aclrr (profiles, len_profs)
	    do i = first, last {
		j = 2 + mod (i - 1, EX_NAVG(ex))
		call ex_gprofs (ex, im, i, aps, naps, pstart, pcen, pend,
		    YES, profiles[1,j], bckgrnd)
	        call aaddr (profiles, profiles[1,j], profiles, len_profs)
	    }
	} else if (vec > (EX_NAVG(ex) - 1) / 2) {
	    if (last < EX_DLEN(im)) {
	        j = 2 + mod (first - 1, EX_NAVG(ex))
	        call asubr (profiles, profiles[1,j], profiles, len_profs)
	        first = first + 1

	        last = last + 1
	        j = 2 + mod (last - 1, EX_NAVG(ex))
		call ex_gprofs (ex, im, last, aps, naps, pstart, pcen, pend,
		    YES, profiles[1,j], bckgrnd)
	        call aaddr (profiles, profiles[1,j], profiles, len_profs)
	    }
	}
end


# EX_MVSUM2 -- Compute a moving sum of aperture profiles which trails the
# specified line or column (except at the start of the image).  The line
# or column being extracted is excluded from the sum and may be replaced
# later after removing deviant points before being added into the sum.  A
# working array maintains all profiles in the sum indexed by a modulus.
# This procedure is called sequentially through the image so the first
# line or column initializs the sum, each image line or column is
# accessed only once, and the next sum is found by subtracting the first
# profiles in the sum (previously stored) and adding the next profiles in
# the sum.  The profiles are extracted from the image and centered by the
# procedure EX_GPROFS.

procedure ex_mvsum2 (ex, im, vec, aps, naps, pstart, pcen, pend, profiles,
	bckgrnd, len_profs)

pointer	ex				# Pointer to extraction parameters
pointer	im				# EXIO pointer
int	vec				# Image vec
pointer	aps[naps]			# Apertures
int	naps				# Number of apertures
int	pstart[naps]			# Starting profile index
real	pcen[naps]			# Center of data profiles
int	pend[naps]			# Ending profile index
real	profiles[len_profs, ARB]	# Average profiles for vec
real	bckgrnd[len_profs]		# Background profiles
int	len_profs			# Length of profiles

int	i, j, first, last

begin
	if (vec == 1) {
	    first = 1
	    last = EX_NAVG(ex)
	    call aclrr (profiles, len_profs)
	    do i = first, last {
		j = 2 + mod (i - 1, EX_NAVG(ex))
		call ex_gprofs (ex, im, i, aps, naps, pstart, pcen, pend,
		    YES, profiles[1,j], bckgrnd)
		if (i != vec)
	            call aaddr (profiles, profiles[1,j], profiles, len_profs)
	    }
	} else if (vec > EX_NAVG(ex)) {
	    j = 2 + mod (first - 1, EX_NAVG(ex))
	    call asubr (profiles, profiles[1,j], profiles, len_profs)
	    first = first + 1
	    j = 2 + mod (last - 1, EX_NAVG(ex))
	    call aaddr (profiles, profiles[1,j], profiles, len_profs)
	    last = last + 1

	    j = 2 + mod (vec - 1, EX_NAVG(ex))
	    call ex_gprofs (ex, im, vec, aps, naps, pstart, pcen, pend,
		YES, profiles[1,j], bckgrnd)
	} else {
	    j = 2 + mod (vec - 2, EX_NAVG(ex))
	    call aaddr (profiles, profiles[1,j], profiles, len_profs)
	    j = 2 + mod (vec - 1, EX_NAVG(ex))
	    call asubr (profiles, profiles[1,j], profiles, len_profs)
	}
end


# EX_REPLACE -- Replace data profiles in the working array used by
# EX_MVSUM2 after removing deviant points.  The data profiles must be
# recentered before being entered into the working array at the proper
# point computed by the modulus.

procedure ex_replace (ex, line, pstart, pcen, pend, pnrep, naps, data, avg,
	len_profs)

pointer	ex				# Pointer to extraction parameters
int	line				# Image line
int	pstart[naps]			# Starting profile index
real	pcen[naps]			# Center of data profiles
int	pend[naps]			# Ending profile index
int	pnrep[naps]			# Number of replaced pixels
int	naps				# Number of apertures
real	data[len_profs]			# Cleaned data profiles
real	avg[len_profs,ARB]		# Average profiles
int	len_profs			# Length of profiles

int	i, j

begin
	do i = 1, naps {
	    if ((pstart[i] == 0) || (pnrep[i] == 0))
		next
	    j = 2 + mod (line - 1, EX_NAVG(ex))
	    call ex_recen (ex, pstart[i], pcen[i], pend[i], 1,
		data, avg[1,j], -1)
	}
end
