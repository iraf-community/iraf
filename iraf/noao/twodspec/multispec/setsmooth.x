include	<imhdr.h>
include	"ms.h"

.help set_smooth Jul84 MULTISPEC
.sh
Procedure set_smooth

     This procedure returns data profiles for the requested image line and model
profiles consisting of the sum of (naverage =) nlines - 1 image line data
profiles surrounding (and excluding) the data image line.

     A buffer of nlines + 1 set of profiles is kept.  The first set of profiles
is used to keep the sum of the nlines - 1 profiles which excludes the current
line of data profiles (thus, the number of lines in the sum = nlines - 1).
The remaining sets of profiles (2 to nlines + 1) contain data profiles for all
the lines in the sum plus the current data line.  The lines are stored in a
cyclic fashion with the buffer line being related to the data line by 

	buffer line = 2 + mod (data line - 1, nlines)

Each data line is read and converted into a set of profiles and put into the
buffer only if it is not already in the buffer.

     The algorithm first checks the state of the previous profiles buffer.
If it would be unchanged then it returns.  Otherwise, it subtracts the profiles
which are not in common with the new set of summation lines from the
sum profiles before replacing those lines in the profiles buffer with new data
profiles.  If the number of vector subtractions exceeds the number of vector
additions to readd the common lines to the sum profiles then the common
profiles are readded instead.  The new data profiles are then obtained from
the image (with procedure msgprofiles) and added, if needed, to the sum
profiles.  Finally, the sum profiles are copied to the model profiles and
the profiles from the profiles buffer corresponding to the requested data
line are copied to the data profiles array.

     This algorithm is maximumally efficient with its imageio.  If the model
lines are requested sequentially through the image then each image data line
will be read only once and each new model line will, on average, require only
one image read, two vector additions, and two vector subtractions.  The
number of vector additions and subtractions is two because the current data
line is excluded from the sum.
.sh
Procedure msgprofiles

     In order to obtain model profiles based on summing the profiles from
a number of neighboring lines, the profiles from each line must be
shifted to the relative profile centers.  The procedure msgprofiles reads
an image line and computes an interpolation function for the line.
The spectra profiles are then extracted using the position interpolation
function to determine the spectra centers in the image line.  The
profiles are aligned to the same relative positions in the profiles
array based on the ranges array.
.endhelp


# SET_SMOOTH -- Set the SMOOTH model profiles.

procedure set_smooth (ms, im, line, ranges, profiles, coeff,
    len_prof, nspectra, nlines, data, model)

pointer	ms					# MULTISPEC data structure
int	im					# IMIO image descriptor
int	line					# Image line to be modeled
real	ranges[nspectra, LEN_RANGES]		# Ranges array
real	profiles[len_prof, nspectra, ARB]	# Profiles array
real	coeff[ARB]				# Image interpolator coeffs.
int	len_prof				# Length of profiles
int	nspectra				# Number of spectra
int	nlines					# Number of lines in average
real	data[len_prof, nspectra]		# Data profiles for line
real	model[len_prof, nspectra]		# SMOOTH model profiles

int	i, j, navg
int	len_profs, last_line, last_start, last_end, line_start, line_end
pointer	k

data	len_profs/0/

begin
	# Initialize
	if (len_profs == 0) {
	    navg = nlines - 1
	    len_profs = len_prof * nspectra
	    last_line = 0
	    last_start = -nlines
	    last_end = 0
	}

	# Determine range of lines for averaging.

	# The following is to use the center of the averaging region.
	#line_start = max (1, line - nlines / 2)

	# The following uses the preceeding nlines - 1 lines.
	line_start = max (1, line - (nlines - 1))

	line_start = min (line_start, IM_LEN(im, 2) - nlines)
	line_end = line_start + nlines - 1

	# Return if the same line is the same and the lines used in the
	# sum profile are the same.

	if ((line_start == last_start) && (line == last_line))
	    return

	# If the number of lines in common with the previous sum profile is
	# < nlines / 2 then it is more efficient to clear the sum profile
	# and readd the common lines.

	if (abs (line_start - last_start) > nlines / 2) {
	    call aclrr (profiles[1,1,1], len_profs)
	    do i = last_start, last_end {
		j = i - line_start

		# If the old line i is within the new sum add it to the sum.
		# However, if the line is the new data line do not add it.
		if ((j >= 0) && (j < nlines) && (i != line)) {
		    k = 2 + mod (i - 1, nlines)
		    call aaddr (profiles[1,1,1], profiles[1,1,k],
			profiles[1,1,1], len_profs)
		}
	    }

	# If the number in lines in common is >= nlines / 2 then it is more
	# efficient to subtract the lines not in common from the sum.

	} else {
	    do i = last_start, last_end {
		j = i - line_start
		k = 2 + mod (i - 1, nlines)

		# If the old line i is not within the new sum subtract it.
		# Also, if the line is the new data line subtract it.
		if ((j < 0) || (j >= nlines) || (i == line)) {
		    # However, don't subtract the last data line since it was
		    # not in the previous sum.
		    if (i != last_line) {
		        call asubr (profiles[1,1,1], profiles[1,1,k],
			    profiles[1,1,1], len_profs)
		    }

		# If the old line is within the new sum but it was the old data
		# line then add it to the sum since it was not in the old sum.
		} else if (i == last_line) {
		    call aaddr (profiles[1,1,1], profiles[1,1,k],
			profiles[1,1,1], len_profs)
		}
	    }
	}

	# Get the new profiles into the profile buffer and the add to the sum.
	do i = line_start, line_end {
	    j = i - last_start
	    if ((j < 0) || (j >= nlines)) {
	        k = 2 + mod (i - 1, nlines)

		# Get the data profile for line i and put it in profiles k.
		call msgprofiles (ms, im, i, ranges, profiles[1,1,k], coeff,
		    len_prof, nspectra)

		# If the new line in the buffer is not the data line then
		# add it to the sum profile.
		if (i != line) {
	            call aaddr (profiles[1,1,1], profiles[1,1,k],
			profiles[1,1,1], len_profs)
		}
	    }
	}

	# Record current state of the average.
	last_line = line
	last_start = line_start
	last_end = line_end

	# Set the data profiles and model profiles.  The copies are
	# made rather than working directly from the profiles buffer so that
	# changes can be made in the data and model profiles without affecting
	# the buffer.

	k = 2 + mod (line - 1, nlines)
	call amovr (profiles[1,1,k], data, len_profs)
	call amovr (profiles[1,1,1], model, len_profs)
end


# UPDATE_SMOOTH -- Replace an updated data profile in the profiles buffer.

procedure update_smooth (line, data, profiles, len_prof, nspectra, nlines)

int	line					# Data image line
real	data[len_prof, nspectra]		# Data profiles
real	profiles[len_prof, nspectra, ARB]	# Profiles buffer
int	len_prof				# Length of profiles
int	nspectra				# Number of spectra
int	nlines					# Number of lines in buffer

int	i

begin
	i = 2 + mod (line - 1, nlines)
	call amovr (data, profiles[1,1,i], len_prof * nspectra)
end


# MSGPROFILES -- Read image line and extract profiles in standard positions.

procedure msgprofiles (ms, im, line, ranges, profile, coeff, len_prof,
    nspectra)

pointer	ms					# MULTISPEC data structure
pointer	im					# IMIO image descriptor
int	line					# Image line to be read
real	ranges[nspectra, LEN_RANGES]		# Ranges array for profiles
real	profile[len_prof, nspectra]		# Profiles to be obtained
real	coeff[ARB]				# Image interpolator coeffs.
int	len_prof				# Length of profiles
int	nspectra				# Number of spectra

int	i, j
real	x
pointer	im_buf

pointer	imgl2r()
real	cveval(), asival()

begin
	# Read image line.
	im_buf = imgl2r (im, line)

	# Fit image interpolation function.
	call asifit (Memr[im_buf], IM_LEN(im, 1), coeff)

	# For each spectrum extract the profiles.
	do j = 1, nspectra {

	    # Determine profile starting point in image coordinates using the
	    # fit function for the spectrum center.
	    x = cveval (CV(ms, X0_FIT, j), real (line)) +
		ranges[j, DX_START] - 1

	    # For each point in the profile evaluate the image interpolator.
	    do i = 1, len_prof {
		x = x + 1
		if ((x < 1) || (x > IM_LEN(im, 1)))
		    profile[i, j] = 0.
		else
		    profile[i, j] = asival (x, coeff)
	    }
	}
end
