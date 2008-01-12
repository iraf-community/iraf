include "ms.h"

# SET_RANGES -- Set profile starting range array.
#
# The ranges array relates the starting point of the profiles relative
# to the center of profile and relative to the image line.  For more
# details see the MULTISPEC system documentation.

procedure set_ranges (ms, lower, ranges, nspectra)

pointer	ms				# MULTISPEC data structure
real	lower				# Relative lower limit of profiles
real	ranges[nspectra, LEN_RANGES]	# Ranges array to be set
int	nspectra			# Number of spectra

int	i

begin
	do i = 1, nspectra {
	    ranges[i, X_START] = int (PARAMETER(ms, X0, i)) + lower
	    ranges[i, DX_START] = ranges[i, X_START] - PARAMETER(ms, X0, i)
	}
end
