# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<pkg/rg.h>

# RG_RANGES -- Parse a range string.  Return a pointer to the ranges.

pointer procedure rg_ranges (rstr, rmin, rmax)

char	rstr[ARB]		# Range string
int	rmin			# Minimum value
int	rmax			# Maximum value

pointer	rg			# Ranges
int	nrgs			# Number of ranges

int	i, x1, x2, ctoi()

begin
	# Check for a valid string and deterimine the number of ranges.

	i = 1
	nrgs = 0

	while (rstr[i] != EOS) {

	    # Skip delimiters
	    while (IS_WHITE(rstr[i]) || (rstr[i]==',') || (rstr[i]=='\n'))
	        i = i + 1

	    # Check for end of string.

	    if (rstr[i] == EOS)
		break

	    # First character must be a *, -, or digit.

	    if ((rstr[i] == '*') || (rstr[i] == '-') || IS_DIGIT(rstr[i])) {
		i = i + 1
	        nrgs = nrgs + 1

		# Remaining characters must be :, -, or digits.
	        while ((rstr[i]==':') || (rstr[i]=='-') || IS_DIGIT(rstr[i]))
		    i = i + 1
	    } else
		call error (0, "Syntax error in range string")
	}

	# Allocate memory for the ranges.

	call malloc (rg, LEN_RG + 2 * max (1, nrgs), TY_STRUCT)

	# Rescan the string and set the ranges.

	i = 1
	nrgs = 0

	while (rstr[i] != EOS) {

	    # Skip delimiters.
	    while (IS_WHITE(rstr[i]) || (rstr[i]==',') || (rstr[i]=='\n'))
	        i = i + 1

	    # Check for end of string.

	    if (rstr[i] == EOS)
		break

	    # If first character is * then set range to full range.
	    # Otherwise parse the range.

	    if (rstr[i] == '*') {
		i = i + 1
		x1 = rmin
		x2 = rmax

	    } else {
		# First digit is starting value.
		if (ctoi (rstr, i, x1) == 0) {
		    nrgs = 0
		    break
		}
		x2 = x1

		# Check for an ending value for the range.
		if (rstr[i] == ':') {
		    i = i + 1
		    if (ctoi (rstr, i, x2) == 0) {
			nrgs = 0
			break
		    }
		}
	    }

	    # Check limits.
	    if ((max (x1, x2) >= rmin) && (min (x1, x2) <= rmax)) {
		nrgs = nrgs + 1
		RG_X1(rg, nrgs) = max (rmin, min (rmax, x1))
		RG_X2(rg, nrgs) = max (rmin, min (rmax, x2))
	    }
	}

	RG_NRGS(rg) = nrgs
	RG_NPTS(rg) = 0
	do i = 1, RG_NRGS(rg)
	    RG_NPTS(rg) = RG_NPTS(rg) +
		abs (RG_X1(rg, i) - RG_X2(rg, i)) + 1

	return (rg)
end
