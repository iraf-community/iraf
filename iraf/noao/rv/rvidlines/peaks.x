# PEAKS -- The following procedures are general numerical functions
# dealing with finding peaks in a data array.
#
# FIND_PEAKS		Find the peaks in the data array.
# FIND_LOCAL_MAXIMA	Find the local minima/maxima in the data array.
# IS_LOCAL_MAX		Test a point to determine if it is a local min/max.
# FIND_THRESHOLD	Find the peaks with positions satisfying threshold
#			and contrast constraints.
# FIND_ISOLATED		Flag peaks which are within separation of a peak
#			with a higher peak value.
# FIND_NMAX		Select up to the nmax highest ranked peaks.
# COMPARE		Compare procedure for sort used in FIND_PEAKS.

# FIND_PEAKS -- Find the peaks in the data array.
#
# The peaks are found using the following algorithm:
#
# 1.  Find the local minima/maxima.
# 2.  Reject peaks below the threshold.
# 3.  Determine the ranks of the remaining peaks.
# 4.  Flag weaker peaks within separation of a stronger peak.
# 5.  Accept at most the nmax strongest peaks.
#
# Indefinite points are ignored.  The peak positions are returned in the
# array x.


int procedure find_peaks (data, x, npoints, contrast, separation, edge, nmax,
    threshold, debug)

# Procedure parameters:
real	data[npoints]		# Input data array
real	x[npoints]		# Output peak position array
int	npoints			# Number of data points
real	contrast		# Maximum contrast between strongest and weakest
int	separation		# Minimum separation between peaks
int	edge			# Minimum distance from the edge
int	nmax			# Maximum number of peaks to be returned
real	threshold		# Minimum threshold level for peaks
bool	debug			# Print diagnostic information?

int	i, j
int	nlmax, nthreshold, nisolated, npeaks
pointer	sp, rank

int	find_local_maxima(), find_threshold(), find_isolated(), find_nmax()
int	compare()

extern	compare()

pointer	y
int	maxima
common	/sort/ y, maxima

begin
	# Find the local minima/maxima in data and put column positions in x..
	if (contrast < 0.)
	    maxima = NO
	else
	    maxima = YES

	nlmax = find_local_maxima (data, x, npoints, debug)

	# Reject local minima/maxima near the edge.
	if (edge > 0) {
	    j = 0
	    do i = 1, nlmax {
		if ((x[i] > edge) && (x[i] <= npoints - edge)) {
		    j = j + 1
		    x[j] = x[i]
		}
	    }
	    nlmax = j
	}

	# Allocate a working array y.
	call smark (sp)
	call salloc (y, npoints, TY_REAL)

	# Reject the local minima/maxima which do not satisfy the thresholds.
	# The array y is set to the peak values of the remaining peaks.
	nthreshold = find_threshold (data, x, Memr[y], nlmax,
	    contrast, threshold, debug)

	# Rank the peaks by peak value.
	call salloc (rank, nthreshold, TY_INT)
	do i = 1, nthreshold
	    Memi[rank + i - 1] = i
	call qsort (Memi[rank], nthreshold, compare)

	# Reject the weaker peaks within sep of a stronger peak.
	nisolated = find_isolated (x, Memi[rank], nthreshold, separation,
	    debug)

	# Select the strongest nmax peaks.
	npeaks = find_nmax (data, x, Memi[rank], nthreshold, nmax, debug)

	call sfree (sp)
	return (npeaks)
end


# FIND_LOCAL_MAXIMA -- Find the local minima/maxima in the data array.
#
# A data array is input and the local minima/maxima positions array is output.
# The number of local minima/maxima found is returned.

int procedure find_local_maxima (data, x, npoints, debug)

real	data[npoints]			# Input data array
real	x[npoints]			# Output local min/max positions array
int	npoints				# Number of input points
bool	debug				# Print debugging information?

int	i, nlmax

bool	is_local_max()

begin
	nlmax = 0
	do i = 1, npoints {
	    if (is_local_max (i, data, npoints)) {
		nlmax = nlmax + 1
		x[nlmax] = i
	    }
	}

	if (debug) {
	    call printf ("  Number of local minima/maxima found = %d.\n")
		call pargi (nlmax)
	}

	return (nlmax)
end


# IS_LOCAL_MAX -- Test a point to determine if it is a local minima/maximum.
#
# Indefinite points are ignored.

bool procedure is_local_max (index, data, npoints)

# Procedure parameters:
int	index			# Index to test for local minimum/maximum
real	data[npoints]		# Data values
int	npoints			# Number of points in the data vector

int	i, j, nright, nleft

pointer	y
int	maxima
common	/sort/ y, maxima

begin
	# INDEF points cannot be local minima/axima.
	if (IS_INDEFR (data[index]))
	    return (FALSE)

	# Find the left and right indices where data values change and the
	# number of points with the same value.  Ignore INDEF points.
	nleft = 0
	for (i = index - 1; i >= 1; i = i - 1) {
	    if (!IS_INDEFR (data[i])) {
		if (data[i] != data[index])
		    break
		nleft = nleft + 1
	    }
	}
	nright = 0
	for (j = index + 1; i <= npoints; j = j + 1) {
	    if (!IS_INDEFR (data[j])) {
		if (data[j] != data[index])
		    break
		nright = nright + 1
	    }
	}

	# Test for failure to be a local minima/axima
	if (maxima == YES) {
	    if ((i == 0) && (j == npoints)) {
		return (FALSE)		# Data is constant
	    } else if (i == 0) {
		if (data[j] > data[index])
		    return (FALSE)		# Data increases to right
	    } else if (j == npoints) {
		if (data[i] > data[index])	# Data increase to left
		    return (FALSE)
	    } else if ((data[i] > data[index]) || (data[j] > data[index])) {
		return (FALSE)		# Not a local maximum
	    } else if (!((nleft - nright == 0) || (nleft - nright == 1))) {
		return (FALSE)		# Not center of plateau
	    }
	} else {
	    if ((i == 0) && (j == npoints)) {
		return (FALSE)		# Data is constant
	    } else if (i == 0) {
		if (data[j] < data[index])
		    return (FALSE)		# Data decreases to right
	    } else if (j == npoints) {
		if (data[i] < data[index])	# Data decrease to left
		    return (FALSE)
	    } else if ((data[i] < data[index]) || (data[j] < data[index])) {
		return (FALSE)		# Not a local maximum
	    } else if (!((nleft - nright == 0) || (nleft - nright == 1))) {
		return (FALSE)		# Not center of plateau
	    }
	}

	# Point is a local minima/maxima
	return (TRUE)
end


# FIND_THRESHOLD -- Find the peaks with positions satisfying threshold
# and contrast constraints.
#
# The input is the data array, data, and the peak positions array, x.
# The x array is resorted to the nthreshold peaks satisfying the constraints.
# The corresponding nthreshold data values are returned the y array.
# The number of peaks satisfying the constraints (nthreshold) is returned.

int procedure find_threshold (data, x, y, npoints, contrast, threshold, debug)

real	data[ARB]			# Input data values
real	x[npoints]			# Input/Output peak positions
real	y[npoints]			# Output peak data values
int	npoints				# Number of peaks input
real	contrast			# Contrast constraint
real	threshold			# Threshold constraint
bool	debug				# Print debugging information?

int	i, j, nthreshold
real	minval, maxval, lcut

pointer	dummy
int	maxima
common	/sort/ dummy, maxima

begin
	# Set the y array to be the values at the peak positions.
	do i = 1, npoints {
	    j = x[i]
	    y[i] = data[j]
	}

	# Determine the min and max values of the peaks.
	call alimr (y, npoints, minval, maxval)

	# Set the threshold based on the max of the absolute threshold and the
	# contrast.  Use arlt to set peaks below threshold to INDEF.

	if (maxima == YES) {
	    lcut = max (real (threshold), real (contrast * maxval))
	    call arltr (y, npoints, lcut, INDEFR)
	} else {
	    lcut = threshold
	    call argtr (y, npoints, lcut, INDEFR)
	}

	if (debug) {
	    call printf ("  Highest peak value = %g.\n")
		call pargr (maxval)
	    call printf ("  Peak cutoff threshold = %g.\n")
		call pargr (lcut)
	    do i = 1, npoints {
		if (IS_INDEFR (y[i])) {
		    j = x[i]
		    call printf (
			"  Peak at column %d with value %g below threshold.\n")
			call pargi (j)
			call pargr (data[j])
		}
	    }
	}

	# Determine the number of acceptable peaks & resort the x and y arrays.
	nthreshold = 0
	do i = 1, npoints {
	    if (IS_INDEFR (y[i]))
		next
	    nthreshold = nthreshold + 1
	    x[nthreshold] = x[i]
	    y[nthreshold] = y[i]
	}

	if (debug) {
	    call printf ("  Number of peaks above the threshold = %d.\n")
		call pargi (nthreshold)
	}

	return (nthreshold)
end

# FIND_ISOLATED -- Flag peaks which are within separation of a peak
# with a higher peak value.
#
# The peak positions, x, and their ranks, rank, are input.
# The rank array contains the indices of the peak positions in order from
# the highest peak value to the lowest peak value.  Starting with
# highest rank (rank[1]) all peaks of lower rank within separation
# are marked by setting their positions to INDEF.  The number of
# unflaged peaks is returned.

int procedure find_isolated (x, rank, npoints, separation, debug)

# Procedure parameters:
real	x[npoints]		# Positions of points
int	rank[npoints]		# Rank of peaks
int	npoints			# Number of peaks
int	separation		# Minimum allowed separation
bool	debug			# Print diagnostic information

int	i, j
int	nisolated

begin
	# Eliminate close neighbors.  The eliminated
	# peaks are marked by setting their positions to INDEF.
	nisolated = 0
	do i = 1, npoints {
	    if (IS_INDEFR (x[rank[i]]))
		next
	    nisolated = nisolated + 1
	    do j = i + 1, npoints {
		if (IS_INDEFR (x[rank[j]]))
		    next
		if (abs (x[rank[i]] - x[rank[j]]) < separation) {
		    if (debug) {
			call printf (
			    "  Peak at column %d too near peak at column %d.\n")
			    call pargi (int (x[rank[j]]))
			    call pargi (int (x[rank[i]]))
		    }
		    x[rank[j]] = INDEFR
		}
	    }
	}

	if (debug) {
	    call printf ("  Number of peaks separated by %d pixels = %d.\n")
		call pargi (separation)
		call pargi (nisolated)
	}

	# Return number of isolated peaks.
	return (nisolated)
end


# FIND_NMAX -- Select up to the nmax highest ranked peaks.
#
# The data values, data, peak positions, x, and their ranks, rank, are input.
# The data values are used only in printing debugging information.
# Peak positions previously eliminated are flaged by the value INDEF.
# The rank array contains the indices to the peak positions in order from
# the highest peak value to the lowest peak value.
# First all but the nmax highest ranked peaks (which have not been previously
# eliminated) are eliminated by marking their positions with the value INDEF.
# Then the remaining peaks are resorted to contain only the unflaged
# peaks and the number of such peaks is returned.

int procedure find_nmax (data, x, rank, npoints, nmax, debug)

real	data[ARB]			# Input data values
real	x[npoints]			# Peak positions
int	rank[npoints]			# Ranks of peaks
int	npoints				# Number of input peaks
int	nmax				# Max number of peaks to be selected
bool	debug				# Print debugging information?

int	i, j, npeaks

begin
	# Only mark peaks to reject if the number peaks is greater than nmax.
	if (nmax < npoints) {
	    npeaks = 0
	    do i = 1, npoints {
	        if (IS_INDEFR (x[rank[i]]))
		    next
	        npeaks = npeaks + 1
	        if (npeaks > nmax) {
		    if (debug) {
			j = x[rank[i]]
			call printf (
		    "  Reject peak at column %d with rank %d and value %g.\n")
			    call pargi (j)
			    call pargi (i)
			    call pargr (data[j])
		    }
	            x[rank[i]] = INDEFR
	        }
	    }
	}

	# Eliminate INDEF points and determine the number of spectra found.
	npeaks = 0
	do i = 1, npoints {
	    if (IS_INDEFR (x[i]))
		next
	    npeaks = npeaks + 1
	    x[npeaks] = x[i]
	}

	return (npeaks)
end


# COMPARE -- Compare procedure for sort used in FIND_PEAKS.
# Larger values are indexed first.  INDEF values are indexed last.

int procedure compare (index1, index2)

# Procedure parameters:
int	index1				# Comparison index
int	index2				# Comparison index

pointer	y
int	maxima
common	/sort/ y, maxima

begin
	# INDEF points are considered to be smallest/largest possible values.
	if (maxima == YES) {
	    if (IS_INDEFR (Memr[y - 1 + index1]))
		return (1)
	    else if (IS_INDEFR (Memr[y - 1 + index2]))
		return (-1)
	    else if (Memr[y - 1 + index1] < Memr[y - 1 + index2])
		return (1)
	    else if (Memr[y - 1 + index1] > Memr[y - 1 + index2])
		return (-1)
	    else
		return (0)
	} else {
	    if (IS_INDEFR (Memr[y - 1 + index1]))
		return (-1)
	    else if (IS_INDEFR (Memr[y - 1 + index2]))
		return (1)
	    else if (Memr[y - 1 + index1] < Memr[y - 1 + index2])
		return (-1)
	    else if (Memr[y - 1 + index1] > Memr[y - 1 + index2])
		return (1)
	    else
		return (0)
	}
end
