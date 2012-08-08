# PEAKS -- The following procedures are general numerical functions
# dealing with finding peaks in a data array.
#
# FIND_PEAKS		Find additional peaks in the data array.
# FIND_LOCAL_MAXIMA	Find the local maxima in the data array.
# IS_LOCAL_MAX		Test a point to determine if it is a local maximum.
# FIND_CONTRAST		Find the peaks satisfying the contrast constraint.
# FIND_ISOLATED		Flag peaks which are within separation of a peak
#			with a higher peak value.
# FIND_NMAX		Select up to the nmax highest ranked peaks.
# COMPARE		Compare procedure for sort used in FIND_PEAKS.

# FIND_PEAKS -- Find the peaks in the data array.
#
# The peaks are found using the following algorithm:
#
# 1.  Find the local maxima which are not near the edge of existing peaks.
# 2.  Reject those below the absolute threshold.
# 3.  Reject those below the contrast threshold.
# 4.  Determine the ranks of the remaining peaks.
# 5.  Flag weaker peaks within separation of a stronger peak.
# 6.  Add strongest peaks to the peaks array.
#
# Indefinite data points are ignored.

procedure find_peaks (data, npts, contrast, edge, nmax, separation, threshold,
    peaks, npeaks)

real	data[npts]		# Input data array
int	npts			# Number of data points

real	contrast		# Maximum contrast between strongest and weakest
int	edge			# Minimum distance from the edge
int	nmax			# Maximum number of peaks to be returned
real	separation		# Minimum separation between peaks
real	threshold		# Minimum threshold level for peaks

real	peaks[nmax]		# Positons of input peaks / output peaks
int	npeaks			# Number of input peaks / number of output peaks

int	i, nx
pointer	sp, x, y, rank

int	compare()
extern	compare()

common	/sort/ y

begin
	if (npeaks >= nmax)
	    return

	call smark (sp)
	call salloc (x, npts, TY_INT)
	call salloc (y, npts, TY_REAL)

	# Find the positions of the local maxima.
	call find_local_maxima (data, npts, peaks, npeaks, edge, separation,
	    threshold, Memi[x], Memr[y], nx)

	# Eliminate points not satisfying the contrast constraint.
	call find_contrast (data, Memi[x], Memr[y], nx, contrast)

	# Rank the peaks by peak value.
	call salloc (rank, nx, TY_INT)
	for (i = 1; i <= nx; i = i + 1)
	    Memi[rank + i - 1] = i
	call qsort (Memi[rank], nx, compare)

	# Reject weaker peaks within a specified separation of a stronger peak.
	call find_isolated (Memi[x], Memi[rank], nx, separation)

	# Add the strongest peaks.
	call find_nmax (Memi[x], Memi[rank], nx, nmax, peaks, npeaks)

	call sfree (sp)
end


# FIND_LOCAL_MAXIMA -- Find the local maxima in the data array.

procedure find_local_maxima (data, npts, peaks, npeaks, edge, separation,
    threshold, x, y, nx)

real	data[npts]		# Input data array
int	npts			# Number of input points
real	peaks[ARB]		# Positions of peaks
int	npeaks			# Number of peaks
int	edge			# Edge buffer distance
real	separation		# Minimum separation from peaks
real	threshold		# Data threshold
int	x[npts]			# Output positions
real	y[npts]			# Output data values
int	nx			# Number of maxima

int	i, j

bool	is_local_max()

begin
	# Find the local maxima above the threshold and not near the edge.
	nx = 0
	for (i = edge + 1; i <= npts - edge; i = i + 1) {
	    if ((data[i] >= threshold) && (is_local_max (i, data, npts))) {
	        nx = nx + 1
	        x[nx] = i
	    }
	}

	# Flag maxima within separation of previous peaks.
	for (j = 1; j <= npeaks; j = j + 1) {
	    for (i = 1; i <= nx; i = i + 1) {
		if (IS_INDEFI (x[i]))
		    next
		if (x[i] < peaks[j] - separation)
		    next
		if (x[i] > peaks[j] + separation)
		    break
		x[i] = INDEFI
	    }
	}

	# Eliminate flagged maxima and set y values.
	j = 0
	for (i = 1; i <= nx; i = i + 1) {
	    if (!IS_INDEFI (x[i])) {
		j = j + 1
		x[j] = x[i]
		y[j] = data[x[j]]
	    }
	}

	nx = j
end


# IS_LOCAL_MAX -- Test a point to determine if it is a local maximum.
#
# Indefinite points are ignored.

bool procedure is_local_max (index, data, npts)

# Procedure parameters:
int	index			# Index to test for local maximum
real	data[npts]		# Data values
int	npts			# Number of points in the data vector

int	i, j, nright, nleft

begin
	# INDEFR points cannot be local maxima.
	if (IS_INDEFR (data[index]))
	    return (false)

	# Find the left and right indices where data values change and the
	# number of points with the same value.  Ignore INDEFR points.
	nleft = 0
	for (i = index - 1; i >= 1; i = i - 1) {
	    if (!IS_INDEFR (data[i])) {
		if (data[i] != data[index])
		    break
		nleft = nleft + 1
	    }
	}
	nright = 0
	for (j = index + 1; i <= npts; j = j + 1) {
	    if (!IS_INDEFR (data[j])) {
		if (data[j] != data[index])
		    break
		nright = nright + 1
	    }
	}

	# Test for failure to be a local maxima
	if ((i == 0) && (j == npts)) {
	    return (FALSE)		# Data is constant
	} else if (i == 0) {
	    if (data[j] > data[index])
	        return (FALSE)		# Data increases to right
	} else if (j == npts) {
	    if (data[i] > data[index])	# Data increase to left
		return (FALSE)
	} else if ((data[i] > data[index]) || (data[j] > data[index])) {
	    return (FALSE)		# Not a local maximum
	} else if (!((nleft - nright == 0) || (nleft - nright == 1))) {
	    return (FALSE)		# Not center of plateau
	}

	# Point is a local maxima
	return (TRUE)
end



# FIND_CONTRAST -- Find the peaks with positions satisfying contrast constraint.

procedure find_contrast (data, x, y, nx, contrast)

real	data[ARB]		# Input data values
int	x[ARB]			# Input/Output peak positions
real	y[ARB]			# Output peak data values
int	nx			# Number of peaks input
real	contrast		# Contrast constraint

int	i, j
real	minval, maxval, threshold

begin
	if ((nx == 0.) || (contrast <= 0.))
	    return

	call alimr (y, nx, minval, maxval)
	threshold = contrast * maxval

	j = 0
	do i = 1, nx {
	    if (y[i] < threshold) {
		j = j + 1
		x[j] = x[i]
		y[j] = y[i]
	    }
	}

	nx = j
end

# FIND_ISOLATED -- Flag peaks which are within separation of a peak
# with a higher peak value.
#
# The peak positions, x, and their ranks, rank, are input.
# The rank array contains the indices of the peak positions in order from
# the highest peak value to the lowest peak value.  Starting with
# highest rank (rank[1]) all peaks of lower rank within separation
# are marked by setting their positions to INDEFI.

procedure find_isolated (x, rank, nx, separation)

int	x[ARB]			# Positions of points
int	rank[ARB]		# Rank of peaks
int	nx			# Number of peaks
real	separation		# Minimum allowed separation

int	i, j

begin
	if ((nx == 0) || (separation <= 0.))
	    return

	# Eliminate close neighbors.  The eliminated
	# peaks are marked by setting their positions to INDEFI.

	for (i = 1; i < nx; i = i + 1) {
	    if (IS_INDEFI (x[rank[i]]))
		next
	    for (j = i + 1; j <= nx; j = j + 1) {
		if (IS_INDEFI (x[rank[j]]))
		    next
		if (abs (x[rank[i]] - x[rank[j]]) < separation)
		    x[rank[j]] = INDEFI
	    }
	}
end


# FIND_NMAX -- Select up to the nmax highest ranked peaks.

procedure find_nmax (x, rank, nx, nmax, peaks, npeaks)

int	x[ARB]			# Peak positions
int	rank[ARB]		# Ranks of peaks
int	nx			# Number of input / output peaks
int	nmax			# Max number of peaks to be selected
real	peaks[nmax]		# Output peak position array
int	npeaks			# Output number of peaks

int	i

begin
	for (i = 1; (i <= nx) && (npeaks < nmax); i = i + 1) {
	    if (IS_INDEFI (x[rank[i]]))
		next
	    npeaks = npeaks + 1
	    peaks[npeaks] = x[rank[i]]
	}
end


# COMPARE -- Compare procedure for sort used in FIND_PEAKS.
# Larger values are indexed first.  INDEFR values are indexed last.

int procedure compare (index1, index2)

# Procedure parameters:
int	index1				# Comparison index
int	index2				# Comparison index

pointer	y

common	/sort/ y

begin
	# INDEFR points are considered to be smallest possible values.
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
end
