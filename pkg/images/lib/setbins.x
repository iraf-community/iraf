# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# SET_BINS -- Divide the x points into bins described by a bin
# origin and bin width.  Return the indices for the end of each
# bin with at least one point.  The number of bins is returned as
# the function value.

int procedure set_bins (x, bins, npts, x1, dx)

real	x[npts]				# Ordinates to be binned
int	bins[npts]			# Bin starting indices
int	npts				# Number of points
real	x1				# Origin of bins
real	dx				# Width of bins

int	i, j, nbins, bin
real	xnext

begin
	nbins = 0
	for (i = 1; i <= npts; i = j) {
	    bin = (x[i] - x1) / dx + 1
	    xnext = dx * bin + x1
	    for (j = i + 1; (j <= npts) && (x[j] < xnext); j = j + 1)
		;
	    nbins = nbins + 1
	    bins[nbins] = j - 1
	}
	return (nbins)
end

# BIN_AVG -- Average the array into the given bins.

procedure bin_avg (x, bins, nbins)

real	x[ARB]				# Data to be averaged
int	bins[nbins]			# Indices of the starting data points
int	nbins				# Number of bins

int	i, j, n
real	asumr()

begin
	x[1] = asumr (x[1], bins[1]) / bins[1]
	for (i = 2; i <= nbins; i = i + 1) {
	    j = bins[i-1] + 1
	    n = bins[i] - bins[i-1]
	    x[i] = asumr (x[j], n) / n
	}
end

# BIN_MEDIAN -- Median the array into the given bins.

procedure bin_median (x, bins, nbins)

real	x[ARB]				# Data to be averaged
int	bins[nbins]			# Indices of the starting data points
int	nbins				# Number of bins

int	i, j, n
real	amedr()

begin
	x[1] = amedr (x[1], bins[1])
	do i = 2, nbins {
	    j = bins[i-1] + 1
	    n = bins[i] - bins[i-1]
	    x[i] = amedr (x[j], n)
	}
end
