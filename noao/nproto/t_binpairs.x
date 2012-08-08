define	MAXNBINS 100			# Maximum number of bins
define	MAXNPTS	10000			# Maximum number of data points


# T_BIN_PAIRS -- Bin pairs in separation
#
# The data points in two files, given as (x,y) values, are binned as a
# function of log separation.  The number of bins and the separation range
# are specified.  A list of separation, number of pairs in the bin,
# the number of pairs normalized by the total number of input pairs, and 
# the area of the bin are output.

procedure t_binpairs ()

char	file1[SZ_FNAME]			# Data file1
char	file2[SZ_FNAME]			# Data file2
real	rmin				# Minimum separation
real	rmax				# Maximum separation
int	nbins				# Number of separation bins
bool	verbose				# Verbose output

real	x1[MAXNPTS], y1[MAXNPTS]	# Data coordinates
real	x2[MAXNPTS], y2[MAXNPTS]	# Data coordinates
int	npts1, npts2			# Number of data points
int	npairs[MAXNBINS]		# Number of pairs

int	fd, i, nall
real	r1, r2

bool	clgetb(), strne()
real	clgetr()
int	clgeti(), open(), get_data()

begin
	# Get the pairs from file1.
	call clgstr ("file1", file1, SZ_FNAME)
	fd = open (file1, READ_ONLY, TEXT_FILE)
	npts1 = get_data (fd, x1, y1, MAXNPTS)
	call close (fd)

	# Get the pairs from file2 if different from file1.
	call clgstr ("file2", file2, SZ_FNAME)
	if (strne (file1, file2)) {
	    fd = open (file2, READ_ONLY, TEXT_FILE)
	    npts2 = get_data (fd, x2, y2, MAXNPTS)
	    call close (fd)
	} else
	    npts2 = 0

	# Get the separation bin parameters.
	rmin = clgetr ("rmin")
	rmax = clgetr ("rmax")
	nbins = min (clgeti ("nbins"), MAXNBINS)
	verbose = clgetb ("verbose")

	# Compute the pairs.
	call setbins (rmin, rmax, nbins)
	call bin_pairs (x1, y1, npts1, x2, y2, npts2, npairs, nbins, verbose)
	if (npts2 == 0)
	    nall = npts1 * (npts1 - 1)
	else
	    nall = npts1 * npts2

	# Print the results.
	call binr (1, r1)
	do i = 1, nbins {
	    call binr (i + 1, r2)
	    call printf ("%g %d %g %g\n")
		call pargr (r1)
		call pargi (npairs[i])
		call pargr (real (npairs[i]) / nall)
		call pargr (3.14159 * (r2 ** 2 - r1 ** 2))
	    r1 = r2
	}
end


# GET_DATA -- Get a list of x,y coordinates from a file and return the number
# of points.

int procedure get_data (fd, x, y, maxnpts)

int	fd			# Input file descriptor
real	x[maxnpts]		# X data coordinate
real	y[maxnpts]		# Y data coordinate
int	maxnpts			# Maximum number of data points to get
int	npts			# Return number of points

int	fscan(), nscan()

begin
	# Read the data
	npts = 0
	while (npts < MAXNPTS) {
	    if (fscan (fd) == EOF)
		break
	    npts = npts + 1
	    call gargr (x[npts])
	    call gargr (y[npts])
	    if (nscan() != 2)
		npts = npts - 1
	}
	return (npts)
end


# BIN_PAIRS -- Bin pairs in the input vectors.
#
# The points in the input vector(s) are binned according to the
# binnum procedure.  If npts2 is zero then the first vector is paired
# against itself (autocorrelation).

procedure bin_pairs (x1, y1, npts1, x2, y2, npts2, npairs, nbins, verbose)

real	x1[npts1], y1[npts1]		# Coordinates of points
int	npts1				# Number of points
real	x2[npts2], y2[npts2]		# Coordinates of points
int	npts2				# Number of points
int	npairs[nbins]			# Number of pairs
int	nbins				# Number of separation bins
bool	verbose				# Verbose output

int	i, j, k, bin

begin
	# Initialize bins
	do bin = 1, nbins
	    npairs[bin] = 0

	# Set printing interval
	if (verbose)
	    k = max (1, npts1 / 20)

	# Loop through all pairs of points
	do i = 1, npts1 {

	    # If npts2 is zero then pair the points in the first vector
	    # otherwise pair the points between the two vectors.

	    if (npts2 == 0) {
	        do j = i + 1, npts1 {
		    call binnum (x1[i], y1[i], x1[j], y1[j], bin)
		    if (bin > 0)
		        npairs[bin] = npairs[bin] + 2
	        }
	    } else {
	        do j = 1, npts2 {
		    call binnum (x1[i], y1[i], x2[j], y2[j], bin)
		    if (bin > 0)
		        npairs[bin] = npairs[bin] + 1
	        }
	    }

	    if (verbose) {
		if (mod (i, k) == 0) {
		    call eprintf ("%5.1f%%...\n")
		        call pargr (100. * i / npts1)
		}
	    }
	}
end


define  R2BINS		100	# Maximum number of r2 bins
define	HASHBINS	1000	# Size of r2 hash table

# SETBINS -- Set the mapping between separation and bin
# BINNUM  -- Return bin number for the given data points
# BINR    -- Return separation for the given bin

procedure setbins (rmin, rmax, nr)

real	rmin				# Minimum separation
real	rmax				# Maximum separation
int	nr				# Number of separation bins

real	x1, y1				# Data coordinate
real	x2, y2				# Data coordinate
int	bin				# Correlation Bin
real	r				# Separation

real	r2bins[R2BINS]			# r2 bins
int	hash[HASHBINS]			# Hash table

int	i, j, nbins
real	r2, dr2, r2zero
real	logr2, dlogr2, logr2zero

begin
	r2 = rmin ** 2
	dr2 = (rmax ** 2 - r2) / HASHBINS
	r2zero = 1 - r2 / dr2

	logr2 = 2 * log10 (rmin)
	dlogr2 = (2 * log10 (rmax) - logr2) / nr
	logr2zero = 1 - logr2 / dlogr2

	do i = 1, HASHBINS {
	    hash[i] = log10 (r2) / dlogr2 + logr2zero    
	    r2 = r2 + dr2
	}

	nbins = nr + 1
	do i = 1, nbins {
	    r2bins[i] = 10 ** logr2
	    logr2 = logr2 + dlogr2
	}

	return

entry binnum (x1, y1, x2, y2, bin)

	r2 = (x1 - x2) ** 2 + (y1 - y2) ** 2
	i = r2 / dr2 + r2zero

	if ((i < 1) || (i > HASHBINS))
	    bin = 0

	else {
	    j = hash[i]
	    do i = j + 1, nbins {
	        if (r2 < r2bins[i]) {
		    bin = i - 1
		    return
		}
	    }
	}

	return

entry binr (bin, r)

	r = sqrt (r2bins[bin])
end
