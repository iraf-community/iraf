include	<imhdr.h>
include	<error.h>
include	<time.h>
include	<ctype.h>

define	OPTIONS		"|mean|median|mode|"
define	MEAN	1	# Mean of image
define	MEDIAN	2	# Median of image
define	MODE	3	# Mode of image

define	BINWIDTH	0.1	# Bin width (in sigmas) for mode 
define	BINSEP		0.01	# Bin separation (in sigmas) for mode

# T_BSCALE -- Scale image brightness
#
#	out = (in - bzero) / bscale

procedure t_bscale ()

int	list		# List of images
real	bzero		# Zero point
real	bscale		# Scale
pointer	section		# Sample image section
int	step		# Default sample step
pointer	logfile		# Logfile
bool	noact		# No action flag

int	i, bz, bs
double	temp
real	mean, median, mode, sigma
pointer	sp, str, image, im,

int	imtopenp(), strdic(), gctod(), clgeti(), imtgetim()
bool	clgetb()
pointer	immap()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)

	list = imtopenp ("images")
	call clgstr ("bzero", Memc[str], SZ_LINE)
	bz = strdic (Memc[str], Memc[str], SZ_LINE, OPTIONS)
	if (bz == 0) {
	    i = 1
	    if (gctod (Memc[str], i, temp) == 0)
		call error (0, "Invalid `bzero' parameter")
	    bzero = temp
	}
	call clgstr ("bscale", Memc[str], SZ_LINE)
	bs = strdic (Memc[str], Memc[str], SZ_LINE, OPTIONS)
	if (bs == 0) {
	    i = 1
	    if (gctod (Memc[str], i, temp) == 0)
		call error (0, "Invalid `bscale' parameter")
	    bscale = temp
	}
	call clgstr ("section", Memc[section], SZ_FNAME)
	step = max (1, clgeti ("step"))
	call clgstr ("logfile", Memc[logfile], SZ_FNAME)
	noact = clgetb ("noact")
	    
	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
	    iferr (im = immap (Memc[image], READ_WRITE, 0)) {
		call erract (EA_WARN)
		next
	    }

	    if ((bz != 0) || (bs != 0))
		call bs_imstats (im, Memc[section], step, BINWIDTH, BINSEP,
		    mean, median, mode, sigma)
	    else {
		mean = INDEF
		median = INDEF
		mode = INDEF
	    }

	    switch (bz) {
	    case MODE:
		bzero = mode
	    case MEAN:
		bzero = mean
	    case MEDIAN:
		bzero = median
	    }

	    switch (bs) {
	    case MODE:
		bscale = mode
	    case MEAN:
		bscale = mean
	    case MEDIAN:
		bscale = median
	    }

	    iferr (call bs_log (Memc[logfile], Memc[image], mean, median, mode,
		bzero, bscale))
		call error (EA_WARN)

	    if (!noact) {
		call bs_scale (im, bzero, bscale)
		call imaddr (im, "BS_ZERO", bzero)
		call imaddr (im, "BS_SCALE", bscale)
	    }

	    call imunmap (im)
	}

	call imtclose (list)
	call sfree (sp)
end


# BSCALE -- Scale image brightness.

procedure bs_scale (im, bzero, bscale)

pointer	im		# IMIO pointer
real	bzero		# Zero point
real	bscale		# Scale

int	nc
real	bz, bs
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	in, out, imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	bz = -bzero
	bs = 1. / bscale
	nc = IM_LEN(im,1)
	while ((imgnlr (im, in, v1) != EOF) && (impnlr (im, out, v2) != EOF))
	    call altar (Memr[in], Memr[out], nc, bz, bs)
end


# BS_LOG -- Log the scaling operation.

procedure bs_log (logfile, image, mean, median, mode, bzero, bscale)

char	logfile[ARB]		# Log file (STDOUT for terminal, null for none)
char	image[ARB]		# Image name
real	mean			# Image mean
real	median			# Image median
real	mode			# Image mode
real	bzero, bscale			# Scale values

int	log, open()
long	clktime()
pointer	sp, time
errchk	open

begin
	if (logfile[1] == EOS)
	    return

	call smark (sp)
	call salloc (time, SZ_DATE, TY_CHAR)
	call cnvdate (clktime(0), Memc[time], SZ_DATE)

	log = open (logfile, APPEND, TEXT_FILE)
	call fprintf (log, "%s BSCALE: image = %s\n  bzero=%g  bscale=%g")
	    call pargstr (Memc[time])
	    call pargstr (image)
	    call pargr (bzero)
	    call pargr (bscale)
	if (!IS_INDEF(mean)) {
	    call fprintf (log, "  mean=%g  median=%g  mode=%g")
		call pargr (mean)
		call pargr (median)
		call pargr (mode)
	}
	call fprintf (log, "\n")
	call close (log)

	call sfree (sp)
end


# BS_IMSTATS -- Compute the image statistics within a section of an image.
# This routine parses the image section and samples the image.  The actual
# statistics are evaluated by BS_STATS.

procedure bs_imstats (im, section, step, binwidth, binsep, mean, median, mode,
    sigma)

pointer	im			# Image
char	section[ARB]		# Sample section
int	step			# Default sample step
real	binwidth		# Bin width
real	binsep			# Separation between bins
real	mean			# Mean
real	median			# Median
real	mode			# Mode
real	sigma			# Sigma

int	i, n, nx, ndim
pointer	sp, x1, x2, xs, v, v1, dv, data, ptr1, ptr2, imgnlr()

begin
	call smark (sp)
	call salloc (x1, IM_MAXDIM, TY_INT)
	call salloc (x2, IM_MAXDIM, TY_INT)
	call salloc (xs, IM_MAXDIM, TY_INT)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (dv, IM_MAXDIM, TY_LONG)

	# Parse the sample section.
	ndim = IM_NDIM(im)
	do i = 1, ndim {
	    Memi[x1+i-1] = 1
	    Memi[x2+i-1] = IM_LEN(im,i)
	    Memi[xs+i-1] = step
	}
	call bs_section (section, Memi[x1], Memi[x2], Memi[xs], ndim)

	# Accumulate the pixel values within the section.
	n = 1
	do i = 1, ndim {
	    nx = (Memi[x2+i-1] - Memi[x1+i-1]) / Memi[xs+i-1] + 1
	    Meml[v+i-1] = Memi[x1+i-1]
	    if (nx == 1)
		Meml[dv+i-1] = 1
	    else
	        Meml[dv+i-1] = (Memi[x2+i-1] - Memi[x1+i-1]) / (nx - 1)
	    n = n * nx
	}
	Meml[v] = 1

	call salloc (data, n, TY_REAL)
	ptr1 = data
	call amovl (Meml[v], Meml[v1], IM_MAXDIM)
	while (imgnlr (im, ptr2, Meml[v1]) != EOF) {
	    ptr2 = ptr2 + Memi[x1] - 1
	    do i = Memi[x1], Memi[x2], Meml[dv] {
		Memr[ptr1] = Memr[ptr2]
		ptr1 = ptr1 + 1
		ptr2 = ptr2 + Meml[dv]
	    }
	    for (i=2; i<=ndim; i=i+1) {
		Meml[v+i-1] = Meml[v+i-1] + Meml[dv+i-1]
		if (Meml[v+i-1] <= Memi[x2+i-1])
		    break
		Meml[v+i-1] = Memi[x1+i-1]
	    }
	    if (i > ndim)
		break
	    call amovl (Meml[v], Meml[v1], IM_MAXDIM)
	}

	call bs_stats (Memr[data], n, binwidth, binsep, mean, median, mode,
	    sigma)
	call sfree (sp)
end


define	EXCLUDE1	0.	# Fraction of low pixels to exclude.
define	EXCLUDE2	0.	# Fraction of high pixels to exclude.

# BS_STATS -- Compute the vector statistics.
# 1. Sort the data
# 2. The median is at the midpoint of the sorted data
# 3. Exclude extreme points
# 4. Compute the mean
# 5. Compute the sigmas about the mean
# 6. Scale the bin width and separations by the sigma
# 7. Find the mode over all the bins (which may overlap)

procedure bs_stats (data, npts, binwidth, binsep, mean, median, mode, sigma)

real	data[npts]		# Data array which will be sorted.
int	npts			# Number of data points
real	binwidth		# Bin width
real	binsep			# Separation between bins
real	mean			# Mean
real	median			# Median
real	mode			# Mode
real	sigma			# Sigma

int	x1, x2, x3, n, nmax
real	width, sep, y1, y2
int	bs_awvgr()

begin
	# Initialize.
	if (npts <= 0) {
	    mean = INDEFR
	    median = INDEFR
	    mode = INDEFR
	    sigma = INDEFR
	    return
	}

	# Compute the lower and upper data bounds.
	x1 = 1 + npts * EXCLUDE1
	x3 = npts * (1. - EXCLUDE2)
	n = x3 - x1 + 1

	# Compute the median.
	call asrtr (data[x1], data[x1], n)
	median = data[x1+npts/2-1]
	mode = median

	# Compute the mean and sigma.
	if (bs_awvgr (data[x1], n, mean, sigma, 0.0, 0.0) <= 0)
	    return

	# Check for no dispersion in the data.
	if (sigma <= 0.0)
	    return

	width = binwidth * sigma
	sep = binsep * sigma

	# Compute the mode.
	nmax = 0
	x2 = x1
	for (y1 = data[x1]; x2 < x3; y1 = y1 + sep) {
	    for (; data[x1] < y1; x1 = x1 + 1)
		;
	    y2 = y1 + width
	    for (; (x2 < x3) && (data[x2] < y2); x2 = x2 + 1)
		;
	    if (x2 - x1 > nmax) {
	        nmax = x2 - x1
	        mode = data[(x2+x1)/2]
	    }
	}
end


# BS_AWVGR -- Compute the mean and standard deviation (sigma) of a sample.
# Pixels whose value lies outside the specified lower and upper limits are
# not used. If the upper and lower limits have the same value (e.g., zero),
# no limit checking is performed.  The number of pixels in the sample is
# returned as the function value.

int procedure bs_awvgr (a, npix, mean, sigma, lcut, hcut)

real	a[ARB]
real	mean, sigma, lcut, hcut, value
real	sum, sumsq, temp
int	npix, i, ngpix

begin
	sum = 0.0
	sumsq = 0.0
	ngpix = 0

	# Accumulate sum, sum of squares.  The test to disable limit checking
	# requires numerical equality of two floating point numbers; this should
	# be ok since they are used as flags not as numbers (they are not used
	# in computations).

	if (hcut == lcut) {
	    do i = 1, npix {
		    value = a[i]
		sum = sum + value
		sumsq = sumsq + value ** 2
	    }
	    ngpix = npix

	} else {
	    do i = 1, npix {
		    value = a[i]
		if (value >= lcut && value <= hcut) {
		    ngpix = ngpix + 1
		    sum = sum + value
		    sumsq = sumsq + value ** 2
		}
	    }
	}

	switch (ngpix) {		# compute mean and sigma
	case 0:
	    mean = INDEFR
	    sigma = INDEFR
	case 1:
	    mean = sum
	    sigma = INDEFR
	default:
	    mean = sum / ngpix
	    temp = (sumsq - mean * sum) / (ngpix - 1)
	    if (temp <= 0.0)
		sigma = 0.0
	    else
		sigma = sqrt (temp)
	}

	return (ngpix)
end


# BS_SECTION -- Parse an image section into its elements.
# 1. The default values must be set by the caller.
# 2. A null image section is OK.
# 3. The first nonwhitespace character must be '['.
# 4. The last interpreted character must be ']'.
#
# This procedure should be replaced with an IMIO procedure at some
# point.

procedure bs_section (section, x1, x2, xs, ndim)

char	section[ARB]		# Image section
int	x1[ndim]		# Starting pixel
int	x2[ndim]		# Ending pixel
int	xs[ndim]		# Step
int	ndim			# Number of dimensions

int	i, ip, a, b, c, temp, ctoi()
define	error_	99

begin
	# Decode the section string.
	ip = 1
	while (IS_WHITE(section[ip]))
	    ip = ip + 1
	if (section[ip] == '[')
	    ip = ip + 1
	else if (section[ip] == EOS)
	    return
	else
	    goto error_

	do i = 1, ndim {
	    while (IS_WHITE(section[ip]))
	        ip = ip + 1
	    if (section[ip] == ']')
		break

	    # Default values
	    a = x1[i]
	    b = x2[i]
	    c = xs[i]

	    # Get a:b:c.  Allow notation such as "-*:c"
	    # (or even "-:c") where the step is obviously negative.

	    if (ctoi (section, ip, temp) > 0) {			# a
		a = temp
	        if (section[ip] == ':') {	
		    ip = ip + 1
		    if (ctoi (section, ip, b) == 0)		# a:b
		        goto error_
	        } else
		    b = a
	    } else if (section[ip] == '-') {			# -*
		temp = a
		a = b
		b = temp
	        ip = ip + 1
	        if (section[ip] == '*')
		    ip = ip + 1
	    } else if (section[ip] == '*')			# *
	        ip = ip + 1
	    if (section[ip] == ':') {				# ..:step
	        ip = ip + 1
	        if (ctoi (section, ip, c) == 0)
		    goto error_
	        else if (c == 0)
		    goto error_
	    }
	    if (a > b && c > 0)
	        c = -c

	    x1[i] = a
	    x2[i] = b
	    xs[i] = c

	    while (IS_WHITE(section[ip]))
	        ip = ip + 1
	    if (section[ip] == ',')
		ip = ip + 1
	}

	if (section[ip] != ']')
	    goto error_

	return
error_
	call error (0, "Error in image section specification")
end
