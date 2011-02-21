include	<imhdr.h>
include	<error.h>
include	<ctype.h>
include <mach.h>

define	OPTIONS		"|mean|median|mode|"
define	MEAN	1	# mean of image
define	MEDIAN	2	# median of image
define	MODE	3	# mode of image

define	BINWIDTH	0.1	# bin width (in sigmas) for mode 
define	BINSEP		0.01	# bin separation (in sigmas) for mode

# T_BSCALE -- Linearly transform the intensity scales of a list of images
# using the following expression.
#
#	out = (in - bzero) / bscale

procedure t_bscale ()

pointer inlist		# list of input images
pointer outlist		# list of output images
real	bzero		# zero point
real	bscale		# scale factor
real    lower           # lower limit for mean, median, or mode computation
real    upper           # upper limit for mean, median, or mode computation
pointer	section		# image section for statistics
int	step		# default sample step
int	verbose		# verbose mode

double	temp
int	i, bz, bs 
real	mean, median, mode, sigma, tlower, tupper 
pointer	sp, str, image1, image2, imtemp, inim, outim

bool	clgetb()
int	btoi(), imtopenp(), strdic(), gctod(), clgeti(), imtgetim(), imtlen()
pointer	immap()
real	clgetr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (image2, SZ_FNAME, TY_CHAR)
	call salloc (imtemp, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)

	# Open the input and output image lists.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
        if (imtlen(inlist) != imtlen(outlist)) { 
           call sfree (sp)
           call imtclose (inlist)
           call imtclose (outlist)
           call error (0, "Length of input and output lists not equal.")
        }

	# Get the zero point algorithm.
	call clgstr ("bzero", Memc[str], SZ_LINE)
	bz = strdic (Memc[str], Memc[str], SZ_LINE, OPTIONS)
	if (bz == 0) {
	    i = 1
	    if (gctod (Memc[str], i, temp) == 0)
		call error (0, "Invalid `bzero' parameter")
	    bzero = temp
	}

	# Get the scale algorithm.
	call clgstr ("bscale", Memc[str], SZ_LINE)
	bs = strdic (Memc[str], Memc[str], SZ_LINE, OPTIONS)
	if (bs == 0) {
	    i = 1
	    if (gctod (Memc[str], i, temp) == 0)
		call error (0, "Invalid `bscale' parameter")
	    bscale = temp
	}

	# Get the section to be used for statistics computation.
	call clgstr ("section", Memc[section], SZ_FNAME)
	step = max (1, clgeti ("step"))

	# Get the upper and lower good data limits.
	lower = clgetr ("lower")
	if (IS_INDEFR(lower))
	    tlower = -MAX_REAL
	else
	    tlower = lower
	upper = clgetr ("upper")	
	if (IS_INDEFR(upper))
	    tupper = MAX_REAL
	else
	    tupper = upper

	verbose = btoi (clgetb ("verbose"))
	    
	# Loop over the input and output image lists.
	while ((imtgetim (inlist, Memc[image1], SZ_FNAME) != EOF) &&
              (imtgetim (outlist, Memc[image2], SZ_FNAME) != EOF)) {

	    # Open the input and output images.
            call xt_mkimtemp (Memc[image1], Memc[image2], Memc[imtemp],
                SZ_FNAME)
	    iferr (inim = immap (Memc[image1], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }
	    iferr (outim = immap (Memc[image2], NEW_COPY, inim)) {
		call imunmap (inim)
		call erract (EA_WARN)
		next
	    }

	    # Compute the required statistics.
	    if ((bz != 0) || (bs != 0))
		call bs_imstats (inim, Memc[section], step, BINWIDTH, BINSEP,
		    mean, median, mode, sigma, tupper, tlower)
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

	    # Log the output.
	    if (verbose == YES) {
	        call bs_log (Memc[image1], Memc[imtemp], mean, median, mode,
		    bzero, bscale, upper, lower)
		call flush (STDOUT)
	    }

	    # Scale the image.
	    call bs_scale (inim, outim, bzero, bscale)

	    call imunmap (inim)
	    call imunmap (outim)
            call xt_delimtemp (Memc[image2], Memc[imtemp])
	}

	call imtclose (inlist)
	call imtclose (outlist)
	call sfree (sp)
end


# BSCALE -- Scale the image brightness.

procedure bs_scale (inim, outim, bzero, bscale)

pointer	inim		# pointer to the input image
pointer	outim		# pointer to the output image
real	bzero		# zero point
real	bscale		# scale

int	nc
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
real	bz, bs

pointer	in, out
int	imgnlr(), impnlr(), imgnlx(), impnlx(), imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	bz = -bzero
	bs = 1. / bscale
	nc = IM_LEN(inim,1)

	switch (IM_PIXTYPE(inim)) {
	case TY_DOUBLE :
	  while ((imgnld (inim, in, v1) != EOF) && (impnld (outim,
	      out, v2) != EOF))
	  call altad (Memd[in], Memd[out], nc, double(bz), double(bs))

	case TY_COMPLEX:
	  while ((imgnlx (inim, in, v1) != EOF) && (impnlx (outim,
              out, v2) != EOF))
	  call altax (Memx[in], Memx[out], nc, bz, bs)

	default:
	  while ((imgnlr (inim, in, v1) != EOF) && (impnlr (outim,
              out, v2) != EOF))
	  call altar (Memr[in], Memr[out], nc, bz, bs)
	}
end


# BS_LOG -- Log the scaling operation.

procedure bs_log (image1, image2, mean, median, mode, bzero, bscale, upper,
	lower)

char	image1[ARB]		# input image name
char	image2[ARB]		# output image name
real	mean			# input image mean
real	median			# input image median
real	mode			# input image mode
real	bzero, bscale		# the computed scale values
real    upper                   # upper limit for mean
real    lower                   # lower limit for mean

begin
	call printf ("%s -> %s  using bzero: %g  and bscale: %g\n")
	    call pargstr (image1)
	    call pargstr (image2)
	    call pargr (bzero)
	    call pargr (bscale)

	if (! IS_INDEF(mean)) {
	    call printf ("    mean: %g  median: %g  mode: %g  ")
		call pargr (mean)
		call pargr (median)
		call pargr (mode)
	    call printf ("    upper: %g  lower: %g\n")
	        call pargr (upper)
                call pargr (lower)
	}
end


# BS_IMSTATS -- Compute the image statistics within a section of an image.
# This routine parses the image section and samples the image.  The actual
# statistics are evaluated by BS_STATS.

procedure bs_imstats (im, section, step, binwidth, binsep, mean, median, mode,
        sigma, upper, lower)

pointer	im			# input image
char	section[ARB]		# sample section
int	step			# default sample step
real	binwidth		# bin width
real	binsep			# separation between bins
real	mean			# mean
real	median			# median
real	mode			# mode
real	sigma			# sigma
real    upper			# upper limit for statistics 
real	lower			# lower limit for statistics 

int	i, n, nx, ndim
pointer	sp, x1, x2, xs, v, v1, dv, data, ptr1, ptr2
int	imgnlr()

begin
	call smark (sp)
	call salloc (x1, IM_MAXDIM, TY_INT)
	call salloc (x2, IM_MAXDIM, TY_INT)
	call salloc (xs, IM_MAXDIM, TY_INT)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (dv, IM_MAXDIM, TY_LONG)

	# Initialize the section.
	ndim = IM_NDIM(im)
	do i = 1, ndim {
	    Memi[x1+i-1] = 1
	    Memi[x2+i-1] = IM_LEN(im,i)
	    Memi[xs+i-1] = 0
	}

	# Parse the sample section.
	call bs_section (section, Memi[x1], Memi[x2], Memi[xs], ndim)

	# Check the step sizes.
	do i = 1, ndim {
	    if (Memi[xs+i-1] == 0)
		Memi[xs+i-1] = step
	}

	# Define the region of the image to be extracted.
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

	# Accumulate the pixel values within the section.
	call salloc (data, n, TY_REAL)
	Meml[v] = 1
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

	# Compute the statistics.
	call bs_stats (Memr[data], n, binwidth, binsep, mean, median, mode,
	    sigma, upper, lower)

	call sfree (sp)
end


# BS_STATS -- Compute the vector statistics.
#
# 1. Sort the data
# 2. Exclude the extreme points
# 3. The median is at the midpoint of the sorted data
# 4. Compute the mean
# 5. Compute the sigmas about the mean
# 6. Scale the bin width and separations by the sigma
# 7. Find the mode over all the bins (which may overlap)

procedure bs_stats (data, npts, binwidth, binsep, mean, median, mode, sigma,
	upper, lower)

real	data[npts]		# sata array which will be sorted.
int	npts			# number of data points
real	binwidth		# bin width
real	binsep			# separation between bins
real	mean			# mean
real	median			# median
real	mode			# mode
real	sigma			# sigma
real	upper			# upper limit for mean
real	lower			# lower limit for mean

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

	# Sort the data.
	call asrtr (data, data, npts)

	# Find the array indices for the lower and upper data bounds.
        x1 = 1
        while (data[x1] < lower)
	   x1 = x1 + 1
        x3 = npts
        while (data[x3] > upper)
           x3 = x3 - 1

	# Assign number of elements within the bounds.
	n = x3 - x1 + 1

	# Compute the median.
	median = data[x1 + n/2 - 1]
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

real	a[ARB]		# input array of data
int	npix		# the number of data points
real	mean		# the computed mean
real	sigma		# the computed standard deviation
real	lcut, hcut	# lower and upper cutoff for statistics calculation

int	i, ngpix
real	value, sum, sumsq, temp

begin
	sum = 0.0
	sumsq = 0.0
	ngpix = 0

	# Accumulate sum, sum of squares.  The test to disable limit checking
	# requires numerical equality of two floating point numbers; this
	# should be ok since they are used as flags not as numbers (they are
	# not used in computations).

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
#
# 1. The default values must be set by the caller.
# 2. A null image section is OK.
# 3. The first nonwhitespace character must be '['.
# 4. The last interpreted character must be ']'.
#
# This procedure should be replaced with an IMIO procedure at some point.

procedure bs_section (section, x1, x2, xs, ndim)

char	section[ARB]		# Image section
int	x1[ndim]		# Starting pixel
int	x2[ndim]		# Ending pixel
int	xs[ndim]		# Step size in pixels
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
		c = 1
	    } else if (section[ip] == '-') {			# -*
		temp = a
		a = b
		b = temp
		c = 1
	        ip = ip + 1
	        if (section[ip] == '*')
		    ip = ip + 1
	    } else if (section[ip] == '*') {			# *
		c = 1
	        ip = ip + 1
	    }

	    if (section[ip] == ':') {				# :step
	        ip = ip + 1
	        if (ctoi (section, ip, c) == 0)
		    goto error_
	        else if (c == 0)
		    goto error_
	    }

	    if ((a > b) && (c > 0))
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
