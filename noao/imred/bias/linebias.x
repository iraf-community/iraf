include	<imhdr.h>
include	<imio.h>
include <pkg/gtools.h>
include	<pkg/xtanswer.h>

# LINEBIAS -- Remove column by column bias from images.
#
# A one dimensional bias vector is extracted from the bias lines.
# A function is fit to the bias vector and the function is subtracted
# from the image columns.  A trim section may be specified to output
# only a part of the bias subtracted image.

# Control procedure for mapping the images.
#
# The input and output images are given by image templates.  The
# number of output images must match the number of input images.  Image
# sections are not allowed.  The output image may be the same as the input
# image.

procedure linebias ()

int	listin				# List of input images
int	listout				# List of output images
int	logfiles			# List of log files
char	biassec[SZ_FNAME]		# Bias section
char	trimsec[SZ_FNAME]		# Trim section
int	median				# Median of bias section?
int	interactive			# Interactive?

char	function[SZ_LINE]		# Curve fitting function
int	order				# Order of curve fitting function

char	image[SZ_FNAME]
char	input[SZ_FNAME]
char	biasimage[SZ_FNAME]
char	output[SZ_FNAME]
char	logfile[SZ_FNAME]
char	original[SZ_FNAME]
char	title[SZ_LINE]

int	logfd
pointer	in, bias, out, ic, gt

int	clgeti(), clpopnu(), clgfil(), open(), gt_init(), nowhite()
int	imtopen(), imtlen(), imtgetim(), btoi()
bool	clgetb()
long	clktime()
pointer	immap()
real	clgetr()

begin
	# Get input and output lists and check that the number of images
	# are the same.

	call clgstr ("input", title, SZ_LINE)
	listin = imtopen (title)
	call clgstr ("output", title, SZ_LINE)
	listout = imtopen (title)
	if (imtlen (listin) != imtlen (listout)) {
	    call imtclose (listin)
	    call imtclose (listout)
	    call error (0, "Input and output image lists do not match")
	}

	# Get the bias and trim sections.

	call clgstr ("bias", biassec, SZ_FNAME)
	call clgstr ("trim", trimsec, SZ_FNAME)
	if (nowhite (biassec, biassec, SZ_FNAME) == 0)
	    ;
	if (nowhite (trimsec, trimsec, SZ_FNAME) == 0)
	    ;
	median = btoi (clgetb ("median"))

	# Determine if the task is interactive.  If not set the interactive
	# flag to always no.

	if (clgetb ("interactive"))
	    interactive = YES
	else
	    interactive = ALWAYSNO

	# Initialize the curve fitting package.

	call ic_open (ic)
	call clgstr ("function", function, SZ_LINE)
	call ic_pstr (ic, "function", function)
	order = clgeti ("order")
	call ic_puti (ic, "order", order)
	call ic_putr (ic, "low", clgetr ("low_reject"))
	call ic_putr (ic, "high", clgetr ("high_reject"))
	call ic_puti (ic, "niterate", clgeti ("niterate"))
	call ic_pstr (ic, "xlabel", "Column")
	call ic_pstr (ic, "ylabel", "Bias")

	gt = gt_init()
	call gt_sets (gt, GTTYPE, "line")

	# Get the list of log files.

	logfiles = clpopnu ("logfiles")

	# For each input and output image map the bias image, the
	# trimmed input image, and the output image.  Use a temporary
	# image header for overwritting the input image.

	while ((imtgetim (listin, image, SZ_FNAME) != EOF) &&
	    (imtgetim (listout, output, SZ_FNAME) != EOF)) {

	    call sprintf (biasimage, SZ_FNAME, "%s%s")
		call pargstr (image)
		call pargstr (biassec)
	    call sprintf (input, SZ_FNAME, "%s%s")
		call pargstr (image)
		call pargstr (trimsec)

	    in = immap (input, READ_ONLY, 0)
	    bias = immap (biasimage, READ_ONLY, 0)
	    call xt_mkimtemp (image, output, original, SZ_FNAME)
	    out = immap (output, NEW_COPY, in)
	    IM_PIXTYPE(out) = TY_REAL

	    call sprintf (title, SZ_LINE, "linebias %s")
		call pargstr (image)
	    call xt_answer (title, interactive)
	    call gt_sets (gt, GTTITLE, title)

	    # Enter a header in the log file.

	    while (clgfil (logfiles, logfile, SZ_FNAME) != EOF) {
		logfd = open (logfile, APPEND, TEXT_FILE)
	        call cnvtime (clktime (0), title, SZ_LINE)
	        call fprintf (logfd, "\nLINEBIAS:  %s\n")
		    call pargstr (title)
		call fprintf (logfd, "input = %s\noutput = %s\nbias = %s\n")
		    call pargstr (input)
		    call pargstr (output)
		    call pargstr (biasimage)
		if (median == YES)
		    call fprintf (logfd, "Median of bias section used.\n")
		call close (logfd)
	    }
	    call clprew (logfiles)

	    call lb_linebias (in, bias, out, ic, gt, median, logfiles,
		interactive)

	    call imunmap (in)
	    call imunmap (bias)
	    call imunmap (out)
	    call xt_delimtemp (output, original)
	}

	call ic_closer (ic)
	call gt_free (gt)
	call clpcls (logfiles)
	call imtclose (listin)
	call imtclose (listout)
end


# LB_LINEBIAS -- Get an average line bias vector from the bias image.
# Fit a function to the bias vector and subtract it from the input image
# to form the output image.  Column coordinates are in terms of the full
# input image.

procedure lb_linebias (in, bias, out, ic, gt, median, logfiles, interactive)

pointer	in			# Input image pointer
pointer	bias			# Bias image pointer
pointer	out			# Output image pointer
pointer	ic			# ICFIT pointer
pointer	gt			# GTOOLS pointer
int	median			# Median of bias section?
int	logfiles		# List of log files
int	interactive		# Interactive curve fitting?

char	graphics[SZ_FNAME]	# Graphics output device
char	logfile[SZ_FNAME]
int	i, nbias, nx, ny, xdim, xoff, xstep, xlen
real	x
pointer	cv, gp, sp, xbias, zbias, wts, z

int	clgfil()
real	cveval()
pointer	gopen(), imgl2r(), impl2r()

begin
	# The bias coordinates are in terms of the full input image because
	# the input and bias images may have different sections.

	nx = IM_LEN(in, 1)
	ny = IM_LEN(in, 2)

	xdim = IM_VMAP(in, 1)
	xoff = IM_VOFF(in, xdim)
	xstep = IM_VSTEP(in, xdim)
	xlen = IM_SVLEN(in, xdim)

	# Get the bias vector and set the weights.

	call lb_getlinebias (bias, xbias, zbias, nbias, median)
	call smark (sp)
	call salloc (wts, nbias, TY_REAL)
	call amovkr (1., Memr[wts], nbias)

	# Do the curve fitting using the interactive curve fitting package.
	# Free memory when the fit is complete.

	call ic_putr (ic, "xmin", 1.)
	call ic_putr (ic, "xmax", real (xlen))
	if ((interactive == YES) || (interactive == ALWAYSYES)) {
	    call clgstr ("graphics", graphics, SZ_FNAME)
	    gp = gopen (graphics, NEW_FILE, STDGRAPH)
	    call gt_setr (gt, GTXMIN, 1.)
	    call gt_setr (gt, GTXMAX, real (xlen))
	    call icg_fit (ic, gp, "cursor", gt, cv, Memr[xbias], Memr[zbias],
		Memr[wts], nbias)
	    call gclose (gp)
	} else {
	    call ic_fit (ic, cv, Memr[xbias], Memr[zbias], Memr[wts], nbias,
		YES, YES, YES, YES)
	}

	# Log the fitting information.

	while (clgfil (logfiles, logfile, SZ_FNAME) != EOF) {
	    call ic_show (ic, logfile, gt)
	    call ic_errors (ic, logfile, cv, Memr[xbias], Memr[zbias],
		Memr[wts], nbias)
	}
	call clprew (logfiles)

	call mfree (xbias, TY_REAL)
	call mfree (zbias, TY_REAL)
	call sfree (sp)

	# Subtract the bias function from the input image..

	call smark (sp)
	call salloc (z, nx, TY_REAL)
	do i = 1, nx {
	    x = xoff + i * xstep
	    Memr[z+i-1] = cveval (cv, x)
	}

	do i = 1, ny
	    call asubr (Memr[imgl2r(in,i)], Memr[z], Memr[impl2r(out,i)], nx)

	# Free allocated memory.

	call cvfree (cv)
	call sfree (sp)
end


# LB_GETLINEBIAS -- Get the line bias vector.
# The xbias column values are in terms of the full image.

define	MAXPIX	100000		# Maximum number of pixels to buffer

procedure lb_getlinebias (bias, xbias, zbias, nbias, median)

pointer	bias			# Bias image pointer
pointer	xbias, zbias		# Bias vector
int	nbias			# Number of bias points
int	median			# Median of bias section?

int	i, j, k, n, nx, ny, xdim, xoff, xstep, maxn
pointer	buf1, buf2

real	amedr()
pointer	imgl1r(), imgl2r(), imgs2r()

begin
	# Check for a bias consisting of a single line which is turned
	# into a 1D image by IMIO.
	if (IM_NDIM(bias) == 1) {
	    nx = IM_LEN(bias, 1)
	    xdim = IM_VMAP(bias, 1)
	    xoff = IM_VOFF(bias, xdim)
	    xstep = IM_VSTEP(bias, xdim)

	    nbias = nx
	    call malloc (xbias, nbias, TY_REAL)
	    call malloc (zbias, nbias, TY_REAL)

	    do i = 1, nbias
		Memr[xbias+i-1] = xoff + i * xstep
	    call amovr (Memr[imgl1r(bias)], Memr[zbias], nbias)

	    return
	}

	nx = IM_LEN(bias, 1)
	ny = IM_LEN(bias, 2)
	xdim = IM_VMAP(bias, 1)
	xoff = IM_VOFF(bias, xdim)
	xstep = IM_VSTEP(bias, xdim)
	
	nbias = nx
	call malloc (xbias, nbias, TY_REAL)
	call calloc (zbias, nbias, TY_REAL)

	if (median == NO) {
	    do i = 1, ny
	        call aaddr (Memr[imgl2r(bias,i)], Memr[zbias], Memr[zbias], nx)
	    call adivkr (Memr[zbias], real (ny), Memr[zbias], nx)
	} else {
	    call malloc (buf1, ny, TY_REAL)

	    maxn = MAXPIX / ny
	    j = 1
	    do i = 1, nx {
		if (i == j) {
	            n = min (nx - j + 1, maxn)
		    buf2 = imgs2r (bias, j, j + n - 1, 1, ny)
		    j = j + n
		}
		do k = 1, ny
		    Memr[buf1+k-1] = Memr[buf2+k*n+i-j]
		Memr[zbias+i-1] = amedr (Memr[buf1], ny)
	    }

	    call mfree (buf1, TY_REAL)
	}

	do i = 1, nx
	    Memr[xbias+i-1] = xoff + i * xstep
end
