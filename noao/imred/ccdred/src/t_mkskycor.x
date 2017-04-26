include <imhdr.h>
include <imset.h>
include	<mach.h>
include	"ccdred.h"

define	MINSIGMA	1.	# Minimum sigma
define	NITERATE	10	# Maximum number of clipping iterations

# T_MKSKYCOR -- Make sky illumination correction images.
#
# The input images processed and smoothed to obtain an illumination correction
# image.  This task is a version of T_CCDPROC which treats the images as
# illumination images regardless of there CCD image type.

procedure t_mkskycor()

int	listin			# List of input CCD images
int	listout			# List of output CCD images
int	ccdtype			# CCD image type
int	interactive		# Fit overscan interactively?

bool	flatcor, ccdflag(), clgetb(), streq()
int	imtopenp(), imtgetim()
pointer	sp, input, output, tmp, str, in, out, ccd
errchk	set_input, set_output, ccddelete

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (tmp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the lists and instrument translation file.  Open the translation
	# file.  Initialize the interactive flag and the calibration images.

	listin = imtopenp ("input")
	listout = imtopenp ("mkskycor.output")
	call clgstr ("instrument", Memc[input], SZ_FNAME)
	call hdmopen (Memc[input])
	call set_interactive ("", interactive)
	call cal_open (NULL)
	call ccd_open (0)

	# Process each image.
	while (imtgetim (listin, Memc[input], SZ_FNAME) != EOF) {
	    if (clgetb ("noproc")) {
		call printf ("%s: mkskycor\n")
		    call pargstr (Memc[input])
	    }

	    #  Set input and output images.
	    call set_input (Memc[input], in, ccdtype)
	    if (in == NULL)
		next

	    if (imtgetim (listout, Memc[output], SZ_FNAME) == EOF)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    if (Memc[output] == EOS)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    if (streq (Memc[input], Memc[output]))
	        call mktemp ("tmp", Memc[tmp], SZ_FNAME)
	    else
		call strcpy (Memc[output], Memc[tmp], SZ_FNAME)
	    call set_output (in, out, Memc[tmp])

	    # Process image as an illumination image.
	    call set_proc (in, out, ccd)
	    call set_sections (ccd)
	    call set_trim (ccd)
	    call set_fixpix (ccd)
	    call set_overscan (ccd)
	    call set_zero (ccd)
	    call set_dark (ccd)
	    call set_flat (ccd)

	    # Do the processing if the COR flag is set.
	    if (COR(ccd) == YES) {
	        call doproc (ccd)
	        call set_header (ccd)

		# Replace the input image by the corrected image.
	        flatcor = ccdflag (out, "flatcor")
	        call imunmap (in)
	        call imunmap (out)
		if (streq (Memc[input], Memc[output])) {
		    call ccddelete (Memc[input])
	            call imrename (Memc[tmp], Memc[input])
		} else
		    call strcpy (Memc[output], Memc[input], SZ_FNAME)
	    } else {
		# Make a copy if necessary.
	        flatcor = ccdflag (out, "flatcor")
	        call imunmap (in)
	        call imunmap (out)
		call imdelete (Memc[tmp])
	    }
	    call free_proc (ccd)

	    # Do special processing.
	    if (!flatcor) {
	        call eprintf (
		    "%s: WARNING - Image should be flat fielded first\n")
		    call pargstr (Memc[input])
	    }
	    call mkillumination (Memc[input], Memc[output], NO, YES)
	    if (!streq (Memc[input], Memc[output]))
		call ccdcopy (Memc[input], Memc[output])
	}

	# Finish up.
	call hdmclose ()
	call imtclose (listin)
	call imtclose (listout)
	call cal_close ()
	call ccd_close ()
	call sfree (sp)
end


# MKILLUMINATION -- Make illumination images.
#
# The images are boxcar smoothed to obtain the large scale illumination.
# Objects in the images are excluded from the average by sigma clipping.

procedure mkillumination (input, output, inverse, log)

char	input[SZ_FNAME]		# Input image
char	output[SZ_FNAME]	# Output image
int	inverse			# Return inverse of illumination
int	log			# Add log info?

real	xbminr			# Minimum size of X smoothing box
real	ybminr			# Minimum size of Y smoothing box
real	xbmaxr			# Maximum size of X smoothing box
real	ybmaxr			# Maximum size of Y smoothing box
bool	clip			# Sigma clip
real	lowsigma		# Low sigma clip
real	highsigma		# High sigma clip

int	xbmin, ybmin, xbmax, ybmax
pointer	sp, str, tmp, in, out, out1

bool	clgetb(), ccdflag(), streq()
real	clgetr()
pointer	immap()
errchk	immap, ccddelete

real	rdivzero	# Result for divion by zero
int	ndivzero	# Number of zero divisions
common	/cdivzero/ rdivzero, ndivzero

begin
	# Check if this operation has been done.  Unfortunately this requires
	# mapping the image.

	in = immap (input, READ_ONLY, 0)
	if (ccdflag (in, "mkillum")) {
	    call imunmap (in)
	    return
	}

	if (clgetb ("noproc")) {
	    call eprintf (
		"  [TO BE DONE] Convert %s to illumination correction\n")
		call pargstr (input)
	    call imunmap (in)
	    return
	}

	# Get task parameters
	xbminr = clgetr ("xboxmin")
	ybminr = clgetr ("yboxmin")
	xbmaxr = clgetr ("xboxmax")
	ybmaxr = clgetr ("yboxmax")
	clip = clgetb ("clip")
	if (clip) {
	    lowsigma = max (MINSIGMA, clgetr ("lowsigma"))
	    highsigma = max (MINSIGMA, clgetr ("highsigma"))
	}
	if (inverse == YES)
	    rdivzero = clgetr ("divbyzero")
	ndivzero = 0

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (tmp, SZ_FNAME, TY_CHAR)

	# Create output.
	if (streq (input, output)) {
	    call mktemp ("tmp", Memc[tmp], SZ_FNAME)
	    call set_output (in, out, Memc[tmp])
	    out1 = in
	} else {
	    call set_output (in, out, output)
	    out1 = out
	}

	if (xbminr < 1.)
	    xbminr = xbminr * IM_LEN(in,1)
	if (ybminr < 1.)
	    ybminr = ybminr * IM_LEN(in,2)
	if (xbmaxr < 1.)
	    xbmaxr = xbmaxr * IM_LEN(in,1)
	if (ybmaxr < 1.)
	    ybmaxr = ybmaxr * IM_LEN(in,2)

	xbmin = max (1, min (IM_LEN(in,1), nint (min (xbminr, xbmaxr))))
	xbmax = max (1, min (IM_LEN(in,1), nint (max (xbminr, xbmaxr))))
	ybmin = max (1, min (IM_LEN(in,2), nint (min (ybminr, ybmaxr))))
	ybmax = max (1, min (IM_LEN(in,2), nint (max (ybminr, ybmaxr))))

	if (clip)
	    call illumination (in, out, xbmin, ybmin, xbmax, ybmax,
		lowsigma, highsigma, inverse)
	else
	    call qillumination (in, out, xbmin, ybmin, xbmax, ybmax, inverse)

	# Log the operation.
	if (log == YES) {
	    if (ndivzero > 0) {
		call sprintf (Memc[str], SZ_LINE,
		    "Warning: %d divisions by zero replaced by %g")
		    call pargi (ndivzero)
		    call pargr (rdivzero)
		call ccdlog (out1, Memc[str])
	    }
	    call sprintf (Memc[str], SZ_LINE,
		"Illumination correction created from %s")
		call pargstr (input)
	    call timelog (Memc[str], SZ_LINE)
	    call ccdlog (out1, Memc[str])
	}
	call hdmpstr (out, "mkillum", Memc[str])
	call hdmpstr (out, "imagetyp", "illum")

	# Finish up
	call imunmap (in)
	call imunmap (out)
	if (streq (input, output)) {
	    call ccddelete (input)
	    call imrename (Memc[tmp], input)
	} else
	    call strcpy (output, input, SZ_FNAME)
	call sfree (sp)
end


# ILLUMINATION -- Make illumination correction image with clipping.

procedure illumination (in, out, xbmin, ybmin, xbmax, ybmax, low, high, inverse)

pointer	in		# Pointer to the input image
pointer	out		# Pointer to the output image
int	xbmin, ybmin	# Minimum dimensions of the boxcar
int	xbmax, ybmax	# Maximum dimensions of the boxcar
real	low, high	# Clipping sigma thresholds
int	inverse		# Return inverse of illumination?

real	scale, ccdmean
int	i, ncols, nlines, linein, lineout, ybox2, nrej
pointer	sp, ptr, ptrs, data, sum, avg, output

long	clktime()
int	boxclean()
real	asumr(), divzero()
pointer	imgl2r(), impl2r()
extern	divzero()

begin
	# Set up an array of linepointers and accumulators
	ncols = IM_LEN(out,1)
	nlines = IM_LEN(out,2)
	call smark (sp)
	call salloc (ptrs, ybmax, TY_POINTER)
	call salloc (sum, ncols, TY_REAL)
	call salloc (avg, ncols, TY_REAL)
	if (inverse == YES)
	    call salloc (output, ncols, TY_REAL)
	else
	    output = avg

	# Set input buffers.
	if (ybmax < nlines)
	    call imseti (in, IM_NBUFS, ybmax)

	# Get the first average over the minimum y box.
	call aclrr (Memr[sum], ncols)
	linein = 0
	while (linein < ybmin) {
	    linein = linein + 1
	    data = imgl2r (in, linein)
	    call aaddr (Memr[data], Memr[sum], Memr[sum], ncols)
	    ptr = ptrs + mod (linein, ybmax)
	    Memi[ptr] = data
	}
	ybox2 = ybmin
	scale = ybmin
	call agboxcar (Memr[sum], Memr[avg], ncols, xbmin, xbmax, scale)

	# Iteratively clean the initial lines.
	ptr = ptrs
	if (ybox2 != ybmax)
	    ptr = ptr + 1
	do i = 1, NITERATE {
	    nrej = 0
	    do lineout = 1, linein {
		data = Memi[ptr+lineout-1]
		nrej = nrej + boxclean (Memr[data], Memr[avg], Memr[sum],
		    ncols, low, high)
	    }
	    if (nrej > 0)
		call agboxcar (Memr[sum], Memr[avg], ncols, xbmin, xbmax,
		    scale)
	    else
		break
	}

	# Output the minimum smoothing y box.
	if (inverse == YES)
	    call arczr (1., Memr[avg], Memr[output], ncols, divzero)
	ybox2 = (ybmin + 1) / 2
	lineout = 0
	while (lineout < ybox2) {
	    lineout = lineout + 1
	    call amovr (Memr[output], Memr[impl2r(out, lineout)], ncols)
	}
	ccdmean = ybox2 * asumr (Memr[output], ncols)

	# Increase the y box size by factors of 2 until the maximum size.
	while (linein < ybmax) {
	    linein = linein + 1
	    data = imgl2r (in, linein)
	    call aaddr (Memr[sum], Memr[data], Memr[sum], ncols)
	    ptr = ptrs + mod (linein, ybmax)
	    Memi[ptr] = data
	    scale = scale + 1

	    nrej = boxclean (Memr[data], Memr[avg], Memr[sum], ncols,
		low, high)
	    call agboxcar (Memr[sum], Memr[avg], ncols, xbmin, xbmax, scale)

	    linein = linein + 1
	    data = imgl2r (in, linein)
	    call aaddr (Memr[sum], Memr[data], Memr[sum], ncols)
	    ptr = ptrs + mod (linein, ybmax)
	    Memi[ptr] = data

	    nrej = boxclean (Memr[data], Memr[avg], Memr[sum], ncols, low, high)
	    scale = scale + 1
	    call agboxcar (Memr[sum], Memr[avg], ncols, xbmin, xbmax, scale)

	    lineout = lineout + 1
	    data = impl2r (out, lineout)
	    if (inverse == YES)
	        call arczr (1., Memr[avg], Memr[data], ncols, divzero)
	    else
		call amovr (Memr[avg], Memr[data], ncols)
	    ccdmean = ccdmean + asumr (Memr[data], ncols)
	}

	# For each line subtract the last line from the sum, add the
	# next line to the sum, and output a line.

	while (linein < nlines) {
	    linein = linein + 1
	    ptr = ptrs + mod (linein, ybmax)
	    data = Memi[ptr]
	    call asubr (Memr[sum], Memr[data], Memr[sum], ncols)
	    data = imgl2r (in, linein)
	    call aaddr (Memr[sum], Memr[data], Memr[sum], ncols)
	    Memi[ptr] = data

	    nrej = boxclean (Memr[data], Memr[avg], Memr[sum], ncols, low, high)

	    lineout = lineout + 1
	    data = impl2r (out, lineout)
	    call agboxcar (Memr[sum], Memr[avg], ncols, xbmin, xbmax, scale)

	    if (inverse == YES)
	        call arczr (1., Memr[avg], Memr[data], ncols, divzero)
	    else
		call amovr (Memr[avg], Memr[data], ncols)
	    ccdmean = ccdmean + asumr (Memr[data], ncols)
	}

	# Decrease the y box in factors of 2 until minimum y box.
	while (lineout < nlines - ybox2) {
	    linein = linein + 1
	    ptr = ptrs + mod (linein, ybmax)
	    data = Memi[ptr]
	    call asubr (Memr[sum], Memr[data], Memr[sum], ncols)
	    linein = linein + 1
	    ptr = ptrs + mod (linein, ybmax)
	    data = Memi[ptr]
	    call asubr (Memr[sum], Memr[data], Memr[sum], ncols)
	    scale = scale - 2

	    lineout = lineout + 1
	    data = impl2r (out, lineout)
	    call agboxcar (Memr[sum], Memr[data], ncols, xbmin, xbmax, scale)
	    if (inverse == YES)
	        call arczr (1., Memr[data], Memr[data], ncols, divzero)
	    ccdmean = ccdmean + asumr (Memr[data], ncols)
	}

	# Output the last lines of the minimum y box size.
	call agboxcar (Memr[sum], Memr[avg], ncols, xbmin, xbmax, scale)
	if (inverse == YES)
	    call arczr (1., Memr[avg], Memr[output], ncols, divzero)
	ybox2 = nlines - lineout
	while (lineout < nlines) {
	    lineout = lineout + 1
	    data = impl2r (out, lineout)
	    call amovr (Memr[output], Memr[data], ncols)
	}
	ccdmean = ccdmean + ybox2 * asumr (Memr[output], ncols)

	# Write scale factor out.
	ccdmean = ccdmean / (ncols * nlines)
	call hdmputr (out, "ccdmean", ccdmean)
	call hdmputi (out, "ccdmeant", int (clktime (long (0))))

	# Free buffers
	call sfree (sp)
end


# QILLUMCOR -- Quick (no clipping) illumination correction image.

procedure qillumination (in, out, xbmin, ybmin, xbmax, ybmax, inverse)

pointer	in		# pointer to the input image
pointer	out		# pointer to the output image
int	xbmin, ybmin	# Minimum dimensions of the boxcar
int	xbmax, ybmax	# Maximum dimensions of the boxcar
int	inverse		# return inverse of illumination

real	scale, ccdmean
int	ncols, nlines, linein, lineout, ybox1
pointer	sp, ptr, ptrs, data, sum, output

long	clktime()
real	asumr(), divzero()
pointer	imgl2r(), impl2r()
extern	divzero()

begin
	# Set up an array of linepointers and accumulators
	ncols = IM_LEN(out,1)
	nlines = IM_LEN(out,2)

	call smark (sp)
	call salloc (ptrs, ybmax, TY_POINTER)
	call salloc (sum, ncols, TY_REAL)
	call salloc (output, ncols, TY_REAL)

	# Set input buffers.
	if (ybmax < nlines)
	    call imseti (in, IM_NBUFS, ybmax)

	# Accumulate the minimum y box.
	call aclrr (Memr[sum], ncols)
	linein = 0
	while (linein < ybmin) {
	    linein = linein + 1
	    data = imgl2r (in, linein)
	    call aaddr (Memr[data], Memr[sum], Memr[sum],  ncols)
	    ptr = ptrs + mod (linein, ybmax)
	    Memi[ptr] = data
	}

	# Output the minimum y box.
	ybox1 = (ybmin + 1) / 2
	scale = ybmin
	call agboxcar (Memr[sum], Memr[output], ncols, xbmin, xbmax, scale)
	if (inverse == YES)
	    call arczr (1., Memr[output], Memr[output], ncols, divzero)
	lineout = 0
	while (lineout < ybox1) {
	    lineout = lineout + 1
	    data = impl2r (out, lineout)
	    call amovr (Memr[output], Memr[data], ncols)
	}
	ccdmean = ybox1 * asumr (Memr[output], ncols)

	# Increase the y box size by steps of 2 until the maximum size.
	while (linein < ybmax) {
	    linein = linein + 1
	    data = imgl2r (in, linein)
	    call aaddr (Memr[sum], Memr[data], Memr[sum], ncols)
	    ptr = ptrs + mod (linein, ybmax)
	    Memi[ptr] = data
	    linein = linein + 1
	    data = imgl2r (in, linein)
	    call aaddr (Memr[sum], Memr[data], Memr[sum], ncols)
	    ptr = ptrs + mod (linein, ybmax)
	    Memi[ptr] = data

	    scale = scale + 2
	    lineout = lineout + 1
	    data = impl2r (out, lineout)
	    call agboxcar (Memr[sum], Memr[data], ncols, xbmin, xbmax, scale)
	    if (inverse == YES)
	        call arczr (1., Memr[data], Memr[data], ncols, divzero)
	    ccdmean = ccdmean + asumr (Memr[data], ncols)
	}

	# For each line subtract the last line from the sum, add the
	# next line to the sum, and output a line.

	while (linein < nlines) {
	    linein = linein + 1
	    ptr = ptrs + mod (linein, ybmax)
	    data = Memi[ptr]
	    call asubr (Memr[sum], Memr[data], Memr[sum], ncols)
	    data = imgl2r (in, linein)
	    call aaddr (Memr[sum], Memr[data], Memr[sum], ncols)
	    Memi[ptr] = data

	    lineout = lineout + 1
	    data = impl2r (out, lineout)
	    call agboxcar (Memr[sum], Memr[data], ncols, xbmin, xbmax, scale)
	    if (inverse == YES)
		call arczr (1., Memr[data], Memr[data], ncols, divzero)
	    ccdmean = ccdmean + asumr (Memr[data], ncols)
	}

	# Decrease the y box in steps of 2 until minimum y box.
	while (lineout < nlines - ybox1) {
	    linein = linein + 1
	    ptr = ptrs + mod (linein, ybmax)
	    data = Memi[ptr]
	    call asubr (Memr[sum], Memr[data], Memr[sum], ncols)
	    linein = linein + 1
	    ptr = ptrs + mod (linein, ybmax)
	    data = Memi[ptr]
	    call asubr (Memr[sum], Memr[data], Memr[sum], ncols)

	    lineout = lineout + 1
	    scale = scale - 2
	    data = impl2r (out, lineout)
	    call agboxcar (Memr[sum], Memr[data], ncols, xbmin, xbmax, scale)
	    if (inverse == YES)
		call arczr (1., Memr[data], Memr[data], ncols, divzero)
	    ccdmean = ccdmean + asumr (Memr[data], ncols)
	}

	# Output the last lines of the minimum y box size.
	call agboxcar (Memr[sum], Memr[output], ncols, xbmin, xbmax, scale)
	if (inverse == YES)
	    call arczr (1., Memr[output], Memr[output], ncols, divzero)
	ybox1 = nlines - lineout
	while (lineout < nlines) {
	    lineout = lineout + 1
	    data = impl2r (out, lineout)
	    call amovr (Memr[output], Memr[data], ncols)
	}
	ccdmean = ccdmean + ybox1 * asumr (Memr[output], ncols)

	# Write scale factor out.
	ccdmean = ccdmean / (ncols * nlines)
	call hdmputr (out, "ccdmean", ccdmean)
	call hdmputi (out, "ccdmeant", int (clktime (long (0))))

	# Free buffers
	call sfree (sp)
end


# AGBOXCAR -- Vector growing boxcar smooth.
# This implements the growing box algorithm which differs from the
# normal boxcar smoothing which uses a fixed size box.

procedure agboxcar (in, out, ncols, xbmin, xbmax, ybox)

real	in[ncols]		# Sum of ybox lines
real	out[ncols]		# Boxcar smoothed output
int	ncols			# Number of columns
int	xbmin, xbmax		# Boxcar size in x
real	ybox			# Boxcar size in y

int	colin, colout, lastcol, npix, xbmin2
real	sum, output

begin
	xbmin2 = (xbmin + 1) / 2
	colin = 0
	sum = 0.
	while (colin < xbmin) {
	    colin = colin + 1
	    sum = sum + in[colin]
	}

	npix = xbmin * ybox
	output = sum / npix
	colout = 0
	while (colout < xbmin2) {
	    colout = colout + 1
	    out[colout] = output
	}

	while (colin < xbmax) {
	    colin = colin + 1
	    sum = sum + in[colin]
	    colin = colin + 1
	    sum = sum + in[colin]
	    npix = npix + 2 * ybox
	    colout = colout + 1
	    out[colout] = sum / npix
	}

	lastcol = 0
	while (colin < ncols) {
	    colin = colin + 1
	    lastcol = lastcol + 1
	    sum = sum + in[colin] - in[lastcol]
	    colout = colout + 1
	    out[colout] = sum / npix
	}

	while (colout < ncols - xbmin2) {
	    lastcol = lastcol + 1
	    sum = sum - in[lastcol]
	    lastcol = lastcol + 1
	    sum = sum - in[lastcol]
	    npix = npix - 2 * ybox
	    colout = colout + 1
	    out[colout] = sum / npix
	}

	output = sum / npix
	while (colout < ncols) {
	    colout = colout + 1
	    out[colout] = output
	}
end


# BOXCLEAN -- Reject data values from the sum for the next boxcar average
# which exceed the minimum and maximum residual values from the current
# boxcar average.  This excludes data from the moving average before it
# enters the average.

int procedure boxclean (data, boxavg, sum, ncols, low, high)

real	data[ncols]		# Data line
real	boxavg[ncols]		# Box average line
real	sum[ncols]		# Moving sum
int	ncols			# Number of columns
real	low			# Low clipping factor
real	high			# High clipping factor

int	i, nrej
real	rms, resid, minresid, maxresid

begin
	rms = 0.
	do i = 1, ncols
	    rms = rms + (data[i] - boxavg[i]) ** 2
	rms = sqrt (rms / ncols)
	minresid = -low * rms
	maxresid = high * rms

	nrej = 0
	do i = 1, ncols {
	    resid = data[i] - boxavg[i]
	    if ((resid < minresid) || (resid > maxresid)) {
		data[i] = boxavg[i]
		sum[i] = sum[i] - resid
		nrej = nrej + 1
	    }
	}

	return (nrej)
end


# DIVZERO -- Error action for division by zero.

real procedure divzero (x)

real	x		# Value to be inversed

real	rdivzero	# Result for divion by zero
int	ndivzero	# Number of zero divisions
common	/cdivzero/ rdivzero, ndivzero

begin
	ndivzero = ndivzero + 1
	return (rdivzero)
end
