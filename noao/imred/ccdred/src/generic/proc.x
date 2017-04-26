include	<imhdr.h>
include	"ccdred.h"


.help proc Feb87 noao.imred.ccdred
.nf ----------------------------------------------------------------------------
proc -- Process CCD images

These are the main CCD reduction procedures.  There is one for each
readout axis (lines or columns) and one for short and real image data.
They apply corrections for bad pixels, overscan levels, zero levels,
dark counts, flat field response, illumination response, and fringe
effects.  The image is also trimmed if it was mapped with an image
section.  The mean value for the output image is computed when the flat
field or illumination image is processed to form the scale factor for
these calibrations in order to avoid reading through these image a
second time.

The processing information and parameters are specified in the CCD
structure. The processing operations to be performed are specified by
the correction array CORS in the ccd structure.  There is one array
element for each operation with indices defined symbolically by macro
definitions (see ccdred.h); i.e.  FLATCOR.  The value of the array
element is an integer bit field in which the bit set is the same as the
array index; i.e element 3 will have the third bit set for an operation
with array value 2**(3-1)=4.  If an operation is not to be performed
the bit is not set and the array element has the numeric value zero.
Note that the addition of several correction elements gives a unique
bit field describing a combination of operations.  For efficiency the
most common combinations are implemented as separate units.

The CCD structure also contains the correction or calibration data
consisting either pointers to data, IMIO pointers for the calibration
images, and scale factors.

The processing is performed line-by-line.  The procedure CORINPUT is
called to get an input line.  This procedure trims and fixes bad pixels by
interpolation.  The output line and lines from the various calibration
images are read.  The image vectors as well as the overscan vector and
the scale factors are passed to the procedure COR (which also
dereferences the pointer data into simple arrays and variables).  That
procedure does the actual corrections apart from bad pixel
corrections.

The final optional step is to add each corrected output line to form a
mean.  This adds efficiency since the operation is done only if desired
and the output image data is already in memory so there is no I/O
penalty.

SEE ALSO
    ccdred.h, cor, fixpix, setfixpix, setoverscan, settrim,
    setzero, setdark, setflat, setillum, setfringe
.endhelp ----------------------------------------------------------------------



# PROC1 -- Process CCD images with readout axis 1 (lines).

procedure proc1s (ccd)

pointer	ccd		# CCD structure

int	line, ncols, nlines, findmean, rep
int	overscan_type, overscan_c1, noverscan
real	overscan, darkscale, flatscale, illumscale, frgscale, mean
short	minrep
pointer	in, out, zeroim, darkim, flatim, illumim, fringeim, overscan_vec
pointer	inbuf, outbuf, zerobuf, darkbuf, flatbuf, illumbuf, fringebuf

real	asums()
real	find_overscans()
pointer	imgl2s(), impl2s(), ccd_gls(), xt_fpss()

begin
	# Initialize.  If the correction image is 1D then just get the
	# data once.

	in = IN_IM(ccd)
	out = OUT_IM(ccd)
	ncols = OUT_C2(ccd) - OUT_C1(ccd) + 1
	nlines = OUT_L2(ccd) - OUT_L1(ccd) + 1

	findmean = CORS(ccd, FINDMEAN)
	if (findmean == YES)
	    mean = 0.
	rep = CORS(ccd, MINREP)
	if (rep == YES)
	    minrep = MINREPLACE(ccd)

	if (CORS(ccd, OVERSCAN) == 0)
	    overscan_type = 0
	else {
	    overscan_type = OVERSCAN_TYPE(ccd)
	    overscan_vec = OVERSCAN_VEC(ccd)
	    overscan_c1 = BIAS_C1(ccd) - 1
	    noverscan = BIAS_C2(ccd) - overscan_c1
	}

	if (CORS(ccd, ZEROCOR) == 0) {
	    zeroim = NULL
	    zerobuf = 1
	} else if (IM_LEN(ZERO_IM(ccd),2) == 1) {
	    zeroim = NULL
	    zerobuf = ccd_gls (ZERO_IM(ccd), ZERO_C1(ccd), ZERO_C2(ccd), 1)
	} else
	    zeroim = ZERO_IM(ccd)

	if (CORS(ccd, DARKCOR) == 0) {
	    darkim = NULL
	    darkbuf = 1
	} else if (IM_LEN(DARK_IM(ccd),2) == 1) {
	    darkim = NULL
	    darkbuf = ccd_gls (DARK_IM(ccd), DARK_C1(ccd), DARK_C2(ccd), 1)
	    darkscale = FLATSCALE(ccd)
	} else {
	    darkim = DARK_IM(ccd)
	    darkscale = DARKSCALE(ccd)
	}

	if (CORS(ccd, FLATCOR) == 0) {
	    flatim = NULL
	    flatbuf = 1
	} else if (IM_LEN(FLAT_IM(ccd),2) == 1) {
	    flatim = NULL
	    flatbuf = ccd_gls (FLAT_IM(ccd), FLAT_C1(ccd), FLAT_C2(ccd), 1)
	    flatscale = FLATSCALE(ccd)
	} else {
	    flatim = FLAT_IM(ccd)
	    flatscale = FLATSCALE(ccd)
	}

	if (CORS(ccd, ILLUMCOR) == 0) {
	    illumim = NULL
	    illumbuf = 1
	} else {
	    illumim = ILLUM_IM(ccd)
	    illumscale = ILLUMSCALE(ccd)
	}

	if (CORS(ccd, FRINGECOR) == 0) {
	    fringeim = NULL
	    fringebuf = 1
	} else {
	    fringeim = FRINGE_IM(ccd)
	    frgscale = FRINGESCALE(ccd)
	}

	# For each line read lines from the input.  Procedure XT_FPS replaces
	# bad pixels by interpolation.  The trimmed region is copied to the
	# output.  Get lines from the output image and from the zero level,
	# dark count, flat field, illumination, and fringe images.  Call COR1
	# to do the actual pixel corrections.  Finally, add the output pixels
	# to a sum for computing the mean.  We must copy data outside of the
	# output data section.

	do line = 2 - OUT_L1(ccd), 0
	    call amovs (
		Mems[imgl2s(in,IN_L1(ccd)+line-1)+IN_C1(ccd)-OUT_C1(ccd)],
		Mems[impl2s(out,OUT_L1(ccd)+line-1)], IM_LEN(out,1))

	do line = 1, nlines {
	    outbuf = impl2s (out, OUT_L1(ccd)+line-1)

	    inbuf = xt_fpss (MASK_FP(ccd), in, IN_L1(ccd)+line-1, IN_C1(ccd),
		IN_C2(ccd), IN_L1(ccd), IN_L2(ccd), NULL)
	    call amovs (Mems[inbuf+IN_C1(ccd)-OUT_C1(ccd)], Mems[outbuf],
		IM_LEN(out,1))

	    outbuf = outbuf + OUT_C1(ccd) - 1
	    if (overscan_type != 0) {
		if (overscan_type < OVERSCAN_FIT)
		    overscan = find_overscans (Mems[inbuf+overscan_c1],
			noverscan, overscan_type)
		else
		    overscan = Memr[overscan_vec+line-1]
	    }
	    if (zeroim != NULL)
		zerobuf = ccd_gls (zeroim, ZERO_C1(ccd), ZERO_C2(ccd),
		    ZERO_L1(ccd)+line-1)
	    if (darkim != NULL)
		darkbuf = ccd_gls (darkim, DARK_C1(ccd), DARK_C2(ccd),
		    DARK_L1(ccd)+line-1)
	    if (flatim != NULL)
		flatbuf = ccd_gls (flatim, FLAT_C1(ccd), FLAT_C2(ccd),
		    FLAT_L1(ccd)+line-1)
	    if (illumim != NULL)
		illumbuf = ccd_gls (illumim, ILLUM_C1(ccd), ILLUM_C2(ccd),
		    ILLUM_L1(ccd)+line-1)
	    if (fringeim != NULL)
		fringebuf = ccd_gls (fringeim, FRINGE_C1(ccd), FRINGE_C2(ccd),
		    FRINGE_L1(ccd)+line-1)

	    call cor1s (CORS(ccd,1), Mems[outbuf],
		overscan, Mems[zerobuf], Mems[darkbuf],
		Mems[flatbuf], Mems[illumbuf], Mems[fringebuf], ncols,
		darkscale, flatscale, illumscale, frgscale)

	    if (rep == YES)
		call amaxks (Mems[outbuf], minrep, Mems[outbuf], ncols)
	    if (findmean == YES)
		mean = mean + asums (Mems[outbuf], ncols)
	}

	do line = nlines+1, IM_LEN(out,2)-OUT_L1(ccd)+1
	    call amovs (
		Mems[imgl2s(in,IN_L1(ccd)+line-1)+IN_C1(ccd)-OUT_C1(ccd)],
		Mems[impl2s(out,OUT_L1(ccd)+line-1)], IM_LEN(out,1))

	# Compute the mean from the sum of the output pixels.
	if (findmean == YES)
	    MEAN(ccd) = mean / ncols / nlines
end


# PROC2 -- Process CCD images with readout axis 2 (columns).

procedure proc2s (ccd)

pointer	ccd		# CCD structure

int	line, ncols, nlines, findmean, rep
real	darkscale, flatscale, illumscale, frgscale, mean
short	minrep
pointer	in, out, zeroim, darkim, flatim, illumim, fringeim, overscan_vec
pointer	inbuf, outbuf, zerobuf, darkbuf, flatbuf, illumbuf, fringebuf

real	asums()
pointer	imgl2s(), impl2s(), imgs2s(), ccd_gls(), xt_fpss()

begin
	# Initialize.  If the correction image is 1D then just get the
	# data once.

	in = IN_IM(ccd)
	out = OUT_IM(ccd)
	ncols = OUT_C2(ccd) - OUT_C1(ccd) + 1
	nlines = OUT_L2(ccd) - OUT_L1(ccd) + 1

	findmean = CORS(ccd, FINDMEAN)
	if (findmean == YES)
	    mean = 0.
	rep = CORS(ccd, MINREP)
	if (rep == YES)
	    minrep = MINREPLACE(ccd)

	overscan_vec = OVERSCAN_VEC(ccd)

	if (CORS(ccd, ZEROCOR) == 0) {
	    zeroim = NULL
	    zerobuf = 1
	} else if (IM_LEN(ZERO_IM(ccd),1) == 1) {
	    zeroim = NULL
	    zerobuf = imgs2s (ZERO_IM(ccd), 1, 1, ZERO_L1(ccd), ZERO_L2(ccd))
	} else
	    zeroim = ZERO_IM(ccd)

	if (CORS(ccd, DARKCOR) == 0) {
	    darkim = NULL
	    darkbuf = 1
	} else if (IM_LEN(DARK_IM(ccd),1) == 1) {
	    darkim = NULL
	    darkbuf = imgs2s (DARK_IM(ccd), 1, 1, DARK_L1(ccd), DARK_L2(ccd))
	    darkscale = DARKSCALE(ccd)
	} else {
	    darkim = DARK_IM(ccd)
	    darkscale = DARKSCALE(ccd)
	}

	if (CORS(ccd, FLATCOR) == 0) {
	    flatim = NULL
	    flatbuf = 1
	} else if (IM_LEN(FLAT_IM(ccd),1) == 1) {
	    flatim = NULL
	    flatbuf = imgs2s (FLAT_IM(ccd), 1, 1, FLAT_L1(ccd), FLAT_L2(ccd))
	    flatscale = FLATSCALE(ccd)
	} else {
	    flatim = FLAT_IM(ccd)
	    flatscale = FLATSCALE(ccd)
	}

	if (CORS(ccd, ILLUMCOR) == 0) {
	    illumim = NULL
	    illumbuf = 1
	} else {
	    illumim = ILLUM_IM(ccd)
	    illumscale = ILLUMSCALE(ccd)
	}

	if (CORS(ccd, FRINGECOR) == 0) {
	    fringeim = NULL
	    fringebuf = 1
	} else {
	    fringeim = FRINGE_IM(ccd)
	    frgscale = FRINGESCALE(ccd)
	}

	# For each line read lines from the input.  Procedure CORINPUT
	# replaces bad pixels by interpolation and applies a trim to the
	# input.  Get lines from the output image and from the zero level,
	# dark count, flat field, illumination, and fringe images.
	# Call COR2 to do the actual pixel corrections.  Finally, add the
	# output pixels to a sum for computing the mean.
	# We must copy data outside of the output data section.

	do line = 2 - OUT_L1(ccd), 0
	    call amovs (
		Mems[imgl2s(in,IN_L1(ccd)+line-1)+IN_C1(ccd)-OUT_C1(ccd)],
		Mems[impl2s(out,OUT_L1(ccd)+line-1)], IM_LEN(out,1))

	do line = 1, nlines {
	    outbuf = impl2s (out, OUT_L1(ccd)+line-1)

	    inbuf = xt_fpss (MASK_FP(ccd), in, IN_L1(ccd)+line-1, IN_C1(ccd),
		IN_C2(ccd), IN_L1(ccd), IN_L2(ccd), NULL)
	    call amovs (Mems[inbuf+IN_C1(ccd)-OUT_C1(ccd)], Mems[outbuf],
		IM_LEN(out,1))

	    outbuf = outbuf + OUT_C1(ccd) - 1
	    if (zeroim != NULL)
		zerobuf = ccd_gls (zeroim, ZERO_C1(ccd), ZERO_C2(ccd),
		    ZERO_L1(ccd)+line-1)
	    if (darkim != NULL)
		darkbuf = ccd_gls (darkim, DARK_C1(ccd), DARK_C2(ccd),
		    DARK_L1(ccd)+line-1)
	    if (flatim != NULL)
		flatbuf = ccd_gls (flatim, FLAT_C1(ccd), FLAT_C2(ccd),
		    FLAT_L1(ccd)+line-1)
	    if (illumim != NULL)
		illumbuf = ccd_gls (illumim, ILLUM_C1(ccd), ILLUM_C2(ccd),
		    ILLUM_L1(ccd)+line-1)
	    if (fringeim != NULL)
		fringebuf = ccd_gls (fringeim, FRINGE_C1(ccd), FRINGE_C2(ccd),
		    FRINGE_L1(ccd)+line-1)

	    call cor2s (line, CORS(ccd,1), Mems[outbuf],
		Memr[overscan_vec], Mems[zerobuf], Mems[darkbuf],
		Mems[flatbuf], Mems[illumbuf], Mems[fringebuf], ncols,
		zeroim, flatim, darkscale, flatscale, illumscale, frgscale)

	    if (rep == YES)
		call amaxks (Mems[outbuf], minrep, Mems[outbuf], ncols)
	    if (findmean == YES)
		mean = mean + asums (Mems[outbuf], ncols)
	}

	do line = nlines+1, IM_LEN(out,2)-OUT_L1(ccd)+1
	    call amovs (
		Mems[imgl2s(in,IN_L1(ccd)+line-1)+IN_C1(ccd)-OUT_C1(ccd)],
		Mems[impl2s(out,OUT_L1(ccd)+line-1)], IM_LEN(out,1))

	# Compute the mean from the sum of the output pixels.
	if (findmean == YES)
	    MEAN(ccd) = mean / ncols / nlines
end


# FIND_OVERSCAN -- Find the overscan value for a line.
# No check is made on the number of pixels.
# The median is the (npix+1)/2 element.

real procedure find_overscans (data, npix, type)

short	data[npix]	#I Overscan data
int	npix		#I Number of overscan points
int	type		#I Type of overscan calculation

int	i
real	overscan, d, dmin, dmax
short	asoks()

begin
	if (type == OVERSCAN_MINMAX) {
	    overscan = data[1]
	    dmin = data[1]
	    dmax = data[1]
	    do i = 2, npix {
		d = data[i]
		overscan = overscan + d
		if (d < dmin)
		    dmin = d
		else if (d > dmax)
		    dmax = d
	    }
	    overscan = (overscan - dmin - dmax) / (npix - 2)
	} else if (type == OVERSCAN_MEDIAN)
	    overscan = asoks (data, npix, (npix + 1) / 2)
	else {
	    overscan = data[1]
	    do i = 2, npix
		overscan = overscan + data[i]
	    overscan = overscan / npix
	}

	return (overscan)
end

# PROC1 -- Process CCD images with readout axis 1 (lines).

procedure proc1r (ccd)

pointer	ccd		# CCD structure

int	line, ncols, nlines, findmean, rep
int	overscan_type, overscan_c1, noverscan
real	overscan, darkscale, flatscale, illumscale, frgscale, mean
real	minrep
pointer	in, out, zeroim, darkim, flatim, illumim, fringeim, overscan_vec
pointer	inbuf, outbuf, zerobuf, darkbuf, flatbuf, illumbuf, fringebuf

real	asumr()
real	find_overscanr()
pointer	imgl2r(), impl2r(), ccd_glr(), xt_fpsr()

begin
	# Initialize.  If the correction image is 1D then just get the
	# data once.

	in = IN_IM(ccd)
	out = OUT_IM(ccd)
	ncols = OUT_C2(ccd) - OUT_C1(ccd) + 1
	nlines = OUT_L2(ccd) - OUT_L1(ccd) + 1

	findmean = CORS(ccd, FINDMEAN)
	if (findmean == YES)
	    mean = 0.
	rep = CORS(ccd, MINREP)
	if (rep == YES)
	    minrep = MINREPLACE(ccd)

	if (CORS(ccd, OVERSCAN) == 0)
	    overscan_type = 0
	else {
	    overscan_type = OVERSCAN_TYPE(ccd)
	    overscan_vec = OVERSCAN_VEC(ccd)
	    overscan_c1 = BIAS_C1(ccd) - 1
	    noverscan = BIAS_C2(ccd) - overscan_c1
	}

	if (CORS(ccd, ZEROCOR) == 0) {
	    zeroim = NULL
	    zerobuf = 1
	} else if (IM_LEN(ZERO_IM(ccd),2) == 1) {
	    zeroim = NULL
	    zerobuf = ccd_glr (ZERO_IM(ccd), ZERO_C1(ccd), ZERO_C2(ccd), 1)
	} else
	    zeroim = ZERO_IM(ccd)

	if (CORS(ccd, DARKCOR) == 0) {
	    darkim = NULL
	    darkbuf = 1
	} else if (IM_LEN(DARK_IM(ccd),2) == 1) {
	    darkim = NULL
	    darkbuf = ccd_glr (DARK_IM(ccd), DARK_C1(ccd), DARK_C2(ccd), 1)
	    darkscale = FLATSCALE(ccd)
	} else {
	    darkim = DARK_IM(ccd)
	    darkscale = DARKSCALE(ccd)
	}

	if (CORS(ccd, FLATCOR) == 0) {
	    flatim = NULL
	    flatbuf = 1
	} else if (IM_LEN(FLAT_IM(ccd),2) == 1) {
	    flatim = NULL
	    flatbuf = ccd_glr (FLAT_IM(ccd), FLAT_C1(ccd), FLAT_C2(ccd), 1)
	    flatscale = FLATSCALE(ccd)
	} else {
	    flatim = FLAT_IM(ccd)
	    flatscale = FLATSCALE(ccd)
	}

	if (CORS(ccd, ILLUMCOR) == 0) {
	    illumim = NULL
	    illumbuf = 1
	} else {
	    illumim = ILLUM_IM(ccd)
	    illumscale = ILLUMSCALE(ccd)
	}

	if (CORS(ccd, FRINGECOR) == 0) {
	    fringeim = NULL
	    fringebuf = 1
	} else {
	    fringeim = FRINGE_IM(ccd)
	    frgscale = FRINGESCALE(ccd)
	}

	# For each line read lines from the input.  Procedure XT_FPS replaces
	# bad pixels by interpolation.  The trimmed region is copied to the
	# output.  Get lines from the output image and from the zero level,
	# dark count, flat field, illumination, and fringe images.  Call COR1
	# to do the actual pixel corrections.  Finally, add the output pixels
	# to a sum for computing the mean.  We must copy data outside of the
	# output data section.

	do line = 2 - OUT_L1(ccd), 0
	    call amovr (
		Memr[imgl2r(in,IN_L1(ccd)+line-1)+IN_C1(ccd)-OUT_C1(ccd)],
		Memr[impl2r(out,OUT_L1(ccd)+line-1)], IM_LEN(out,1))

	do line = 1, nlines {
	    outbuf = impl2r (out, OUT_L1(ccd)+line-1)

	    inbuf = xt_fpsr (MASK_FP(ccd), in, IN_L1(ccd)+line-1, IN_C1(ccd),
		IN_C2(ccd), IN_L1(ccd), IN_L2(ccd), NULL)
	    call amovr (Memr[inbuf+IN_C1(ccd)-OUT_C1(ccd)], Memr[outbuf],
		IM_LEN(out,1))

	    outbuf = outbuf + OUT_C1(ccd) - 1
	    if (overscan_type != 0) {
		if (overscan_type < OVERSCAN_FIT)
		    overscan = find_overscanr (Memr[inbuf+overscan_c1],
			noverscan, overscan_type)
		else
		    overscan = Memr[overscan_vec+line-1]
	    }
	    if (zeroim != NULL)
		zerobuf = ccd_glr (zeroim, ZERO_C1(ccd), ZERO_C2(ccd),
		    ZERO_L1(ccd)+line-1)
	    if (darkim != NULL)
		darkbuf = ccd_glr (darkim, DARK_C1(ccd), DARK_C2(ccd),
		    DARK_L1(ccd)+line-1)
	    if (flatim != NULL)
		flatbuf = ccd_glr (flatim, FLAT_C1(ccd), FLAT_C2(ccd),
		    FLAT_L1(ccd)+line-1)
	    if (illumim != NULL)
		illumbuf = ccd_glr (illumim, ILLUM_C1(ccd), ILLUM_C2(ccd),
		    ILLUM_L1(ccd)+line-1)
	    if (fringeim != NULL)
		fringebuf = ccd_glr (fringeim, FRINGE_C1(ccd), FRINGE_C2(ccd),
		    FRINGE_L1(ccd)+line-1)

	    call cor1r (CORS(ccd,1), Memr[outbuf],
		overscan, Memr[zerobuf], Memr[darkbuf],
		Memr[flatbuf], Memr[illumbuf], Memr[fringebuf], ncols,
		darkscale, flatscale, illumscale, frgscale)

	    if (rep == YES)
		call amaxkr (Memr[outbuf], minrep, Memr[outbuf], ncols)
	    if (findmean == YES)
		mean = mean + asumr (Memr[outbuf], ncols)
	}

	do line = nlines+1, IM_LEN(out,2)-OUT_L1(ccd)+1
	    call amovr (
		Memr[imgl2r(in,IN_L1(ccd)+line-1)+IN_C1(ccd)-OUT_C1(ccd)],
		Memr[impl2r(out,OUT_L1(ccd)+line-1)], IM_LEN(out,1))

	# Compute the mean from the sum of the output pixels.
	if (findmean == YES)
	    MEAN(ccd) = mean / ncols / nlines
end


# PROC2 -- Process CCD images with readout axis 2 (columns).

procedure proc2r (ccd)

pointer	ccd		# CCD structure

int	line, ncols, nlines, findmean, rep
real	darkscale, flatscale, illumscale, frgscale, mean
real	minrep
pointer	in, out, zeroim, darkim, flatim, illumim, fringeim, overscan_vec
pointer	inbuf, outbuf, zerobuf, darkbuf, flatbuf, illumbuf, fringebuf

real	asumr()
pointer	imgl2r(), impl2r(), imgs2r(), ccd_glr(), xt_fpsr()

begin
	# Initialize.  If the correction image is 1D then just get the
	# data once.

	in = IN_IM(ccd)
	out = OUT_IM(ccd)
	ncols = OUT_C2(ccd) - OUT_C1(ccd) + 1
	nlines = OUT_L2(ccd) - OUT_L1(ccd) + 1

	findmean = CORS(ccd, FINDMEAN)
	if (findmean == YES)
	    mean = 0.
	rep = CORS(ccd, MINREP)
	if (rep == YES)
	    minrep = MINREPLACE(ccd)

	overscan_vec = OVERSCAN_VEC(ccd)

	if (CORS(ccd, ZEROCOR) == 0) {
	    zeroim = NULL
	    zerobuf = 1
	} else if (IM_LEN(ZERO_IM(ccd),1) == 1) {
	    zeroim = NULL
	    zerobuf = imgs2r (ZERO_IM(ccd), 1, 1, ZERO_L1(ccd), ZERO_L2(ccd))
	} else
	    zeroim = ZERO_IM(ccd)

	if (CORS(ccd, DARKCOR) == 0) {
	    darkim = NULL
	    darkbuf = 1
	} else if (IM_LEN(DARK_IM(ccd),1) == 1) {
	    darkim = NULL
	    darkbuf = imgs2r (DARK_IM(ccd), 1, 1, DARK_L1(ccd), DARK_L2(ccd))
	    darkscale = DARKSCALE(ccd)
	} else {
	    darkim = DARK_IM(ccd)
	    darkscale = DARKSCALE(ccd)
	}

	if (CORS(ccd, FLATCOR) == 0) {
	    flatim = NULL
	    flatbuf = 1
	} else if (IM_LEN(FLAT_IM(ccd),1) == 1) {
	    flatim = NULL
	    flatbuf = imgs2r (FLAT_IM(ccd), 1, 1, FLAT_L1(ccd), FLAT_L2(ccd))
	    flatscale = FLATSCALE(ccd)
	} else {
	    flatim = FLAT_IM(ccd)
	    flatscale = FLATSCALE(ccd)
	}

	if (CORS(ccd, ILLUMCOR) == 0) {
	    illumim = NULL
	    illumbuf = 1
	} else {
	    illumim = ILLUM_IM(ccd)
	    illumscale = ILLUMSCALE(ccd)
	}

	if (CORS(ccd, FRINGECOR) == 0) {
	    fringeim = NULL
	    fringebuf = 1
	} else {
	    fringeim = FRINGE_IM(ccd)
	    frgscale = FRINGESCALE(ccd)
	}

	# For each line read lines from the input.  Procedure CORINPUT
	# replaces bad pixels by interpolation and applies a trim to the
	# input.  Get lines from the output image and from the zero level,
	# dark count, flat field, illumination, and fringe images.
	# Call COR2 to do the actual pixel corrections.  Finally, add the
	# output pixels to a sum for computing the mean.
	# We must copy data outside of the output data section.

	do line = 2 - OUT_L1(ccd), 0
	    call amovr (
		Memr[imgl2r(in,IN_L1(ccd)+line-1)+IN_C1(ccd)-OUT_C1(ccd)],
		Memr[impl2r(out,OUT_L1(ccd)+line-1)], IM_LEN(out,1))

	do line = 1, nlines {
	    outbuf = impl2r (out, OUT_L1(ccd)+line-1)

	    inbuf = xt_fpsr (MASK_FP(ccd), in, IN_L1(ccd)+line-1, IN_C1(ccd),
		IN_C2(ccd), IN_L1(ccd), IN_L2(ccd), NULL)
	    call amovr (Memr[inbuf+IN_C1(ccd)-OUT_C1(ccd)], Memr[outbuf],
		IM_LEN(out,1))

	    outbuf = outbuf + OUT_C1(ccd) - 1
	    if (zeroim != NULL)
		zerobuf = ccd_glr (zeroim, ZERO_C1(ccd), ZERO_C2(ccd),
		    ZERO_L1(ccd)+line-1)
	    if (darkim != NULL)
		darkbuf = ccd_glr (darkim, DARK_C1(ccd), DARK_C2(ccd),
		    DARK_L1(ccd)+line-1)
	    if (flatim != NULL)
		flatbuf = ccd_glr (flatim, FLAT_C1(ccd), FLAT_C2(ccd),
		    FLAT_L1(ccd)+line-1)
	    if (illumim != NULL)
		illumbuf = ccd_glr (illumim, ILLUM_C1(ccd), ILLUM_C2(ccd),
		    ILLUM_L1(ccd)+line-1)
	    if (fringeim != NULL)
		fringebuf = ccd_glr (fringeim, FRINGE_C1(ccd), FRINGE_C2(ccd),
		    FRINGE_L1(ccd)+line-1)

	    call cor2r (line, CORS(ccd,1), Memr[outbuf],
		Memr[overscan_vec], Memr[zerobuf], Memr[darkbuf],
		Memr[flatbuf], Memr[illumbuf], Memr[fringebuf], ncols,
		zeroim, flatim, darkscale, flatscale, illumscale, frgscale)

	    if (rep == YES)
		call amaxkr (Memr[outbuf], minrep, Memr[outbuf], ncols)
	    if (findmean == YES)
		mean = mean + asumr (Memr[outbuf], ncols)
	}

	do line = nlines+1, IM_LEN(out,2)-OUT_L1(ccd)+1
	    call amovr (
		Memr[imgl2r(in,IN_L1(ccd)+line-1)+IN_C1(ccd)-OUT_C1(ccd)],
		Memr[impl2r(out,OUT_L1(ccd)+line-1)], IM_LEN(out,1))

	# Compute the mean from the sum of the output pixels.
	if (findmean == YES)
	    MEAN(ccd) = mean / ncols / nlines
end


# FIND_OVERSCAN -- Find the overscan value for a line.
# No check is made on the number of pixels.
# The median is the (npix+1)/2 element.

real procedure find_overscanr (data, npix, type)

real	data[npix]	#I Overscan data
int	npix		#I Number of overscan points
int	type		#I Type of overscan calculation

int	i
real	overscan, d, dmin, dmax
real	asokr()

begin
	if (type == OVERSCAN_MINMAX) {
	    overscan = data[1]
	    dmin = data[1]
	    dmax = data[1]
	    do i = 2, npix {
		d = data[i]
		overscan = overscan + d
		if (d < dmin)
		    dmin = d
		else if (d > dmax)
		    dmax = d
	    }
	    overscan = (overscan - dmin - dmax) / (npix - 2)
	} else if (type == OVERSCAN_MEDIAN)
	    overscan = asokr (data, npix, (npix + 1) / 2)
	else {
	    overscan = data[1]
	    do i = 2, npix
		overscan = overscan + data[i]
	    overscan = overscan / npix
	}

	return (overscan)
end
