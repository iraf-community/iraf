include	<mach.h>
include	<ctype.h>
include	<error.h>
include	<imhdr.h>
include	<fset.h>
include	<math/iminterp.h>
include "oned.h"
include "idsmtn.h"

# Combining options
define	COMBINE	"|average|sum|"
define	AVERAGE	1
define	SUM	2

# Rebinning defs
define	INTERP_MODE	"|linear|spline3|poly3|poly5|sums|"
define	RB_LINEAR	1
define	RB_SPLINE3	2
define	RB_POLY3	3
define	RB_POLY5	4
define	RB_SUMS		5

# Weighting options
define	WT_TYPE		"|none|expo|user|"
define	WT_NONE		1
define	WT_EXPO		2
define	WT_USER		3

define	MAX_NR_SPECTRA	100	# Maximum number of spectra to be combined
define	BAD_PIX		0.0	# Value for pixel to be ignored in average

# T_COMBINE -- Combine spectra having arbitrary wavelength coverage
#  and dispersion.
#
#  The list of input spectra is reviewed to determine the minmimum
#  and maximum wavelength range for which coverage is available.
#  The highest dispersion (most pixels per angstrom) from the input
#  set is adopted for the output spectrum.
#
#  Each input spectrum is rebinned to the total range of the new
#  combination spectrum and then the entire new group is averaged
#  with zero values being considered bad pixels.
#  The process of rebinning in memory all input spectra is somewhat
#  expensive for memory but easy to code and reasoanbly fast.

procedure t_combine ()

char	image[SZ_FNAME]
char	rec_numbers[SZ_LINE], ofile[SZ_FNAME], wtfile[SZ_FNAME]
char	interp_mode[SZ_LINE]
char	wt_mode[SZ_LINE], ctemp[SZ_LINE]
int	root, nfiles
int	nrecs, records[3, MAX_RANGES]
int	mode, ifile, option
int	npts, i, nimages
int	wt_type
real	wstart, wend, dw
bool	logarithm, makewt
pointer	sp, imout, im, ids, imrebin, invert, pixout, wtimage, xids, wt, wtpix
pointer	p_images

int	clpopni(), clplen(), clgwrd()
int	get_next_image(), decode_ranges()
real	clgetr()
bool	streq()
pointer	immap(), impl1r()

begin
	# Open input file name template
	root   = clpopni ("input")
	nfiles = clplen (root)

	# Get range specification if any
	call clgstr ("records", rec_numbers, SZ_LINE)
	if (decode_ranges (rec_numbers, records, MAX_RANGES, nrecs) == ERR)
	    call error (0, "Bad range specification")

	# Get rootname for output files and starting record
	call clgstr ("output", ofile, SZ_FNAME)

	# Get various options.
	option = clgwrd ("combine", interp_mode, SZ_LINE, COMBINE)
	mode = clgwrd ("interp_mode", interp_mode, SZ_LINE, INTERP_MODE)
	wt_type = clgwrd ("wt_type", wt_mode, SZ_LINE, WT_TYPE)

	# Create a weighting image?
	call clgstr ("wt_image", wtfile, SZ_FNAME)
	if (streq (wtfile, ""))
	    makewt = false
	else
	    makewt = true

	# Force STDOUT flush
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate space for the potential input spectra pointers
	call smark (sp)
	call salloc (im, MAX_NR_SPECTRA, TY_POINTER)
	call salloc (ids, MAX_NR_SPECTRA, TY_POINTER)
	call salloc (imrebin, MAX_NR_SPECTRA, TY_POINTER)
	call salloc (p_images, MAX_NR_SPECTRA, TY_POINTER)

	# Initialize range decoder
	call reset_next_image ()

	# And file counter
	ifile = 1

	# Loop over all input images - read in all headers
	while (get_next_image (root, records, nrecs, image, 
	    SZ_FNAME) != EOF) {

	    # Open image
	    iferr (MEMP[im+ifile-1] = immap (image, READ_ONLY, 0)) {
		call eprintf ("[%s]")
		    call pargstr (image)
		call error (0, " Image not found or header info not available")
	    }

	    # Load header data into input and output header arrays
	    call salloc (MEMP[ids+ifile-1], LEN_IDS, TY_STRUCT)
	    call salloc (POINT[MEMP[ids+ifile-1]], MAX_NCOEFF, TY_REAL)

	    call load_ids_hdr (MEMP[ids+ifile-1], MEMP[im+ifile-1], 1)
	    if (DC_FLAG(MEMP[ids+ifile-1]) == -1)
		call error (0, " All images must be dispersion corrected")

	    # Save image name
	    call salloc (MEMP[p_images+ifile-1], SZ_FNAME, TY_CHAR)
	    call strcpy (image, Memc[MEMP[p_images+ifile-1]], SZ_FNAME)

	    ifile = ifile + 1
	}

	nimages = ifile - 1

	# Get weighting parameters if necessary
	call salloc (wt, nimages, TY_REAL)
	if (wt_type == WT_USER)
	    do i = 1, nimages {
		call printf ("For [%s]:")
		call pargstr (Memc[MEMP[p_images+i-1]])
		call flush (STDOUT)
		Memr[wt+i-1] = clgetr ("weight")
	    }
	else if (wt_type == WT_NONE)
	    call amovkr (1.0, Memr[wt], nimages)

	# Identify wavelength range and dispersion
	call review_hdrs (im, ids, nimages, wstart, wend, dw, logarithm, 
	    wt_type, Memr[wt])

	npts = int ((wend - wstart) / dw + 0.5) + 1

	call printf ("Combined spectrum (%d pts) extends from %9.2f to %9.2f")
	    call pargi (npts)
	    call pargr (wstart)
	    call pargr (wend)
	call printf (" in increments of %8.4f\n")
	    call pargr (dw)
	
	# Allocate space for pixel map and output pixels
	call salloc (invert, npts, TY_REAL)
	call salloc (pixout, npts, TY_REAL)

	call salloc (wtpix, npts, TY_REAL)

	# Rebin all images to new wavelength set
	do i = 1, nimages {
	    call salloc (MEMP[imrebin+i-1], npts, TY_REAL)
	    call co_rebin (MEMP[im+i-1], MEMP[ids+i-1], mode, logarithm,
		npts, wstart, dw, Memr[invert], Memr[MEMP[imrebin+i-1]])

	    call printf ("[%s] rebinned\n")
		call pargstr (Memc[MEMP[p_images+i-1]])

	    if (i > 1)
		call imunmap (MEMP[im+i-1])
	}

	# Combine all rebinned images
	call combine (imrebin, nimages, npts, Memr[wt], Memr[pixout], 
	    Memr[wtpix], option)

	# Write image out
	imout = immap (ofile, NEW_COPY, MEMP[im])
	IM_NDIM (imout) = 1
	IM_LEN  (imout, 1) = npts
	IM_PIXTYPE (imout) = TY_REAL
	call strcpy (IM_TITLE (MEMP[im]), IM_TITLE (imout), SZ_LINE)

	call amovr (Memr[pixout], Memr[impl1r(imout, 1)], npts)

	# Set header parameters
	xids = MEMP[ids]
	NP1(xids) = 0
	NP2(xids) = npts
	W0 (xids) = wstart
	WPC(xids) = dw
	call store_keywords (xids, imout)

	call imunmap (MEMP[im])

	# Write out weight array
	if (makewt) {
	    wtimage = immap (wtfile, NEW_IMAGE, 0)
	    IM_NDIM (wtimage) = 1
	    IM_LEN  (wtimage, 1) = npts
	    IM_PIXTYPE (wtimage) = TY_REAL
	    call sprintf (ctemp, SZ_LINE, "%s %s")
		call pargstr ("Weighting array for:")
		call pargstr (IM_TITLE (imout))
	    call strcpy (ctemp, IM_TITLE (wtimage), SZ_LINE)

	    call amovr (Memr[wtpix], Memr[impl1r (wtimage, 1)], npts)

	    call store_keywords (xids, wtimage)
	    call imunmap (wtimage)
	}
	
	call imunmap (imout)

	# Free space
	call sfree (sp)
	call clpcls (root)
end

# CO_REBIN -- Rebin the image according to the specified alignment

procedure co_rebin (im, ids, mode, logarithm, cols_out, w0, wpc, invert, pixout)

pointer	im, ids
int	mode, cols_out
real	w0, wpc
bool	logarithm
real	invert[ARB]
real	pixout[ARB]

pointer	pixin
bool	login
int	nlen, ncols

pointer	imgl1r()

begin
	# Length of input spectrum
	nlen   = IM_LEN (im, 1)

	if (DC_FLAG(ids) == 1)
	    login = true

	# Length of output spectrum
	ncols = cols_out

	# Compute pixel position as a function of lambda.
	# Interpolate according to the flexure parameter.
	call lambda_to_pixel2 (w0, wpc, W0(ids), WPC(ids), login, ncols, 
	    logarithm, invert)

	# Map image pixels
	pixin = imgl1r (im, 1)

	switch (mode) {
	case RB_LINEAR:
	    call reinterp (Memr[pixin], pixout, invert, ncols, nlen, II_LINEAR)
	case RB_SPLINE3:
	    call reinterp (Memr[pixin], pixout, invert, ncols, nlen, II_SPLINE3)
	case RB_POLY3:
	    call reinterp (Memr[pixin], pixout, invert, ncols, nlen, II_POLY3)
	case RB_POLY5:
	    call reinterp (Memr[pixin], pixout, invert, ncols, nlen, II_POLY5)
	case RB_SUMS:
	    call resum (Memr[pixin], pixout, invert, ncols, nlen)
	}
end

# REVIEW_HDRS -- Scan through all image headers to ascertain the minimum
#  and maximum wavelengths, and the highest dispersion

procedure review_hdrs (im, ids, nimages, wstart, wend, dw, logarithm,
	wt_type, wt)

pointer	im, ids
int	nimages
real	wstart, wend, dw
bool	logarithm
int	wt_type
real	wt[ARB]

int	i, npts
real	xwend
pointer	xim, xids

begin
	wstart = MAX_REAL
	wend   = -MAX_REAL
	dw     = MAX_REAL

	do i = 1, nimages {
	    xim  = MEMP[im +i-1]
	    xids = MEMP[ids+i-1]

	    # If counter is not 1 check for consistency in the
	    # linearization mode - must be either all linear or all log
	    if (i == 1)
		if (DC_FLAG(xids) == 1)
		    logarithm = true
		else
		    logarithm = false

	    else
		if (((DC_FLAG(xids) == 0) && logarithm) ||
		    ((DC_FLAG(xids) == 1) && !logarithm))
		    call error (0, "Cannot combine log and linear spectra")

	    npts = IM_LEN (xim, 1)
	    xwend = W0(xids) + (npts - 1) * WPC(xids)

	    wstart = min (wstart, W0(xids))
	    wend   = max (wend  , xwend)
	    dw     = min (dw    , WPC(xids))

	    if (wt_type == WT_EXPO)
		wt[i] = ITM(xids)
	}
end

# COMBINE -- Combine overlapping pixels from all spectra

procedure combine (imrebin, nimages, npts, wt, imout, wtpix, option)

pointer	imrebin
int	nimages, npts
real	wt[ARB]
real	imout[ARB]
real	wtpix[ARB]
int	option

int	i, j
real	pix, sum, wtsum
pointer	xim

begin
	# Combine them
	switch (option) {
	case AVERAGE:
	    do j = 1, npts {
	        sum = 0.0
	        wtsum = 0.0
	        do i = 1, nimages {
		    xim = MEMP[imrebin+i-1]
		    pix = Memr[xim+j-1]
		    if (pix != BAD_PIX) {
		        sum = sum + pix * wt[i]
		        wtsum = wtsum + wt[i]
		    }
	        }
	        imout[j] = sum / max (wtsum, 1.0)
	        wtpix[j] = wtsum
	    }
	case SUM:
	    do j = 1, npts {
	        sum = 0.0
	        wtsum = 0.0
	        do i = 1, nimages {
		    xim = MEMP[imrebin+i-1]
		    pix = Memr[xim+j-1]
		    if (pix != BAD_PIX) {
		        sum = sum + pix * wt[i]
		        wtsum = wtsum + wt[i]
		    }
	        }
	        imout[j] = sum
	        wtpix[j] = wtsum
	    }
	}
end


# RESUM -- Rebinning using a partial pixel summation technique to
#  preserve the total flux.

procedure resum (pixin, pixout, invert, ncols, nlen)

real	pixin[ARB], pixout[ARB], invert[ARB]
int	ncols, nlen

int	i
real	x1, x2, xa, xb, dx

real	pixel_parts()

begin
	# Initialize
	x1 = invert [1]
	x2 = invert [2]
	dx = x2 - x1
	xa = x1 - dx/2
	xb = x1 + dx/2
	pixout[1] = pixel_parts (pixin, nlen, xa, xb)

	do i = 2, ncols {
	    x2 = invert [i]
	    dx = x2 - x1
	    x1 = x2
	    xa = xb
	    xb = x1 + dx/2

	    pixout[i] = pixel_parts (pixin, nlen, xa, xb)
	}
end


# If the interpolation is less than this distance from a input point then
# don't interpolate.
define	RB_MINDIST	0.001

# REINTERP -- Rebin the vector by interpolation
#
# This requires a little care to propagate bad pixels and to avoid
# interpolations in which the inversion point is essentially a pixel
# position except for very small errors.  A zero input value is assumed
# to be a bad point.  Any interpolation using a bad point is set to be
# a bad point.  The use of the image interpolator may be questionable
# in the case of bad points since there may be ringing even away from
# the zero value point.

procedure reinterp (pixin, pixout, invert, ncols, nlen, mode)

real	pixin[ARB], pixout[ARB], invert[ARB]
int	ncols, nlen, mode

int	j, ipos
real	xpos

real	arieval()

begin
	do j = 1, ncols {
	    xpos = invert[j]
	    ipos = xpos
	    if (ipos < 1 || ipos > nlen)
		pixout[j] = 0.0
	    else if (abs (xpos - ipos) < RB_MINDIST)
		pixout[j] = pixin[ipos]
	    else if (pixin[ipos] == 0.0)
		pixout[j] = 0.0
	    else if (ipos < nlen && pixin[ipos+1] == 0.0)
		pixout[j] = 0.0
	    else
		pixout[j] = arieval (xpos, pixin, nlen, mode)
	}
end


# PIXEL_PARTS -- Integrate over partial pixels to obtain total flux
# over specified region.

real procedure pixel_parts (y, n, xa, xb)

int	n
real	y[n], xa, xb

int	i, i1, i2
real	x1, x2, cx1, cx2, frac1, frac2, sum

begin
	# Remember that pixel centers occur at integral values
	# so a pixel extends from i-0.5 to i+0.5

	x1 = max (0.5, min (xa, xb))
	x2 = min (n + 0.5, max (xa, xb))
	if (x1 >= x2)
	    return (0.)

	cx1 = x1 - 0.5
	cx2 = x2 - 0.5

	i1 = int (cx1) + 1
	i2 = int (cx2) + 1

	if (i1 == i2) {
	    frac1 = x2 - x1
	    frac2 = 0.0
	} else {
	    frac1 = int (cx1) + 1.0 - cx1
	    frac2 = cx2 - int(cx2)
	}

	sum = frac1 * y[i1]  +  frac2 * y[i2]

	# Include inclusive whole pixels
	do i = i1+1, i2-1
	    sum = sum + y[i]

	return (sum)
end
