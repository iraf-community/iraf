# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <imhdr.h>
include <math/surfit.h>
include "imsurfit.h"

# IMSURFIT -- Procedure to fit a surface to a single image including
# optional pixel rejection.

procedure imsurfit (imin, imout, imfit, gl)

pointer	imin	# pointer to the input image
pointer	imout	# pointer to the output image
pointer	imfit	# pointer to the imsurfit parameters
pointer	gl	# pointer to the good regions list

pointer	sf, rl
errchk	isfree, prl_free
errchk	all_pixels, good_pixels, good_median, all_medians, do_reject
errchk	set_outimage

begin
	sf = NULL
	rl = NULL

	# Accumulate and solve the surface.
	if (gl == NULL) {
	    if (MEDIAN(imfit) == NO)
	        call all_pixels (imin, imfit, sf)
	    else
		call all_medians (imin, imfit, sf)
	} else {
	    if (MEDIAN(imfit) == NO)
	        call good_pixels (imin, imfit, gl, sf)
	    else
		call good_medians (imin, imfit, gl, sf)
	}

	# Perform the reject cycle.
	if (REJECT(imfit) == YES || TYPE_OUTPUT(imfit) == CLEAN)
	    call do_reject (imin, imfit, gl, sf, rl)

	# Evaluate surface for appropriate output type.
	call set_outimage (imin, imout, imfit, sf, rl)

	# Cleanup.
	call prl_free (rl)
	call isfree (sf)

	rl = NULL
	sf = NULL
end


# ALL_PIXELS -- Accumulate surface when there are no bad regions
# and no median processing.

procedure all_pixels (im, imfit, sf)

pointer	im	# pointer to the input image
pointer	imfit	# pointer to the imsurfit structure
pointer	sf	# pointer to the surface descriptor

int	i, lp, ncols, nlines, ier
long	v[IM_MAXDIM]
pointer	sp, cols, lines, wgt, lbuf
int	imgnlr()
errchk	smark, salloc, sfree, imgnlr
errchk isinit, islfit, islrefit, issolve

begin
	ncols = IM_LEN(im, 1)
	nlines = IM_LEN(im,2)

	# Initialize the surface fit.
	call isinit (sf, SURFACE_TYPE(imfit), XORDER(imfit), YORDER(imfit),
	    CROSS_TERMS(imfit), ncols, nlines) 

	# Allocate working space for fitting.
	call smark (sp)
	call salloc (cols, ncols, TY_INT)
	call salloc (lines, nlines, TY_INT)
	call salloc (wgt, ncols, TY_REAL)

	# Initialize the x and weight buffers.
	do i = 1, ncols
	    Memi[cols - 1 + i] = i
	 call amovkr (1.0, Memr[wgt], ncols)

	# Loop over image lines.
	lp = 0
	call amovkl (long (1), v, IM_MAXDIM)
	do i = 1, nlines {

	    # Read in the image line.
	    if (imgnlr (im, lbuf, v) == EOF)
		call error (0, "Error reading image")

	    # Fit each image line.
	    if (i == 1)
	        call islfit (sf, Memi[cols], i, Memr[lbuf], Memr[wgt],
		ncols, SF_USER, ier)
	    else
		call islrefit (sf, Memi[cols], i, Memr[lbuf], Memr[wgt])

	    # Handle fitting errors.
	    switch (ier) {
	    case NO_DEG_FREEDOM:
		call eprintf ("Warning: Too few columns to fit line: %d\n")
		    call pargi (i)
	    case SINGULAR:
		call eprintf ("Warning: Solution singular for line: %d\n")
		    call pargi (i)
		Memi[lines + lp] = i
		lp = lp + 1
	    default:
		Memi[lines + lp] = i
		lp = lp + 1
	    }

	}

	# Solve the surface.
	call issolve (sf, Memi[lines], lp, ier)

	# Handle fitting errors.
	switch (ier) {
	case NO_DEG_FREEDOM:
	    call error (0, "ALL_PIXELS: Cannot fit surface.")
	case SINGULAR:
	    call eprintf ("Warning: Solution singular for surface.\n")
	default:
	    # everything OK
	}

	# Free space.
	call sfree (sp)
end


# GOOD_PIXELS -- Get surface when good regions are defined and median
# processing is off.

procedure good_pixels (im, imfit, gl, sf)

pointer	im	# input image
pointer	imfit	# pointer to imsurfit header structure
pointer	gl	# pointer to good region list
pointer	sf	# pointer to the surface descriptor

int	lp, lineno, prevlineno, ncols, nlines, npts, nranges, ier, ijunk
int	max_nranges
pointer	sp, colsfit, lines, buf, fbuf, wgt, ranges
int	prl_nextlineno(), prl_eqlines(), prl_get_ranges(), is_expand_ranges()
int	is_choose_rangesr()
pointer	imgl2r()

errchk	smark, salloc, sfree, imgl2r
errchk	isinit, islfit, islrefit, issolve
errchk	prl_nextlineno, prl_eqlines, prl_get_ranges
errchk	is_choose_rangesr

begin
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	max_nranges = ncols

	# Initialize the surface fit.
	call isinit (sf, SURFACE_TYPE(imfit), XORDER(imfit), YORDER(imfit),
	    CROSS_TERMS(imfit), ncols, nlines)

	# Allocate temporary space for fitting.
	call smark (sp)
	call salloc (colsfit, ncols, TY_INT)
	call salloc (lines, nlines, TY_INT)
	call salloc (fbuf, ncols, TY_REAL)
	call salloc (wgt, ncols, TY_REAL)
	call salloc (ranges, 3 * max_nranges + 1, TY_INT)
	call amovkr (1., Memr[wgt], ncols)

	# Intialize counters and pointers.
	lp = 0
	lineno = 0
	prevlineno = 0

	# Loop over those lines to be fit.
	while (prl_nextlineno (gl, lineno) != EOF) {

	    # Read in the image line.
	    buf = imgl2r (im, lineno)
	    if (buf == EOF)
		call error (0, "GOOD_PIXELS: Error reading image.")

	    # Get the ranges for that image line.
	    nranges = prl_get_ranges (gl, lineno, Memi[ranges], max_nranges)
	    if (nranges == 0)
		next

	    # If ranges are not equal to previous line fit else refit.
	    if (lp == 0 || prl_eqlines (gl, lineno, prevlineno) == NO) {
		npts = is_expand_ranges (Memi[ranges], Memi[colsfit], ncols)
		ijunk = is_choose_rangesr (Memi[colsfit], Memr[buf], Memr[fbuf],
		    npts, 1, ncols)
		call islfit (sf, Memi[colsfit], lineno, Memr[fbuf], Memr[wgt],
		    npts, SF_USER, ier)
	    } else {
		ijunk = is_choose_rangesr (Memi[colsfit], Memr[buf], Memr[fbuf],
		    npts, 1, ncols)
		call islrefit (sf, Memi[colsfit], lineno, Memr[fbuf], Memr[wgt])
	    }

	    # Handle fitting errors.
	    switch (ier) {
	    case NO_DEG_FREEDOM:
		call eprintf ("Warning: Too few columns to fit line: %d\n")
		    call pargi (lineno)
	    case SINGULAR:
		call eprintf ("Warning: Solution singular for line: %d\n")
		    call pargi (lineno)
		Memi[lines+lp] = lineno
		lp = lp + 1
	    default:
		Memi[lines+lp] = lineno
		lp = lp + 1
	    }

	    prevlineno = lineno
	}

	# Solve the surface.
	call issolve (sf, Memi[lines], lp, ier)

	# Handle fitting errors.
	switch (ier) {
	case NO_DEG_FREEDOM:
	    call error (0, "GOOD_PIXELS: Cannot fit surface.")
	case SINGULAR:
	    call eprintf ("Warning: Solution singular for surface.\n")
	default:
	    # everything OK
	}

	# Free space.
	call sfree (sp)
end


# ALL_MEDIANS -- Get surface when median processor on and all
# pixels good.

procedure  all_medians (im, imfit, sf)

pointer	im	# input image
pointer	imfit	# pointer to the imsurfit header structure
pointer	sf	# pointer to the surface descriptor

int	i, lp, cp, op, lineno, x1, x2, y1, y2, ier
int	nimcols, nimlines, ncols, nlines, npts
pointer	sp, cols, lines, wgt, z, med, sbuf, lbuf, buf

pointer	imgs2r()
real	asokr()
errchk	salloc, sfree, smark
errchk	isinit, islfit, islrefit, issolve

begin
	# Determine the number of lines and columns for a median processed
	# image.
	nimcols = IM_LEN(im,1)
	if (mod (int (IM_LEN(im,1)), XMEDIAN(imfit)) != 0)
	    ncols = IM_LEN(im,1) / XMEDIAN(imfit) + 1
	else
	    ncols = IM_LEN(im,1) / XMEDIAN(imfit)
	nimlines = IM_LEN(im,2)
	if (mod (int (IM_LEN(im,2)), YMEDIAN(imfit)) != 0)
	    nlines = IM_LEN(im,2) / YMEDIAN(imfit) + 1
	else
	    nlines = IM_LEN(im,2) / YMEDIAN(imfit)

	# Initialize the surface fitting.
	call isinit (sf, SURFACE_TYPE(imfit), XORDER(imfit), YORDER(imfit),
	   CROSS_TERMS(imfit), ncols, nlines)

	# Allocate workin memory.
	call smark (sp)
	call salloc (cols, ncols, TY_INT)
	call salloc (wgt, ncols, TY_REAL)
	call salloc (lines, nlines, TY_INT)
	call salloc (z, ncols, TY_REAL)
	call salloc (med, XMEDIAN(imfit) * YMEDIAN(imfit), TY_REAL)

	# Intialize the x and weight arrays.
	do i = 1, ncols
	    Memi[cols - 1 + i] = i
	call amovkr (1.0, Memr[wgt], ncols)

	# Loop over image sections.
	lp = 0
	lineno = 1
	for (y1 = 1; y1 <= nimlines; y1 = y1 + YMEDIAN(imfit)) {

	    # Get image section.
	    y2 = min (y1 + YMEDIAN(imfit) - 1, nimlines)
	    sbuf = imgs2r (im, 1, nimcols, y1, y2)
	    if (sbuf == EOF)
		call error (0, "Error reading image section.")

	    # Loop over median boxes.
	    cp = 0
	    for (x1 = 1; x1 <= nimcols; x1 = x1 + XMEDIAN(imfit)) {

		x2 = min (x1 + XMEDIAN(imfit) - 1, nimcols)
		npts = x2 - x1 + 1
		lbuf = sbuf - 1 + x1

		# Loop over lines in the median box.
		op = 0
		buf = lbuf
		for (i = 1; i <= y2 - y1 + 1; i = i + 1) {
		   call amovr (Memr[buf], Memr[med+op], npts)
		   op = op + npts
		   buf = buf + nimcols
		}

		# Calculate the median.
		Memr[z+cp] = asokr (Memr[med], op, (op + 1) / 2)
		cp = cp + 1

	    }

	    # Fit each image "line".
	    if (y1 == 1)
		call islfit (sf, Memi[cols], lineno, Memr[z], Memr[wgt],
		     ncols, SF_USER, ier)
	    else
		call islrefit (sf, Memi[cols], lineno, Memr[z], Memr[wgt])

	    # Handle fitting errors.
	    switch (ier) {
	    case NO_DEG_FREEDOM:
		call eprintf ("Warning: Too few columns to fit line: %d\n")
		    call pargi (lineno)
	    case SINGULAR:
		call eprintf ("Warning: Solution singular for line: %d\n")
		    call pargi (lineno)
		Memi[lines + lp] = lineno
		lp = lp + 1
	    default:
		Memi[lines + lp] = lineno
		lp = lp + 1
	    }

	    lineno = lineno + 1
	}

	# Solve th surface.
	call issolve (sf, Memi[lines], lp, ier)

	# Handle fitting errors.
	switch (ier) {
	case NO_DEG_FREEDOM:
	    call error (0, "ALL_MEDIANS: Cannot fit surface.")
	case SINGULAR:
	    call eprintf ("Warning: Solution singular for surface.\n")
	default:
	    # everything OK
	}

	# Free space
	call sfree (sp)
end


# GOOD_MEDIANS -- Procedure to fetch medians when the good regions
# list is defined.

procedure good_medians (im, imfit, gl, sf)

pointer	im	# input image
pointer	imfit	# pointer to surface descriptor structure
pointer	gl	# pointer to good regions list
pointer	sf	# pointer the surface descriptor

int	i, cp, lp, x1, x2, y1, y2, ier, ntemp
int	nimcols, nimlines, ncols, nlines, nranges, nbox, nxpts
int	lineno, current_line, lines_per_box, max_nranges
pointer	sp, colsfit, cols, lines, wgt, npts, lbuf, med, mbuf, z, ranges

int	prl_get_ranges(), prl_nextlineno(), is_expand_ranges()
int	is_choose_rangesr()
pointer	imgl2r()
real	asokr()
errchk	smark, salloc, sfree, imgl2r
errchk	isinit, islfit, issolve
errchk	prl_get_ranges, prl_nextlineno, is_choose_rangesr()

begin
	# Determine the number of lines and columns for a median processed
	# image.
	nimcols = IM_LEN(im,1)
	if (mod (int (IM_LEN(im,1)), XMEDIAN(imfit)) != 0)
	    ncols = IM_LEN(im,1) / XMEDIAN(imfit) + 1
	else
	    ncols = IM_LEN(im,1) / XMEDIAN(imfit)
	nimlines = IM_LEN(im,2)
	if (mod (int (IM_LEN(im,2)), YMEDIAN(imfit)) != 0)
	    nlines = IM_LEN(im,2) / YMEDIAN(imfit) + 1
	else
	    nlines = IM_LEN(im,2) / YMEDIAN(imfit)
	nbox = XMEDIAN(imfit) * YMEDIAN(imfit)
	max_nranges = nimcols

	# Initialize the surface fitting.
	call isinit (sf, SURFACE_TYPE(imfit), XORDER(imfit), YORDER(imfit),
	    CROSS_TERMS(imfit), ncols, nlines)

	# Allocate working memory.
	call smark (sp)
	call salloc (colsfit, nimcols, TY_INT)
	call salloc (cols, ncols, TY_INT)
	call salloc (npts, ncols, TY_INT)
	call salloc (lines, nlines, TY_INT)
	call salloc (wgt, ncols, TY_REAL)
	call salloc (med, nbox * ncols, TY_REAL)
	call salloc (z, ncols, TY_REAL)
	call salloc (ranges, 3 * max_nranges + 1, TY_INT)
	call amovkr (1., Memr[wgt], ncols)

	# Loop over median boxes in y.
	lp = 0
	lineno = 0
	for (y1 = 1; y1 <= nimlines; y1 = y1 + YMEDIAN(imfit)) {

	    lineno = lineno + 1
	    current_line = y1 - 1
	    y2 = min (y1 + YMEDIAN(imfit) - 1, nimlines)

	    # If lines not in range, next image section.
	    lines_per_box = 0
	    while (prl_nextlineno (gl, current_line) != EOF) {
		if (current_line > y2)
		    break
		lines_per_box = lines_per_box + 1
	    }
	    if (lines_per_box < (YMEDIAN(imfit) * (MEDIAN_PERCENT(imfit)/100.)))
		next

	    # Loop over the image lines.
	    call aclri (Memi[npts], ncols)
	    do i = y1, y2 {

		# Get image line, and check the good regions list.
		lbuf = imgl2r (im, i)
		nranges = prl_get_ranges (gl, i, Memi[ranges], max_nranges)
		if (nranges == 0)
		    next
		nxpts = is_expand_ranges (Memi[ranges], Memi[colsfit], nimcols)

		# Loop over the median boxes in x.
		cp= 0
		mbuf = med
		for (x1 = 1; x1 <= nimcols; x1 = x1 + XMEDIAN(imfit)) {
		    x2 = min (x1 + XMEDIAN(imfit) - 1, nimcols)
		    ntemp = is_choose_rangesr (Memi[colsfit], Memr[lbuf],
		        Memr[mbuf+Memi[npts+cp]], nxpts, x1, x2)
		    Memi[npts+cp] = Memi[npts+cp] + ntemp
		    mbuf = mbuf + nbox
		    cp = cp + 1
		}
	    }

	    # Calculate the medians.
	    nxpts = 0
	    mbuf = med
	    do i = 1, ncols {
		if (Memi[npts+i-1] > ((MEDIAN_PERCENT(imfit) / 100.) * nbox)) {
		    Memr[z+nxpts] = asokr (Memr[mbuf], Memi[npts+i-1],
			(Memi[npts+i-1] + 1) / 2)
		    Memi[cols+nxpts] = i
		    nxpts = nxpts + 1
		}
		mbuf = mbuf + nbox
	    }

	    # Fit the line.
	    call islfit (sf, Memi[cols], lineno, Memr[z], Memr[wgt], nxpts,
		SF_USER, ier)

	    # Handle fitting errors.
	    switch (ier) {
	    case NO_DEG_FREEDOM:
		call eprintf ("Warning: Too few columns to fit line: %d\n")
		    call pargi (lineno)
	    case SINGULAR:
		call eprintf ("Warning: Solution singular for line: %d\n")
		    call pargi (lineno)
		Memi[lines+lp] = lineno
		lp = lp + 1
	    default:
		Memi[lines+lp] = lineno
		lp = lp + 1
	    }
	}

	# Solve the surface.
	call issolve (sf, Memi[lines], lp, ier)

	# Handle fitting errors.
	switch (ier) {
	case NO_DEG_FREEDOM:
	    call error (0, "GOOD_MEDIANS: Cannot fit surface.")
	case SINGULAR:
	    call eprintf ("Warning: Solution singular for surface.")
	default:
	    # everyting OK
	}

	# Free space.
	call sfree (sp)
end


# SET_OUTIMAGE -- Procedure to write an output image of the desired type.

procedure set_outimage (imin, imout, imfit, sf, rl)

pointer	imin	# input image
pointer	imout	# output image
pointer	imfit	# pointer to the imsurfut header structure
pointer	sf	# pointer to the surface descriptor
pointer	rl	# pointer to the rejected pixel list regions list

int	i, k, ncols, nlines, max_nranges
long	u[IM_MAXDIM], v[IM_MAXDIM]
real	b1x, b2x, b1y, b2y
pointer	sp, x, y, inbuf, outbuf, ranges

int	impnlr(), imgnlr()
real	ims_divzero()
extern	ims_divzero
errchk	malloc, mfree, imgnlr, impnlr

begin
	ncols = IM_LEN(imin,1)
	nlines = IM_LEN(imin,2)
	max_nranges = ncols

	# Calculate transformation constants from real coordinates to
	# median coordinates if median processing specified. 
	if (MEDIAN(imfit) == YES) {
	    b1x = (1. + XMEDIAN(imfit)) / (2. * XMEDIAN(imfit))
	    b2x = (2. * ncols + XMEDIAN(imfit) - 1.) / (2. * XMEDIAN(imfit))
	    b1y = (1. + YMEDIAN(imfit)) / (2. * YMEDIAN(imfit))
	    b2y = (2. * nlines + YMEDIAN(imfit) - 1.) / (2. * YMEDIAN(imfit))
	}

	# Allocate space for x coordinates, initialize to image coordinates
	# and transform to median coordinates.
	call smark (sp)
	call salloc (x, ncols, TY_REAL)
	call salloc (y, ncols, TY_REAL)
	call salloc (ranges, 3 * max_nranges + 1, TY_INT)

	# Intialize the x array.
	do i = 1, ncols
	    Memr[x - 1 + i] = i
	if (MEDIAN(imfit) == YES)
	    call amapr (Memr[x], Memr[x], ncols, 1., real (ncols), b1x, b2x)

	# loop over the images lines
	call amovkl (long (1), v, IM_MAXDIM)
	call amovkl (long (1), u, IM_MAXDIM)
	do i = 1, nlines {

	    # Get input and output image buffers.
	    if (TYPE_OUTPUT(imfit) != FIT) {
		if (imgnlr (imin, inbuf, v) == EOF)
		    call error (0, "Error reading input image.")
	    }
	    if (impnlr (imout, outbuf, u) == EOF)
		call error (0, "Error writing output image.")

	    # Intialize y coordinates to image coordinates, and
	    # transform to median coordinates.
	    if (MEDIAN(imfit) == YES) {
	        Memr[y] = real (i)
		call amapr (Memr[y], Memr[y], 1, 1., real (nlines),
			    b1y, b2y)
		call amovkr (Memr[y], Memr[y+1], (ncols-1))
	    } else
		call amovkr (real (i), Memr[y], ncols)

	    # Write output image.
	    switch (TYPE_OUTPUT(imfit)) {
	    case FIT:
	        call isvector (sf, Memr[x], Memr[y], Memr[outbuf], ncols)
	    case CLEAN:
		call clean_line (Memr[x], Memr[y], Memr[inbuf], ncols, nlines,
		   rl, sf, i, NGROW(imfit)) 
		call amovr (Memr[inbuf], Memr[outbuf], ncols)
	    case RESID:
	        call isvector (sf, Memr[x], Memr[y], Memr[outbuf], ncols)
		call asubr (Memr[inbuf], Memr[outbuf], Memr[outbuf], ncols)
	    case RESP:
	        call isvector (sf, Memr[x], Memr[y], Memr[outbuf], ncols)
		if (IS_INDEF(DIV_MIN(imfit))) {
		    iferr (call adivr (Memr[inbuf], Memr[outbuf], Memr[outbuf],
		        ncols))
			call advzr (Memr[inbuf], Memr[outbuf], Memr[outbuf],
			    ncols, ims_divzero)
		} else {
		    do k = 1, ncols {
			if (Memr[outbuf-1+k] < DIV_MIN(imfit))
			    Memr[outbuf-1+k] = 1.
			else
			    Memr[outbuf-1+k] = Memr[inbuf-1+k] /
			    Memr[outbuf-1+k]
		    }
		}
	    default:
		call error (0, "SET_OUTIMAGE: Unknown output type.")
	    }
	}

	# Free space
	call sfree (sp)
end


# CLEAN_LINE -- Procedure to set weights of rejected points to zero

procedure clean_line (x, y, z, ncols, nlines, rl, sf, line, ngrow)

real	x[ARB]		# array of weights set to 1
real	y		# y value of line
real	z[ARB]		# line of data
int	ncols		# number of image columns
int	nlines		# number of image lines
pointer	rl		# pointer to reject pixel list
pointer	sf		# surface fitting
int	line		# line number
int	ngrow		# radius for region growing

int	cp, j, k, nranges, dist, yreg_min, yreg_max, xreg_min, xreg_max
pointer	sp, branges
real	r2
int	prl_get_ranges(), is_next_number()
real	iseval()

begin
	call smark (sp)
	call salloc (branges, 3 * ncols + 1, TY_INT)

	r2 = ngrow ** 2
	yreg_min = max (1, line - ngrow)
	yreg_max = min (nlines, line + ngrow)

	do j = yreg_min, yreg_max {
	    nranges = prl_get_ranges (rl, j, Memi[branges], ncols)
	    if (nranges == 0)
		next
	    dist = int (sqrt (r2 - (j - line) ** 2))
	    cp = 0
	    while (is_next_number (Memi[branges], cp) != EOF) {
		xreg_min = max (1, cp - dist)
		xreg_max = min (ncols, cp + dist)
		do k = xreg_min, xreg_max
		    z[k] = iseval (sf, x[k], y)
		cp = xreg_max
	    }
	}

	call sfree (sp)
end


# DO_REJECT -- Procedure to detect rejected pixels in an image.

procedure do_reject (im, imfit, gl, sf, rl)

pointer		im	# pointer to in put image
pointer		imfit	# pointer to image fitting structure
pointer		gl	# pointer to good regions list
pointer		sf	# pointer to surface descriptor
pointer		rl	# pointer to rejected pixel list

int	niter, nrejects
real	sigma
int	detect_rejects()
real	get_sigma()
errchk  prl_init, detect_rejects, get_sigma, refit_surface

begin
	# Initialize rejected pixel list.
	call prl_init (rl, int(IM_LEN(im,1)), int(IM_LEN(im,2)))

	# Do an iterative rejection cycle on the image.
	niter = 0
	repeat {

	    # Get the sigma of the fit.
	    sigma = get_sigma (im, gl, sf, rl)

	    # Detect rejected pixels.
	    nrejects = detect_rejects (im, imfit, gl, sf, rl, sigma)

	    # If no rejected pixels quit, else refit surface.
	    if (nrejects == 0 || NITER(imfit) == 0)
		break
	    call refit_surface (im, imfit, gl, sf, rl)

	    niter = niter + 1

	} until (niter == NITER(imfit))
end


# REFIT_SURFACE -- Procedure tp refit the surface.

procedure refit_surface (im, imfit, gl, sf, rl)

pointer		im	# pointer to image
pointer		imfit	# pointer to surface fitting structure
pointer		gl	# pointer to good regions list
pointer		sf	# pointer to surface descriptor
pointer		rl	# pointer to rejected pixels list

int	i, ijunk, lp, ier, max_nranges
int	ncols, nlines, npts, nfree, nrejects, nranges, ncoeff
pointer	sp, cols, colsfit, lines, buf, fbuf, wgt, granges

int	prl_get_ranges(), grow_regions(), is_expand_ranges()
int	is_choose_rangesr()
pointer	imgl2r()
errchk	smark, salloc, sfree, imgl2r
errchk	iscoeff, islfit, issolve
errchk	prl_get_ranges, grow_regions
errchk	is_choose_rangesr

begin
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	max_nranges = ncols

	# Allocate up temporary storage.
	call smark (sp)
	call salloc (cols, ncols, TY_INT)
	call salloc (colsfit, ncols, TY_INT)
	call salloc (lines, nlines, TY_INT)
	call salloc (fbuf, ncols, TY_REAL)
	call salloc (wgt, ncols, TY_REAL)
	call salloc (granges, 3 * max_nranges + 1, TY_INT)

	# Initialize columns.
	do i = 1, ncols
	    Memi[cols+i-1] = i
	call amovi (Memi[cols], Memi[colsfit], ncols)

	# Get number of coefficients.
	switch (SURFACE_TYPE(imfit)) {
	case SF_LEGENDRE, SF_CHEBYSHEV:
	    ncoeff = XORDER(imfit)
	case SF_SPLINE3:
	    ncoeff = XORDER(imfit) + 3
	case SF_SPLINE1:
	    ncoeff = XORDER(imfit) + 1
	}

	# Refit affected lines and solve for surface.
	lp = 0
	do i = 1, nlines {

	    # Determine whether image line is good.
	    if (gl != NULL) {
		nranges = prl_get_ranges (gl, i, Memi[granges], max_nranges)
		if (nranges == 0)
		    next
	    }

	    # Define rejected points with region growing.
	    call amovkr (1., Memr[wgt], ncols)
	    nrejects = grow_regions (Memr[wgt], ncols, nlines, rl, i,
	        NGROW(imfit))

	    # Get number of data points.
	    if (gl == NULL)
		npts = ncols
	    else
		npts = is_expand_ranges (Memi[granges], Memi[colsfit], ncols)
	    nfree = npts - nrejects

	    # If no rejected pixels skip to next line.
	    if (nrejects == 0) {
		if (nfree >= ncoeff ) {
		    Memi[lines+lp] = i
		    lp = lp + 1
		}
		next
	    }

	    # Read in image line.
	    buf = imgl2r (im, i)
	    if (buf == EOF)
		call error (0, "REFIT_SURFACE: Error reading image.")

	    # Select the data.
	    if (gl == NULL) {
		npts = ncols
	        if (nfree >= ncoeff)
	            call islfit (sf, Memi[colsfit], i, Memr[buf], Memr[wgt],
		        npts, SF_USER, ier)
	        else
		    ier = NO_DEG_FREEDOM
	    } else {
		ijunk = is_choose_rangesr (Memi[colsfit], Memr[buf],
		    Memr[fbuf], npts, 1, ncols)
		ijunk = is_choose_rangesr (Memi[colsfit], Memr[wgt], Memr[wgt],
		    npts, 1, ncols)
	        if (nfree >= ncoeff)
	            call islfit (sf, Memi[colsfit], i, Memr[fbuf], Memr[wgt],
		        npts, SF_USER, ier)
	        else
		    ier = NO_DEG_FREEDOM
	    }

	    # Evaluate fitting errors.
	    switch (ier) {
	    case NO_DEG_FREEDOM:
		call eprintf ("REFIT_SURFACE: Too few points to fit line: %d\n")
		    call pargi (i)
	    case SINGULAR:
		call eprintf ("REFIT_SURFACE: Solution singular for line: %d\n")
		    call pargi (i)
		Memi[lines+lp] = i
		lp = lp + 1
	    default:
		Memi[lines+lp] = i
		lp = lp + 1
	    }
	}

	# Resolve surface.
	call issolve (sf, Memi[lines], lp, ier)

	# Evaluate fitting errors for surface.
	switch (ier) {
	case NO_DEG_FREEDOM:
	    call error (0, "REFIT_SURFACE: Too few points to fit surface\n")
	case SINGULAR:
	    call eprintf ("REFIT_SURFACE: Solution singular for surface\n")
	default:
	    # everything OK
	}

	call sfree (sp)

end


# GROW_REGIONS -- Procedure to set weights of rejected points to zero.

int procedure grow_regions (wgt, ncols, nlines, rl, line, ngrow)

real	wgt[ARB]	# array of weights set to 1
int	ncols		# number of image columnspoints
int	nlines		# number of images lines
pointer	rl		# pointer to reject pixel list
int	line		# line number
int	ngrow		# radius for region growing

int	cp, j, k, nrejects, nranges, max_nranges
int	dist, yreg_min, yreg_max, xreg_min, xreg_max
pointer	sp, branges
real	r2
int	prl_get_ranges(), is_next_number()
errchk	smark, salloc, sfree
errchk	prl_get_ranges, is_next_number 

begin
	max_nranges = ncols

	call smark (sp)
	call salloc (branges, 3 * max_nranges + 1, TY_INT)

	r2 = ngrow ** 2
	nrejects = 0
	yreg_min = max (1, line - ngrow)
	yreg_max = min (nlines, line + ngrow)

	do j = yreg_min, yreg_max {
	    nranges = prl_get_ranges (rl, j, Memi[branges], max_nranges)
	    if (nranges == 0)
		next
	    dist = int (sqrt (r2 - (j - line) ** 2))
	    cp = 0
	    while (is_next_number (Memi[branges], cp) != EOF) {
		xreg_min = max (1, cp - dist)
		xreg_max = min (ncols, cp + dist)
		do k = xreg_min, xreg_max {
		    if (wgt[k] > 0.) {
			wgt[k] = 0.
			nrejects = nrejects + 1
		    }
		}
		cp = xreg_max
	    }
	}

	call sfree (sp)
	return (nrejects)
end


# GET_SIGMA -- Procedure to calculate the sigma of the surface fit

real procedure get_sigma (im, gl, sf, rl)

pointer		im	# pointer to image
pointer		gl	# pointer to good pixel list
pointer		sf	# pointer to surface deascriptor
pointer		rl	# pointer to rejected pixel list

int	i, ijunk, cp, nranges, npts, ntpts, ncols, nlines, max_nranges
pointer	sp, colsfit, x, xfit, y, zfit, buf, fbuf, wgt, granges, branges
real	sum, sigma
int	prl_get_ranges(), is_next_number(), is_expand_ranges()
int	is_choose_rangesr()
pointer	imgl2r()
real	asumr(), awssqr()
errchk	smark, salloc, sfree, imgl2r

begin
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	max_nranges = ncols

	# Allocate working space.
	call smark (sp)
	call salloc (colsfit, ncols, TY_INT)
	call salloc (x, ncols, TY_REAL)
	call salloc (xfit, ncols, TY_REAL)
	call salloc (y, ncols, TY_REAL)
	call salloc (fbuf, ncols, TY_REAL)
	call salloc (zfit, ncols, TY_REAL)
	call salloc (wgt, ncols, TY_REAL)
	call salloc (granges, 3 * max_nranges + 1, TY_INT)
	call salloc (branges, 3 * max_nranges + 1, TY_INT)

	# Intialize the x array.
	do i = 1, ncols
	    Memr[x+i-1] = i
	call amovr (Memr[x], Memr[xfit], ncols)

	sum = 0.
	sigma = 0.
	ntpts = 0

	# Loop over the image.
	do i = 1, nlines {

	    # Check that line is in range.
	    if (gl != NULL) {
		nranges = prl_get_ranges (gl, i, Memi[granges], max_nranges)
		if (nranges == 0)
		    next
		npts = is_expand_ranges (Memi[granges], Memi[colsfit], ncols)
	    }
	    
	    # Read in image.
	    buf = imgl2r (im, i)
	    if (buf == EOF)
		call error (0, "GET_SIGMA: Error reading image.")

	    # Select appropriate data and fit.
	    call amovkr (real (i), Memr[y], ncols)
	    if (gl == NULL) {
		npts = ncols
	        call isvector (sf, Memr[xfit], Memr[y], Memr[zfit], npts)
	        call asubr (Memr[buf], Memr[zfit], Memr[zfit], npts)
	    } else {
		ijunk = is_choose_rangesr (Memi[colsfit], Memr[x], Memr[xfit],
		    npts, 1, ncols)
		ijunk = is_choose_rangesr (Memi[colsfit], Memr[buf], Memr[fbuf],
		    npts, 1, ncols)
	        call isvector (sf, Memr[xfit], Memr[y], Memr[zfit], npts)
	        call asubr (Memr[fbuf], Memr[zfit], Memr[zfit], npts)
	    }

	    # Get ranges of rejected pixels for the line and set weights.
	    call amovkr (1., Memr[wgt], ncols)
	    nranges = prl_get_ranges (rl, i, Memi[branges], max_nranges)
	    if (nranges > 0) {
		cp = 0
		while (is_next_number (Memi[branges], cp) != EOF)
		    Memr[wgt+cp-1] = 0.
		if (gl != NULL)
		    ijunk = is_choose_rangesr (Memi[colsfit], Memr[wgt],
		        Memr[wgt], npts, 1, ncols)
	    }

	    # Calculate sigma.
	    sigma = sigma + awssqr (Memr[zfit], Memr[wgt], npts)
	    ntpts = ntpts + asumr (Memr[wgt], npts)
	}

	call sfree (sp)

	return (sqrt (sigma / (ntpts - 1)))
end


# DETECT_REJECTS - Procedure to detect rejected pixels.

int procedure detect_rejects (im, imfit, gl, sf, rl, sigma)

pointer		im	# pointer to image
pointer		imfit	# pointer to surface fitting structure
pointer		gl	# pointer to good pixel list
pointer		sf	# pointer to surface descriptor
pointer		rl	# pointer to rejected pixel list
real		sigma	# standard deviation of fit

int	i, j, ijunk, cp, ncols, nlines, npts, nranges, nlrejects, ntrejects
int	norejects, max_nranges
pointer	sp, granges, branges, x, xfit, cols, colsfit, y, zfit, buf, fbuf
pointer	wgt, list
real	upper, lower

int	prl_get_ranges(), is_next_number(), is_make_ranges(), is_expand_ranges()
int	is_choose_rangesr()
pointer	imgl2r()

begin
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	max_nranges = ncols

	# Allocate temporary space.
	call smark (sp)
	call salloc (x, ncols, TY_REAL)
	call salloc (xfit, ncols, TY_REAL)
	call salloc (cols, ncols, TY_INT)
	call salloc (colsfit, ncols, TY_INT)
	call salloc (y, ncols, TY_REAL)
	call salloc (fbuf, ncols, TY_REAL)
	call salloc (zfit, ncols, TY_REAL)
	call salloc (wgt, ncols, TY_REAL)
	call salloc (granges, 3 * max_nranges + 1, TY_INT)
	call salloc (branges, 3 * max_nranges + 1, TY_INT)
	call salloc (list, ncols, TY_INT)

	# Intialize x and column values.
	do i = 1, ncols {
	    Memi[cols+i-1] = i
	    Memr[x+i-1] = i
	}
	call amovr (Memr[x], Memr[xfit], ncols)
	call amovi (Memi[cols], Memi[colsfit], ncols)

	ntrejects = 0
	if (LOWER(imfit) <= 0.0)
	    lower = -MAX_REAL
	else
	    lower = -sigma * LOWER(imfit)
	if (UPPER(imfit) <= 0.0)
	    upper = MAX_REAL
	else
	    upper = sigma * UPPER(imfit)

	# Loop over the image.
	do i = 1, nlines {

	    # Get ranges if appropriate.
	    if (gl != NULL) {
		nranges = prl_get_ranges (gl, i, Memi[granges], max_nranges)
		if (nranges == 0)
		    next
		npts = is_expand_ranges (Memi[granges], Memi[colsfit], ncols) 
	    }

	    # Read in image.
	    buf = imgl2r (im, i)
	    if (buf == EOF)
		call error (0, "GET_SIGMA: Error reading image.")

	    # Select appropriate data and fit.
	    call amovkr (real (i), Memr[y], ncols)
	    if (gl == NULL) {
		npts = ncols
	        call isvector (sf, Memr[xfit], Memr[y], Memr[zfit], npts)
	        call asubr (Memr[buf], Memr[zfit], Memr[zfit], npts)
	    } else {
		ijunk = is_choose_rangesr (Memi[colsfit], Memr[x], Memr[xfit],
		    npts, 1, ncols)
		ijunk = is_choose_rangesr (Memi[colsfit], Memr[buf], Memr[fbuf],
		    npts, 1, ncols)
	        call isvector (sf, Memr[xfit], Memr[y], Memr[zfit], npts)
	        call asubr (Memr[fbuf], Memr[zfit], Memr[zfit], npts)
	    }

	    # Get ranges of rejected pixels for the line and set weights.
	    call amovkr (1., Memr[wgt], ncols)
	    nranges = prl_get_ranges (rl, i, Memi[branges], max_nranges)
	    norejects = 0
	    if (nranges > 0) {
		cp = 0
		while (is_next_number (Memi[branges], cp) != EOF) {
		    Memi[list+norejects] = cp
		    norejects = norejects + 1
		    Memr[wgt+cp-1] = 0.
		}
		if (gl != NULL)
		    ijunk = is_choose_rangesr (Memi[colsfit], Memr[wgt],
		        Memr[wgt], npts, 1, ncols)
	    }

	    # Detect deviant pixels.
	    nlrejects = 0
	    do j = 1, npts {
		if ((Memr[zfit+j-1] < lower || Memr[zfit+j-1] > upper) &&
		    Memr[wgt+j-1] != 0.) {
		    Memi[list+norejects+nlrejects] = Memi[colsfit+j-1]
		    nlrejects = nlrejects + 1
		}
	    }

	    # Add to rejected pixel list.
	    if (nlrejects > 0) {
		call asrti (Memi[list], Memi[list], norejects + nlrejects)
		nranges = is_make_ranges (Memi[list], norejects + nlrejects,
		    Memi[granges], max_nranges)
		call prl_put_ranges (rl, i, i, Memi[granges])
	    }

	    ntrejects = ntrejects + nlrejects
	}

	call sfree (sp)
	return (ntrejects)
end


# AWSSQR -- Procedure to calculate the weighted sum of the squares

real procedure awssqr (a, w, npts)

real	a[npts]		# array of data
real	w[npts]		# array of points
int	npts		# number of data points

int	i
real	sum

begin
	sum = 0.
	do i = 1, npts
	    sum = sum + w[i] * a[i] ** 2

	return (sum)
end


# IMS_DIVZER0 -- Return 1. on a divide by zero

real	procedure ims_divzero (x)

real	x

begin
	return (1.)
end
