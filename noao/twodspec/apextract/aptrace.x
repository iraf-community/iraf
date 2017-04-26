include	<imhdr.h>
include	<math/curfit.h>
include	<pkg/center1d.h>
include	<pkg/gtools.h>
include	"apertures.h"

define	MAXBUF	100000		# Column buffer size


# AP_TRACE -- Trace features in a two dimensional image.
#
# Given an image pointer, the starting dispersion position, and a set
# of apertures defining the centers of features, trace the feature
# centers to other dispersion positions and fit a curve to the positions.
# The user specifies the dispersion step size, the number of dispersion
# lines to sum, and parameters for the feature centering function
# fitting.

procedure ap_trace (image, line, aps, naps, apedit)

char	image[SZ_FNAME]		# Image name
int	line			# Starting dispersion position
pointer	aps[ARB]		# Apertures
int	naps			# Number of apertures
int	apedit			# Called from APEDIT?

int	step			# Tracing step
int	nsum			# Number of dispersion lines to sum
int	nlost			# Number of steps lost before quitting
real	cradius			# Centering radius
real	cwidth			# Centering width
real	cthreshold		# Detection threshold for centering

int	i, na, dispaxis, apaxis
real	center
pointer	im, ic, ic1, sp, str
data	ic1 /NULL/

int	apgeti()
real	apgetr()
bool	clgetb(), ap_answer()
pointer	ap_immap()

errchk	ap_immap, ic_open, ap_ltrace, ap_ctrace, ap_default

common	/apt_com/ ic

begin
	na = 0
	do i = 1, naps
	    if (AP_SELECT(aps[i]) == YES)
		na = na + 1
	if (naps > 0 && na == 0)
	    return

	# Query user.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	if (apedit == NO) {
	    call sprintf (Memc[str], SZ_LINE, "Trace apertures for %s?")
	        call pargstr (image)
	    if (!ap_answer ("anstrace", Memc[str])) {
	        call sfree (sp)
	        return
	    }

	    call sprintf (Memc[str], SZ_LINE,
	        "Fit traced positions for %s interactively?")
	        call pargstr (image)
	    if (ap_answer ("ansfittrace", Memc[str])) {
	        call apgstr ("ansfittrace", Memc[str], SZ_LINE)
	        call appstr ("ansfittrace1", Memc[str])
	    } else
	        call appstr ("ansfittrace1", "NO")

	    if (clgetb ("verbose"))
	        call printf ("Tracing apertures ...\n")
	}

	# Tracing parameters
	step = apgeti ("t_step")
	nsum = max (1, abs (apgeti ("t_nsum")))
	nlost = apgeti ("t_nlost")
	if (ic == NULL || ic1 == NULL) {
	    call ic_open (ic)
	    ic1 = ic
	    call apgstr ("t_function", Memc[str], SZ_LINE)
	    call ic_pstr (ic, "function", Memc[str])
	    call ic_puti (ic, "order", apgeti ("t_order"))
	    call apgstr ("t_sample", Memc[str], SZ_LINE)
	    call ic_pstr (ic, "sample", Memc[str])
	    call ic_puti (ic, "naverage", apgeti ("t_naverage"))
	    call ic_puti (ic, "niterate", apgeti ("t_niterate"))
	    call ic_putr (ic, "low", apgetr ("t_low_reject"))
	    call ic_putr (ic, "high", apgetr ("t_high_reject"))
	    call ic_putr (ic, "grow", apgetr ("t_grow"))
	}

	im = ap_immap (image, apaxis, dispaxis)

	# If no apertures are defined default to the center of the image.
	if (naps == 0) {
	    naps = 1
	    center = IM_LEN (im, apaxis) / 2.
	    call ap_default (im, 1, 1, apaxis, center, real (line),
		aps[naps])
	    call sprintf (Memc[str], SZ_LINE,
	       "TRACE - Default aperture defined centered on %s")
	       call pargstr (image)
	    call ap_log (Memc[str], YES, NO, YES)
	}

	# Centering parameters
	cwidth = apgetr ("t_width")
	cradius = apgetr ("radius")
	cthreshold = apgetr ("threshold")

	switch (dispaxis) {
	case 1:
	    call ap_ctrace (image, im, ic, line, step, nsum, nlost, cradius,
		cwidth, cthreshold, aps, naps)
	case 2:
	    call ap_ltrace (image, im, ic, line, step, nsum, nlost, cradius,
		cwidth, cthreshold, aps, naps)
	}

	# Log the tracing and write the traced apertures to the database.

	call sprintf (Memc[str], SZ_LINE,
	    "TRACE - %d apertures traced in %s.")
       	    call pargi (na)
       	    call pargstr (image)
	if (apedit == NO)
	    call ap_log (Memc[str], YES, YES, NO)
	else
	    call ap_log (Memc[str], YES, NO, NO)

	call appstr ("ansdbwrite1", "yes")

	call imunmap (im)
	call sfree (sp)
end


procedure ap_trfree ()

pointer	ic
common	/apt_com/ ic

begin
	call ic_closer (ic)
end


# AP_CTRACE -- Trace feature positions for aperture axis 2.

procedure ap_ctrace (image, im, ic, start, step, nsum, nlost, cradius, cwidth,
    threshold, aps, naps)

char	image[ARB]		# Image to be traced.
pointer	im			# IMIO pointer
pointer	ic			# ICFIT pointer
int	start			# Starting column
int	step			# Tracing step size
int	nsum			# Number of lines or columns to sum
int	nlost			# Number of steps lost before quiting
real	cradius			# Centering radius
real	cwidth			# Centering width
real	threshold		# Detection threshold for centering
pointer	aps[ARB]		# Apertures
int	naps			# Number of apertures

int	nlines, col, col1, col2, line1, line2
int	i, j, n, nx, ny, ntrace, istart, lost, fd
real	yc, yc1
pointer	co, data, sp, str, x, y, wts, gp, gt

real	center1d(), ap_cveval()
bool	ap_answer()
pointer comap(), gt_init()

errchk	ap_cveval, xt_csum, xt_csumb, center1d, icg_fit, ic_fit
errchk	ap_gopen, ap_popen

begin
	# Set up column access buffering.

	co = comap (im, MAXBUF)

	# Determine the number of lines to be traced and allocate memory.

	nx = IM_LEN(im, 1)
	ny = IM_LEN(im, 2)
	if (IS_INDEFI (start))
	    start = nx / 2
	nlines = 5 * cwidth
	istart = (start - 1) / step + 1
	ntrace = istart + (nx - start) / step

	# Allocate memory for the traced positions and the weights for fitting.

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (x, ntrace, TY_REAL)
	call salloc (y, ntrace, TY_REAL)
	call salloc (wts, ntrace, TY_REAL)
	call aclrr (Memr[y], ntrace)
	data = NULL

	# Initialize the ICFIT limits and the GTOOLS parameters.
	# Set initial interactive flag.

	call ic_putr (ic, "xmin", 1.)
	call ic_putr (ic, "xmax", real (nx))
	call ic_pstr (ic, "xlabel", "Column")
	call ic_pstr (ic, "ylabel", "Line")

	gt = gt_init()
	call gt_setr (gt, GTXMIN, 1. - step / 2)
	call gt_setr (gt, GTXMAX, real (nx + step / 2))

	# Trace each feature.

	line1 = 0
	line2 = 0
	do j = 1, naps {
	    if (AP_SELECT(aps[j]) == NO)
		next

	    # Trace from the starting column to the last column while the
	    # position is not INDEF.

	    lost = 0
	    yc = AP_CEN(aps[j], 2) + ap_cveval (AP_CV(aps[j]), real (start))
	    do i = istart, ntrace {
		Memr[y+i-1] = INDEF
		if (lost < nlost) {
		    # Update the scrolling buffer if the feature center is less
		    # than cwidth from the edge of the buffer.
		    if (((yc-line1) < cwidth) || ((line2-yc) < cwidth)) {
		        line1 = max (1, int (yc + .5 - nlines / 2))
		        line2 = min (ny, line1 + nlines - 1)
		        line1 = max (1, line2 - nlines + 1)
		    }

		    # Sum columns to form the 1D vector for centering.

	            col = start + (i - istart) * step
		    col1 = max (1, col - nsum / 2)
		    col2 = min (nx, col1 + nsum - 1)
		    col1 = max (1, col2 - nsum + 1)

		    # If columns in the sum overlap then use buffering.

		    if (step < nsum)
		        call xt_csumb (co, col1, col2, line1, line2, data)
		    else
		        call xt_csum (co, col1, col2, line1, line2, data)

		    # Center the feature for the new column using the previous
		    # center as the starting point.  Convert to position
		    # relative to the start of the data buffer for centering
		    # and then convert back to position relative to the
		    # edge of the image.

		    yc1 = center1d (yc-line1+1, Memr[data], line2-line1+1,
			cwidth, EMISSION, cradius, threshold)

		    if (!IS_INDEF (yc1)) {
			lost = 0
		        yc = yc1 + line1 - 1
			Memr[y+i-1] = yc
			if (IS_INDEF (Memr[y+i-2])) {
	    		    call sprintf (Memc[str], SZ_LINE,
		   "TRACE - Trace of aperture %d in %s recovered at column %d.")
			        call pargi (AP_ID(aps[j]))
			        call pargstr (image)
			        call pargi (col)
			    call ap_log (Memc[str], YES, NO, YES)
			}
		    } else {
			lost = lost + 1
	    		call sprintf (Memc[str], SZ_LINE,
		    "TRACE - Trace of aperture %d in %s lost at column %d.")
			    call pargi (AP_ID(aps[j]))
			    call pargstr (image)
			    call pargi (col)
			call ap_log (Memc[str], YES, NO, YES)
		    }
		}
	    }

	    # Trace from the starting column to the first column while the
	    # position is not INDEF.

	    lost = 0
	    yc = AP_CEN(aps[j], 2) + ap_cveval (AP_CV(aps[j]), real (start))
	    do i = istart - 1, 1, -1 {
		Memr[y+i-1] = INDEF
		if (lost < nlost) {
		    # Update the scrolling buffer if the feature center is less
		    # than cwidth from the edge of the buffer.

		    if (((yc-line1) < cwidth) || ((line2-yc) < cwidth)) {
		        line1 = max (1, int (yc + .5 - nlines / 2))
		        line2 = min (ny, line1 + nlines - 1)
		        line1 = max (1, line2 - nlines + 1)
		    }

		    # Sum columns to form the 1D vector for centering.

	            col = start + (i - istart) * step
		    col1 = max (1, col - nsum / 2)
		    col2 = min (nx, col1 + nsum - 1)
		    col1 = max (1, col2 - nsum + 1)

		    # If columns in the sum overlap then use buffering.

		    if (step < nsum)
		        call xt_csumb (co, col1, col2, line1, line2, data)
		    else
		        call xt_csum (co, col1, col2, line1, line2, data)

		    # Center the feature for the new column using the previous
		    # center as the starting point.  Convert to position
		    # relative to the start of the data buffer for centering
		    # and then convert back to position relative to the
		    # edge of the image.

		    yc1 = center1d (yc-line1+1, Memr[data], line2-line1+1,
			cwidth, EMISSION, cradius, threshold)

		    if (!IS_INDEF (yc1)) {
			lost = 0
		        yc = yc1 + line1 - 1
			Memr[y+i-1] = yc
			if (IS_INDEF (Memr[y+i])) {
	    		    call sprintf (Memc[str], SZ_LINE,
		   "TRACE - Trace of aperture %d in %s recovered at column %d.")
			        call pargi (AP_ID(aps[j]))
			        call pargstr (image)
			        call pargi ((i - 1) * step + 1)
			    call ap_log (Memc[str], YES, NO, YES)
			}
		    } else {
			lost = lost + 1
	    		call sprintf (Memc[str], SZ_LINE,
		    "TRACE - Trace of aperture %d in %s lost at column %d.")
			    call pargi (AP_ID(aps[j]))
			    call pargstr (image)
			    call pargi ((i - 1) * step + 1)
			call ap_log (Memc[str], YES, NO, YES)
		    }
		}
	    }

	    # Order the traced points and exclude INDEF positions.

	    n = 0
	    do i = 1, ntrace {
		if (IS_INDEF (Memr[y+i-1]))
		    next
		n = n + 1
		Memr[x+n-1] = start + (i - istart) * step
		Memr[y+n-1] = Memr[y+i-1]
		Memr[wts+n-1] = 1.
	    }

	    # If all positions are INDEF print a message and go on to the next
	    # aperture.

	    if (n < 2) {
		call eprintf (
		    "Not enough points traced for aperture %d of %s\n")
		    call pargi (AP_ID(aps[j]))
		    call pargstr (image)
		next
	    }
		    
	    # Fit a curve to the traced positions and graph the result.

	    call sprintf (Memc[str], SZ_LINE, "Aperture %d of %s")
	        call pargi (AP_ID(aps[j]))
		call pargstr (image)
	    call gt_sets (gt, GTTITLE, Memc[str])

	    call sprintf (Memc[str], SZ_LINE,
		"Fit curve to aperture %d of %s interactively")
		call pargi (AP_ID(aps[j]))
		call pargstr (image)
	    if (ap_answer ("ansfittrace1", Memc[str])) {
		call ap_gopen (gp)
	        call icg_fit (ic, gp, "gcur", gt,
		    AP_CV(aps[j]), Memr[x], Memr[y], Memr[wts], n)
	    } else
	        call ic_fit (ic, AP_CV(aps[j]), Memr[x], Memr[y], Memr[wts], n,
		    YES, YES, YES, YES)

	    call ap_popen (gp, fd, "trace")
	    if (gp != NULL) {
		call icg_graphr (ic, gp, gt, AP_CV(aps[j]),
		    Memr[x], Memr[y], Memr[wts], n)
		call ap_pclose (gp, fd)
	    }

	    call asubkr (Memr[y], AP_CEN(aps[j], 2), Memr[y], n)
	    call ic_fit (ic, AP_CV(aps[j]), Memr[x], Memr[y], Memr[wts], n,
		YES, YES, YES, YES)
	}

	# Free allocated memory.

	call gt_free (gt)
	call mfree (data, TY_REAL)
	call counmap (co)
	call sfree (sp)
end


# AP_LTRACE -- Trace feature positions for aperture axis 1.

procedure ap_ltrace (image, im, ic, start, step, nsum, nlost, cradius, cwidth,
    threshold, aps, naps)

char	image[ARB]		# Image to be traced
pointer	im			# IMIO pointer
pointer	ic			# ICFIT pointer
int	start			# Starting line
int	step			# Tracing step size
int	nsum			# Number of lines or columns to sum
int	nlost			# Number of steps lost before quiting
real	cradius			# Centering radius
real	cwidth			# Centering width
real	threshold		# Detection threshold for centering
pointer	aps[ARB]		# Apertures
int	naps			# Number of apertures

real	xc1
int	i, j, n, nx, ny, ntrace, istart, line, line1, line2, fd
pointer	data, sp, str, x, y, wts, xc, lost, x1, x2, gp, gt

real	center1d(), ap_cveval()
bool	ap_answer()
pointer	gt_init()

errchk	ap_cveval, xt_lsum, xt_lsumb, center1d, icg_fit, ic_fit
errchk	ap_gopen, ap_popen

begin
	# Determine the number of lines to be traced and allocate memory.

	nx = IM_LEN(im, 1)
	ny = IM_LEN(im, 2)
	if (IS_INDEFI (start))
	    start = ny / 2

	istart = (start - 1) / step + 1
	ntrace = istart + (ny - start) / step

	# Allocate memory for the traced positions and the weights for fitting.

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (x, ntrace * naps, TY_REAL)
	call salloc (y, ntrace, TY_REAL)
	call salloc (wts, ntrace, TY_REAL)
	call salloc (xc, naps, TY_REAL)
	call salloc (lost, naps, TY_INT)
	call aclrr ( Memr[x], ntrace * naps)
	data = NULL

	# Set the dispersion lines to be traced.

	do i = 1, ntrace
	    Memr[y+i-1] = start + (i - istart) * step

	# Trace from the starting line to the last line.

	x1 = x + istart - 1
	do i = 1, naps {
	    if (AP_SELECT(aps[i]) == NO)
		next
	    Memr[xc+i-1] = AP_CEN(aps[i], 1) +
		ap_cveval (AP_CV(aps[i]), real (start))
	    Memi[lost+i-1] =  0
	}

	do i = istart, ntrace {
	    line = Memr[y+i-1]
	    line1 = max (1, line - nsum / 2)
	    line2 = min (ny, line1 + nsum - 1)
	    line1 = max (1, line2 - nsum + 1)

	    # If the sums overlap use buffering.

	    if (step < nsum)
	        call xt_lsumb (im, 1, nx, line1, line2, data)
	    else
	        call xt_lsum (im, 1, nx, line1, line2, data)

	    do j = 1, naps {
		if (AP_SELECT(aps[j]) == NO)
		    next
		x2 = x1 + (j - 1) * ntrace
		Memr[x2] = INDEF
		if (Memi[lost+j-1] < nlost) {
		    xc1 = center1d (Memr[xc+j-1], Memr[data], nx,
			cwidth, EMISSION, cradius, threshold)
		    if (IS_INDEF(xc1)) {
			Memi[lost+j-1] = Memi[lost+j-1] + 1
			call sprintf (Memc[str], SZ_LINE,
			"TRACE - Trace of aperture %d in %s lost at line %d.")
			    call pargi (AP_ID(aps[j]))
			    call pargstr (image)
			    call pargi (line)
			call ap_log (Memc[str], YES, NO, YES)
		    } else {
			Memi[lost+j-1] = 0
			Memr[xc+j-1] = xc1
			Memr[x2] = xc1
			if (IS_INDEF (Memr[x2-1])) {
			    call sprintf (Memc[str], SZ_LINE,
		    "TRACE - Trace of aperture %d in %s recovered at line %d.")
				call pargi (AP_ID(aps[j]))
				call pargstr (image)
				call pargi (line)
			    call ap_log (Memc[str], YES, NO, YES)
			}
		    }
		}
	    }
	    x1 = x1 + 1
	}

	# Trace from the starting line to the first line.

	x1 = x + istart - 2
	do i = 1, naps {
	    if (AP_SELECT(aps[i]) == NO)
		next
	    Memr[xc+i-1] = AP_CEN(aps[i], 1) +
		ap_cveval (AP_CV(aps[i]), real (start))
	    Memi[lost+i-1] = 0
	}

	do i = istart - 1, 1, -1 {
	    line = Memr[y+i-1]
	    line1 = max (1, line - nsum / 2)
	    line2 = min (ny, line1 + nsum - 1)
	    line1 = max (1, line2 - nsum + 1)

	    # If the sums overlap use buffering.

	    if (step < nsum)
	        call xt_lsumb (im, 1, nx, line1, line2, data)
	    else
	        call xt_lsum (im, 1, nx, line1, line2, data)

	    do j = 1, naps {
		if (AP_SELECT(aps[j]) == NO)
		    next
		x2 = x1 + (j - 1) * ntrace
		Memr[x2] = INDEF
		if (Memi[lost+j-1] < nlost) {
		    xc1 = center1d (Memr[xc+j-1], Memr[data], nx,
			cwidth, EMISSION, cradius, threshold)
		    if (IS_INDEF(xc1)) {
			Memi[lost+j-1] = Memi[lost+j-1] + 1
			call sprintf (Memc[str], SZ_LINE,
			    "TRACE - Trace of aperture %d in %s lost at line %d.")
			    call pargi (AP_ID(aps[j]))
			    call pargstr (image)
			    call pargi (line)
			call ap_log (Memc[str], YES, NO, YES)
		    } else {
			Memi[lost+j-1] = 0
			Memr[xc+j-1] = xc1
			Memr[x2] = xc1
			if (IS_INDEF (Memr[x2+1])) {
			    call sprintf (Memc[str], SZ_LINE,
		    "TRACE - Trace of aperture %d in %s recovered at line %d.")
				call pargi (AP_ID(aps[j]))
				call pargstr (image)
				call pargi (line)
			    call ap_log (Memc[str], YES, NO, YES)
			}
		    }
		}
	    }
	    x1 = x1 - 1
	}

	# Initialize the the GTOOLS parameters.
	call ic_putr (ic, "xmin", 1.)
	call ic_putr (ic, "xmax", real (ny))
	call ic_pstr (ic, "xlabel", "Line")
	call ic_pstr (ic, "ylabel", "Column")

	gt = gt_init()
	call gt_setr (gt, GTXMIN, 1. - step / 2)
	call gt_setr (gt, GTXMAX, real (ny + step / 2))

	do j = 1, naps {
	    if (AP_SELECT(aps[j]) == NO)
		next

	    # Order the traced points and exclude INDEF positions.

	    x1 = x + (j - 1) * ntrace
	    n = 0

	    do i = 1, ntrace {
		if (IS_INDEF (Memr[x1+i-1]))
		    next
		n = n + 1
		Memr[x1+n-1] = Memr[x1+i-1]
		Memr[y+n-1] = start + (i - istart) * step
		Memr[wts+n-1] = 1.
	    }

	    # If all positions are INDEF print a message and go on to the next
	    # aperture.

	    if (n < 2) {
		call eprintf (
		    "Not enough points traced for aperture %d of %s\n")
		    call pargi (AP_ID(aps[j]))
		    call pargstr (image)
		next
	    }

	    # Fit a curve to the traced positions and graph the result.

	    call sprintf (Memc[str], SZ_LINE, "Aperture %d of %s")
	        call pargi (AP_ID(aps[j]))
		call pargstr (image)
	    call gt_sets (gt, GTTITLE, Memc[str])

	    call sprintf (Memc[str], SZ_LINE,
		"Fit curve to aperture %d of %s interactively")
		call pargi (AP_ID(aps[j]))
		call pargstr (image)
	    if (ap_answer ("ansfittrace1", Memc[str])) {
		call ap_gopen (gp)
	        call icg_fit (ic, gp, "gcur", gt,
		    AP_CV(aps[j]), Memr[y], Memr[x1], Memr[wts], n)
	    } else
	        call ic_fit (ic, AP_CV(aps[j]), Memr[y], Memr[x1], Memr[wts], n,
		    YES, YES, YES, YES)

	    call ap_popen (gp, fd, "trace")
	    if (gp != NULL) {
		call icg_graphr (ic, gp, gt, AP_CV(aps[j]),
		    Memr[y], Memr[x1], Memr[wts], n)
		call ap_pclose (gp, fd)
	    }

	    # Subtract the aperture center and refit offset curve.
	    call asubkr (Memr[x1], AP_CEN(aps[j], 1), Memr[x1], n)
	    call ic_fit (ic, AP_CV(aps[j]), Memr[y], Memr[x1], Memr[wts], n,
		YES, YES, YES, YES)
	}

	# Free allocated memory.

	call gt_free (gt)
	call mfree (data, TY_REAL)
	call sfree (sp)
end
