include	<imhdr.h>
include	<math/curfit.h>
include	<pkg/center1d.h>
include	<pkg/gtools.h>
include	<pkg/xtanswer.h>
include	"apertures.h"

# TR_CTRACE -- Trace feature positions for aperture axis 2.

define	MAXBUF	100000	# Column buffer size

procedure tr_ctrace (image, im, ic, start, step, nsum, cradius, cwidth,
    threshold, fittrace, aps, naps)

char	image[ARB]		# Image to be traced.
pointer	im			# IMIO pointer
pointer	ic			# ICFIT pointer
int	start			# Starting column
int	step			# Tracing step size
int	nsum			# Number of lines or columns to sum
real	cradius			# Centering radius
real	cwidth			# Centering width
real	threshold		# Detection threshold for centering
int	fittrace		# Interactive trace fitting?
pointer	aps[AP_MAXAPS]		# Apertures
int	naps			# Number of apertures

int	nlines, col, col1, col2, line1, line2, interactive
int	i, j, n, nx, ny, ntrace, istart, fd
real	yc
pointer	co, data, sp, str, x, y, wts, gp, gt

real	center1d(), cveval()
pointer comap(), gt_init()

errchk	cveval, xt_csum, xt_csumb, center1d, icg_fit, ic_fit, ap_gopen, ap_popen

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

	interactive = fittrace
	if (fittrace == NO)
	    interactive = ALWAYSNO

	# Trace each feature.

	do j = 1, naps {

	    # Trace from the starting column to the last column while the
	    # position is not INDEF.

	    yc = AP_CEN(aps[j], 2) + cveval (AP_CV(aps[j]), real (start))
	    do i = istart, ntrace {
		if (IS_INDEF (yc))
		    yc = Memr[y+i-3]

		if (! IS_INDEF (yc)) {
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

		    yc = yc - line1 + 1
		    yc = center1d (yc, Memr[data], line2-line1+1, cwidth,
			EMISSION, cradius, threshold)

		    if (!IS_INDEF (yc)) {
		        yc = yc + line1 - 1
			if (IS_INDEF (Memr[y+i-2])) {
	    		    call sprintf (Memc[str], SZ_LINE,
			   "Trace of aperture %d in %s recovered at column %d.")
			        call pargi (AP_ID(aps[j]))
			        call pargstr (image)
			        call pargi (col)
			    call ap_log (Memc[str])
			}
		    } else {
	    		call sprintf (Memc[str], SZ_LINE,
			    "Trace of aperture %d in %s lost at column %d.")
			    call pargi (AP_ID(aps[j]))
			    call pargstr (image)
			    call pargi (col)
			call ap_log (Memc[str])
		    }
		}

		Memr[y+i-1] = yc
	    }

	    # Trace from the starting column to the first column while the
	    # position is not INDEF.

	    yc = AP_CEN(aps[j], 2) + cveval (AP_CV(aps[j]), real (start))
	    do i = istart - 1, 1, -1 {
		if (IS_INDEF (yc))
		    yc = Memr[y+i+1]

		if (!IS_INDEF (yc)) {
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

		    yc = yc - line1 + 1
		    yc = center1d (yc, Memr[data], line2-line1+1, cwidth,
			EMISSION, cradius, threshold)

		    if (!IS_INDEF (yc)) {
		        yc = yc + line1 - 1
			if (IS_INDEF (Memr[y+i])) {
	    		    call sprintf (Memc[str], SZ_LINE,
			   "Trace of aperture %d in %s recovered at column %d.")
			        call pargi (AP_ID(aps[j]))
			        call pargstr (image)
			        call pargi ((i - 1) * step + 1)
			    call ap_log (Memc[str])
			}
		    } else {
	    		call sprintf (Memc[str], SZ_LINE,
			    "Trace of aperture %d in %s lost at column %d.")
			    call pargi (AP_ID(aps[j]))
			    call pargstr (image)
			    call pargi ((i - 1) * step + 1)
			call ap_log (Memc[str])
		    }
		}

		Memr[y+i-1] = yc
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

	    if (n < 4) {
		call eprintf (
		    "Not enough points traced for aperture %d of %s\n")
		    call pargi (AP_ID(aps[j]))
		    call pargstr (image)
		next
	    }
		    
	    # Fit a curve to the traced positions and graph the result.

	    call sprintf (Memc[str], SZ_LINE,
		"Fit curve to aperture %d of %s interactively")
		call pargi (AP_ID(aps[j]))
		call pargstr (image)
	    call xt_answer (Memc[str], interactive)

	    call sprintf (Memc[str], SZ_LINE, "Aperture %d of %s")
	        call pargi (AP_ID(aps[j]))
		call pargstr (image)
	    call gt_sets (gt, GTTITLE, Memc[str])

	    if ((interactive == YES) || (interactive == ALWAYSYES)) {
		call ap_gopen (gp)
	        call icg_fit (ic, gp, "apio.cursor", gt,
		    AP_CV(aps[j]), Memr[x], Memr[y], Memr[wts], n)
	    } else
	        call ic_fit (ic, AP_CV(aps[j]), Memr[x], Memr[y], Memr[wts], n,
		    YES, YES, YES, YES)

	    call ap_popen (gp, fd)
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
