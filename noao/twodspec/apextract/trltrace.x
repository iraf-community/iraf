include	<imhdr.h>
include	<math/curfit.h>
include	<pkg/center1d.h>
include	<pkg/gtools.h>
include	<pkg/xtanswer.h>
include	"apertures.h"

# TR_LTRACE -- Trace feature positions for aperture axis 1.

procedure tr_ltrace (image, im, ic, start, step, nsum, cradius, cwidth,
    threshold, fittrace, aps, naps)

char	image[ARB]		# Image to be traced
pointer	im			# IMIO pointer
pointer	ic			# ICFIT pointer
int	start			# Starting line
int	step			# Tracing step size
int	nsum			# Number of lines or columns to sum
real	cradius			# Centering radius
real	cwidth			# Centering width
real	threshold		# Detection threshold for centering
int	fittrace		# Interactive trace fitting?
pointer	aps[AP_MAXAPS]		# Apertures
int	naps			# Number of apertures

int	i, j, n, nx, ny, ntrace, istart, line, line1, line2, interactive, fd
pointer	data, sp, str, x, y, wts, xc, x1, x2, gp, gt

real	center1d(), cveval()
pointer	gt_init()

errchk	cveval, xt_lsum, xt_lsumb, center1d, icg_fit, ic_fit, ap_gopen, ap_popen

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
	data = NULL

	# Set the dispersion lines to be traced.

	do i = 1, ntrace
	    Memr[y+i-1] = start + (i - istart) * step

	# Trace from the starting line to the last line.

	x1 = x + istart - 1
	do i = 1, naps
	    Memr[xc+i-1] = AP_CEN(aps[i], 1) +
		cveval (AP_CV(aps[i]), real (start))

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

	    x2 = x1
	    do j = 1, naps {
		Memr[xc+j-1] = center1d (Memr[xc+j-1], Memr[data], nx,
		    cwidth, EMISSION, cradius, threshold)
		Memr[x2] = Memr[xc+j-1]
		if ((IS_INDEF (Memr[x2])) && (!IS_INDEF (Memr[x2-1]))) {
		    Memr[xc+j-1] = Memr[x2-1]
	    	    call sprintf (Memc[str], SZ_LINE,
			"Trace of aperture %d in %s lost at line %d.")
			call pargi (AP_ID(aps[j]))
			call pargstr (image)
			call pargi (line)
		    call ap_log (Memc[str])
		} else if ((!IS_INDEF (Memr[x2])) && (IS_INDEF (Memr[x2-1]))) {
		    call sprintf (Memc[str], SZ_LINE,
			"Trace of aperture %d in %s recovered at line %d.")
			call pargi (AP_ID(aps[j]))
			call pargstr (image)
			call pargi (line)
		    call ap_log (Memc[str])
		}
		x2 = x2 + ntrace
	    }
	    x1 = x1 + 1
	}

	# Trace from the starting line to the first line.

	x1 = x + istart - 2
	do i = 1, naps
	    Memr[xc+i-1] = AP_CEN(aps[i], 1) +
		cveval (AP_CV(aps[i]), real (start))

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

	    x2 = x1
	    do j = 1, naps {
		Memr[xc+j-1] = center1d (Memr[xc+j-1], Memr[data], nx,
		    cwidth, EMISSION, cradius, threshold)
		Memr[x2] = Memr[xc+j-1]
		if ((IS_INDEF (Memr[x2])) && (!IS_INDEF (Memr[x2+1]))) {
		    Memr[xc+j-1] = Memr[x2+1]
	    	    call sprintf (Memc[str], SZ_LINE,
			"Trace of aperture %d in %s lost at line %d.")
			call pargi (AP_ID(aps[j]))
			call pargstr (image)
			call pargi (line)
		    call ap_log (Memc[str])
		} else if ((!IS_INDEF (Memr[x2])) && (IS_INDEF (Memr[x2+1]))) {
		    call sprintf (Memc[str], SZ_LINE,
			"Trace of aperture %d in %s recovered at line %d.")
			call pargi (AP_ID(aps[j]))
			call pargstr (image)
			call pargi (line)
		    call ap_log (Memc[str])
		}
		x2 = x2 + ntrace
	    }
	    x1 = x1 - 1
	}

	# Initialize the the GTOOLS parameters.
	# Set initial interactive flag.

	call ic_putr (ic, "xmin", 1.)
	call ic_putr (ic, "xmax", real (ny))
	call ic_pstr (ic, "xlabel", "Line")
	call ic_pstr (ic, "ylabel", "Column")

	gt = gt_init()
	call gt_setr (gt, GTXMIN, 1. - step / 2)
	call gt_setr (gt, GTXMAX, real (ny + step / 2))

	interactive = fittrace
	if (fittrace == NO)
	    interactive = ALWAYSNO

	do j = 1, naps {
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
		    AP_CV(aps[j]), Memr[y], Memr[x1], Memr[wts], n)
	    } else
	        call ic_fit (ic, AP_CV(aps[j]), Memr[y], Memr[x1], Memr[wts], n,
		    YES, YES, YES, YES)

	    call ap_popen (gp, fd)
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
