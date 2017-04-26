# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GCTRAN -- Transform a point in world coordinate system WCS_A to world
# coordinate system WCS_B.  The transformation is performed by transforming
# from WCS_A to NDC, followed by a transformation from NDC to WCS_B.  The
# transformation parameters are cached for efficiency when transforming
# multiple points in the same pairs of coordinate systems.  Three types of
# transformations are supported: linear, log, and "elog".  The latter is
# a logarithmic function defined for all X, i.e., for negative as well as
# positive X.

procedure gctran (gp, x1,y1, x2,y2, wcs_a, wcs_b)

pointer	gp			# graphics descriptor
real	x1, y1			# coords of point in WCS_A (input)
real	x2, y2			# coords of point in WCS_B (output)
int	wcs_a			# input WCS
int	wcs_b			# output WCS

int	w, a
int	wcsord, tran[2,2], wcs[2]
real	morigin[2,2], worigin[2,2], scale[2,2], ds
real	w1[2,2], w2[2,2], s1[2,2], s2[2,2], p1[2], p2[2]
pointer	wp

bool	fp_nondegenr()
real	elogr(), aelogr()

begin
	# Verify that the WCS has not changed since we were last called.
	# WCSORD is a unique integer (ordinal) assigned by GIO each time a
	# WCS is fixed.

	if (GP_WCSSTATE(gp) != FIXED || GP_WCSORD(gp) != wcsord)
	    wcs[1] = -1

	# Verify that cached transformation parameters are up to date, and if
	# not, recompute them.

	if (wcs[1] != wcs_a || wcs[2] != wcs_b) {
	    wcsord = GP_WCSORD(gp)
	    wcs[1] = wcs_a
	    wcs[2] = wcs_b

	    # Copy the WCS parameters into 2-dim arrays so that we can use the
	    # same code for both axes.

	    do w = 1, 2 {
		wp = GP_WCSPTR (gp, wcs[w])
		tran[1,w] = WCS_XTRAN(wp)
		tran[2,w] = WCS_YTRAN(wp)

		# If the window is degenerate enlarge the window until there
		# is enough range to make a plot.

		if (fp_nondegenr (WCS_WX1(wp), WCS_WX2(wp)))
		    GP_WCSSTATE(gp) = MODIFIED
		if (fp_nondegenr (WCS_WY1(wp), WCS_WY2(wp)))
		    GP_WCSSTATE(gp) = MODIFIED

		w1[1,w] = WCS_WX1(wp)
		w2[1,w] = WCS_WX2(wp)
		w1[2,w] = WCS_WY1(wp)
		w2[2,w] = WCS_WY2(wp)

		s1[1,w] = WCS_SX1(wp)
		s2[1,w] = WCS_SX2(wp)
		s1[2,w] = WCS_SY1(wp)
		s2[2,w] = WCS_SY2(wp)
	    }

	    # Compute the transformation parameters for both axes and both
	    # world coordinate systems.

	    do w = 1, 2				# w = wcs index
		do a = 1, 2 {			# a = axis
		    morigin[a,w] = s1[a,w]
		    ds = s2[a,w] - s1[a,w]

		    if (tran[a,w] == LINEAR) {
			worigin[a,w] = w1[a,w]
			scale[a,w] = ds / (w2[a,w] - w1[a,w])
		    } else if (tran[a,w] == LOG && w1[a,w] > 0 && w2[a,w] > 0) {
			worigin[a,w] = log10 (w1[a,w])
			scale[a,w] = ds / (log10(w2[a,w]) - worigin[a,w])
		    } else {
			worigin[a,w] = elogr (w1[a,w])
			scale[a,w] = ds / (elogr(w2[a,w]) - worigin[a,w])
		    }
		}
	}

	p1[1] = x1
	p1[2] = y1

	# Forward Transformation.  Transform P1 (point-A) in wcs_a to NDC
	# coordinates, if the input WCS is not number zero (the NDC coordinate
	# system).

	if (wcs_a != 0)
	    do a = 1, 2 {
		if (tran[a,1] != LINEAR)
		    if (tran[a,1] == LOG) {
			if (p1[a] <= 0)
			    p1[a] = INDEF
			else
			    p1[a] = log10 (p1[a])
		    } else
			p1[a] = elogr (p1[a])
		p1[a] = ((p1[a] - worigin[a,1]) * scale[a,1]) + morigin[a,1]
	    }

	# Inverse Transformation.  Transform point P1, now in NDC coordinates,
	# to WCS-B.  If WCS-B is zero (NDC), we need only copy the points.

	if (wcs_b == 0) {
	    p2[1] = p1[1]
	    p2[2] = p1[2]
	} else {
	    do a = 1, 2 {
		if (IS_INDEF (p1[a]))
		    p2[a] = INDEF
		else {
		    p2[a] = (p1[a] - morigin[a,2]) / scale[a,2] + worigin[a,2]
		    if (tran[a,2] != LINEAR)
			if (tran[a,2] == LOG)
			    p2[a] = 10.0 ** p2[a]
			else
			    p2[a] = aelogr (p2[a])
		}
	    }
	}

	x2 = p2[1]
	y2 = p2[2]
end
