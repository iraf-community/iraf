# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include	"gtools.h"
	
# GT_ASCALE -- Set graphics window to the range of the data.
# Unlike GASCALE the data is limited to the GTOOLS window.
# It also clips a fraction of the high and low points.

procedure gt_ascale (gp, gt, x, y, npts)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
real	x[npts], y[npts]	# Data to scale
int	npts			# Number of data points

int	i, j, k, n
real	xmin, xmax, ymin, ymax, x1, x2, y1, y2, temp
pointer	buf

begin
	if (gt == NULL) {
	    call gascale (gp, x, npts, 1)
	    call gascale (gp, y, npts, 1)
	    return
	}

	if (GT_TRANSPOSE(gt) == NO) {
	    xmin = GT_XMIN(gt)
	    xmax = GT_XMAX(gt)
	    ymin = GT_YMIN(gt)
	    ymax = GT_YMAX(gt)
	} else {
	    ymin = GT_XMIN(gt)
	    ymax = GT_XMAX(gt)
	    xmin = GT_YMIN(gt)
	    xmax = GT_YMAX(gt)
	}

	if (IS_INDEF(xmin))
	    xmin = -MAX_REAL
	if (IS_INDEF(xmax))
	    xmax = MAX_REAL
	if (IS_INDEF(ymin))
	    ymin = -MAX_REAL
	if (IS_INDEF(ymax))
	    ymax = MAX_REAL

	temp = max (xmin, xmax)
	xmin = min (xmin, xmax)
	xmax = temp
	temp = max (ymin, ymax)
	ymin = min (ymin, ymax)
	ymax = temp

	x1 = xmax
	x2 = xmin
	y1 = ymax
	y2 = ymin
	n = 0
	do i = 1, npts {
	    if ((x[i]<xmin)||(x[i]>xmax)||(y[i]<ymin)||(y[i]>ymax))
		next
	    x1 = min (x1, x[i])
	    x2 = max (x2, x[i])
	    y1 = min (y1, y[i])
	    y2 = max (y2, y[i])
	    n = n + 1
	}
	if ((GT_LCLIP(gt) > 0. || GT_HCLIP(gt) > 0.) && n > 0) {
	    call malloc (buf, n, TY_REAL)
	    n = 0
	    do i = 1, npts {
		if ((x[i]<xmin)||(x[i]>xmax)||(y[i]<ymin)||(y[i]>ymax))
		    next
		Memr[buf+n] = y[i]
		n = n + 1
	    }
	    call asrtr (Memr[buf], Memr[buf], n)
	    if (GT_LCLIP(gt) > 1.)
		j = GT_LCLIP(gt) / 100. * n
	    else
		j = max (0., GT_LCLIP(gt) * n)
	    if (GT_HCLIP(gt) > 1.)
		k = GT_HCLIP(gt) / 100. * n
	    else
		k = max (0., GT_HCLIP(gt) * n)
	    k = n - 1 - k
	    if (j > k) {
		y1 = Memr[buf+j]
		y2 = Memr[buf+k]
	    }
	    call mfree (buf, TY_REAL)
	}

	if (x1 <= x2)
	    call gswind (gp, x1, x2, INDEF, INDEF)
	if (y1 <= y2)
	    call gswind (gp, INDEF, INDEF, y1, y2)
end
