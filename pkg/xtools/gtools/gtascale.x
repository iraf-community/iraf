# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include	"gtools.h"
	
# GT_ASCALE -- Set graphics window to the range of the data.
# Unlike GASCALE the data is limited to the GTOOLS window.

procedure gt_ascale (gp, gt, x, y, npts)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
real	x[npts], y[npts]	# Data to scale
int	npts			# Number of data points

int	i
real	xmin, xmax, ymin, ymax, x1, x2, y1, y2, temp

begin
	if (gt == NULL) {
	    call gascale (gp, x, npts, 1)
	    call gascale (gp, y, npts, 2)
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
	do i = 1, npts {
	    if ((x[i]<xmin)||(x[i]>xmax)||(y[i]<ymin)||(y[i]>ymax))
		next
	    x1 = min (x1, x[i])
	    x2 = max (x2, x[i])
	    y1 = min (y1, y[i])
	    y2 = max (y2, y[i])
	}

	if (x1 <= x2)
	    call gswind (gp, x1, x2, INDEF, INDEF)
	if (y1 <= y2)
	    call gswind (gp, INDEF, INDEF, y1, y2)
end
