# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include <mach.h>
include	"gtools.h"
	
# GT_SWIND -- Set graphics window.

procedure gt_swind (gp, gt)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer

real	xmin, xmax, dx, ymin, ymax, dy

begin
	if (gt != NULL) {
	    if (GT_TRANSPOSE(gt) == NO) {
	        call gseti (gp, G_XTRAN, GT_XTRAN(gt))
	        call gseti (gp, G_YTRAN, GT_YTRAN(gt))
	    } else {
	        call gseti (gp, G_YTRAN, GT_XTRAN(gt))
	        call gseti (gp, G_XTRAN, GT_YTRAN(gt))
	    }
	    call ggwind (gp, xmin, xmax, ymin, ymax)
	    dx = xmax - xmin
	    dy = ymax - ymin

	    if (IS_INDEF (GT_XMIN(gt)))
		xmin = xmin - GT_XBUF(gt) * dx
	    else
		xmin = GT_XMIN(gt)

	    if (IS_INDEF (GT_XMAX(gt)))
		xmax = xmax + GT_XBUF(gt) * dx
	    else
		xmax = GT_XMAX(gt)

	    if (IS_INDEF (GT_YMIN(gt)))
		ymin = ymin - GT_YBUF(gt) * dy
	    else
		ymin = GT_YMIN(gt)

	    if (IS_INDEF (GT_YMAX(gt)))
		ymax = ymax + GT_YBUF(gt) * dy
	    else
		ymax = GT_YMAX(gt)

	    if (GT_XFLIP(gt) == YES) {
		dx = xmin
		xmin = xmax
		xmax = dx
	    }
	    if (GT_YFLIP(gt) == YES) {
		dy = ymin
		ymin = ymax
		ymax = dy
	    }

	    if (GT_TRANSPOSE(gt) == NO)
		call gswind (gp, xmin, xmax, ymin, ymax)
	    else
	        call gswind (gp, ymin, ymax, xmin, xmax)
	}
end
