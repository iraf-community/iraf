# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gtools.h"

# GT_GRAPH -- Plot polymarks or polypoints.

procedure gt_plot (gp, gt, x, y, npts)

pointer	gp		# GIO pointer
pointer	gt		# GTOOLS pointer
real	x[npts]		# Abscissas
real	y[npts]		# Ordinates
int	npts		# Number of points

int	i, color, pltype, gstati()
real	x1, x2

begin
	switch (GT_TYPE(gt)) {
	case 1:
	    #color = gstati (gp, G_PMCOLOR)
	    #call gseti (gp, G_PMCOLOR, GT_COLOR(gt))
	    color = gstati (gp, G_PLCOLOR)
	    call gseti (gp, G_PLCOLOR, GT_COLOR(gt))
	    if (GT_TRANSPOSE(gt) == NO)
	        call gpmark (gp, x, y, npts, GT_MARK(gt), GT_XSIZE(gt),
		    GT_YSIZE(gt))
	    else
	        call gpmark (gp, y, x, npts, GT_MARK(gt), GT_YSIZE(gt),
		    GT_XSIZE(gt))
	    #call gseti (gp, G_PMCOLOR, color)
	    call gseti (gp, G_PLCOLOR, color)
	case 2:
	    color = gstati (gp, G_PLCOLOR)
	    call gseti (gp, G_PLCOLOR, GT_COLOR(gt))
	    pltype = gstati (gp, G_PLTYPE)
	    call gseti (gp, G_PLTYPE, GT_LINE(gt))
	    if (GT_TRANSPOSE(gt) == NO)
	        call gpline (gp, x, y, npts)
	    else
	        call gpline (gp, y, x, npts)
	    call gseti (gp, G_PLTYPE, pltype)
	    call gseti (gp, G_PLCOLOR, color)
	case 3:
	    color = gstati (gp, G_PLCOLOR)
	    call gseti (gp, G_PLCOLOR, GT_COLOR(gt))
	    pltype = gstati (gp, G_PLTYPE)
	    call gseti (gp, G_PLTYPE, GT_LINE(gt))
	    if (GT_TRANSPOSE(gt) == NO) {
		x1 = x[1]
		x2 = (x[1] + x[2]) / 2
		call gline (gp, x1, y[1], x2, y[1])
		do i = 2, npts - 1 {
		    x1 = x2
		    x2 = (x[i] + x[i+1]) / 2
		    call gline (gp, x1, y[i-1], x1, y[i]) 
		    call gline (gp, x1, y[i], x2 , y[i])
		}
		x1 = x2
		x2 = x[npts]
		call gline (gp, x1, y[i-1], x1, y[i]) 
		call gline (gp, x1, y[i], x2 , y[i])
	    } else {
		x1 = y[1]
		x2 = (y[1] + y[2]) / 2
		call gline (gp, x1, x[1], x2, x[1])
		do i = 2, npts - 1 {
		    x1 = x2
		    x2 = (y[i] + y[i+1]) / 2
		    call gline (gp, x1, x[i-1], x1, x[i]) 
		    call gline (gp, x1, x[i], x2 , x[i])
		}
		x1 = x2
		x2 = y[npts]
		call gline (gp, x1, y[i-1], x1, y[i]) 
		call gline (gp, x1, y[i], x2 , y[i])
	    }
	    call gseti (gp, G_PLTYPE, pltype)
	    call gseti (gp, G_PLCOLOR, color)
	}
end
