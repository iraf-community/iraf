# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<pkg/gtools.h>

# GT_GRAPH -- Plot polymarks or polypoints.

procedure gt_plot (gp, gt, x, y, npts)

pointer	gp		# GIO pointer
pointer	gt		# GTOOLS pointer
real	x[npts]		# Abscissas
real	y[npts]		# Ordinates
int	npts		# Number of points

int	pltype, gstati()

begin
	switch (GT_TYPE(gt)) {
	case 1:
	    if (GT_TRANSPOSE(gt) == NO)
	        call gpmark (gp, x, y, npts, GT_MARK(gt), GT_XSIZE(gt),
		    GT_YSIZE(gt))
	    else
	        call gpmark (gp, y, x, npts, GT_MARK(gt), GT_YSIZE(gt),
		    GT_XSIZE(gt))
	case 2:
	    pltype = gstati (gp, G_PLTYPE)
	    call gseti (gp, G_PLTYPE, GT_LINE(gt))
	    if (GT_TRANSPOSE(gt) == NO)
	        call gpline (gp, x, y, npts)
	    else
	        call gpline (gp, y, x, npts)
	    call gseti (gp, G_PLTYPE, pltype)
	}
end
