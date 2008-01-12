# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gtools.h"

# GT_VPLOT -- Plot vector polymarks or polylines.

procedure gt_vplot (gp, gt, v, npts, x1, x2)

pointer	gp		# GIO pointer
pointer	gt		# GTOOLS pointer
real	v[npts]		# Abscissas
int	npts		# Number of points
real	x1, x2		# Vector range

int	i, pltype, color, gstati()
real	x, dx

begin
	switch (GT_TYPE(gt)) {
	case 1:
	    color = gstati (gp, G_PMCOLOR)
	    call gseti (gp, G_PMCOLOR, GT_COLOR(gt))
	    call gvmark (gp, v, npts, x1, x2, GT_MARK(gt), GT_XSIZE(gt),
		GT_YSIZE(gt))
	    call gseti (gp, G_PMCOLOR, color)
	case 2:
	    color = gstati (gp, G_PLCOLOR)
	    call gseti (gp, G_PLCOLOR, GT_COLOR(gt))
	    pltype = gstati (gp, G_PLTYPE)
	    call gseti (gp, G_PLTYPE, GT_LINE(gt))
	    call gvline (gp, v, npts, x1, x2)
	    call gseti (gp, G_PLTYPE, pltype)
	    call gseti (gp, G_PLCOLOR, color)
	case 3:
	    color = gstati (gp, G_PLCOLOR)
	    call gseti (gp, G_PLCOLOR, GT_COLOR(gt))
	    pltype = gstati (gp, G_PLTYPE)
	    call gseti (gp, G_PLTYPE, GT_LINE(gt))
	    dx = (x2 - x1) / (npts - 1)
	    x = x1 - dx / 2
	    do i = 1, npts-1 {
		x = x + dx
		call gline (gp, x-dx, v[i], x, v[i])
		call gline (gp, x, v[i], x, v[i+1])
	    }
	    call gline (gp, x, v[npts], x+dx, v[npts])
	    call gseti (gp, G_PLTYPE, pltype)
	    call gseti (gp, G_PLCOLOR, color)
	}
end
