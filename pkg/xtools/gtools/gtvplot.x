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

int	i, pltype, gstati()
real	x, dx

begin
	switch (GT_TYPE(gt)) {
	case 1:
	    call gvmark (gp, v, npts, x1, x2, GT_MARK(gt), GT_XSIZE(gt),
		GT_YSIZE(gt))
	case 2:
	    pltype = gstati (gp, G_PLTYPE)
	    call gseti (gp, G_PLTYPE, GT_LINE(gt))
	    call gvline (gp, v, npts, x1, x2)
	    call gseti (gp, G_PLTYPE, pltype)
	case 3:
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
	}
end
