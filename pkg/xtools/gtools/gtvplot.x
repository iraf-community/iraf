# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<pkg/gtools.h>

# GT_VPLOT -- Plot vector polymarks or polylines.

procedure gt_vplot (gp, gt, v, npts, x1, x2)

pointer	gp		# GIO pointer
pointer	gt		# GTOOLS pointer
real	v[npts]		# Abscissas
int	npts		# Number of points
real	x1, x2		# Vector range

int	pltype, gstati()

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
	}
end
