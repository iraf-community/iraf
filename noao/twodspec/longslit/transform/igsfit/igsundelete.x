include	<mach.h>
include	<gset.h>
include	<pkg/gtools.h>
include	<pkg/igsfit.h>

int procedure igs_nearestu (gp, ztype, refpt, axis, pts, npts, wx, wy, wcs)

pointer	gp			# GIO pointer
int	ztype			# Zoom type
int	refpt			# Reference point
int	axis[2]			# Axes
real	pts[npts, ARB]		# Data points
int	npts			# Number of data points
real	wx, wy			# Cursor coordinates
int	wcs			# WCS

int	i, j, x, y
real	r2, r2min, x0, y0

begin
	x = axis[1]
	y = axis[2]

	call gctran (gp, wx, wy, wx, wy, wcs, 0)
	r2min = MAX_REAL
	j = 0

	if (IS_INDEFI (ztype)) {
	    do i = 1, npts {
	        if (pts[i,W] != 0.)
		    next
	        call gctran (gp, pts[i, x], pts[i, y], x0, y0, wcs, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }
	} else {
	    do i = 1, npts {
	        if ((pts[i,ztype] != pts[refpt,ztype]) || (pts[i,W] != 0.))
		    next
	        call gctran (gp, pts[i, x], pts[i, y], x0, y0, wcs, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }
	}

	return (j)
end


# IGS_UNDELETE - Undelete point or subset.

procedure igs_undelete (gp, gt, ztype, refpt, axis, pts, wts, npts, dtype)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
int	ztype			# Zoom type
int	refpt			# Reference point for undeletion
int	axis[2]			# Axes
real	pts[npts, ARB]		# Data points
real	wts[npts]		# Original weights
int	npts			# Number of data points
int	dtype			# Undeletion type

int	i, x, y
real	xsize, ysize

real	gt_getr()

begin
	x = axis[1]
	y = axis[2]

	xsize = gt_getr (gt, GTXSIZE)
	ysize = gt_getr (gt, GTYSIZE)

	switch (dtype) {
	case X, Y, Z:
	    do i = 1, npts {
		if (!IS_INDEFI (ztype))
		    if (pts[refpt,ztype] != pts[i,ztype])
		        next
		if (pts[refpt,dtype] != pts[i,dtype])
		    next
	        call gseti (gp, G_PMLTYPE, 0)
	        call gmark (gp, pts[i,x], pts[i,y], GM_CROSS, xsize, ysize)
	        call gseti (gp, G_PMLTYPE, 1)
	        call gmark (gp, pts[i,x], pts[i,y], GM_PLUS, xsize, ysize)
	        if (wts[i] == 0)
		    wts[i] = 1
	        pts[i,W] = wts[i]
	    }
	default:
	    call gseti (gp, G_PMLTYPE, 0)
	    call gmark (gp, pts[refpt,x], pts[refpt,y], GM_CROSS, xsize, ysize)
	    call gseti (gp, G_PMLTYPE, 1)
	    call gmark (gp, pts[refpt,x], pts[refpt,y], GM_PLUS, xsize, ysize)
	    if (wts[refpt] == 0)
		wts[refpt] = 1
	    pts[refpt,W] = wts[refpt]
	}
end
