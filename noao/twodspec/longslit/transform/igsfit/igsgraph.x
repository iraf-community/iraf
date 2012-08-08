include	<mach.h>
include	<gset.h>
include	<pkg/gtools.h>
include	<pkg/igsfit.h>

procedure igs_graph (gp, gt, ztype, refpt, axis, pts, npts, labels)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
int	ztype			# Zoom type
int	refpt			# Reference point
int	axis[2]			# Axis definitions
real	pts[npts, ARB]		# Data
int	npts			# Number of pts points
char	labels[SZ_LINE, ARB]	# Data labels

int	i, x, y
real	xmin, xmax, ymin, ymax, xsize, ysize, gt_getr()

begin
	if (gp == NULL)
	    return

	x = axis[1]
	y = axis[2]

	call gt_sets (gt, GTXLABEL, labels[1, x])
	call gt_sets (gt, GTYLABEL, labels[1, y])
	xsize = gt_getr (gt, GTXSIZE)
	ysize = gt_getr (gt, GTYSIZE)

	call gclear (gp)

	if (IS_INDEFI (ztype)) {
	    call gascale (gp, pts[1, x], npts, 1)
	    call gascale (gp, pts[1, y], npts, 2)
	} else {
	    xmin = MAX_REAL
	    xmax = -MAX_REAL
	    ymin = MAX_REAL
	    ymax = -MAX_REAL
	    do i = 1, npts {
		if (pts[i,ztype] != pts[refpt,ztype])
		    next
		xmin = min (xmin, pts[i,x])
		xmax = max (xmax, pts[i,x])
		ymin = min (ymin, pts[i,y])
		ymax = max (ymax, pts[i,y])
	    }
	    call gswind (gp, xmin, xmax, ymin, ymax)
	}

	call gt_swind (gp, gt)
	call gt_labax (gp, gt)

	if (IS_INDEFI (ztype)) {
	    do i = 1, npts {
	        if (pts[i,W] == 0.)
		    call gmark (gp, pts[i,x], pts[i,y], GM_CROSS, xsize, ysize)
	        else
		    call gmark (gp, pts[i,x], pts[i,y], GM_PLUS, xsize, ysize)
	    }
	} else {
	    do i = 1, npts {
		if (pts[i,ztype] != pts[refpt,ztype])
		    next
	        if (pts[i,W] == 0.)
		    call gmark (gp, pts[i,x], pts[i,y], GM_CROSS, xsize, ysize)
	        else
		    call gmark (gp, pts[i,x], pts[i,y], GM_PLUS, xsize, ysize)
	    }
	}
end
