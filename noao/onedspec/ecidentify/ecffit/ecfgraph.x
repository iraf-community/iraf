include	<gset.h>
include	<mach.h>
include	<pkg/gtools.h>

# ECF_GRAPH -- Graph the fitted data.

procedure ecf_graph (gp, gt, x, y, w, rej, npts)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
real	x[npts]			# X data
real	y[npts]			# Y data
double	w[npts]			# Weights
double	rej[npts]		# Rejected points
int	npts			# Number of pts points

int	i
real	xsize, ysize, ymin, ymax, gt_getr()

begin
	xsize = gt_getr (gt, GTXSIZE)
	ysize = gt_getr (gt, GTYSIZE)

	call gclear (gp)

	ymin = MAX_REAL
	ymax = -MAX_REAL
	do i = 1, npts
	    if (w[i] > 0.) {
		ymin = min (ymin, y[i])
		ymax = max (ymax, y[i])
	    }

	call gascale (gp, x, npts, 1)
	call gswind (gp, INDEF, INDEF, ymin, ymax)
	call gt_swind (gp, gt)
	call gt_labax (gp, gt)

	do i = 1, npts {
	    if (rej[i] == 0.) {
		if (y[i] >= ymin && y[i] <= ymax) {
		    if (w[i] == 0.)
		        call gmark (gp, x[i], y[i], GM_CROSS, xsize, ysize)
		    else
		        call gmark (gp, x[i], y[i], GM_DIAMOND, xsize, ysize)
		}
	    } else
		call gmark (gp, x[i], y[i], GM_PLUS, xsize, ysize)
	}
end
