include	<mach.h>
include	<gset.h>
include	<pkg/gtools.h>

# ECF_NEAREST -- Find nearest point to the cursor.

int procedure ecf_nearest (gp, gt, wx, wy, wcs, key, x, y, w, npts)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
real	wx, wy			# Cursor coordinates
int	wcs			# WCS
int	key			# Nearest key
real	x[npts]			# Data points
real	y[npts]			# Data points
double	w[npts]			# Weight
int	npts			# Number of data points

int	i, j
real	r2, r2min, x0, y0, xsize, ysize, gt_getr()

begin
	call gctran (gp, wx, wy, wx, wy, wcs, 0)
	r2min = MAX_REAL
	j = 0

	switch (key) {
	case 'c':
	    do i = 1, npts {
	        call gctran (gp, x[i], y[i], x0, y0, wcs, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }
	    call gscur (gp, x[j], y[j])
	case 'd':
	    do i = 1, npts {
		if (w[i] == 0.)
		    next
	        call gctran (gp, x[i], y[i], x0, y0, wcs, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }
	    if (j > 0) {
		xsize = gt_getr (gt, GTXSIZE)
		ysize = gt_getr (gt, GTYSIZE)

		call gseti (gp, G_PMLTYPE, 0)
		call gmark (gp, x[j], y[j], GM_PLUS, xsize, ysize)
		call gseti (gp, G_PMLTYPE, 1)
		call gmark (gp, x[j], y[j], GM_CROSS, xsize, ysize)
		w[j] = 0.
	        call gscur (gp, x[j], y[j])
	    }
	case 'u':
	    do i = 1, npts {
		if (w[i] != 0.)
		    next
	        call gctran (gp, x[i], y[i], x0, y0, wcs, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }
	    if (j > 0) {
		xsize = gt_getr (gt, GTXSIZE)
		ysize = gt_getr (gt, GTYSIZE)

		call gseti (gp, G_PMLTYPE, 0)
		call gmark (gp, x[j], y[j], GM_CROSS, xsize, ysize)
		call gseti (gp, G_PMLTYPE, 1)
		call gmark (gp, x[j], y[j], GM_PLUS, xsize, ysize)
		w[j] = 1.
	        call gscur (gp, x[j], y[j])
	    }
	}

	return (j)
end
