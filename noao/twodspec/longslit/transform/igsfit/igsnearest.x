include	<mach.h>
include	<gset.h>
include	<pkg/igsfit.h>

int procedure igs_nearest (gp, ztype, refpt, axis, pts, npts, wx, wy, wcs)

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
	        call gctran (gp, pts[i,x], pts[i,y], x0, y0, wcs, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }
	} else {
	    do i = 1, npts {
		if (pts[i,ztype] != pts[refpt,ztype])
		    next
	        call gctran (gp, pts[i,x], pts[i,y], x0, y0, wcs, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    j = i
	        }
	    }
	}

	call gscur (gp, real (pts[j,x]), real (pts[j,y]))
	return (j)
end
