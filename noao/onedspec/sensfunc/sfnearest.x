include	<gset.h>
include	<mach.h>
include "sensfunc.h"

# SF_NEAREST -- Find the nearest point to the cursor.  Return the standard
# star index and the wavelength point index.  The metric is in NDC.
# The cursor is moved to the nearest point selected.  Return zero for
# the standard star index if valid point not found.

procedure sf_nearest (gp, stds, nstds, wx, wy, wcs, type, istd, ipt)

pointer	gp			# Graphics pointer
pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
real	wx, wy			# Cursor position
int	wcs			# WCS
int	type			# Type of points (0=not del, 1=del, 2=both)
int	istd			# Index of standard star (returned)
int	ipt			# Index of point (returned)

int	i, j, n, stridx()
real	x0, y0, x1, y1, x2, y2, r2, r2min
pointer	x, y, w, gio

begin
	# Check for valid wc.
	istd = 0
	if (stridx (GP_GRAPHS(gp,wcs), "ars") == 0)
	    return

	# Transform world cursor coordinates to NDC.
	gio = GP_GIO(gp)
	call gctran (gio, wx, wy, wx, wy, wcs, 0)

	# Search for nearest point.
	r2min = MAX_REAL
	do i = 1, nstds {
	    if (STD_FLAG(stds[i]) != SF_INCLUDE)
		next
	    n = STD_NWAVES(stds[i])
	    x = STD_X(stds[i]) - 1
	    y = STD_Y(stds[i]) - 1
	    w = STD_WTS(stds[i]) - 1
	    do j = 1, n {
		if (type == 0) {
		    if (Memr[w+j] == 0.)
			next
		} else if (type == 1) {
		    if (Memr[w+j] != 0.)
			next
		}
		x1 = Memr[x+j]
		y1 = Memr[y+j]
	        call gctran (gio, x1, y1, x0, y0, wcs, 0)
	        r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	        if (r2 < r2min) {
		    r2min = r2
		    istd = i
		    ipt = j
		    x2 = x1
		    y2 = y1
	        }
	    }
	}

	# Move the cursor to the selected point.
	call gseti (gio, G_WCS, wcs)
	call gscur (gio, x2, y2)
end
