include	<mach.h>
include	<pkg/gtools.h>


# ING_NEAREST -- Find the nearest point to the cursor and return the index.
# The cursor is moved to the nearest point selected.

int procedure ing_nearestd (in, gp, gt, nl, x, y, npts, nvars, wx, wy)

pointer	in				# INLFIT pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
pointer	nl				# NLFIT pointer
double	x[ARB]				# Independent variables (npts * nvars)
double	y[npts]				# Dependent variables
int	npts				# Number of points
int	nvars				# Number of variables
real	wx, wy				# Cursor position

int	pt
pointer	sp, xout, yout

int	ing_nd(), gt_geti()

begin
	# Allocate memory for axes data
	call smark (sp)
	call salloc (xout, npts, TY_DOUBLE)
	call salloc (yout, npts, TY_DOUBLE)

	# Set axes data
	call ing_axesd (in, gt, nl, 1, x, y, Memd[xout], npts, nvars)
	call ing_axesd (in, gt, nl, 2, x, y, Memd[yout], npts, nvars)

	# Check for transposed axes
	if (gt_geti (gt, GTTRANSPOSE) == NO)
	    pt = ing_nd (gp, Memd[xout], Memd[yout], npts, wx, wy)
	else
	    pt = ing_nd (gp, Memd[yout], Memd[xout], npts, wy, wx)
	call sfree (sp)
	
	# Return index
	return (pt)
end


# ING_N -- Find position and move the cursor.

int procedure ing_nd (gp, x, y, npts, wx, wy)

pointer	gp					# GIO pointer
double	x[npts], y[npts]			# Data points
int	npts					# Number of points
real	wx, wy					# Cursor position

int	i, j
real	xc, yc, x0, y0, r2, r2min

begin
	# Transform world cursor coordinates to NDC.
	call gctran (gp, wx, wy, xc, yc, 1, 0)

	# Search for nearest point.
	r2min = MAX_REAL
	do i = 1, npts {
	    call gctran (gp, real (x[i]), real (y[i]), x0, y0, 1, 0)
	    r2 = (x0 - xc) ** 2 + (y0 - yc) ** 2
	    if (r2 < r2min) {
		r2min = r2
		j = i
	    }
	}

	# Move the cursor to the selected point and return the index.
	if (j != 0) {
	    call gscur (gp, real (x[j]), real (y[j]))
	    wx = x[j]
	    wy = y[j]
	}
	return (j)
end
