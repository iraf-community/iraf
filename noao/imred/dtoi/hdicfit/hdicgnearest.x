include	<mach.h>
include	<pkg/gtools.h>

# ICG_NEAREST -- Find the nearest point to the cursor and return the index.
# The nearest point to the cursor in NDC coordinates is determined.
# The cursor is moved to the nearest point selected.

int procedure icg_nearestd (ic, gp, gt, cv, x, y, npts, wx, wy)

pointer	ic					# ICFIT pointer
pointer	gp					# GIO pointer
pointer	gt					# GTOOLS pointer
pointer	cv					# CURFIT pointer
double	x[npts], y[npts]			# Data points
int	npts					# Number of points
real	wx, wy					# Cursor position

int	pt
pointer	sp, xout, yout
int	icg_nd(), gt_geti()

begin
	call smark (sp)
	call salloc (xout, npts, TY_DOUBLE)
	call salloc (yout, npts, TY_DOUBLE)

	call icg_axesd (ic, gt, cv, 1, x, y, Memd[xout], npts)
	call icg_axesd (ic, gt, cv, 2, x, y, Memd[yout], npts)

	if (gt_geti (gt, GTTRANSPOSE) == NO)
	    pt = icg_nd (gp, Memd[xout], Memd[yout], npts, wx, wy)
	else
	    pt = icg_nd (gp, Memd[yout], Memd[xout], npts, wy, wx)
	call sfree (sp)
	
	return (pt)
end


# ICG_ND -- ??

int procedure icg_nd (gp, x, y, npts, wx, wy)

pointer	gp					# GIO pointer
double	x[npts], y[npts]			# Data points
int	npts					# Number of points
real	wx, wy					# Cursor position

int	i, j
real	x0, y0, r2, r2min

begin
	# Transform world cursor coordinates to NDC.
	call gctran (gp, wx, wy, wx, wy, 1, 0)

	# Search for nearest point.
	r2min = MAX_REAL
	do i = 1, npts {
	    call gctran (gp, real (x[i]), real (y[i]), x0, y0, 1, 0)
	    r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    if (r2 < r2min) {
		r2min = r2
		j = i
	    }
	}

	# Move the cursor to the selected point and return the index.
	if (j != 0)
	    call gscur (gp, real (x[j]), real (y[j]))

	return (j)
end
