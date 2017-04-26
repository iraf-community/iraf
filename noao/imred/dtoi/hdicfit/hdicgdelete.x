include	<gset.h>
include	<mach.h>
include	<pkg/gtools.h>
include	"hdicfit.h"

define	MSIZE		2.		# Mark size

# ICG_DELETE -- Delete data point nearest the cursor.
# The nearest point to the cursor in NDC coordinates is determined.

procedure icg_deleted (ic, gp, gt, cv, x, y, wts, npts, wx, wy)

pointer	ic					# ICFIT pointer
pointer	gp					# GIO pointer
pointer	gt					# GTOOLS pointer
pointer	cv					# CURFIT pointer
double	x[npts], y[npts]			# Data points
double	wts[npts]				# Weight array
int	npts					# Number of points
real	wx, wy					# Position to be nearest

int	gt_geti()
pointer	sp, xout, yout

begin
	call smark (sp)
	call salloc (xout, npts, TY_DOUBLE)
	call salloc (yout, npts, TY_DOUBLE)

	call icg_axesd (ic, gt, cv, 1, x, y, Memd[xout], npts)
	call icg_axesd (ic, gt, cv, 2, x, y, Memd[yout], npts)
	if (gt_geti (gt, GTTRANSPOSE) == NO)
	    call icg_d1d (ic, gp, Memd[xout], Memd[yout], wts, npts, wx, wy)
	else
	    call icg_d1d (ic, gp, Memd[yout], Memd[xout], wts, npts, wy, wx)

	call sfree (sp)
end


# ICG_D1D - Do the actual delete.

procedure icg_d1d (ic, gp, x, y, wts, npts, wx, wy)

pointer	ic					# ICFIT pointer
pointer	gp					# GIO pointer
double	x[npts], y[npts]			# Data points
double	wts[npts]				# Weight array
int	npts					# Number of points
real	wx, wy					# Position to be nearest

int	i, j
real	x0, y0, r2, r2min

begin
	# Transform world cursor coordinates to NDC.
	call gctran (gp, wx, wy, wx, wy, 1, 0)

	# Search for nearest point to a point with non-zero weight.
	r2min = MAX_REAL
	do i = 1, npts {
	    if (wts[i] == 0.)
		next

	    call gctran (gp, real (x[i]), real (y[i]), x0, y0, 1, 0)
		
	    r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    if (r2 < r2min) {
		r2min = r2
		j = i
	    }
	}

	# Mark the deleted point with a cross and set the weight to zero.
	if (j != 0) {
	    call gscur (gp, real (x[j]), real (y[j]))
	    call gmark (gp, real (x[j]), real (y[j]), GM_CROSS, MSIZE, MSIZE)
	    wts[j] = 0.
	    IC_NEWWTS(ic) = YES
	}
end
