include	<gset.h>
include	<mach.h>
include	<pkg/gtools.h>

define	MSIZE		2.0		# Mark size (real)


# ING_UNDELETE -- Undelete data point nearest the cursor. The nearest point to
# the cursor in NDC coordinates is determined.

procedure ing_undeleter (in, gp, gt, nl, x, y, wts, userwts, npts, nvars,
	wx, wy)

pointer	in				# INLFIT pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
pointer	nl				# NLFIT pointer
real	x[ARB]				# Independent variables (npts * nvars)
real	y[npts]				# Dependent variables
real	wts[npts], userwts[npts]	# Weight arrays
int	npts				# Number of points
int	nvars				# Number of variables
real	wx, wy				# Position to be nearest

pointer	sp, xout, yout
int	gt_geti()

begin
	# Allocate memory for the axes data.
	call smark (sp)
	call salloc (xout, npts, TY_REAL)
	call salloc (yout, npts, TY_REAL)

	# Get the axes data.
	call ing_axesr (in, gt, nl, 1, x, y, Memr[xout], npts, nvars)
	call ing_axesr (in, gt, nl, 2, x, y, Memr[yout], npts, nvars)

	# Transpose axes if necessary.
	if (gt_geti (gt, GTTRANSPOSE) == NO)
	    call ing_u1r (in, gp, Memr[xout], Memr[yout], wts, userwts,
		npts, wx, wy)
	else
	    call ing_u1r (in, gp, Memr[yout], Memr[xout], wts, userwts,
		npts, wy, wx)

	# Free memory.
	call sfree (sp)
end


# ING_U1 -- Do the actual undelete.

procedure ing_u1r (in, gp, x, y, wts, userwts, npts, wx, wy)

pointer	in					# ICFIT pointer
pointer	gp					# GIO pointer
real	x[npts], y[npts]			# Data points
real	wts[npts], userwts[npts]		# Weight arrays
int	npts					# Number of points
real	wx, wy					# Position to be nearest

int	i, j
real	x0, y0, r2, r2min

begin
	# Transform world cursor coordinates to NDC.
	call gctran (gp, wx, wy, wx, wy, 1, 0)

	# Search for nearest point to a point with zero weight.
	r2min = MAX_REAL
	do i = 1, npts {
	    if (wts[i] != real (0.0))
		next
	    call gctran (gp, real (x[i]), real (y[i]), x0, y0, 1, 0)
	    r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    if (r2 < r2min) {
		r2min = r2
		j = i
	    }
	}

	# Unmark the deleted point and reset the weight.
	if (j != 0) {
	    call gscur (gp, real (x[j]), real (y[j]))
	    call gseti (gp, G_PMLTYPE, GL_CLEAR)
	    call gmark (gp, real (x[j]), real (y[j]), GM_CROSS, MSIZE, MSIZE)
	    call gseti (gp, G_PMLTYPE, GL_SOLID)
	    #call gline (gp, real (x[j]), real (y[j]), real (x[j]), real (y[j]))
	    call gmark (gp, real (x[j]), real (y[j]), GM_PLUS, MSIZE, MSIZE)
	    wts[j] = userwts[j]
	}
end
