include <gset.h>
include <math.h>
include "igi.h"

define	RESOLUTION	10

#  IG_ELLIPSE -- Draw an ellipse at the current position in the current
#  size, and angle.  The eccentricity is a command argument.

#  8/20/91 Removed ^Ls. ZGL

procedure ig_ellipse (igs)

pointer	igs		# igi parameters structure

real	eccen		# Ellipse eccentricity

real	get_real()

errchk	get_real

begin
	call lcmdcat (igs, YES)

	iferr (eccen = get_real (igs))
	    return

	call ii_ellipse (igs, eccen)

	call cmdcat  (igs, YES)
end


procedure ii_ellipse (igs, e)

pointer	igs		# igi parameters structure
real	e		# Ellipse eccentricity

real	a		# Semi-major axis (NDC)
real	pa		# Rotation angle (radians)
real	xc, yc		# Position of center (NDC)
int	n		# Number of polyline points

pointer	igps
pointer	gp
pointer	sp
pointer	x, y		# Polyline

begin
	igps = PLOT_PARMS(igs)

	gp = GIO_GP(igs)
	call gseti (gp, G_CLIP, NO)

	a  = MG_EXPAND(igps) * MG_PNTSIZE(igps)
	pa = DEGTORAD(MG_ANGLE(igps))
	xc = MG_XPOS(igps)
	yc = MG_YPOS(igps)

	# Find the number of points
	call ellres (gp, a, n)

	call smark  (sp)
	call salloc (x, n, TY_REAL)
	call salloc (y, n, TY_REAL)

	# Compute the raw ellipse
	call elline (a, e, Memr[x], Memr[y], n)

	# Adjust it
	call elladj (gp, pa, xc, yc, Memr[x], Memr[y], n)

	# Draw the ellipse
	call gpline (gp, Memr[x], Memr[y], n)

	call sfree  (sp)
	call gamove (gp, xc, yc)
	call gflush (gp)
end


procedure elline (a, e, x, y, n)

real	a			# Semi-major axis
real	e			# Eccentricity
real	x[ARB], y[ARB]		# Polyline
int	n

real 	b
real	dtheta
real	theta
int	i

begin
	# Semi-minor axis
	b = a * sqrt (1.0 - e*e)
	dtheta = TWOPI / real (n)

	do i = 1, n - 1 {
	    theta = (i - 1) * dtheta
	    x[i] = a * cos (theta)
	    y[i] = b * sin (theta)
	}

	x[n] = x[1]
	y[n] = y[1]
end


procedure elladj (gp, pa, xc, yc, x, y, n)

pointer	gp
real	pa
real	xc, yc
real	x[ARB], y[ARB]
int	n

real	ca, sa
real	xa, ya
real	xs, ys
int	i

begin
	if (pa != 0.0) {
	    # Rotate by position angle
	    ca = cos (pa) 
	    sa = sin (pa)
	    do i = 1, n {
		xa = x[i] * ca - y[i] * sa
		ya = x[i] * sa + y[i] * ca
		x[i] = xa
		y[i] = ya
	    }
	}

	# Adjust size for aspect ratio
	call ellscale (gp, xs, ys)

	# Adjust for device aspect ratio
	if (xs != 1.0)
	    call amulkr (x, xs, x, n)
	if (ys != 1.0)
	    call amulkr (y, ys, y, n)

	# Translate to current position
	call aaddkr (x, xc, x, n)
	call aaddkr (y, yc, y, n)
end


procedure ellscale (gp, xscale, yscale)

pointer	gp
real	xscale, yscale

real	ar
real	xs, ys

real	ggetr()

begin
	# Device aspect ratio
	xs = ggetr (gp, "xs")
	ys = ggetr (gp, "ys")
	ar = ys / xs

	if (ar > 0.0 && ar < 1.0) {
	    # Landscape
	    xscale = ar
	    yscale = 1.0
	} else if (ar > 1.0) {
	    # Portrait
	    xscale = 1.0
	    yscale = 1.0 / ar
	}
end


procedure ellres (gp, a, n)

pointer	gp
real	a
int	n

real	dtheta
int	xr, yr
real	res

int	ggeti()

begin
	xr = ggeti (gp, "xr")
	yr = ggeti (gp, "yr")

	res = RESOLUTION / real (min (xr, yr))
	dtheta = atan2 (res, a)
	n = int (TWOPI / dtheta) + 1
end
