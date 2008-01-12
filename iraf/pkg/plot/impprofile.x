include	<gset.h>
include	<math/iminterp.h>

define	INTERP_TYPE	II_SPLINE3	# Interpolation type
define	STEP		0.01		# Approximate step size
define	NITERATE	10		# Number of iteration to find endpoints
define	DX		0.001		# Accuracy


# IMP_PROFILE -- IMPLOT profile analysis.

procedure imp_profile (gp, x, y, n, x1, y1, x2, y2, sl, sline)

pointer	gp		#I gio pointer
real	x[n]		#I x coordinates
real	y[n]		#I y coordinates
int	n		#I number of points
real	x1, y1		#I first endpoint
real	x2, y2		#I second endpoint
pointer	sl		#U status line pointer
int	sline		#U line to print

int	i
real	p, p1, p2, pc, pl, pr, step
real	a, b, c, y0, dy, xc, xl, xr, der[2]
double	sumb, sum0, sum1, sum2, sum3
pointer	xasi, yasi, sl_getstr
real	asieval()
bool	fp_equalr()

begin
	# Fit an interpolator to the input arrays.
	call asiinit (xasi, INTERP_TYPE)
	call asiinit (yasi, INTERP_TYPE)
	call asifit (xasi, x, n)
	call asifit (yasi, y, n)

	# Find the pixel endpoints given the x endpoints to an accuracy of DX.
	p1 = 1. + n / 2.
	b = 1.
	do i = 1, NITERATE {
	    call asider (xasi, p1, der, 2)
	    if (!fp_equalr (der[2], 0.))
		b = der[2]
	    a = x1 - der[1]
	    p1 = max (1., min (real(n), p1 + a / der[2]))
	    if (abs (a) < DX)
		break
	}
	p2 = p1
	do i = 1, NITERATE {
	    call asider (xasi, p2, der, 2)
	    if (!fp_equalr (der[2], 0.))
		b = der[2]
	    a = x2 - der[1]
	    p2 = max (1., min (real(n), p2 + a / der[2]))
	    if (abs (a) < DX)
		break
	}

	# Set the linear baseline.
	if (fp_equalr (p1, p2)) {
	    y0 = (y1 + y2) / 2.
	    dy = 1.
	    step = STEP
	} else if (p1 < p2) {
	    y0 = y1
	    dy = (y2 - y0) / (p2 - p1)
	    step = (p2 - p1) / (nint(p2) - nint(p1) + 1) * STEP
	} else {
	    pc = p1
	    p1 = p2
	    p2 = pc
	    y0 = y2
	    dy = (y1 - y0) / (p2 - p1)
	    step = (p2 - p1) / (nint(p2) - nint(p1) + 1) * STEP
	}

	# Compute the first 2 moments using trapezoidal integration.
	p = p1
	a = asieval (xasi, p)
	b = y0 + (p - p1) * dy
	c = asieval (yasi, p) - b
	sumb = b / 2
	sum0 = c / 2
	sum1 = a * c / 2
	for (p=p+step; p<=p2; p=p+step) {
	    a = asieval (xasi, p)
	    b = y0 + (p - p1) * dy
	    c = asieval (yasi, p) - b
	    sumb = sumb + b
	    sum0 = sum0 + c
	    sum1 = sum1 + a * c
	}
	sumb = (sumb - b / 2) * step
	sum0 = (sum0 - c / 2) * step
	sum1 = (sum1 - a * c / 2) * step

	# Compute the higher central moments using trapezoidal integration.
	if (sum0 == 0D0) {
	    sum1 = INDEFD
	    sum2 = INDEFD
	    sum3 = INDEFD
	} else {
	    sum1 = sum1 / sum0
	    p = p1
	    a = asieval (xasi, p) - sum1
	    b = y0 + (p - p1) * dy
	    c = asieval (yasi, p) - b
	    sum2 = a * a * c / 2
	    sum3 = a * a * a * c / 2
	    for (p=p+step; p<=p2; p=p+step) {
		a = asieval (xasi, p) - sum1
		b = y0 + (p - p1) * dy
		c = asieval (yasi, p) - b
		sum2 = sum2 + a * a * c
		sum3 = sum3 + a * a * a * c
	    }
	    sum2 = (sum2 - a * a * c / 2) * step
	    sum3 = (sum3 - a * a * a * c / 2) * step
	    sum2 = sqrt (sum2 / sum0)
	    if (sum2 > 0.)
		sum3 = (sum3 / sum0) / (sum2 ** 3)
	}

	# Find the maximum value away from the baseline.
	pc= p1
	c = 0.
	for (p=p1; p<=p2; p=p+step) {
	    a = abs (asieval (yasi, p) - y0 - (p - p1) * dy)
	    if (a > c) {
		pc = p
		c = a
	    }
	}
	xc = asieval (xasi, pc)

	# Find the half width points.
	c = c / 2
	pl = INDEF
	xl = INDEF
	for (p=pc; p>=p1; p=p-step) {
	    a = abs (asieval (yasi, p) - y0 - (p - p1) * dy)
	    if (a < c) {
		pl = p + (c - a) / (b - a) * step
		xl = asieval (xasi, pl)
		break
	    }
	    b = a
	}

	pr = INDEF
	xr = INDEF
	for (p=pc; p<p2; p=p+step) {
	    a = abs (asieval (yasi, p) - y0 - (p - p1) * dy)
	    if (a < c) {
		pr = p - (c - a) / (b - a) * step
		xr = asieval (xasi, pr)
		break
	    }
	    b = a
	}

	b = y0 + (pc - p1) * dy
	p = asieval (yasi, pc)
	a = (p - b) / 2 + b
	if (!IS_INDEF(xl)) {
	    if (xl > xc) {
		c = pl
		pl = pr
		pr = c
		c = xl
		xl = xr
		xr = c
	    }
	}
	if (IS_INDEF(xl) || IS_INDEF(xr))
	    c = INDEF
	else
	    c = xr - xl

	# Draw marks to show the baseline, center, and width.
	call gseti (gp, G_PLTYPE, 2)
	call gline (gp, x1, y1, x2, y2)
	call gline (gp, xc, b, xc, p)
	if (!IS_INDEF(xl))
	    call gline (gp, xc, a, xl, a-b+y0+(pl-p1)*dy)
	if (!IS_INDEF(xr))
	    call gline (gp, xc, a, xr, a-b+y0+(pr-p1)*dy)
	call gseti (gp, G_PLTYPE, 1)

	# Record the results.
	call sl_init (sl, 4)
	call sprintf (Memc[sl_getstr(sl,1)], SZ_LINE,
	    "[1/4] Profile: Center=%8g, Width=%8g, Peak=%8g, Bkg=%8g\n")
	    call pargr (xc)
	    call pargr (c)
	    call pargr (p)
	    call pargr (b)
	call sprintf (Memc[sl_getstr(sl,2)], SZ_LINE,
	    "[2/4] Moments: Centroid=%8g, Width=%8g, Flux=%8g, Asym=%6g\n")
	    call pargd (sum1)
	    call pargd (2.35482 * sum2)
	    call pargd (sum0)
	    call pargd (sum3)
	call sprintf (Memc[sl_getstr(sl,3)], SZ_LINE,
	    "[3/4] Half Intensity: Lower=%8g, Upper=%8g, Width=%8g\n")
	    call pargr (xl)
	    call pargr (xr)
	    call pargr (c)
	call sprintf (Memc[sl_getstr(sl,4)], SZ_LINE,
	    "[4/4] Background: (%8g, %8g) - (%8g, %8g)\n")
	    call pargr (x1)
	    call pargr (y1)
	    call pargr (x2)
	    call pargr (y2)
	sline = 1

	call asifree (xasi)
	call asifree (yasi)
end
