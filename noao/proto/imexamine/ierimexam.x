# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	<gset.h>
include	<math.h>
include	<math/gsurfit.h>
include	"imexam.h"


# IE_RIMEXAM -- Radial profile plot and photometry parameters.
# If no GIO pointer is given then only the photometry parameters are printed.
# First find the center using the marginal distributions.  Then subtract
# a fit to the background.  Compute the moments within the aperture and
# fit a gaussian of fixed center and zero background.  Make the plot
# and print the photometry values.

procedure ie_rimexam (gp, mode, ie, x, y)

pointer	gp
pointer	ie
int	mode
real	x, y

bool	center, background, medsky, clgpsetb()
real	radius, buffer, width, magzero, rplot, clgpsetr()
int	xorder, yorder, clgpseti()

int	i, j, ns, no, np
int	x1, x2, y1, y2, nx, ny, npts
real	median, xcntr, ycntr, mag, e, pa, zcntr, fwhm, w
pointer	sp, title, im, data, pp, ws, xs, ys, zs, gs, ptr
double	sumo, sums, sumxx, sumyy, sumxy, sumw, sumr, suml, sumrr, sumrl
real	r, r1, r2, r3, dx, dy, gseval(), amedr()
pointer	clopset(), ie_gimage(), ie_gdata()
string	label "#\
   COL    LINE    RMAG     FLUX      SKY   N  RMOM ELLIP    PA     PEAK  FWHM\n"

begin
	iferr (im = ie_gimage (ie, NO)) {
	    call erract (EA_WARN)
	    return
	}

	# Open parameter set.
	if (gp != NULL) {
	    if (IE_PP(ie) != NULL)
		call clcpset (IE_PP(ie))
	}
	pp = clopset ("rimexam")

	center = clgpsetb (pp, "center")
	background = clgpsetb (pp, "background")
	radius = clgpsetr (pp, "radius")
	if (background) {
	    buffer = clgpsetr (pp, "buffer")
	    width = clgpsetr (pp, "width")
	    xorder = clgpseti (pp, "xorder")
	    yorder = clgpseti (pp, "yorder")
	    medsky = (xorder <= 0 || yorder <= 0)
	} else {
	    buffer = 0.
	    width = 0.
	}
	magzero = clgpsetr (pp, "magzero")
	rplot = clgpsetr (pp, "rplot")

	# If the initial center is INDEF then use the previous value.
	if (gp != NULL) {
	    if (!IS_INDEF(x))
	        IE_X1(ie) = x
	    if (!IS_INDEF(y))
	        IE_Y1(ie) = y

	    xcntr = IE_X1(ie)
	    ycntr = IE_Y1(ie)
	} else {
	    xcntr = x
	    ycntr = y
	}

	# Center
	if (center)
	    iferr (call ie_center (im, radius, xcntr, ycntr)) {
		call erract (EA_WARN)
		return
	    }

	# Get data including a buffer and background annulus.
	r = max (rplot, radius + buffer + width)
	x1 = xcntr - r
	x2 = xcntr + r
	y1 = ycntr - r
	y2 = ycntr + r
	iferr (data = ie_gdata (im, x1, x2, y1, y2)) {
	    call erract (EA_WARN)
	    return
	}

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	npts = nx * ny

	call smark (sp)
	call salloc (title, IE_SZTITLE, TY_CHAR)
	call salloc (xs, npts, TY_REAL)
	call salloc (ys, npts, TY_REAL)

	# Extract the background data if background subtracting.
	ns = 0
	if (background && width > 0.) {
	    call salloc (zs, npts, TY_REAL)
	    call salloc (ws, npts, TY_REAL)

	    r1 = radius ** 2
	    r2 = (radius + buffer) ** 2
	    r3 = (radius + buffer + width) ** 2

	    ptr = data
	    do j = y1, y2 {
	        dy = (ycntr - j) ** 2
	        do i = x1, x2 {
		    r = (xcntr - i) ** 2 + dy
		    if (r <= r1)
		        ;
		    else if (r >= r2 && r <= r3) {
		        Memr[xs+ns] = i
		        Memr[ys+ns] = j
		        Memr[zs+ns] = Memr[ptr]
		        ns = ns + 1
		    }
		    ptr = ptr + 1
	        }
	    }
	}
 
	# Accumulate the various sums for the moments and the gaussian fit.
	no = 0
	np = 0
	sumo = 0.; sums = 0.; sumxx = 0.; sumyy = 0.; sumxy = 0.
	sumw = 0.; sumr = 0.; suml = 0.; sumrr = 0.; sumrl = 0.
	ptr = data

	if (ns > 0) {		# Background subtraction

	    # If background points are defined fit a surface and subtract
	    # the fitted background from within the object aperture. 

	    if (medsky)
		median = amedr (Memr[zs], ns)
	    else {
		repeat {
		    call gsinit (gs, GS_POLYNOMIAL, xorder, yorder, YES,
			real (x1), real (x2), real (y1), real (y2))
		    call gsfit (gs, Memr[xs], Memr[ys], Memr[zs], Memr[ws], ns,
			WTS_UNIFORM, i)
		    if (i == OK)
			break
		    xorder = max (1, xorder - 1)
		    yorder = max (1, yorder - 1)
		}
	    }

	    do j = y1, y2 {
	        dy = j - ycntr
	        do i = x1, x2 {
		    dx = i - xcntr
		    r = sqrt (dx ** 2 + dy ** 2)

		    if (medsky)
			r2 = median
		    else
			r2 = gseval (gs, real(i), real(j))
		    r1 = Memr[ptr] - r2

		    if (r <= radius) {
			sumo = sumo + r1
			sums = sums + r2
			sumxx = sumxx + dx * dx * r1
			sumyy = sumyy + dy * dy * r1
			sumxy = sumxy + dx * dy * r1
			if (r1 > 0.) {
			    w  = r1 / max (.1, r ** 2)
			    sumw = sumw   + w
			    sumr = sumr   + w * r**2
			    suml = suml   + w * log (r1)
			    sumrr = sumrr + w * r**4
			    sumrl = sumrl + w * r**2 * log(r1)
			}
			no = no + 1
		    }

		    if (r <= rplot) {
		        Memr[xs+np] = r
		        Memr[ys+np] = r1
		        np = np + 1
		    }
		    ptr = ptr + 1
		}
	    }

	    call gs_free (gs)

	} else {		# No background subtraction
	    do j = y1, y2 {
	        dy = j - ycntr
	        do i = x1, x2 {
		    dx = i - xcntr
		    r = sqrt (dx ** 2 + dy ** 2)
		    r1 = Memr[ptr]

		    if (r <= radius) {
			sumo = sumo + r1
			sumxx = sumxx + dx * dx * r1
			sumyy = sumyy + dy * dy * r1
			sumxy = sumxy + dx * dy * r1
			if (r1 > 0.) {
			    w  = r1 / max (.1, r ** 2)
			    sumw = sumw   + w
			    sumr = sumr   + w * r**2
			    suml = suml   + w * log(r1)
			    sumrr = sumrr + w * r**4
			    sumrl = sumrl + w * r**2 * log(r1)
			}
			no = no + 1
		    }

		    if (r <= rplot) {
		        Memr[xs+np] = r
		        Memr[ys+np] = r1
		        np = np + 1
		    }
		    ptr = ptr + 1
		}
	    }
	}

	# Compute the photometry and gaussian fit parameters.
	mag = INDEF
	r = INDEF
	e = INDEF
	pa = INDEF
	zcntr = INDEF
	fwhm = INDEF
	if (sumo > 0.) {
	    mag = magzero - 2.5 * log10 (sumo)
	    r2 = sumxx + sumyy
	    if (r2 > 0.) {
	        r = 2 * sqrt (log (2.) * r2 / sumo)
	        r1 =(sumxx-sumyy)**2+(2*sumxy)**2
		if (r1 > 0.)
		    e = sqrt (r1) / r2
	        else
		    e = 0.
	    }
	    pa = RADTODEG (0.5 * atan2 (2*sumxy, sumxx-sumyy))
	    w = sumw * sumrr - sumr ** 2
	    zcntr = exp ((sumrr * suml - sumr * sumrl) / w)
	    fwhm = (sumw * sumrl - sumr * suml) / w
	    if (fwhm < 0.)
	        fwhm = 2 * sqrt (-log (2.) / fwhm)
	    else
		fwhm = INDEF
	}

	# Plot the radial profile and overplot the gaussian fit.
	if (gp != NULL) {
	    call sprintf (Memc[title], IE_SZTITLE,
	        "%s: Radial profile at %.2f %.2f\n%s")
	        call pargstr (IE_IMAGE(ie))
	        call pargr (xcntr)
	        call pargr (ycntr)
	        call pargstr (IM_TITLE(im))

	    call ie_graph (gp, mode, pp, Memc[title], Memr[xs], Memr[ys], np)

	    if (!IS_INDEF (fwhm)) {
		np = 51
		dx = rplot / (np - 1)
		dy = 4 * log (2.) / fwhm ** 2
		do i = 0, np - 1 {
		    r1 = i * dx
		    r3 = dy * r1 * r1
		    if (r3 < 10.)
			r2 = zcntr * exp (-r3)
		    else
			r2 = 0.
		    Memr[xs+i] = r1
		    Memr[ys+i] = r2
		}
		call gseti (gp, G_PLTYPE, 2)
		call gpline (gp, Memr[xs], Memr[ys], np)
		call gseti (gp, G_PLTYPE, 1)
	    }
	}

	if (IE_LASTKEY(ie) != 'a')
	    call printf (label)

	# Print the photometry values.
	call printf (
	    "%7.2f %7.2f %7.2f %8.1f %8.2f %3d %5.2f %5.3f %5.1f %8.2f %5.2f\n")
	    call pargr (xcntr)
	    call pargr (ycntr)
	    call pargr (mag)
	    call pargd (sumo)
	    call pargd (sums / no)
	    call pargi (no)
	    call pargr (r)
	    call pargr (e)
	    call pargr (pa)
	    call pargr (zcntr)
	    call pargr (fwhm)

	if (IE_LOGFD(ie) != NULL) {
	    if (IE_LASTKEY(ie) != 'a')
		call fprintf (IE_LOGFD(ie), label)

	    call fprintf (IE_LOGFD(ie),
	    "%7.2f %7.2f %7.2f %8.1f %8.2f %3d %5.2f %5.3f %5.1f %8.2f %5.2f\n")
	        call pargr (xcntr)
	        call pargr (ycntr)
	        call pargr (mag)
	        call pargd (sumo)
	        call pargd (sums / no)
	        call pargi (no)
	        call pargr (r)
	        call pargr (e)
	        call pargr (pa)
	        call pargr (zcntr)
	        call pargr (fwhm)
	}

	if (gp == NULL)
	   call clcpset (pp)
	else
	    IE_PP(ie) = pp

	call sfree (sp)
end


# IE_CENTER -- Find the center of gravity from the marginal distributions.

procedure ie_center (im, radius, xcntr, ycntr)

pointer	im
real	radius
real	xcntr, ycntr

int	i, j, k, x1, x2, y1, y2, nx, ny, npts
real	xlast, ylast
real	mean, sum, sum1, sum2, sum3, asumr()
pointer	data, ptr, ie_gdata()
errchk	ie_gdata

begin
	# Find the center of a star image given approximate coords.  Uses
	# Mountain Photometry Code Algorithm as outlined in Stellar Magnitudes
	# from Digital Images.

	do k = 1, 3 {
	    # Extract region around center
	    xlast = xcntr
	    ylast = ycntr
	    x1 = xcntr - radius + 0.5
	    x2 = xcntr + radius + 0.5
	    y1 = ycntr - radius + 0.5
	    y2 = ycntr + radius + 0.5
	    data = ie_gdata (im, x1, x2, y1, y2)

	    nx = x2 - x1 + 1
	    ny = y2 - y1 + 1
	    npts = nx * ny

	    # Find center of gravity for marginal distributions above mean.
	    sum = asumr (Memr[data], npts)
	    mean = sum / nx
	    sum1 = 0.
	    sum2 = 0.

	    do i = x1, x2 {
	        ptr = data + i - x1
		sum3 = 0.
		do j = y1, y2 {
		    sum3 = sum3 + Memr[ptr]
		    ptr = ptr + nx
		}
		sum3 = sum3 - mean
		if (sum3 > 0.) {
		    sum1 = sum1 + i * sum3
		    sum2 = sum2 + sum3
		}
	    }
	    xcntr = sum1 / sum2

	    ptr = data
	    mean = sum / ny
	    sum1 = 0.
	    sum2 = 0.
	    do j = y1, y2 {
		sum3 = 0.
		do i = x1, x2 {
		    sum3 = sum3 + Memr[ptr]
		    ptr = ptr + 1
		}
		sum3 = sum3 - mean
		if (sum3 > 0.) {
		    sum1 = sum1 + j * sum3
		    sum2 = sum2 + sum3
		}
	    }
	    ycntr = sum1 / sum2

	    if (int(xcntr) == int(xlast) && int(ycntr) == int(ylast))
		break
	}
end
