include <math.h>
include <gset.h>
include "igi.h"

#  MGPOINT -- Draw a marker

#  2/7/90 Modified to draw single pixel marker and fixed gmark() call.  ZGL

#  8/20/91 Removed ^Ls. ZGL
## 8/3/92  Simplified calling sequence (use structure).  ZGL
## 8/5/92  Fixed mgsfpnt() to work with fill patterns.  ZGL
## 8/6/97  Validate info read in from graphcap, and set to 
##		default value when xs or ys are 0.   	WJH

procedure mgpoint (igs, xp, yp, n, istyle, wsize)

pointer	igs
real	xp, yp
int	n
int	istyle
real	wsize

pointer	igps		# Plot parameters structure
real	angle
int	fillpat		# Fill pattern index

real	size, xsize, ysize
real	dtheta, theta
real	dx, dy		# Viewport scale in WC/NDC
real	xs, ys		# Device linear scale
real	ar		# Device aspect ratio
int	m

real	ggetr()

begin
	if (IS_INDEF(xp) || IS_INDEF(yp))
	    return

	igps = PLOT_PARMS(igs)

	angle   = MG_ANGLE(igps)
	fillpat = MG_FILLPAT(igps)
	
	call setltype (igs, SOLID_LINE)

	size = wsize * MG_PNTSIZE(PLOT_PARMS(igs))

	# Device aspect ratio
	xs = ggetr (GIO_GP(igs), "xs")
	ys = ggetr (GIO_GP(igs), "ys")

	# Check to see that non-zero values are read in from graphcap entry
	# If they are zero, set them to a default value of 1. to
	#	prevent floating point errors or other odd behavior.
	#	WJH 6 Aug 97
	if (xs == 0.) xs = 1.
	if (ys == 0.) ys = 1.
	
	ar = ys / xs

	# Scale of viewport in WC/NDC
	call ggscale (GIO_GP(igs), xp, yp, dx, dy)
	xsize = size * abs (dx)
	ysize = size * abs (dy)

	if (ar > 0.0 && ar < 1.0)
	    # Landscape
	    xsize = xsize * ar
	else if (ar > 1.0)
	    # Portrait
	    ysize = ysize / ar

	if (n < 2) {
	    # Use GIO markers

	    if (abs (n) == 1)
		# Smallest plottable point
		m = 0
	    else
		m = abs (n)

	    call gmark (GIO_GP(igs), xp, yp, m, xsize, ysize)
	    return
	}

	dtheta = TWOPI / real (n)
	theta = (3.0*PI + dtheta) / 2.0 + DEGTORAD (angle)

	if (istyle == OPEN_MARKER || n == 2) {
	    # Open
	    if (fillpat == HOLLOW_FILL)
		call mgopnt (GIO_GP(igs), xp, yp, n, xsize, ysize,
		    theta, dtheta)

	    else
		call mgfppnt (GIO_GP(igs), xp, yp, n, xsize, ysize,
		    theta, dtheta, fillpat)

	} else if (istyle == SKELETAL_MARKER) {
	    # Skeletal
	    call mgskpnt (GIO_GP(igs), xp, yp, n, xsize, ysize, theta, dtheta)

	} else if (istyle == STARRED_MARKER) {
	    # Starred
	    if (fillpat == HOLLOW_FILL)
		call mgstpnt (GIO_GP(igs), xp, yp, n, xsize, ysize, 
		    theta, dtheta, MG_STELLAR(PLOT_PARMS(igs)))

	    else
		call mgsfpnt (GIO_GP(igs), xp, yp, n, xsize, ysize, 
		    theta, dtheta, MG_STELLAR(PLOT_PARMS(igs)),
		    fillpat)

	} else if (istyle == SOLID_MARKER) {
	    # Solid (filled)
	    call mgfpnt (GIO_GP(igs), xp, yp, n, xsize, ysize, dy, 
		theta, dtheta, MG_PNTFILL(PLOT_PARMS(igs)))

	} else if (istyle == HALF_MARKER) {
	    # Half filled
	    call mgopnt (GIO_GP(igs), xp, yp, n, xsize, ysize,
		theta, dtheta)
	    call mgfpnt (GIO_GP(igs), xp, yp, n, xsize, ysize, dy, 
		theta, dtheta, -MG_PNTFILL(PLOT_PARMS(igs)))
	}
end


#  MGOPNT -- Draw an open polygon marker.

procedure mgopnt (gp, xc, yc, n, xsize, ysize, theta, dtheta)

pointer	gp
real	xc, yc
int	n
real	xsize, ysize
real	theta
real	dtheta

real	xa, ya
real	xb, yb
int	j

begin
	xa = xsize*cos (theta) + xc
	ya = ysize*sin (theta) + yc
	call gamove (gp, xa, ya)

	do j = 1, n {
	    theta = theta + dtheta
	    xb = xsize*cos (theta) + xc
	    yb = ysize*sin (theta) + yc
	    call gadraw (gp, xb, yb)
	    xa = xb
	    ya = yb
	}
end


procedure mgfppnt (gp, xc, yc, n, xsize, ysize, theta, dtheta, fillpat)

#  MGPPNT -- Draw an open polygon marker using the current fill pattern

pointer	gp
real	xc, yc
int	n
real	xsize, ysize
real	theta
real	dtheta
int	fillpat		# Fill pattern index

pointer	sp, xp, yp
int	j
int	gfp

begin
	call smark (sp)
	call salloc (xp, n+1, TY_REAL)
	call salloc (yp, n+1, TY_REAL)

	Memr[xp] = xsize*cos (theta) + xc
	Memr[yp] = ysize*sin (theta) + yc

	do j = 1, n - 1 {
	    theta = theta + dtheta
	    Memr[xp+j] = xsize*cos (theta) + xc
	    Memr[yp+j] = ysize*sin (theta) + yc
	}

	Memr[xp+n] = Memr[xp]
	Memr[yp+n] = Memr[yp]

	switch (fillpat) {
	case CLEAR_FILL:
	    gfp = GF_CLEAR

	case HOLLOW_FILL:
	    gfp = GF_HOLLOW

	case SOLID_FILL:
	    gfp = GF_SOLID

	default:
	    gfp = fillpat
	}

	call gfill (gp, Memr[xp], Memr[yp], n, gfp)

	call sfree (sp)
end


procedure mgskpnt (gp, xc, yc, n, xsize, ysize, theta, dtheta)

pointer	gp
real	xc, yc
int	n
real	xsize, ysize
real	theta
real	dtheta

real	xa, ya
real	xb, yb
real	dx, dy
int	j

begin
	if (2*(n/2) == n)
	    # Even
	    do j = 1, n/2 {
		dx = xsize*cos (theta)
		dy = ysize*sin (theta)
		xa = xc + dx
		ya = yc + dy
		xb = xc - dx
		yb = yc - dy
		call gline (gp, xa, ya, xb, yb)
		theta = theta + dtheta
	    }
	else
	    # Odd
	    do j = 1, n {
		xa = xsize*cos (theta) + xc
		ya = ysize*sin (theta) + yc
		call gline (gp, xc, yc, xa, ya)
		theta = theta + dtheta
	    }
end


procedure mgstpnt (gp, xc, yc, n, xsize, ysize, theta, dtheta, stellar)

pointer	gp
real	xc, yc
int	n
real	xsize, ysize
real	theta
real	dtheta
real	stellar

real	xa, ya
real	xb, yb
int	j
real	xmid, ymid

begin
	xa = xsize*cos (theta) + xc
	ya = ysize*sin (theta) + yc
	call gamove (gp, xa, ya)

	do j = 1, n {
	    theta = theta + dtheta
	    xb = xsize * cos (theta) + xc
	    yb = ysize * sin (theta) + yc
	    xmid = stellar * (xa + xb - 2*xc) + xc
	    ymid = stellar * (ya + yb - 2*yc) + yc
	    call gadraw (gp, xmid, ymid)
	    call gadraw (gp, xb, yb)
	    xa = xb
	    ya = yb
	}
end


procedure mgsfpnt (gp, xc, yc, n, xsize, ysize,
    theta, dtheta, stellar, fillpat)

pointer	gp
real	xc, yc
int	n
real	xsize, ysize
real	theta
real	dtheta
real	stellar
int	fillpat		# Fill pattern index

int	npts
pointer	sp, xp, yp
real	xa, ya
real	xb, yb
int	i, j
int	gfp
real	xmid, ymid

begin
	npts = 2 * n + 1
	call smark (sp)
	call salloc (xp, npts, TY_REAL)
	call salloc (yp, npts, TY_REAL)

	xa = xsize*cos (theta) + xc
	ya = ysize*sin (theta) + yc

	i = 0

	do j = 1, n {
	    Memr[xp+i] = xa
	    Memr[yp+i] = ya
	    i = i + 1

	    theta = theta + dtheta
	    xb = xsize * cos (theta) + xc
	    yb = ysize * sin (theta) + yc
	    xmid = stellar * (xa + xb - 2*xc) + xc
	    ymid = stellar * (ya + yb - 2*yc) + yc

	    Memr[xp+i] = xmid
	    Memr[yp+i] = ymid
	    i = i + 1

	    xa = xb
	    ya = yb
	}

	Memr[xp+npts-1] = Memr[xp]
	Memr[yp+npts-1] = Memr[yp]

	switch (fillpat) {
	case CLEAR_FILL:
	    gfp = GF_CLEAR

	case HOLLOW_FILL:
	    gfp = GF_HOLLOW

	case SOLID_FILL:
	    gfp = GF_SOLID

	default:
	    gfp = fillpat
	}

	call gfill (gp, Memr[xp], Memr[yp], npts, gfp)

	call sfree (sp)
end


#  MGFPNT -- Draw a filled (solid) polygon marker.

procedure mgfpnt (gp, xc, yc, n, xsize, ysize, dy, theta, dtheta, fillfact)

pointer	gp
real	xc, yc
int	n
real	xsize, ysize		# Marker size in NDC
real	dy
real	theta
real	dtheta
real	fillfact

real	xa, ya
real	xb, yb
int	j
real	y
real	ymin, ymax
real	iymin, iymax
real	thb, tha
real	xa1, ya1, xa2, ya2
real	xb1, yb1, xb2, yb2
real	xn, yn
real	yr
real	fill
bool	half

real	ggetr()

begin
	ymax = -1
	ymin = +1
	do j = 1, n {
	    y = sin (theta + j * dtheta)
	    if (y < ymin) {
		ymin = y
		tha = theta + j * dtheta
	    }
	    ymax = max (ymax, y)
	}

	# Device resolution (lines)
	yr = ggetr (gp, "yr")

	if (fillfact < 0) {
	    half = true
	    fillfact = -fillfact
	} else
	    half = false

	# Number of device rasters in marker
	fill = fillfact * yr / abs (dy)

	iymax = nint (fill * ysize * ymax)
	iymin = nint (fill * ysize * ymin)

	thb = tha
	xa1 = fill * xsize * cos (tha)
	ya1 = fill * ysize * sin (tha)
	xa2 = fill * xsize * cos (tha+dtheta)
	ya2 = fill * ysize * sin (tha+dtheta)
	xb1 = fill * xsize * cos (thb)
	yb1 = fill * ysize * sin (thb)
	xb2 = fill * xsize * cos (thb-dtheta)
	yb2 = fill * ysize * sin (thb-dtheta)

	if (abs (ya2 - ya1) < 1) {
	    tha = tha + dtheta
	    xa1 = xa2
	    ya1 = ya2
	    xa2 = fill * xsize * cos (tha+dtheta)
	    ya2 = fill * ysize * sin (tha+dtheta)
	}

	if (abs (yb2 - yb1) < 1) {
	    thb = thb - dtheta
	    xb1 = xb2
	    yb1 = yb2
	    xb2 = fill * xsize * cos (thb-dtheta)
	    yb2 = fill * ysize * sin (thb-dtheta)
	}

	do j = iymin, iymax {
	    if (ya2 < j) {
	    	tha = tha + dtheta
	    	xn = fill * xsize * cos (tha+dtheta)
	    	yn = fill * ysize * sin (tha+dtheta)
	    	if (abs (yn - ya2) > 1) {
	    	    xa1 = xa2
	    	    ya1 = ya2
	    	    xa2 = xn
	    	    ya2 = yn
	    	}
	    }

	    if (yb2 < j) {
	    	thb = thb - dtheta
	    	xn = fill * xsize * cos (thb-dtheta)
	    	yn = fill * ysize * sin (thb-dtheta)
	    	if (abs (yn - yb2) > 1) {
	    	    xb1 = xb2
	    	    yb1 = yb2
	    	    xb2 = xn
	    	    yb2 = yn
	    	}
	    }

	    xa = (xa1 + (xa2-xa1)*(real (j)-ya1)/(ya2-ya1)) / fill + xc
	    ya = real (j) / fill + yc
	    xb = (xb1 + (xb2-xb1)*(real (j)-yb1)/(yb2-yb1)) / fill + xc

	    if (half)
		xb = xc

	    yb = ya
	    call gamove (gp, xa, ya)
	    call gadraw (gp, xb, yb)
	}
end
