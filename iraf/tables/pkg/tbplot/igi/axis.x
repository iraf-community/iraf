include <gset.h>
include	<math.h>
include "igi.h"

# Makes an axis labelled from A1 to A2 at location AX, AY, length nlen
# BIG > 0 ==> use that for spacing of large ticks.
# SMALL < 0 ==> logarithmic axis
# SMALL = 0 ==> default
# SMALL > 0 ==> use that for the spacing of small ticks.
# ILABEL is 0 for no labels, 1 for labels parallel to axis, and 2 for
# perpendicular to axis. 
# MG_ANGLE determines the angle of the axis.
# ICLOCK = 1 for clockwise ticks on the axis, 0 for counterclockwise

# 2/7/91 Changed some of the coordinate variables to double to try and
# get around some precision problems.  ZGL
# 8/20/91 Removed ^Ls.  ZGL

# 9/16/91 Fixed string position for perpendicular tick labels (probably
# trashed by previous bug fix).  ZGL

# 28 May 1992 Removed number() to a separate file.  ZGL
## 17 June 1992 "Hard code" use of soft (igi) fonts.  ZGL
#  1/27/93  Fixed dummy array declarations (ARB).

procedure axis (igs, a1, a2, small, big, nx1, ny1, nlen,
    ilabel, iclock, fmt)

pointer	igs		# Parameters structure pointer
real	a1, a2		# Range of data values
real	small, big	# Minor, major tick interval
real	nx1, ny1	# Start of axis in NDC
real	nlen		# Size of axis in NDC
int	ilabel		# Labels?
int	iclock		# Clockwise ticks?
char	fmt[ARB]	# SPP label format

pointer	igps
real	nx2, ny2
real	wx1, wy1, wx2, wy2
real	par[5]
int	wcs

int	gstati()

extern ticklabel(), tick()

begin
	igps = PLOT_PARMS(igs)

	call setltype (igs, SOLID_LINE)

	wcs = gstati (GIO_GP(igs), G_WCS)
	call ig_gctran (GIO_GP(igs), nx1, ny1, wx1, wy1, 0, wcs)

	nx2 = nlen * cos (DEGTORAD(MG_ANGLE(igps))) + nx1
	ny2 = nlen * sin (DEGTORAD(MG_ANGLE(igps))) + ny1
	call ig_gctran (GIO_GP(igs), nx2, ny2, wx2, wy2, 0, wcs)

	# Draw the axis itself
	call gline (GIO_GP(igs), wx1, wy1, wx2, wy2)

	par[1] = iclock
	call axistick (igs, a1, a2, small, big, nx1, ny1, nlen,
	    par, fmt, tick)

	if (ilabel != 0) {
	    # Label the axis
	    if (ilabel < 0) {
		# Trailing zero
		par[5] = YES
		ilabel = abs (ilabel)
	    } else
		par[5] = NO

	    if (ilabel == 1) {
		par[1] = 1
		par[2] = 0
	    } else {
		par[1] = 2 * iclock
		par[2] = 1
	    }

	    par[3] = iclock
	    if (MG_ANGLE(igps) <= 45.0) {
		if (abs (a1) <= MG_XLEXP(igps) && abs (a2) <= MG_XLEXP(igps) || 
		    abs (a1) >= MG_XHEXP(igps) && abs (a2) >= MG_XHEXP(igps)) 
		    par[4] = 1
		else
		    par[4] = 0

		if (MG_SEXAGX(igps) == YES) {
		    MG_NDECIM(igps) = MG_NDECMX(igps)
		    MG_SEXAGS(igps) = YES

		} else
		    MG_SEXAGS(igps) = NO

	    } else {
		if (abs (a1) <= MG_YLEXP(igps) && abs (a2) <= MG_YLEXP(igps) || 
		    abs (a1) >= MG_YHEXP(igps) && abs (a2) >= MG_YHEXP(igps)) 
		    par[4] = 1

		else
		    par[4] = 0

		if (MG_SEXAGY(igps) == YES) {
		    MG_NDECIM(igps) = MG_NDECMY(igps)
		    MG_SEXAGS(igps) = YES
		} else
		    MG_SEXAGS(igps) = NO
	    }

	    call axistick (igs, a1, a2, small, big, nx1, ny1, nlen, 
		par, fmt, ticklabel)
	}
end


# Makes an axis labelled from A1 to A2 at location AX, AY, length nlen
# If SMALL < 0 make a logarithmic axis, if ASMALL = 0, do the default
# If SMALL > 0 try to use that for the spacing of small ticks.
# If BIG   > 0 use that for spacing of large ticks.
# Passes the x, y positions, the label, the tick size, and
# the two parameters PAR1, PAR2 to the function FUNC.
# ANGLE determines the angle of the axis.

# 2/7/91 Changed some of the coordinate variables to double to try and
# get around some precision problems.  ZGL

procedure axistick (igs, a1, a2, small, big, ax, ay, nlen, par, fmt, func)

pointer	igs		# Parameters structure pointer
real	a1, a2		# Range of data values
real	small, big	# Minor, major tick interval
real	ax, ay		# Start of axis in NDC
real	nlen		# Size of axis in NDC
real	par[ARB]
char	fmt[ARB]	# SPP label format

pointer	igps
int	major, minor
data	major /1/,  minor /0/
double	ax1, ax2
double	bx1, bx2
double	step, tick, off, value
double	angle, cosine, sine
int	nch, nlabel, nsmall
real	adiff, diff, amant
int	iexp, num
int	i
double	xlen, xpos, ypos
real	wx, wy
real	swx, swy
int	wcs
real	sdx, sdy
double	dx, dy
double	alen, lenx, leny
int	xsign, ysign
bool	was_off

int	gstati()
bool	fp_equald()

extern	func()

begin
	if (a1 == a2) 
	    # Null size
	    return

	ax1 = double (a1)
	ax2 = double (a2)

	# Get the NDC/WCS scale
	wcs = gstati (GIO_GP(igs), G_WCS)
	call ig_gctran  (GIO_GP(igs), ax, ay, swx, swy, 0, wcs)

	wx = double (swx)
	wy = double (swy)

	call ggscale (GIO_GP(igs), swx, swy, sdx, sdy)

	dx = double (sdx)
	dy = double (sdy)

	igps   = PLOT_PARMS(igs)
	angle  = MG_ANGLE(igps)

	if (fp_equald (angle, 90.0d0)) {
	    cosine = 0.0
	    sine   = 1.0

	} else if (fp_equald (angle, 0.0d0)) {
	    cosine = 1.0
	    sine   = 0.0

	} else {
	    cosine = cos (DEGTORAD(angle))
	    sine   = sin (DEGTORAD(angle))
	}

	# Axis length in WCS
	lenx = dx * double (nlen) * cosine
	leny = dy * double (nlen) * sine
	alen = sqrt (lenx**2 + leny**2)

	if (big == 0 && small >= 0) {
	    nsmall = 5
	    if (MG_SEXAGS(igps) == YES) 
		nch=12
	    else
		nch=10

	    nlabel = nint (nlen / (nch * MG_CHARSIZE(igps) * MG_EXPAND(igps)))
	    nlabel = max (nlabel, 4)
	    adiff  = abs (a2 - a1)
	    diff   = log10 (adiff / nlabel)
	    iexp   = diff

	    if (diff < 0) 
		iexp = iexp - 1

	    amant = diff - iexp

	    if (amant < 0.15) 
	    	num = 1
	    else if (adiff <= 29 && MG_SEXAGS(igps) == YES) 
		num = 5
	    else if (amant <= 800 && MG_SEXAGS(igps) == YES) {
		num = 6
		nsmall = 4
	    } else if (amant < 0.50) {
	    	num = 2
	    	nsmall = 4
	    } else if (amant < 0.85) 
	    	num = 5
	    else
	    	num = 10

	    step = (num * 10.0d0**iexp)

	    if (small > 0) 
		nsmall = abs (nint (step / small))

	} else if (big > 0 && small > 0) {
	    step = double (big)
	    nsmall = abs (nint (step / small))
	} else if (small < 0) {
	    # Log
	    if (ax1 <= 0 || ax2 <= 0) return
	    bx1 = ax1
	    bx2 = ax2
	    ax1 = log10 (bx1)
	    ax2 = log10 (bx2)
	    step = 1.0d0
	    nsmall = 10
	}

	# Kludge unfavorable integer roundoff
	ax1 = ax1 - 1d-6 * (ax2 - ax1)
	ax2 = ax2 + 1d-6 * (ax2 - ax1)

        # Check for flipped axis.
	if (a2 < a1)
	    step = -step

	# Save the tick spacing
	MG_GSTEP(igps) = step

	off = int (ax1 / step)

	if (ax1 / step < 0) 
	    off = off - 1d0

	tick = step * off

	xsign = dx / abs (dx)
	ysign = dy / abs (dy)

	repeat {
	    # Major ticks
            was_off = false
	    do i = 1, nsmall {
		# Minor ticks
		if (small >= 0) {
		    # Linear
		    value = tick + step * (i - 1) / nsmall
		    if (abs (value) < abs (step * 1d-12))
			value = 0d0
		    xlen = alen * (value - ax1) / (ax2 - ax1)
		} else {
		    # Log
		    value = i * 10d0 ** tick
		    xlen = alen * (log10 (value) - ax1) / (ax2 - ax1)
		}

		if (xlen >= 0.0) {
		    # On the axis
		    if (xlen > alen) {
                        was_off = true
                        next
                    }
                    

		    xpos = wx + xsign * xlen * cosine
		    ypos = wy + ysign * xlen * sine

		    if (i == 1) 
			# Major tick
			call func (igs, xpos, ypos, value, major, par, fmt)

		    else
			# Minor tick
			call func (igs, xpos, ypos, value, minor, par, fmt)
		}
	    }

            # If some tick was off the axis, bail.
            if (was_off)
                return
            
	    tick = tick + step
	}
end


# Tick's location is X,Y
# IMPORT determines whether a big or small tick
# PAR(1) = 1 for clockwise ticks on the axis, 0 for counter
# ANGLE determines the angle of the axis.

# 2/7/91 Changed some of the coordinate variables to double to try and
# get around some precision problems.  ZGL

procedure tick (igs, x, y, value, import, par)

pointer	igs			# Parameters structure pointer
double	x, y
double	value
int	import
real	par[ARB]

pointer	igps
double	angle, sine, cosine
double	major, minor
real	xtick, ytick
real	sdx, sdy
double	dx, dy
real	sx, sy

bool	fp_equald()

begin
	igps   = PLOT_PARMS(igs)

	sx = real (x)
	sy = real (y)

	# Get the plot scale in WCS/NDC
	call ggscale (GIO_GP(igs), sx, sy, sdx, sdy)

	dx = double (sdx)
	dy = double (sdy)

	angle  = MG_ANGLE(igps)

	if (fp_equald (angle, 90.0d0)) {
	    cosine = 0.0
	    sine   = 1.0

	} else if (fp_equald (angle, 0.0d0)) {
	    cosine = 1.0
	    sine   = 0.0

	} else {
	    cosine = cos (DEGTORAD(angle))
	    sine   = sin (DEGTORAD(angle))
	}

	major = MG_CHARSIZE(igps) * MG_EXPAND(igps)
	if (par[1] >= 0.5)
	    # Counterclockwise 
	    major = -major
	minor = 0.5 * major

	if (import == 0) {
	    # Small (minor) tick
	    xtick = x - minor * sine   * dx
	    ytick = y + minor * cosine * dy
	} else {
	    # Large (major) tick
	    xtick = x - major * sine   * dx
	    ytick = y + major * cosine * dy
	}

	# Draw the tick
	call gline (GIO_GP(igs), sx, sy, xtick, ytick)
end


procedure ticklabel (igs, x, y, value, import, par, fmt)

# Label's location is X, Y, VALUE is the number output.
# PAR(1) is 0, 1, 2 for label to be left, center, or right justified
# PAR(2) = 0 for labels parallel to axis, and 1 for perpendicular to axis.
# PAR(3) = 0 for clockwise labels on the axis, 1 for counter
# PAR(4) = 0 for default notation (e.g exponential if value<1e-4 or >1e4,
#            floating point otherwise), 1 for exponential only
# PAR[5] = NO ==> no trailing zero on integral values
# ANGLE determines the angle of the axis.

# 2/7/91 Changed some of the coordinate variables to double to try and
# get around some precision problems.  ZGL

# 9/16/91 Fixed string position for perpendicular tick labels (probably
# trashed by previous bug fix).  ZGL

pointer	igs			# Parameters structure pointer
double	x, y
double	value
int	import
real	par[ARB]
char	fmt[ARB]

pointer	igps
real	sine, cosine
real	space
int	nchar
int	notation, iascii
real	angle
int	just
real	sx, sy
real	sdx, sdy
double	dx, dy
int	trailz		# Trailing zero?

char	label[40], chrstr[16]

bool	fp_equalr()

begin
	if (import == 0) 
	    return

	igps = PLOT_PARMS(igs)

	if (MG_SEXAGS(igps) == YES) {
	    # Label in sexagesimal
	    nchar = 11 + MG_NDECIM(igps)
	    call cflab (chrstr, value, nchar, MG_NDECIM(igps))
	    call strcpy (chrstr, label, 40)

	} else {
	    notation=par[4]

	    if (MG_ANGLE(igps) <= 45) {
		if (abs (value) <= MG_XLEXP(igps) || 
		    abs (value) >= MG_XHEXP(igps)) 
		    notation = 1

	    } else {
		if (abs (value) <= MG_YLEXP(igps) || 
		    abs (value) >= MG_YHEXP(igps))
		    notation = 1
	    }

	    iascii = 0
	    trailz = par[5]

	    call number (value, nchar, label,
		notation, iascii, trailz, fmt)
	}

	angle = MG_ANGLE(igps)

	if (fp_equalr (angle, 90.0)) {
	    cosine = 0.0
	    sine   = 1.0

	} else if (fp_equalr (angle, 0.0)) {
	    cosine = 1.0
	    sine   = 0.0

	} else {
	    cosine = cos (DEGTORAD(angle))
	    sine   = sin (DEGTORAD(angle))
	}

	if (par[2] < 0.5) {
	    # Labels parallel to axis

	    if (par[3] > 0.5)
		# Counterclockwise labels
		just = 8

	    else
		just = 2

	} else {
	    # Labels perpendicular to axis
	    angle = MG_ANGLE(igps) - 90.0

	    if (par[3] > 0.5)
		# Counterclockwise labels
		just = 4

	    else
		just = 6
	}

	sx = real (x)
	sy = real (y)

	# Get the plot scale in WCS/NDC
	call ggscale (GIO_GP(igs), sx, sy, sdx, sdy)

	dx = double (sdx)
	dy = double (sdy)

	space = MG_CHARSIZE(igps)

	if (par[3] > 0.5)
	    # Counterclockwise labels
	    space = -space

	sx = x + space * sine   * dx
	sy = y - space * cosine * dy

	call mgostr (igs, sx, sy, label, 
	    MG_EXPAND(igps), angle, just, SOFT_FONTS)
end


# Convert floating point value to a display of the form of
# the "Babylonian" ddd mm ss.s, where each field represents
# a sexagesimal increment over its successor.  The value x
# must be in the units of the rightmost field.  For instance,
# x must be in seconds of time for hours, minutes, seconds
# display.

# 2/4/91 Changed x and y to double.  ZGL

procedure cflab (chrstr, x, w, d)

char	chrstr[16]
double	x
int	w, d

int	h, m, s, f, lh, lm, ls
char	th[20], tm[20], ts[20]
double	y

begin
	y = abs (x) * 3600.0

	h = y / 3600
	y = y - 3600 * h

	if (x < 0) 
	    h = -h

	m = y / 60
	y = y - 60 * m
	s = y
	y = y - s
	f = (10**d) * y + 0.5

	if (f >= 10**d) {
	    f = f - 10**d
	    s = s + 1
	}

	if (s > 59) {
	    s = 0
	    m = m + 1
	}

	if (m > 59) {
	    m = 0
	    h = h + 1
	}

	ts[1] = EOS

	call sprintf (ts, 20, "%02d")
	    call pargi (s)

	if (d > 0) {
	    call sprintf (ts[3], 20, ".%d")
		call pargi (f)
	    ls = d+3
	} else
	    ls = 2

	tm[1] = EOS
	call sprintf (tm, 20, "%02d")
	    call pargi (m)

	lm = 2

	th[1] = EOS
	lh = log10 (1.0 * abs (h) + 0.0001) + 1
	if ((h == 0) && (x >= 0)) {
	    call strcpy ("00", th, 2)
	    lh = 2
	} else if ((h == 0) && (x < 0)) {
	    call strcpy ("-00", th, 3)
	    lh = 3
	} else {
	    if (h < 0) 
		# Allow for sign
		lh = lh + 1

	    call sprintf (th, lh, "%d")
		call pargi (h)
	}

	chrstr[1] = EOS

	if (w >= lh + lm + ls + 2) {
	    call sprintf (chrstr, w, "%s %s %s")
		call pargstr (th)
	    	call pargstr (tm)
	    	call pargstr (ts)

	} else {
	    # Truncate
	    if (w >= lh + lm + 1) {
		call sprintf (chrstr, w, "%s %s")
		    call pargstr (th)
	    	    call pargstr (tm)

	    } else if (w >= lh) {
		call sprintf (chrstr, w, "%s")
		    call pargstr (th)

	    } else
		call strcpy ("****************************************", 
		    chrstr, w)
	}
end
