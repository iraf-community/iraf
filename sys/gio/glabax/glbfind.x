# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gio.h>
include	"glabax.h"

# GLB_FIND_TICKS -- Find the optimal positions for the tick marks on an axis.
# If rounding is enabled, extend the WCS out to the next tick outside the
# boundary on either end of the axis.  Since this routine may modify the WCS
# it must be called before any other routines (e.g., glb_setaxes).  Our task
# is to position the major ticks in world coordinates at round numbers, e.g.,
# 10, 20, 30 for a linear scale or 10, 100, 1000 for a log scale.  The minor
# ticks are evenly distributed over the range between the major ticks.  If
# log scaling is in use the step size between ticks will change by an order
# of magnitude (by the factor KSTEP) in each decade of the log scale, i.e.,
# at each major tick.  All tick positions and offsets are output in world
# coordinates.

procedure glb_find_ticks (gp, ap, ax1, ax2, angle)

pointer	gp			# graphics descriptor
pointer	ap			# axis parameters (from graphics descriptor)
pointer	ax1, ax2		# axis descriptors (output)
int	angle			# axis orientation, 0 or 90 degrees

pointer	w
int	logflag, nminor, scaling, t1, t2
real	char_height, char_width, wctick, tval
real	p1, p2, tp1, tp2, wcp1, tick1, step, minor_step, length

bool	fp_equalr()
int	gt_ndigits()
real	ggetr(), elogr(), aelogr(), glb_minorstep()

begin
	w = GP_WCSPTR (gp, GP_WCS(gp))

	# Start by zeroing the AX structures so that we do not have to zero
	# fields explicitly.  This is a bit tricky because we are implicitly
	# setting fields that are not named, but it saves time and space.

	call aclri (Memi[ax1], LEN_AX)
	call aclri (Memi[ax2], LEN_AX)

	# If ticks are not to be drawn or if there are fewer than 2 ticks then
	# we are done.

	if (GL_NMAJOR(ap) <= 2)
	    return

	# Call the tick placement algorithm to determine where to put the major
	# tick marks.  The output of this block are the variables P1 and P1,
	# the world coords of the ends of the axis, and TICK1 and STEP, the
	# world coords of the first tick and separation in world coords between
	# major ticks.

	if (angle == 0) {
	    p1 = WCS_WX1(w)
	    p2 = WCS_WX2(w)
	    scaling = WCS_XTRAN(w)
	} else {
	    p1 = WCS_WY1(w)
	    p2 = WCS_WY2(w)
	    scaling = WCS_YTRAN(w)
	}

	AX_SCALING(ax1) = scaling
	AX_SCALING(ax2) = scaling

	if (scaling == LOG) {
	    p1 = log10 (p1)
	    p2 = log10 (p2)
	    logflag = YES
	} else if (scaling == ELOG) {
	    p1 = elogr (p1)
	    p2 = elogr (p2)
	    logflag = YES
	} else
	    logflag = NO

	# Call the tick placement algorithm.
	call gtickr (p1, p2, GL_NMAJOR(ap), logflag, tick1, step)

	# If rounding is enabled, extend the WCS out to the next major tick
	# position outward on either end.  Always round if log scaling.

	if (GL_ROUND(ap) == YES || scaling != LINEAR) {
	    if (!fp_equalr (p1, tick1)) {
		tick1 = tick1 - step
		p1 = tick1
	    }

	    length = (p2 - p1) / step
	    if (!fp_equalr (p1 + int(length) * step, p2))
		p2 = p1 + (int(length) + 1) * step

	    if (scaling == ELOG) {
		tp1 = aelogr (p1)
		tp2 = aelogr (p2)
	    } else if (scaling == LOG) {
		tp1 = 10.0 ** p1
		tp2 = 10.0 ** p2
	    } else {
		tp1 = p1
		tp2 = p2
	    }

	    if (angle == 0) {
		WCS_WX1(w) = tp1
		WCS_WX2(w) = tp2
	    } else {
		WCS_WY1(w) = tp1
		WCS_WY2(w) = tp2
	    }

	    GP_WCSSTATE(gp) = MODIFIED
	}

	# Compute the coords of the axis endpoint and of the first tick in world
	# coords.

	if (scaling == LINEAR) {
	    wctick = tick1
	    wcp1 = p1
	} else if (scaling == LOG) {
	    wctick = 10.0 ** tick1
	    wcp1 = 10.0 ** p1
	} else {
	    wctick = aelogr (tick1)
	    wcp1 = aelogr (p1)
	}

	# Compute the number of minor ticks.  If we are log scaling there
	# are either no minor ticks or 8 minor ticks.  If the scaling is
	# linear the tick placement algorithm is used to compute the best
	# number of minor ticks, using GL_NMINOR as a close estimate.  If
	# NMINOR is negative automatic tick selection is disabled and exactly
	# abs(NMINOR) ticks will be drawn.  If NMINOR is zero no minor ticks
	# are drawn.

	if (GL_NMINOR(ap) == 0)			# no minor ticks
	    nminor = 0
	else if (logflag == YES)		# log scaling
	    nminor = 8
	else {
	    minor_step = glb_minorstep (tick1, tick1+step, GL_NMINOR(ap))
	    nminor = nint (abs (step / minor_step)) - 1
	}

	AX_NMINOR(ax1) = nminor
	AX_NMINOR(ax2) = nminor

	# Compute the step size in world coords between minor ticks and the
	# number of minor ticks to be drawn initially until the first major
	# tick (tick1) is reached.  Note that for ELOG scaling the minor
	# step size and number of minor ticks are different in the range
	# +-10 (which is linear) than elsewhere, but we ignore that here.

	if (scaling == LINEAR) {
	    minor_step = step / (nminor + 1)
	    AX_INLEFT(ax1) = abs (int ((wctick - wcp1) / minor_step))
	} else {
	    t1 = nint (tick1)
	    t2 = nint (tick1 + step)
	    if (scaling == LOG)
		minor_step = (10.0 ** t2 - 10.0 ** t1) / 9.
	    else
		minor_step = (aelogr(real(t2)) - aelogr(real(t1))) / 9.
	    if (nminor == 0)
		minor_step = minor_step * 9.
	    AX_INLEFT(ax1) = 0
	}

	AX_INLEFT(ax2) = AX_INLEFT(ax1)

	# Set KSTEP, the adjustment to the step size at each major tick.  This
	# is always 1.0 if the scale is linear.  Set KSTEP to negative if ELOG
	# scaling, to tell the drawing code to invert kstep (.1->10 or 10->.1)
	# when passing through the origin (necessary for ELOG scaling).  The
	# sign is not otherwise significant.  If heading toward the origin
	# initially then KSTEP is inverted for ELOG scaling vs LOG scaling.

	if (scaling == LINEAR) {
	    AX_IKSTEP(ax1) = 1.0
	} else if (scaling == ELOG) {
	    tval = p1
	    if (abs (tval + step) > abs(t1))
		AX_IKSTEP(ax1) = -10.0
	    else
		AX_IKSTEP(ax1) = -0.1
	} else
	    AX_IKSTEP(ax1) = 10.0 ** step
	AX_IKSTEP(ax2) = AX_IKSTEP(ax1)

	# Set those parameters which differ depending on whether the axis is
	# horizontal or vertical.

	if (angle == 0) {
	    AX_TICK1(ax1,1) = wctick - (AX_INLEFT(ax1) * minor_step)
	    AX_TICK1(ax2,1) = wctick - (AX_INLEFT(ax2) * minor_step)

	    if (GL_SETAXISPOS(ap) == YES) {
		AX_TICK1(ax1,2) = GL_AXISPOS1(ap)
		AX_TICK1(ax2,2) = GL_AXISPOS2(ap)
	    } else {
		AX_TICK1(ax1,2) = WCS_WY1(w)
		AX_TICK1(ax2,2) = WCS_WY2(w)
	    }

	    AX_ISTEP(ax1,1) = minor_step
	    AX_ISTEP(ax2,1) = minor_step

	    char_height = ggetr (gp, "ch")
	    if (char_height < EPSILON)
		char_height = DEF_CHARHEIGHT
	    char_height = char_height * GL_TICKLABELSIZE(ap)

	    AX_TICKLABELOFFSET(ax2,2) = 0.5 * char_height
	    AX_TICKLABELOFFSET(ax1,2) = -AX_TICKLABELOFFSET(ax2,2)

	    # Set gtext format for tick labels.
	    call strcpy ("hj=c,vj=t", AX_TICKLABELPOS(ax1), SZ_FORMAT)
	    call strcpy ("hj=c,vj=b", AX_TICKLABELPOS(ax2), SZ_FORMAT)

	} else {
	    if (GL_SETAXISPOS(ap) == YES) {
		AX_TICK1(ax1,1) = GL_AXISPOS1(ap)
		AX_TICK1(ax2,1) = GL_AXISPOS2(ap)
	    } else {
		AX_TICK1(ax1,1) = WCS_WX1(w)
		AX_TICK1(ax2,1) = WCS_WX2(w)
	    }

	    AX_TICK1(ax1,2) = wctick - (AX_INLEFT(ax1) * minor_step)
	    AX_TICK1(ax2,2) = wctick - (AX_INLEFT(ax2) * minor_step)

	    AX_ISTEP(ax1,2) = minor_step
	    AX_ISTEP(ax2,2) = minor_step

	    char_width = ggetr (gp, "cw")
	    if (char_width < EPSILON)
		char_width = DEF_CHARWIDTH
	    char_width = char_width * GL_TICKLABELSIZE(ap)

	    AX_TICKLABELOFFSET(ax2,1) = 0.5 * char_width
	    AX_TICKLABELOFFSET(ax1,1) = -AX_TICKLABELOFFSET(ax2,1)

	    call strcpy ("hj=r,vj=c", AX_TICKLABELPOS(ax1), SZ_FORMAT)
	    call strcpy ("hj=l,vj=c", AX_TICKLABELPOS(ax2), SZ_FORMAT)
	}

	# Set the tick parameters that are identical for the two axes and
	# which do not depend on whether the axis is horizontal or vertical.

	AX_DRAWTICKS(ax1)	= GL_DRAWTICKS(ap)
	AX_DRAWTICKS(ax2)	= GL_DRAWTICKS(ap)
	AX_TICKLABELSIZE(ax1)	= GL_TICKLABELSIZE(ap)
	AX_TICKLABELSIZE(ax2)	= GL_TICKLABELSIZE(ap)
	AX_TICKLABELCOLOR(ax1)	= GL_TICKLABELCOLOR(ap)
	AX_TICKLABELCOLOR(ax2)	= GL_TICKLABELCOLOR(ap)
	AX_TICKCOLOR(ax1)	= GL_TICKCOLOR(ap)
	AX_TICKCOLOR(ax2)	= GL_TICKCOLOR(ap)
	AX_GRIDCOLOR(ax1)	= GL_GRIDCOLOR(ap)
	AX_GRIDCOLOR(ax2)	= GL_GRIDCOLOR(ap)
	AX_AXISLABELSIZE(ax1)	= GL_AXISLABELSIZE(ap)
	AX_AXISLABELSIZE(ax2)	= GL_AXISLABELSIZE(ap)
	AX_AXISLABELCOLOR(ax1)	= GL_AXISLABELCOLOR(ap)
	AX_AXISLABELCOLOR(ax2)	= GL_AXISLABELCOLOR(ap)
	AX_AXISWIDTH(ax1)	= GL_AXISWIDTH(ap)
	AX_AXISWIDTH(ax2)	= GL_AXISWIDTH(ap)
	AX_AXISCOLOR(ax1)	= GL_AXISCOLOR(ap)
	AX_AXISCOLOR(ax2)	= GL_AXISCOLOR(ap)
	AX_MINORWIDTH(ax1)	= GL_MINORWIDTH(ap)
	AX_MINORWIDTH(ax2)	= GL_MINORWIDTH(ap)
	AX_MAJORWIDTH(ax1)	= GL_MAJORWIDTH(ap)
	AX_MAJORWIDTH(ax2)	= GL_MAJORWIDTH(ap)

	# Compute the number of digits of precision needed for the tick labels.
	AX_NDIGITS(ax1) = max (1, gt_ndigits (p1, p2, step))
	AX_NDIGITS(ax2) = AX_NDIGITS(ax1)

	# If both axes are to be drawn label ticks if enabled.  If only one
	# axis is to be drawn that is the axis that must be labelled.

	if (GL_DRAWAXES(ap) > 0) {
	    AX_LABELTICKS(ax1) = GL_LABELTICKS(ap)
	    AX_LABELTICKS(ax2) = GL_LABELTICKS(ap)
	}
	if (GL_DRAWAXES(ap) == 1 || GL_DRAWAXES(ap) == 3)
	    AX_LABELTICKS(ax2) = NO
	else if (GL_DRAWAXES(ap) == 2)
	    AX_LABELTICKS(ax1) = NO

	# The user may override the tick label format if desired.
	if (GL_TICKFORMAT(ap) == EOS) {
	    call sprintf (AX_TICKFORMAT(ax1), SZ_FORMAT, "%%0.%dg")
		call pargi (AX_NDIGITS(ax1) + 1)
	} else
	    call strcpy (GL_TICKFORMAT(ap), AX_TICKFORMAT(ax1), SZ_FORMAT)
	call strcpy (AX_TICKFORMAT(ax1), AX_TICKFORMAT(ax2), SZ_FORMAT)
end


# GLB_MINORSTEP -- Determine the step size for the minor ticks.  Adapted
# from a routine by J. Eisenhamer (STScI) which was based on some MONGO code.

real procedure glb_minorstep (x1, x2, nminor)

real	x1, x2		#I interval between major ticks
int	nminor		#I suggested number of minor ticks, or actual# if neg

int	iexp
real	amant, diff, num, range

begin
	range = abs (x2 - x1)
	if (nminor < 0)
	    return (range / real (-nminor + 1))
	else {
	    # Determine magnitude of the intervals.
	    diff = log10 (range / nminor)
	    iexp = int (diff)
	    if (diff < 0)
		iexp = iexp - 1
	    amant = diff - real(iexp)

	    # Determine an appropriate step size.
	    if (amant < 0.15)
		num = 1.0
	    else if (amant < 0.50)
		num = 2.0
	    else if (amant < 0.85)
		num = 5.0
	    else
		num = 10.0

	    return (num * 10.0**iexp)
	}
end
