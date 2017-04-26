# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gio.h>
include	"glabax.h"

# GLB_GETTICK -- Get the position and type of the next tick on an axis.
# Ticks are accessed sequentially.  There are three types of tick scalings,
# LINEAR, LOG, and ELOG.  The tick scaling need not necessarily agree with
# the WCS scaling, hence linear tick scaling might be used on a nonlinear
# coordinate system.  If the scaling is linear then the first tick need not
# fall at the endpoint of the axis.  If log (or elog) scaling is in use then
# the axis will have been rounded out to a decade and the first tick will
# necessarily fall on the axis endpoint.  The scalings are described by the
# following parameters:
#
#			(variables)
#	nleft			number of minor ticks left to next major tick
#	step(x,y)		displacement between minor ticks (world coords)
#
#			(constants)
#	tick1(x,y)		world coords of the first tick on an axis
#	nminor			number of minor ticks between major ticks
#	istep(x,y)		initial step (actual step may differ)
#	kstep(x,y)		adjustment to step at major ticks
# 
# KSTEP is unity if the scaling is linear.  The log scalings have a KSTEP of
# either 10.0 or 0.1.  A negative KSTEP value is used to flag ELOG scaling.
# ELOG, or extended range log scaling, is a log scaling which is defined for
# X <=0 as well as x > 0.  This function is logarithmic for values less than
# -10 or greater than 10, and linear in the range [-10:+10].  This complicates
# tick computation because the usual 8 minor ticks per decade characteristic
# of log scaling are not appropriate in the linear regime.  If the scaling
# is ELOG then we ignore NMINOR and ISTEP in the linear range, changing the
# values of these parameters temporarily to reflect 4 minor ticks with a tick
# spacing of 2.0.
#
# (Note to the reader: don't feel discouraged if you don't understand this
# (stuff, it is so complicated I don't understand it either!  Having to deal
# (with linear, log, and elog scaling with both major and minor ticks,
# (sometimes no minor ticks, with the axis starting at any part of the scale
# (seems an inherently difficult problem to program compactly.  Barring
# (programming each case separately, the best approach I could come up with was
# (to walkthrough the code separately for each case, from all initial
# (conditions, until it works for all cases.  If you have problems determine
# (the initial conditions (the case) and do a similar walkthough.  Of course,
# (if you make a change affecting one case, you may well make the code fail for
# (a different case.

int procedure glb_gettick (gp, ax, x, y, major_tick)

pointer	gp			# graphics descriptor
pointer	ax			# axis descriptor
real	x, y			# coordinates of next tick (output)
int	major_tick		# YES if next tick is a major tick

int	i, axis, wcs, w, scaling, nminor, expon
real	kstep, step, astep, ten, sx, sy, tolerance, pos, norm_pos
bool	glb_eq()
define	logscale_ 91

begin
	if (AX_DRAWTICKS(ax) == NO)
	    return (EOF)

	tolerance = TOL
	scaling = AX_SCALING(ax)
	nminor  = AX_NMINOR(ax)
	kstep   = AX_KSTEP(ax)

	if (AX_HORIZONTAL(ax) == YES)
	    axis = 1
	else
	    axis = 2

	# Count down a minor tick.  If nleft is negative then we are being
	# called for the first time for this axis.

	if (AX_NLEFT(ax) < 0) {

	    # Initialize everything and return coords of the first tick.
	    AX_KSTEP(ax) = AX_IKSTEP(ax)
	    AX_NLEFT(ax) = AX_INLEFT(ax)
	    do i = 1, 2 {
		AX_POS(ax,i)  = AX_TICK1(ax,i)
		AX_STEP(ax,i) = AX_ISTEP(ax,i)
	    }

	    step  = AX_STEP(ax,axis)
	    astep = abs (step)

	    if (AX_NLEFT(ax) == 0) {
		# Note that there may not be any minor ticks.
		major_tick = YES
		AX_NLEFT(ax) = nminor
		if (nminor > 0)
		    if (scaling == ELOG && (astep >= .99 && astep < 2.0)) {
			# Elog scaling in linear region.
			AX_NLEFT(ax) = 4
			if (step < 0)
			    step = -2.0
			else
			    step =  2.0
			AX_STEP(ax,axis) = step
		    }
	    } else {
		AX_NLEFT(ax) = AX_NLEFT(ax) - 1
		major_tick = NO
	    }

	    # Elog scaling in linear region.  KSTEP must be inverted as we
	    # pass through the origin.  This normally occurs upon entry to the
	    # linear region, but if we start out at +/- 10 we must set KSTEP
	    # to its linear value during setup.

	    if (scaling == ELOG && glb_eq(step,2.0))
		AX_KSTEP(ax) = -10.0

	} else {
	    # All ticks after the first tick.
	    do i = 1, 2
		AX_POS(ax,i) = AX_POS(ax,i) + AX_STEP(ax,i)
	    AX_NLEFT(ax) = AX_NLEFT(ax) - 1

	    # If we are log scaling the ticks will never have more than 2
	    # digits of precision.  Try to correct for the accumulation of
	    # error by rounding.  When log scaling the error increases by
	    # a factor of ten in each decade and can get quite large if
	    # the log scale covers a large range.

	    if (scaling != LINEAR) {
		pos = AX_POS(ax,axis)
		call fp_normr (pos, norm_pos, expon)
		pos = nint (norm_pos * 10.0) / 10.0
		pos = pos * (10.0 ** expon)
		AX_POS(ax,axis) = pos
	    }

	    if (AX_NLEFT(ax) < 0) {
		# Next tick is a major tick.  If log scaling we must reset
		# the tick parameters for the next decade.

		major_tick = YES
		AX_NLEFT(ax) = nminor

		# The following handles the special case of ELOG scaling in
		# the linear regime when the number of minor ticks is zero.
		# The step size in such a case is 9 to some power in the log
		# region and +/- 10 in the linear region.

		if (scaling == ELOG && nminor == 0) {
		    pos = AX_POS(ax,axis)
		    if (step < 0)
			ten = -10.
		    else
			ten = 10.

		    if (glb_eq (pos, 10.0)) {
			if (glb_eq (step, 10.0)) {
			    if (step < 0)
				AX_STEP(ax,axis) = -9.
			    else
				AX_STEP(ax,axis) = 9.
			    goto logscale_
			} else
			    step = ten
		    } else if (glb_eq (pos, 0.0)) {
			step = ten
			if (pos / step < 0)
			    AX_KSTEP(ax) = -0.1
			else
			    AX_KSTEP(ax) = -10.0
		    } else
			goto logscale_
		    AX_STEP(ax,axis) = step

		} else if (scaling != LINEAR) {
		    # Adjust the tick step by the kstep factor, provided we
		    # are not at the origin in ELOG scaling (the step is 1
		    # on either side of the origin for ELOG scaling).  Reset
		    # the step size to 1.0 if ELOG scaling and just coming out
		    # of the linear regime.
logscale_
		    step = AX_STEP(ax,axis)
		    if (scaling != ELOG || abs(AX_POS(ax,axis)) > 0.1) {
			if (scaling == ELOG && glb_eq (step, 2.0))
			    AX_STEP(ax,axis) = step / 2.0

			do i = 1, 2
			    AX_STEP(ax,i) = AX_STEP(ax,i) * abs (AX_KSTEP(ax))
		    }

		    # Adjust the step size to 2.0 if ELOG scaling and in the
		    # linear regime (initial step size of 1).

		    step = AX_STEP(ax,axis)
		    if (scaling == ELOG && glb_eq(step,1.0)) {
			if (step < 0)
			    step = -2.0
			else
			    step =  2.0
			AX_STEP(ax,axis) = step
		    }

		    # If elog scaling and we have just entered the linear
		    # regime, adjust the number of ticks and the KSTEP factor.

		    if (scaling == ELOG && glb_eq(step,2.0)) {
			# Elog scaling in linear region.  KSTEP must be
			# inverted as we pass through the origin.

			if (abs(AX_POS(ax,axis)) > 0.1)
			    AX_KSTEP(ax) = -10.0

			if (nminor > 0)
			    AX_NLEFT(ax) = 4
		    }
		}
	    } else
		major_tick = NO
	}

	x = AX_POS(ax,1)
	y = AX_POS(ax,2)

	# Return EOF if tick falls beyond end of axis.  The comparison is made
	# in NDC coords to avoid having to check if the WCS is increasing or
	# decreasing and to avoid the problems of comparing unnormalized
	# floating point numbers.

	wcs = GP_WCS(gp)
	w = GP_WCSPTR(gp,wcs)

	call gctran (gp, x,y, sx,sy, wcs, 0)
	if (sx - WCS_SX2(w) > tolerance || sy - WCS_SY2(w) > tolerance)
	    return (EOF)
	else
	    return (OK)
end


# GLB_EQ -- Compare two (near normalized) floating point numbers for
# equality, using the absolute value of the first argument.

bool procedure glb_eq (a, b)

real	a			# compare absolute value of this number
real	b			# to this positive number

begin
	return (abs (abs(a) - b) < 0.1)
end
