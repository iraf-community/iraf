include <gset.h>
include	"igi.h"

#  11/18/91 Fixed extra grid line.  ZGL

procedure ig_grid (igs)

#  ig_grid -- The GRID command.  Draw lines connecting major ticks.
#  This assumes that the grid spacing has been computed by a BOX command.

pointer	igs		# igi parameters structure

begin
	call lcmdcat (igs, YES)
	call cmdcat  (igs, YES)

	call ii_grid (igs)
end


procedure ii_grid (igs)

#  ii_grid -- Draw lines connecting major ticks.  The tick spacing is
#  set by BOX in the igi parameters MG_GXSTEP and MG_GYSTEP.  The current
#  line style and weight are used.

pointer	igs				# igi parameters structure

pointer	igps				# Plot parameters structure
pointer	gp				# GIO structure
real	left, right, bottom, top	# WCS
real	x, x1, x2
real	y, y1, y2
real	step
real	sign

begin
	igps = PLOT_PARMS(igs)

	if (IS_INDEF(MG_GSTEP(igps)))
	    # Need to use BOX first
	    return

	gp = GIO_GP(igs)

	# Get the WCS window
	call ggwind (gp, left, right, bottom, top)

	# Set the polyline type
	call setltype (igs, MG_LTYPEN(igps))

	# Set the line width
	call gsetr (gp, G_PLWIDTH, MG_LWEIGHT(igps))

	step = abs (MG_GXSTEP(igps))

	# Handle reversed axes
	x1 = min (left, right)
	x2 = max (left, right)

	if (x1 == 0.0)
	    sign = 0.0
	else
	    sign = x1 / abs (x1)

	# Make sure we start on a major tick
	x1 = real (int (x1 / step) + sign) * step
	if (x1 < min (left, right))
	    x1 = x1 + step

	for (x = x1;  x < x2;  x = x + step)
	    # Verticals
	    call gline (gp, x, bottom, x, top)

	step = abs (MG_GYSTEP(igps))

	# Handle reversed axes
	y1 = min (bottom, top)
	y2 = max (bottom, top)

	if (y1 == 0.0)
	    sign = 0.0
	else
	    sign = y1 / abs (y1)

	# Make sure we start on a major tick
	y1 = real (int (y1 / step) + sign) * step
	if (y1 < min (bottom, top))
	    y1 = y1 + step

	for (y = y1;  y < y2;  y = y + step)
	    # Horizontals
	    call gline (gp, left, y, right, y)

	# Reset the GIO pen to the igi pen
	call gamove (gp, MG_XPOS(igps), MG_YPOS(igps))

	call gflush (gp)
end
