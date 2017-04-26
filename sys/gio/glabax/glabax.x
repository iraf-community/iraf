# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gset.h>
include	<gio.h>
include	"glabax.h"

# GLABAX -- Draw and label the axes of the plot (normally the viewport
# boundary).  This is done in two steps.  First we compute all the required
# parameters, and then we draw and label the axes.  Up to four axes can be
# drawn.  To simplify matters, all four axes are treated equally and
# independently.  The axes are drawn a tick at a time in world coordinates.

procedure glabax (gp, title, xlabel, ylabel)

pointer	gp			# graphics descriptor
char	title[ARB]		# plot title (may be more than one line)
char	xlabel[ARB]		# X axis label
char	ylabel[ARB]		# Y axis label

char	label[SZ_LABEL]
int	axis, wcs, ntitlelines, ip, major_tick
int	save_plcolor, save_txcolor, save_facolor
int	save_pltype, save_clip, save_txfont
real	xv[4], yv[4], x1, x2, y1, y2
real	save_plwidth, save_txsize
real	dx, dy, x, y, sx, sy, scalar, wc, wstep
pointer	sp, axes[4], ax, w

real	gstatr()
bool	ttygetb()
int	gstati(), glb_gettick()
errchk	glb_setup, gadraw, grdraw, gamove, gtext
errchk	glb_label_axis, glb_plot_title, glb_gettick

begin
	call smark (sp)
	call salloc (axes[1], LEN_AX, TY_STRUCT)
	call salloc (axes[2], LEN_AX, TY_STRUCT)
	call salloc (axes[3], LEN_AX, TY_STRUCT)
	call salloc (axes[4], LEN_AX, TY_STRUCT)

	wcs = GP_WCS(gp)
	w   = GP_WCSPTR(gp,wcs)

	# Count the number of lines in the title block.
	ntitlelines = 0
	if (title[1] != EOS) {
	    for (ip=1;  title[ip] != EOS;  ip=ip+1)
		if (title[ip] == '\n' && title[ip+1] != EOS)
		    ntitlelines = ntitlelines + 1
	    ntitlelines = ntitlelines + 1
	}
	ntitlelines = max (ntitlelines, GP_NTITLELINES(gp))

	# Fix the coordinates systems and set the axis drawing parameters.
	# The number of lines in the title block is needed to determine how
	# much space to allow at the top of the screen.

	call glb_setup (gp, axes, ntitlelines, xlabel, ylabel)

	# Save the values of any user parameters we must change while drawing
	# the axes.

	save_pltype  = gstati (gp, G_PLTYPE)
	save_plwidth = gstatr (gp, G_PLWIDTH)
	save_plcolor = gstati (gp, G_PLCOLOR)
	save_txfont  = gstati (gp, G_TXFONT)
	save_txsize  = gstatr (gp, G_TXSIZE)
	save_txcolor = gstati (gp, G_TXCOLOR)
	save_facolor = gstati (gp, G_FACOLOR)
	save_clip    = WCS_CLIP(w)

	# Prepare the background.
	if (ttygetb (GP_TTY(gp), "fa") &&
	    GP_FRAMECOLOR(gp) != 0 && GP_FRAMEDRAWN(gp) == NO) {

	    call ggview (gp, x1, x2, y1, y2)
	    call gseti (gp, G_WCS, 0)
	    call gseti (gp, G_CLIP, NO)

	    xv[1] = 0.0;  yv[1] = 0.0
	    xv[2] = 1.0;  yv[2] = 0.0
	    xv[3] = 1.0;  yv[3] = 1.0
	    xv[4] = 0.0;  yv[4] = 1.0
	    call gseti (gp, G_FACOLOR, GP_FRAMECOLOR(gp))
	    call gfill (gp, xv, yv, 4, GF_SOLID)

	    xv[1] = x1;  yv[1] = y1
	    xv[2] = x2;  yv[2] = y1
	    xv[3] = x2;  yv[3] = y2
	    xv[4] = x1;  yv[4] = y2
	    call gseti (gp, G_FACOLOR, 0)
	    call gfill (gp, xv, yv, 4, GF_SOLID)

	    call gseti (gp, G_CLIP, save_clip)
	    call gseti (gp, G_WCS, wcs)
	    GP_FRAMEDRAWN(gp) = YES
	}

	# Draw and label the four axes.  First set the linetype and linewidth
	# to be used to draw the axes and ticks; these may be different than
	# that used to plot the data.  Draws are preferred to moves to minimize
	# the number of polylines needed to draw the axis.  An axis is drawn
	# by moving to the start of the axis, drawing each tick in sequence,
	# and then moving to the end of the axis.  Tick labels are drawn at
	# the major ticks if required.  The axes and ticks must be drawn in
	# world coords to get the proper scaling.  Clipping is turned off while
	# drawing the axes to avoid clipping portions of the axes due to small
	# floating point errors.

	call gseti (gp, G_PLTYPE, 1)
	call gseti (gp, G_CLIP, NO)
	call gseti (gp, G_TXFONT, GT_BOLD)

	do axis = 1, 4 {
	    ax = axes[axis]
	    if (AX_DRAWME(ax) == NO)
		next

#  call eprintf ("axis %d: tick1=(%g,%g) istep=(%g,%g) kstep=%g\n")
#  call pargi (axis)
#  call pargr (AX_TICK1(ax,1)); call pargr (AX_TICK1(ax,2))
#  call pargr (AX_ISTEP(ax,1)); call pargr (AX_ISTEP(ax,2))
#  call pargr (AX_IKSTEP(ax))
#  call eprintf ("\tstart=(%g,%g) end=(%g,%g)\n")
#  call pargr (AX_START(ax,1)); call pargr (AX_START(ax,2))
#  call pargr (AX_END(ax,1)); call pargr (AX_END(ax,2))
#  call eprintf ("nminor=%d, inleft=%d, minortick=(%g,%g), majortick=(%g,%g)\n")
#  call pargi (AX_NMINOR(ax)); call pargi (AX_INLEFT(ax))
#  call pargr (AX_MINORTICK(ax,1)); call pargr (AX_MINORTICK(ax,2))
#  call pargr (AX_MAJORTICK(ax,1)); call pargr (AX_MAJORTICK(ax,2))

	    # Set the axis linewidth and move to the start of the axis.
	    call gsetr (gp, G_PLWIDTH, AX_AXISWIDTH(ax))
	    call gseti (gp, G_PLCOLOR, AX_AXISCOLOR(ax))
	    call gamove (gp, AX_START(ax,1), AX_START(ax,2))

	    # Draw the axis and label the major ticks if so indicated.
	    # First set flag to initialize glb_gettick.

	    AX_NLEFT(ax) = -1
	    while (glb_gettick (gp, ax, x, y, major_tick) != EOF) {

		# Advance to the next tick.
		call gsetr (gp, G_PLWIDTH, AX_AXISWIDTH(ax))
		call gseti (gp, G_PLCOLOR, AX_AXISCOLOR(ax))
		call gadraw (gp, x, y)

		if (major_tick == YES) {
		    # Draw a major tick.

		    call gsetr (gp, G_PLWIDTH, AX_MAJORWIDTH(ax))
		    call gseti (gp, G_PLCOLOR, AX_TICKCOLOR(ax))
		    dx = AX_MAJORTICK(ax,1)
		    dy = AX_MAJORTICK(ax,2)
		    call grdraw (gp, dx, dy)
		    call grdraw (gp, -dx, -dy)

		    if (AX_LABELTICKS(ax) == YES) {
			# Get the tick label position in NDC coords.  World
			# coords cannot be used for an offset outside the
			# viewport as the coords might be indefinite if log
			# scaling.

			call gseti (gp, G_WCS, 0)
			call gcurpos (gp, sx, sy)
			dx = AX_TICKLABELOFFSET(ax,1)
			dy = AX_TICKLABELOFFSET(ax,2)

			# Format the numeric tick label string.  The scalar
			# multiplier is used to compute the step size between
			# major ticks.

			scalar = AX_NMINOR(ax) + 1.0
			if (AX_HORIZONTAL(ax) == YES) {
			    wc = x
			    wstep = AX_STEP(ax,1) * scalar
			} else {
			    wc = y
			    wstep = AX_STEP(ax,2) * scalar
			}

			# Draw the label string.
			call gsetr (gp, G_TXSIZE, AX_TICKLABELSIZE(ax))
			call gseti (gp, G_TXCOLOR, AX_TICKLABELCOLOR(ax))

			# If log scaling, label the ticks in log units.
			if (AX_SCALING(ax) == LINEAR) {
			    call glb_encode (wc, label, SZ_LABEL,
				AX_TICKFORMAT(ax), wstep)
			    call gtext (gp, sx + dx, sy + dy, label,
				AX_TICKLABELPOS(ax))
			} else {
			    call glb_loglab (gp, sx+dx, sy+dy, wc,
				AX_TICKLABELPOS(ax), AX_SCALING(ax))
			}

			# Leave the pen back at the base of the tick.
			call gamove (gp, sx, sy)
			call gseti (gp, G_WCS, wcs)
		    } 

		} else {
		    # Draw a minor tick.

		    dx = AX_MINORTICK(ax,1)
		    dy = AX_MINORTICK(ax,2)

		    call gsetr (gp, G_PLWIDTH, AX_MINORWIDTH(ax))
		    call gseti (gp, G_PLCOLOR, AX_TICKCOLOR(ax))
		    call grdraw (gp, dx, dy)
		    call grdraw (gp, -dx, -dy)
		}
	    }

	    # Draw line segment from last tick to the end of the axis.
	    call gadraw (gp, AX_END(ax,1), AX_END(ax,2))

	    # Flush the graphics output.  When working interactively, this
	    # gives the user something to watch while we generate the rest
	    # of the plot.

	    if (AX_NMINOR(ax) > 0)
		call gflush (gp)
	}

	# Draw grid between major ticks.
	if (GL_DRAWGRID (GP_XAP(gp)) == YES) {
	    call gseti (gp, G_PLCOLOR, AX_GRIDCOLOR(axes[3]))
	    call glb_drawgrid (gp, axes[3], axes[2])
	}
	if (GL_DRAWGRID (GP_YAP(gp)) == YES) {
	    call gseti (gp, G_PLCOLOR, AX_GRIDCOLOR(axes[1]))
	    call glb_drawgrid (gp, axes[1], axes[4])
	}

	# Label the X and Y axes.
	do axis = 1, 4 {
	    ax = axes[axis]
	    if (AX_DRAWME(ax) == YES && AX_LABELTICKS(ax) == YES) {
		call gseti (gp, G_TXCOLOR, AX_AXISLABELCOLOR(ax))
		call glb_label_axis (gp, ax, xlabel, ylabel)
	    }
	}

	# Draw plot title block.
	call gseti (gp, G_TXCOLOR, GP_TITLECOLOR(gp))
	call glb_plot_title (gp, title, ntitlelines)

	# Restore the parameters we were originally called with.
	call gseti (gp, G_WCS, wcs)
	call gseti (gp, G_CLIP,    save_clip)
	call gseti (gp, G_PLTYPE,  save_pltype)
	call gsetr (gp, G_PLWIDTH, save_plwidth)
	call gseti (gp, G_PLCOLOR, save_plcolor)
	call gsetr (gp, G_TXSIZE,  save_txsize)
	call gseti (gp, G_TXFONT,  save_txfont)
	call gseti (gp, G_TXCOLOR, save_txcolor)
	call gseti (gp, G_FACOLOR, save_facolor)

	call gflush (gp)
	call sfree (sp)
end
