# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gio.h>
include	"glabax.h"

# GLB_SET_VIEWPORT -- If the viewport has not yet been set, i.e., if the
# viewport is still [0:1,0:1], compute the size of the largest viewport which
# leaves sufficient room around the border for the axis labels and plot title.
# If a nonzero aspect ratio is specified make the viewport have that aspect
# ratio.

procedure glb_set_viewport (gp, ntitlelines, xlabel, ylabel)

pointer	gp			# graphics descriptor
int	ntitlelines		# number of lines to reserve for title block
char	xlabel[ARB]		# x axis label
char	ylabel[ARB]		# y axis label

pointer	w, xap, yap
bool	draw_title, draw_xlabel, draw_ylabel, draw_xticks, draw_yticks
real	char_height, char_width
real	aspect, cur_aspect, dev_aspect, dx, dy
real	xwidth, ywidth, yreserve
real	ggetr()

begin
	w = GP_WCSPTR (gp, GP_WCS(gp))
	xap = GP_XAP(gp)
	yap = GP_YAP(gp)

	if ((WCS_SX1(w) > EPSILON) || (abs(1.0 - WCS_SX2(w)) > EPSILON) ||
	    (WCS_SY1(w) > EPSILON) || (abs(1.0 - WCS_SY2(w)) > EPSILON))
	    return

	draw_title  = (ntitlelines > 0 && GP_DRAWTITLE(gp) == YES)
	draw_xticks = (GL_DRAWAXES(xap) > 0 && GL_LABELTICKS(xap) == YES)
	draw_xlabel =
	    (draw_xticks && xlabel[1] != EOS && GL_LABELAXIS(xap) == YES)
	draw_yticks = (GL_DRAWAXES(yap) > 0 && GL_LABELTICKS(yap) == YES)
	draw_ylabel =
	    (draw_yticks && ylabel[1] != EOS && GL_LABELAXIS(yap) == YES)

	char_width  = ggetr (gp, "cw")
	char_height = ggetr (gp, "ch")

	if (char_width < EPSILON)
	    char_width = DEF_CHARWIDTH
	if (char_height < EPSILON)
	    char_height = DEF_CHARHEIGHT

	# X axis.
	if (draw_yticks && draw_ylabel)
	    xwidth = max (4, LEFT_BORDER + 2)
	else if (draw_yticks)
	    xwidth = max (4, LEFT_BORDER)
	else
	    xwidth = 0
	xwidth = xwidth * char_width * GL_TICKLABELSIZE(xap)
	    
	# Y axis.
	if (draw_xticks && draw_xlabel)
	    ywidth = BOTTOM_BORDER
	else if (draw_xticks)
	    ywidth = max (2, (BOTTOM_BORDER - 2))
	else
	    ywidth = 0
	ywidth = ywidth * char_height * GL_TICKLABELSIZE(yap)

	# Compute amount of extra space to allow for the title block, which
	# may contain more than one line.

	if (!draw_title && !draw_xticks && !draw_yticks)
	    yreserve = 0
	else if (!draw_title && GP_ASPECT(gp) > 0.9)
	    yreserve = 0
	else {
	    yreserve = min (MAX_SZTITLEBLOCK,
		max (MIN_NTITLELINES, ntitlelines + 1) *
		char_height * GP_TITLESIZE(gp))
	}

	# Set the viewport.  The viewport is the largest area yielding the
	# desired borders.  The viewport is centered in X and positioned just
	# below the title block in Y.

	WCS_SX1(w) = xwidth
	WCS_SX2(w) = 1.0 - xwidth
	WCS_SY1(w) = ywidth
	WCS_SY2(w) = 1.0 - yreserve

	# Adjust the viewport to achieve the specified aspect ratio, if a
	# nonzero aspect ratio was given.

	dev_aspect = GP_DEVASPECT(gp)		# device aspect ratio
	aspect     = GP_ASPECT(gp)		# user desired aspect ratio

	if (aspect > EPSILON) {
	    dx = WCS_SX2(w) - WCS_SX1(w)
	    dy = WCS_SY2(w) - WCS_SY1(w)
	    cur_aspect = dy / dx * dev_aspect

	    if (cur_aspect > aspect) {
		# Viewport is taller than desired.
		dy = aspect / dev_aspect * dx
		WCS_SY1(w) = (1.0 - dy) / 2.0
		WCS_SY2(w) = 1.0 - WCS_SY1(w)
	    } else {
		# Viewport is not as wide as desired.
		dx = dev_aspect * dy / aspect
		WCS_SX1(w) = (1.0 - dx) / 2.0
		WCS_SX2(w) = 1.0 - WCS_SX1(w)
	    }
	}

	GP_WCSSTATE(gp) = MODIFIED
end
