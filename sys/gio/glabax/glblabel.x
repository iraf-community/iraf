# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gset.h>
include	<gio.h>
include	"glabax.h"

# GLB_LABEL_AXIS -- Label an axis.  If both axes were drawn only the first is
# labelled, otherwise the label is placed on withever axis was drawn.  This is
# done by drawing the axis labels just outside the tick mark labels, wherever
# those happened to be.  The axis label offset is in the same direction as the
# tick label offset and is centered on each axis.  The distance from the axis
# is a function of the size of the tick labels.

procedure glb_label_axis (gp, ax, xlabel, ylabel)

pointer	gp			# graphics descriptor
pointer	ax			# axis descriptor
char	xlabel[ARB]		# X axis label
char	ylabel[ARB]		# Y axis label

int	wcs
real	x1, x2, y1, y2, x, y, dx, dy
real	char_height, char_width
int	strlen()
real	ggetr()

begin
	wcs = GP_WCS(gp)

	# Get character height and width in NDC coords.
	char_height = ggetr (gp, "ch")
	char_width  = ggetr (gp, "cw")

	if (char_height < EPSILON)
	    char_height = DEF_CHARHEIGHT
	if (char_width < EPSILON)
	    char_width = DEF_CHARWIDTH

	# Compute axis center in NDC coords.
	call gctran (gp, AX_START(ax,1), AX_START(ax,2), x1,y1, wcs, 0)
	call gctran (gp,   AX_END(ax,1),   AX_END(ax,2), x2,y2, wcs, 0)
	x = (x1 + x2) / 2.0
	y = (y1 + y2) / 2.0

	# Set relative text size and get device character size for a text
	# size of 1.0.  Set WCS to NDC coords since the offset to the
	# tick label is in NDC coordinates.

	call gsetr (gp, G_TXSIZE, AX_AXISLABELSIZE(ax))
	call gseti (gp, G_WCS, 0)

	# Draw the axis label.

	if (AX_HORIZONTAL(ax) == YES) {
	    # Axis is horizontal.  Tick label vector tells us whether to
	    # draw axis label above or below axis.

	    if (strlen (xlabel) > 0) {
		dy = 2.0 * AX_TICKLABELSIZE(ax) * char_height +
		     0.5 * AX_AXISLABELSIZE(ax) * char_height
		if (AX_TICKLABELOFFSET(ax,2) < 0)
		    dy = -dy
		call gtext (gp, x, y + dy, xlabel, "hj=c;vj=c")
	    }
	} else {
	    # Axis is vertical.  Always put label fixed distance from axis
	    # regardless of size of tick labels (for consistency and to
	    # avoid clipping at the device screen boundary).  Label runs
	    # bottom to top in a vertical field with char up pointing to
	    # the left.

	    if (strlen (ylabel) > 0) {
		dx = (Y_LABELOFFSET * char_width * AX_TICKLABELSIZE(ax)) + 
		    0.5 * AX_AXISLABELSIZE(ax) * char_height
		    
		if (AX_TICKLABELOFFSET(ax,1) < 0)
		    dx = -dx
		call gtext (gp, x + dx, y, ylabel, "up=180;hj=c;vj=c")
	    }
	}

	call gseti (gp, G_WCS, wcs)
end
