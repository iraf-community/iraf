# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<gio.h>

# GMARK -- Draw a marker of the indicated type and size.  A mark is represented
# as a polyline normalized to the unit square.  Drawing a mark is a simple
# matter of drawing this normalized polyline in a window the size and position
# of the mark.  While the mark window defines the transformation upon the
# normalized marker polyline, clipping is performed on the WCS viewport boundary
# (if enabled), independently of the size and position of the mark.  Redrawing
# a mark with a linetype of clear will erase the mark, device permitting.
# Drawing is carried out in world coordinates, hence the marker shape will
# relect logarithmic scaling if in effect.

procedure gmark (gp, x, y, marktype, xsize, ysize)

pointer	gp			# graphics descriptor
real	x, y			# world coordinates of center of marker
int	marktype		# type of marker to be drawn
real	xsize, ysize		# marker size in X and Y

int	i, m, fill
int	and()
include	"markers.inc"

begin
	# The point marker type cannot be combined with the other types and
	# is treated as a special case.  The remaining markers are drawn
	# using GUMARK, which draws marks represented as polygons

	if (marktype == GM_POINT || (xsize == 0 && ysize == 0)) {
	    call gpl_settype (gp, POLYMARKER)
	    call gamove (gp, x, y)
	    call gadraw (gp, x, y)
	    call gpl_settype (gp, POLYLINE)

	} else {
	    # Some marks can be drawn using area fill.
	    if (and (marktype, GM_FILL) != 0)
		fill = YES
	    else
		fill = NO

	    # Draw and overlay each mark.  The polylines for the standard
	    # marks are stored in MPX and MPY at offsets MXO and MYO.

	    do i = GM_FIRSTMARK, GM_LASTMARK
		if (and (marktype, 2 ** i) != 0) {
		    m = i - GM_FIRSTMARK + 1
		    call gumark (gp, mpx[moff[m]], mpy[moff[m]], mnpts[m],
			x, y, xsize, ysize, fill)
		}
	}
end
