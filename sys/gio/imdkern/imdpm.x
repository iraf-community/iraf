# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include	"imd.h"

# IMD_POLYMARKER -- Draw a polymarker.  The polymarker is defined by the array
# of points P, consisting of successive (x,y) coordinate pairs.

procedure imd_polymarker (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs

pointer	pm
int	i, len_p
int	x, y, oldx, oldy
include	"imd.com"

begin
	if (npts <= 0)
	    return

	len_p = npts * 2

	# Keep track of the number of drawing instructions since the last frame
	# clear.
	g_ndraw = g_ndraw + 1

	# Update polymarker attributes if necessary.

	pm = IMD_PMAP(g_kt)

	if (IMD_TYPE(g_kt) != PM_LTYPE(pm)) {
	    call imd_linetype (PM_LTYPE(pm))
	    IMD_TYPE(g_kt) = PM_LTYPE(pm)
	}
	if (IMD_WIDTH(g_kt) != PM_WIDTH(pm)) {
	    call idk_linewidth (g_out, nint (GKI_UNPACKREAL(PM_WIDTH(pm))))
	    IMD_WIDTH(g_kt) = PM_WIDTH(pm)
	}
	if (IMD_COLOR(g_kt) != PM_COLOR(pm)) {
	    call imd_color (PM_COLOR(pm))
	    IMD_COLOR(g_kt) = PM_COLOR(pm)
	}

	# Draw the polymarker.
	oldx = 0; oldy = 0
	for (i=1;  i <= len_p;  i=i+2) {
	    x = p[i]; y = p[i+1]
	    if (x != oldx || y != oldy) {
		call idk_move (g_out, x, y)
		call idk_draw (g_out, x, y)
	    }
	    oldx = x; oldy = y
	}
end
