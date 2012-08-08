# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include	"gkt.h"

# Nspp particulars.
define	BASELW		8	# base width of line


# GKT_POLYMARKER -- Draw a polymarker.  The polymarker is defined by the array
# of points P, consisting of successive (x,y) coordinate pairs.

procedure gkt_polymarker (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs

pointer	pm
int	i, len_p
int	x, y, oldx, oldy
include	"gkt.com"

begin
	if (npts <= 0)
	    return

	len_p = npts * 2

	# Keep track of the number of drawing instructions since the last frame
	# clear.
	g_ndraw = g_ndraw + 1

	# Update polymarker attributes if necessary.

	pm = GKT_PMAP(g_kt)

	if (GKT_TYPE(g_kt) != PM_LTYPE(pm)) {
	    call gkt_linetype (PM_LTYPE(pm))
	    GKT_TYPE(g_kt) = PM_LTYPE(pm)
	}
	if (GKT_WIDTH(g_kt) != PM_WIDTH(pm)) {
	    if (GKI_UNPACKREAL(PM_WIDTH(pm)) < 1.5)
		call optn (*"inten", *"low")
	    else
		call optn (*"inten", *"high")
	    GKT_WIDTH(g_kt) = PM_WIDTH(pm)
	}
	if (GKT_COLOR(g_kt) != PM_COLOR(pm)) {
	    call gkt_color (PM_COLOR(pm))
	    GKT_COLOR(g_kt) = PM_COLOR(pm)
	}

	# Get to start of marker.
	call frstpt (real(x)/GKI_MAXNDC, real(y)/GKI_MAXNDC)
	oldx = 0; oldy = 0

	# Draw the polymarker.
	for (i=1;  i <= len_p;  i=i+2) {
	    x = p[i]; y = p[i+1]
	    if (x != oldx && y != oldy)
		call point (real(x)/GKI_MAXNDC, real(y)/GKI_MAXNDC)
	    oldx = x; oldy = y
	}
end
