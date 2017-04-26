# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include	"gkt.h"

# GKT_POLYLINE -- Draw a polyline.  The polyline is defined by the array of
# points P, consisting of successive (x,y) coordinate pairs.  The first point
# is not plotted but rather defines the start of the polyline.  The remaining
# points define line segments to be drawn.

procedure gkt_polyline (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs

pointer	pl
int	i, len_p
int	x,y
include	"gkt.com"

begin
	if (npts <= 0)
	    return

	len_p = npts * 2

	# Keep track of the number of drawing instructions since the last frame
	# clear.
	g_ndraw = g_ndraw + 1

	# Update polyline attributes if necessary.
	pl = GKT_PLAP(g_kt)

	if (GKT_TYPE(g_kt) != PL_LTYPE(pl)) {
	    call gkt_linetype (PL_LTYPE(pl))
	    GKT_TYPE(g_kt) = PL_LTYPE(pl)
	}
	if (GKT_WIDTH(g_kt) != PL_WIDTH(pl)) {
	    if (GKI_UNPACKREAL(PL_WIDTH(pl)) < 1.5)
		call optn (*"inten", *"low")
	    else
		call optn (*"inten", *"high")
	    GKT_WIDTH(g_kt) = PL_WIDTH(pl)
	}
	if (GKT_COLOR(g_kt) != PL_COLOR(pl)) {
	    call gkt_color (PL_COLOR(pl))
	    GKT_COLOR(g_kt) = PL_COLOR(pl)
	}

	# Transform the first point from GKI coords to nspp coords and
	# move to the transformed point.

	x = p[1]
	y = p[2]
	call frstpt(real(x)/GKI_MAXNDC, real(y)/GKI_MAXNDC)

	# Draw the polyline.

	for (i=3;  i <= len_p;  i=i+2) {
	    x = p[i]
	    y = p[i+1]
	    call vector (real(x)/GKI_MAXNDC, real(y)/GKI_MAXNDC)
	}
end
