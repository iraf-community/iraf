# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include	"../lib/ids.h"

# nspp particulars
# base width of line
define	BASELW		8

# IDS_POLYLINE -- Draw a polyline.  The polyline is defined by the array of
# points P, consisting of successive (x,y) coordinate pairs.  The first point
# is not plotted but rather defines the start of the polyline.  The remaining
# points define line segments to be drawn.

procedure ids_polyline (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs

pointer	pl
int	i, len_p
int	linewidth

include	"../lib/ids.com"

begin
	if ( npts <= 0)
	    return

	len_p = npts * 2

	# Update polyline attributes if necessary.

	pl = IDS_PLAP(i_kt)

	if (IDS_TYPE(i_kt) != PL_LTYPE(pl)) {
	    call ids_line(PL_LTYPE(pl))
	    IDS_TYPE(i_kt) = PL_LTYPE(pl)
	}
	if (IDS_WIDTH(i_kt) != PL_WIDTH(pl)) {
	    linewidth = int(real(BASELW) * GKI_UNPACKREAL(PL_WIDTH(pl)))
	    i_linewidth = max(1,linewidth)
	    IDS_WIDTH(i_kt) = PL_WIDTH(pl)
	}
	if (IDS_COLOR(i_kt) != PL_COLOR(pl)) {
	    i_linecolor = PL_COLOR(pl)
	    IDS_COLOR(i_kt) = PL_COLOR(pl)
	}

	# Move to the first point.  point() will plot it, which is
	# ok here, and vector may well plot it again.

	call ids_point(p[1], p[2], true)

	# Draw the polyline.

	for (i=3;  i <= len_p;  i=i+2) {
	    call ids_vector ( p[i], p[i+1])

	}
end
