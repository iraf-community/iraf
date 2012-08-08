# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include	"../lib/ids.h"

# nspp particulars
# base width of line
define	BASELW		8

# IDS_POLYMARKER -- Draw a polymarker.  The polymarker is defined by the array
# of points P, consisting of successive (x,y) coordinate pairs.  The first point
# is not plotted but rather defines the start of the polyline.  The remaining
# points define line segments to be drawn.

procedure ids_polymarker (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs

pointer	pm
int	i, len_p
int	linewidth
short	x,y

include	"../lib/ids.com"

begin
	if ( npts <= 0)
	    return

	len_p = npts * 2

	# Update polymarker attributes if necessary.

	pm = IDS_PMAP(i_kt)

	if (IDS_TYPE(i_kt) != PM_LTYPE(pm)) {
	    call ids_line(PM_LTYPE(pm))
	    IDS_TYPE(i_kt) = PM_LTYPE(pm)
	}
	if (IDS_WIDTH(i_kt) != PM_WIDTH(pm)) {
	    linewidth = int(real(BASELW) * GKI_UNPACKREAL(PM_WIDTH(pm)))
	    i_linewidth = max(1,linewidth)
	    IDS_WIDTH(i_kt) = PM_WIDTH(pm)
	}
	if (IDS_COLOR(i_kt) != PM_COLOR(pm)) {
	    i_linecolor = PM_COLOR(pm)
	    IDS_COLOR(i_kt) = PM_COLOR(pm)
	}

	for (i=1;  i <= len_p;  i=i+2) {
	    x = p[i]
	    y = p[i+1]
	    call ids_point (real(x)/GKI_MAXNDC, real(y)/GKI_MAXNDC, true)
	}
end
