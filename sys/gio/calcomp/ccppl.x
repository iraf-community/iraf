# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include <gset.h>
include	"ccp.h"

# CCP_POLYLINE -- Set up a polyline.  The polyline is defined by the array of
# points P, consisting of successive (x,y) coordinate pairs.  The first point
# is not plotted unless it is the only point, but rather defines the start of
# the polyline.  The remaining points define line segments to be drawn.

procedure ccp_polyline (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs

pointer	pl, xseg,yseg
int	i, curpl_pt, nsegpts
int	len_p, segsize, lsize

include	"ccp.com"

begin
	if (npts <= 0)
	    return

	len_p = npts * 2

	# Keep track of number of drawing instructions since last frame clear.
	g_ndraw = g_ndraw + 1

	# Update polyline attributes if necessary.
	pl = CCP_PLAP(g_cc)

	if (CCP_LTYPE(g_cc) != PL_LTYPE(pl)) {
	    call ccp_linetype (PL_LTYPE(pl))		# set g_ltype in ccp.com
	    CCP_LTYPE(g_cc) = PL_LTYPE(pl)
	}
	if (CCP_WIDTH(g_cc) != PL_WIDTH(pl)) {
	    if (GKI_UNPACKREAL(PL_WIDTH(pl)) < 1.5) {
		CCP_WIDTH(g_cc) = GKI_PACKREAL(PL_SINGLE)
	    } else 
	        CCP_WIDTH(g_cc) = PL_WIDTH(pl)
	}
	if (CCP_COLOR(g_cc) != PL_COLOR(pl)) {
	    call ccp_color (PL_COLOR(pl))
	    CCP_COLOR(g_cc) = PL_COLOR(pl)
	}

	# If the overrides are on, or linetype is solid and linewidth is single,
	# do simple move and draws:

	if ((g_ltover && g_lwover) || (!g_lwover && g_lwtype == 'p') ||
	    (g_ltype == GL_SOLID && CCP_WIDTH(g_cc) == GKI_PACKREAL(PL_SINGLE))
	    || (g_ltover && CCP_WIDTH(g_cc) == GKI_PACKREAL(PL_SINGLE)) ||
	    (g_ltype == GL_SOLID && g_lwover)) {

	    if (g_lwtype == 'p') 
		call newpen (PL_WIDTH(pl))

	    call plot (XTRAN(p[1]), YTRAN(p[2]), CCP_UP)
	    if (npts == 1) {
		call plot (XTRAN(p[1]), YTRAN(p[2]), CCP_DOWN)
	    } else {		# draw normally
		do i = 3, len_p, 2 
		    call plot (XTRAN(p[i]), YTRAN(p[i+1]), CCP_DOWN)
	    }

	    # Store maximum-x point plotted for a "newframe" in ccp_clear.
	    do i = 1, len_p, 2
		g_max_x = max (XTRAN(p[i]), g_max_x)


	# Otherwise, must calculate individual segments of dashes and dots,
	# keeping their lengths constant along polyline (ccp_calcseg), before 
	# optionally simulating bold and drawing (ccp_drawseg):

	} else {		# vector polyline; simulate linetype, linewidth

	    segsize = SEGSIZE
	    call malloc (xseg, segsize, TY_REAL)
	    call malloc (yseg, segsize, TY_REAL)

	    curpl_pt = 1
	    lsize    = nint(GKI_UNPACKREAL(CCP_WIDTH(g_cc)))
	    if (!g_ltover && (g_ltype >= GL_DASHED && g_ltype <= GL_DOTDASH)) {

		while (curpl_pt <= npts) {
		    call ccp_calcseg (p, npts, g_ltype, curpl_pt, segsize,
		    		      xseg,yseg, nsegpts)
		    call ccp_drawseg (Memr[xseg],Memr[yseg], nsegpts, lsize)
		}

	    } else {	# either (ltype override or solid line), not single wid.

		call ccp_calcseg (p, npts, GL_SOLID, curpl_pt, segsize, xseg,
		    yseg, nsegpts)
		call ccp_drawseg (Memr[xseg],Memr[yseg], nsegpts, lsize)
	    }

	    call mfree (xseg, TY_REAL)
	    call mfree (yseg, TY_REAL)
	}

end
