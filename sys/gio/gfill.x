# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GFILL -- Fill area.  Fill the area defined by the polygon (X[i],Y[i]) in the
# indicated style.

procedure gfill (gp, x, y, npts, style)

pointer gp			# graphics descriptor
real	x[ARB], y[ARB]		# polygon
int	npts			# npts in polygon
int	style			# style for area fill

pointer	ap

begin
	call gpl_flush()

	ap = GP_FAAP(gp)
	if (style != FA_STYLE(ap) || FA_STATE(ap) != FIXED) {
	    FA_STYLE(ap) = style
	    call gki_faset (GP_FD(gp), ap)
	    FA_STATE(ap) = FIXED
	}

	call gpl_settype (gp, FILLAREA)
	call gpline (gp, x, y, npts)
	call gpl_settype (gp, POLYLINE)
end
