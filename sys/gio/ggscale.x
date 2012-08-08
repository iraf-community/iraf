# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GGSCALE -- Get the WCS scale in world coords per NDC at the point (x,y).
# Used to convert offsets or sizes in NDC coordinates into world coordinates,
# and vice versa.  If log scaling is in use we can only locally approximate
# the scale.

procedure ggscale (gp, x, y, dx, dy)

pointer	gp			# graphics descriptor
real	x, y			# point for which scale is desired
real	dx, dy			# scale wcs/nds (output)

pointer	w
real	x1, x2, y1, y2, xs, ys, elog_scale
real	log10e, elogr()
data	log10e /0.434294482/

begin
	w = GP_WCSPTR(gp,GP_WCS(gp))

	x1 = WCS_WX1(w)
	x2 = WCS_WX2(w)
	y1 = WCS_WY1(w)
	y2 = WCS_WY2(w)
	xs = WCS_SX2(w) - WCS_SX1(w)
	ys = WCS_SY2(w) - WCS_SY1(w)

	switch (WCS_XTRAN(w)) {
	case LOG:
	    dx = (x / log10e) * log10 (x2 / x1) / xs
	case ELOG:
	    # The following is an approximation.
	    elog_scale = (elogr(x2) - elogr(x1)) / xs
	    if (x < 10.0)
		dx = (-x / log10e) * elog_scale
	    else if (x > 10.0)
		dx = (x / log10e) * elog_scale
	    else
		dx = (10. / log10e) * elog_scale
	default:
	    # LINEAR
	    dx = (x2 - x1) / xs
	}

	switch (WCS_YTRAN(w)) {
	case LOG:
	    dy = (y / log10e) * log10 (y2 / y1) / ys
	case ELOG:
	    # The following is an approximation.
	    elog_scale = (elogr(y2) - elogr(y1)) / ys
	    if (y < 10.0)
		dy = (-y / log10e) * elog_scale
	    else if (y > 10.0)
		dy = (y / log10e) * elog_scale
	    else
		dy = (10. / log10e) * elog_scale
	default:
	    # LINEAR
	    dy = (y2 - y1) / ys
	}
end
