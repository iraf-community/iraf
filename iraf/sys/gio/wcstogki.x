# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<gio.h>

# GPL_WCSTOGKI -- Transform world coordinates to GKI coordinates using the
# cached transformation parameters.  There are three possible types of scaling
# on either axis, linear, log, and "elog".  The latter is a piecewise log
# scaling function defined for all X, i.e., for negative as well as positive
# X (see elogr.x).  If a negative number is transformed with normal log
# scaling it is treated as an indefinite, i.e., plotted as a gap in the plot.

procedure gpl_wcstogki (gp, wx, wy, mx, my)

pointer	gp			# graphics device descriptor
real	wx, wy			# world coordinates of point
real	mx, my			# metacode coordinates of point

real	x, y
real	elogr()
include	"gpl.com"

begin
	# Update cached transformation parameters if device changes, cache
	# has been invalidated, or the current WCS has been changed.

	if (gp != gp_out || GP_WCS(gp) != wcs)
	    call gpl_cache (gp)
	    
	# Transform the coordinates.

	if (xtran == LINEAR) {
	    x = wx
	} else if (xtran == LOG) {
	    if (wx <= 0) {
		call gpl_flush()
		return
	    } else
		x = log10 (wx)
	} else
	    x = elogr (wx)

	if (ytran == LINEAR) {
	    y = wy
	} else if (ytran == LOG) {
	    if (wy <= 0) {
		call gpl_flush()
		return
	    } else
		y = log10 (wy)
	} else
	    y = elogr (wy)

	# Return real rather than int GKI coordinates to avoid digitization
	# errors in a sequence of draws relative to the current pen position.

	mx = max (0.0, min (real(GKI_MAXNDC),
	    ((x - wxorigin) * xscale) + mxorigin))
	my = max (0.0, min (real(GKI_MAXNDC),
	    ((y - wyorigin) * yscale) + myorigin))
end
