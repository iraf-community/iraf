# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<gio.h>

# GPL_CACHE -- Cache the transformation parameters for a device in the GADRAW
# common.  Must be called whenever the current WCS changes.  We need only
# check that the WCS has not changed because anything more serious than a
# change current WCS call will cause the cache to be invalidated.

procedure gpl_cache (gp)

pointer	gp			# graphics descriptor
pointer	w
real	wx1, wx2, wy1, wy2
bool	fp_nondegenr()
real	elogr()

int	wcsord
data	wcsord /0/
include	"gpl.com"

begin
	gp_out = gp
	wcs = GP_WCS(gp)
	w = GP_WCSPTR (gp, wcs)

	# The WCS must be fixed to the output device (kernel) when used for
	# coordinate transformations in metacode output.

	if (GP_WCSSTATE(gp) != FIXED) {
	    call gactivate (gp, 0)
	    call gpl_flush()
	    call gki_setwcs (GP_FD(gp), Memi[GP_WCSPTR(gp,1)],
		LEN_WCS * MAX_WCS)
	    GP_WCSSTATE(gp) = FIXED
	    wcsord = wcsord + 1
	    GP_WCSORD(gp) = wcsord
	}

	mx1 = WCS_SX1(w) * GKI_MAXNDC
	mx2 = WCS_SX2(w) * GKI_MAXNDC
	my1 = WCS_SY1(w) * GKI_MAXNDC
	my2 = WCS_SY2(w) * GKI_MAXNDC

	# Compute world -> GKI coordinate transformation.  If log scaling is
	# indicated but one or both window coords are negative, use ELOG
	# scaling instead.

	mxorigin = mx1
	xtran = WCS_XTRAN(w)

	wx1 = WCS_WX1(w)
	wx2 = WCS_WX2(w)

	# Ensure that the window is nondegenerate.
	if (fp_nondegenr (wx1, wx2))
	    ;

	if (xtran == LINEAR) {
	    wxorigin = wx1
	    xscale = (mx2 - mx1) / (wx2 - wx1)
	} else if (xtran == LOG && wx1 > 0 && wx2 > 0) {
	    wxorigin = log10 (wx1)
	    xscale = (mx2 - mx1) / (log10(wx2) - wxorigin)
	} else {
	    wxorigin = elogr (wx1)
	    xscale = (mx2 - mx1) / (elogr(wx2) - wxorigin)
	}

	myorigin = my1
	ytran = WCS_YTRAN(w)

	wy1 = WCS_WY1(w)
	wy2 = WCS_WY2(w)

	# Ensure that the window is nondegenerate.
	if (fp_nondegenr (wy1, wy2))
	    ;

	if (ytran == LINEAR) {
	    wyorigin = wy1
	    yscale = (my2 - my1) / (wy2 - wy1)
	} else if (ytran == LOG && wy1 > 0 && wy2 > 0) {
	    wyorigin = log10 (wy1)
	    yscale = (my2 - my1) / (log10(wy2) - wyorigin)
	} else {
	    wyorigin = elogr (wy1)
	    yscale = (my2 - my1) / (elogr(wy2) - wyorigin)
	}

	# If clipping is disabled move the clipping viewport out to the
	# boundary of the device.

	if (and (WCS_FLAGS(w), WF_CLIP) == 0) {
	    mx1 = 0
	    mx2 = GKI_MAXNDC
	    my1 = 0
	    my2 = GKI_MAXNDC
	}
end
