# AP_ICTR -- Procedure to compute a quick initial guess at the center for
# computing a stellar radial profile.

procedure ap_ictr (im, wx, wy, radius, emission, xcenter, ycenter)

pointer	im		# pointer to the iraf image
real	wx		# initial x coordinate
real	wy		# initial y coordinate
int	radius		# half width of centering box
int	emission	# emission feature
real	xcenter		# fitted x coordinate
real	ycenter		# fitted y coordinate

int	nx, ny, cier
pointer	cbuf
real	xc, yc, datamin, datamax, junk
int	ap_mctr1d()
pointer	ap_ctrpix()

begin
	# Get the pixels.
	cbuf = ap_ctrpix (im, wx, wy, radius, xc, yc, nx, ny)
	if (cbuf == NULL) {
	    xcenter = wx
	    ycenter = wy
	    return
	}

	# Fit the center.
	call alimr (Memr[cbuf], nx * ny, datamin, datamax)
	if (emission == YES)
	    call asubkr (Memr[cbuf], datamin, Memr[cbuf], nx * ny)
	else {
	    call anegr (Memr[cbuf], Memr[cbuf], nx * ny)
	    call aaddkr (Memr[cbuf], datamax, Memr[cbuf], nx * ny)
	}
	cier = ap_mctr1d (Memr[cbuf], nx, ny, xcenter, ycenter, junk, junk)

	if (IS_INDEFR(xcenter))
	    xcenter = wx
	else
	    xcenter = xcenter + wx - xc
	if (IS_INDEFR(ycenter))
	    ycenter = wy
	else
	    ycenter = ycenter + wy - yc
end
