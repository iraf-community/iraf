include	"epix.h"

# EP_BACKGROUND -- Replace aperture by background values.
# The aperture is first centered.  The background is determined from a
# annulus buffered from the aperture and of a specified width.  The
# background is obtained by fitting a surface.  Noise may be added
# using a gaussian or by histogram sampling.

procedure ep_background (ep, ap, xa, ya, xb, yb)

pointer ep			# EPIX structure
int	ap			# Aperture type
int	xa, ya, xb, yb		# Aperture coordinates

int	i, x1, x2, y1, y2
pointer	mask, x, y, w, gs

begin
	i = max (5.,
	    abs (EP_SEARCH(ep)) + EP_BUFFER(ep) + EP_WIDTH(ep) + 1)
	x1 = min (xa, xb) - i
	x2 = max (xa, xb) + i
	y1 = min (ya, yb) - i
	y2 = max (ya, yb) + i
	call ep_gdata (ep, x1, x2, y1, y2)
	if (EP_OUTDATA(ep) != NULL) {
	    call malloc (mask, EP_NPTS(ep), TY_INT)
	    call malloc (x, EP_NPTS(ep), TY_REAL)
	    call malloc (y, EP_NPTS(ep), TY_REAL)
	    call malloc (w, EP_NPTS(ep), TY_REAL)

	    call ep_search (ep, Memr[EP_OUTDATA(ep)], EP_NX(ep), EP_NY(ep),
		ap, xa, ya, xb, yb)
	    call ep_mask (ep, mask, ap, xa, ya, xb, yb)
	    call ep_gsfit (ep, Memr[EP_OUTDATA(ep)], Memi[mask], Memr[x],
		Memr[y], Memr[w], EP_NX(ep), EP_NY(ep), gs)
	    call ep_bg (Memr[EP_OUTDATA(ep)], Memi[mask],
		Memr[x], Memr[y], EP_NPTS(ep), gs)
	    call ep_noise (EP_SIGMA(ep), Memr[EP_OUTDATA(ep)],
		Memi[mask], Memr[x], Memr[y], EP_NPTS(ep), gs)

	    call mfree (mask, TY_INT)
	    call mfree (x, TY_REAL)
	    call mfree (y, TY_REAL)
	    call mfree (w, TY_REAL)
	    call gsfree (gs)
	}
end


# EP_BG -- Replace aperture pixels by the background surface fit values.

procedure ep_bg (data, mask, x, y, npts, gs)

real	data[npts]		# Data subraster
int	mask[npts]		# Mask subraster
real	x[npts], y[npts]	# Coordinates
int	npts			# Number of points
pointer	gs			# Surface pointer

int	i
real	gseval()

begin
	if (gs == NULL)
	    return

	do i = 1, npts
	    if (mask[i] == 1)
		data[i] = gseval (gs, x[i], y[i])
end
