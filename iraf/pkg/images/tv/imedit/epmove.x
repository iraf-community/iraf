include	"epix.h"

# EP_MOVE -- Replace the output aperture by the data in the input aperture.
# There is no centering.  A background is fit to the input data and subtracted
# and then a background is fit to the output aperture and added to the
# input aperture data.

procedure ep_move (ep, ap, xa1, ya1, xb1, yb1, xa2, ya2, xb2, yb2, key)

pointer ep			# EPIX structure
int	ap			# Aperture type
int	xa1, ya1, xb1, yb1	# Aperture coordinates
int	xa2, ya2, xb2, yb2	# Aperture coordinates
int	key			# Key

int	i, x1, x2, y1, y2
pointer	bufdata, mask, x, y, w

begin
	i = EP_BUFFER(ep) + EP_WIDTH(ep) + 1
	x1 = min (xa1, xb1) - i
	x2 = max (xa1, xb1) + i
	y1 = min (ya1, yb1) - i
	y2 = max (ya1, yb1) + i
	call ep_gindata (ep, x1, x2, y1, y2)
	if (EP_INDATA(ep) != NULL) {
	    call malloc (bufdata, EP_NPTS(ep), TY_REAL)
	    call malloc (mask, EP_NPTS(ep), TY_INT)
	    call malloc (x, EP_NPTS(ep), TY_REAL)
	    call malloc (y, EP_NPTS(ep), TY_REAL)
	    call malloc (w, EP_NPTS(ep), TY_REAL)

	    call amovr (Memr[EP_INDATA(ep)], Memr[bufdata], EP_NPTS(ep))
	    call ep_mask (ep, mask, ap, xa1, ya1, xb1, yb1)
	    i = EP_BUFFER(ep) + EP_WIDTH(ep) + 1
	    x1 = min (xa2, xb2) - i
	    x2 = max (xa2, xb2) + i
	    y1 = min (ya2, yb2) - i
	    y2 = max (ya2, yb2) + i
	    i = EP_NPTS(ep)
	    call ep_gdata (ep, x1, x2, y1, y2)
	    if (i != EP_NPTS(ep)) {
		call eprintf ("Raster sizes don't match\n")
		EP_OUTDATA(ep) = NULL
	    }
	    if (EP_OUTDATA(ep) != NULL) {
		switch (key) {
		case 'm':
	            call ep_movem (ep, Memr[bufdata], Memr[EP_OUTDATA(ep)],
		        Memi[mask], Memr[x], Memr[y], Memr[w],
			EP_NX(ep), EP_NY(ep))
		case 'n':
	            call ep_moven (ep, Memr[bufdata], Memr[EP_OUTDATA(ep)],
		        Memi[mask], Memr[x], Memr[y], Memr[w],
			EP_NX(ep), EP_NY(ep))
		}
	    }

	    call mfree (bufdata, TY_REAL)
	    call mfree (mask, TY_INT)
	    call mfree (x, TY_REAL)
	    call mfree (y, TY_REAL)
	    call mfree (w, TY_REAL)
	}
end


# EP_MOVEM -- Move the input aperture to the output.

procedure ep_movem (ep, indata, outdata, mask, x, y, w, nx, ny)

pointer	ep			# EPIX structure
real	indata[nx,ny]		# Input data subraster
real	outdata[nx,ny]		# Output data subraster
int	mask[nx,ny]		# Mask subraster
real	x[nx,ny], y[nx,ny]	# Coordinates
real	w[nx,ny]		# Weights
int	nx, ny			# Size of subraster

int	i, j
real	gseval()
pointer	gsin, gsout

begin
	call ep_gsfit (ep, indata, mask, x, y, w, nx, ny, gsin)
	if (gsin == NULL)
	    return
	call ep_gsfit (ep, outdata, mask, x, y, w, nx, ny, gsout)
	if (gsout == NULL) {
	    call gsfree (gsin)
	    return
	}
	do j = 1, ny
	    do i = 1, nx
	        if (mask[i,j] == 1)
		    outdata[i,j] = indata[i,j] - gseval (gsin, x[i,j], y[i,j]) +
		        gseval (gsout, x[i,j], y[i,j])
	call gsfree (gsin)
	call gsfree (gsout)
end


# EP_MOVEN -- Add the input aperture to the output.

procedure ep_moven (ep, indata, outdata, mask, x, y, w, nx, ny)

pointer	ep			# EPIX structure
real	indata[nx,ny]		# Input data subraster
real	outdata[nx,ny]		# Output data subraster
int	mask[nx,ny]		# Mask subraster
real	x[nx,ny], y[nx,ny]	# Coordinates
real	w[nx,ny]		# Weights
int	nx, ny			# Size of subraster

int	i, j
real	gseval()
pointer	gs

begin
	call ep_gsfit (ep, indata, mask, x, y, w, nx, ny, gs)
	if (gs == NULL)
	    return
	do j = 1, ny
	    do i = 1, nx
	        if (mask[i,j] == 1)
		    outdata[i,j] = indata[i,j] - gseval (gs, x[i,j], y[i,j]) +
		        outdata[i,j]
	call gsfree (gs)
end
