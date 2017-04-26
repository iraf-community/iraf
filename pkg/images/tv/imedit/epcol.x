include	"epix.h"
 
# EP_COL -- Replace aperture by column interpolation from background annulus.
# The aperture is first centered.  The interpolation is across columns
# from the nearest pixel in the background annulus.  Gaussian Noise may
# be added.
 
procedure ep_col (ep, ap, xa, ya, xb, yb)
 
pointer	ep			# EPIX pointer
int	ap			# Aperture type
int	xa, ya, xb, yb		# Aperture coordinates
 
int	i, x1, x2, y1, y2
pointer	mask, gs
 
begin
	i = abs (EP_SEARCH(ep)) + EP_BUFFER(ep) + 1
	x1 = min (xa, xb) - i
	x2 = max (xa, xb) + i
	y1 = min (ya, yb)
	y2 = max (ya, yb)
	call ep_gdata (ep, x1, x2, y1, y2)
	if (EP_OUTDATA(ep) != NULL) {
	    call malloc (mask, EP_NPTS(ep), TY_INT)
 
	    call ep_search (ep, Memr[EP_OUTDATA(ep)], EP_NX(ep),
		EP_NY(ep), ap, xa, ya, xb, yb)
	    call ep_mask (ep, mask, ap, xa, ya, xb, yb)
	    call ep_col1 (Memr[EP_OUTDATA(ep)], Memi[mask], EP_NX(ep),
		EP_NY(ep))
	    if (!IS_INDEF (EP_SIGMA(ep)))
	        call ep_noise (EP_SIGMA(ep), Memr[EP_OUTDATA(ep)],
		    Memi[mask], Memr[EP_OUTDATA(ep)], Memr[EP_OUTDATA(ep)],
		    EP_NPTS(ep), gs)
 
	    call mfree (mask, TY_INT)
	}
end
 
 
# EP_COL1 -- Do column interpolation.
 
procedure ep_col1 (data, mask, nx, ny)
 
real	data[nx,ny]		# Data subraster
int	mask[nx,ny]		# Mask subraster
int	nx, ny			# Number of points
 
int	i, j, xa, xb, xc, xd
real	a, b
 
begin
	do i = 1, ny {
	    for (xa=1; xa<=nx && mask[xa,i]!=1; xa=xa+1)
		;
	    if (xa > nx)
		next
	    for (xb=nx; xb>xa && mask[xb,i]!=1; xb=xb-1)
		;
	    for (xc=xa; xc>=1 && mask[xc,i]!=2; xc=xc-1)
		;
	    for (xd=xb; xd<=nx && mask[xd,i]!=2; xd=xd+1)
		;
	    if (xc < 1 && xd > nx)
		next
	    else if (xc < 1)
		do j = xa, xb
		    data[j,i] = data[xd,i]
	    else if (xd > nx)
		do j = xa, xb
		    data[j,i] = data[xc,i]
	    else {
		a = data[xc,i]
		b = (data[xd,i] - a) / (xd - xc)
		do j = xa, xb
		    data[j,i] = a + b * (j - xc)
	    }
	}
end
