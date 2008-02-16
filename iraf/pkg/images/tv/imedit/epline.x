include	"epix.h"
 
# EP_LINE -- Replace aperture by line interpolation from background annulus.
# The aperture is first centered.  The interpolation is across lines
# from the nearest pixel in the background annulus.  Gaussian noise may
# be added.
 
procedure ep_line (ep, ap, xa, ya, xb, yb)
 
pointer	ep			# EPIX pointer
int	ap			# Aperture type
int	xa, ya, xb, yb		# Aperture coordinates
 
int	i, x1, x2, y1, y2
pointer	mask, gs
 
begin
	i = abs (EP_SEARCH(ep)) + EP_BUFFER(ep) + 1
	x1 = min (xa, xb)
	x2 = max (xa, xb)
	y1 = min (ya, yb) - i
	y2 = max (ya, yb) + i
	call ep_gdata (ep, x1, x2, y1, y2)
	if (EP_OUTDATA(ep) != NULL) {
	    call malloc (mask, EP_NPTS(ep), TY_INT)
 
	    call ep_search (ep, Memr[EP_OUTDATA(ep)], EP_NX(ep),
		EP_NY(ep), ap, xa, ya, xb, yb)
	    call ep_mask (ep, mask, ap, xa, ya, xb, yb)
	    call ep_line1 (Memr[EP_OUTDATA(ep)], Memi[mask],
		EP_NX(ep), EP_NY(ep))
	    if (!IS_INDEF (EP_SIGMA(ep)))
	        call ep_noise (EP_SIGMA(ep), Memr[EP_OUTDATA(ep)],
		    Memi[mask], Memr[EP_OUTDATA(ep)], Memr[EP_OUTDATA(ep)],
		    EP_NPTS(ep), gs)
 
	    call mfree (mask, TY_INT)
	}
end
 
 
# EP_LINE1 -- Interpolate across lines.
 
procedure ep_line1 (data, mask, nx, ny)
 
real	data[nx,ny]		# Data subraster
int	mask[nx,ny]		# Mask subraster
int	nx, ny			# Number of points
 
int	i, j, ya, yb, yc, yd
real	a, b
 
begin
	do i = 1, nx {
	    for (ya=1; ya<=ny && mask[i,ya]!=1; ya=ya+1)
		;
	    if (ya > ny)
		next
	    for (yb=ny; yb>ya && mask[i,yb]!=1; yb=yb-1)
		;
	    for (yc=ya; yc>=1 && mask[i,yc]!=2; yc=yc-1)
		;
	    for (yd=yb; yd<=ny && mask[i,yd]!=2; yd=yd+1)
		;
	    if (yc < 1 && yd > ny)
		next
	    else if (yc < 1)
		do j = ya, yb
		    data[i,j] = data[i,yd]
	    else if (yd > ny)
		do j = ya, yb
		    data[i,j] = data[i,yc]
	    else {
		a = data[i,yc]
		b = (data[i,yd] - a) / (yd - yc)
		do j = ya, yb
		    data[i,j] = a + b * (j - yc)
	    }
	}
end
