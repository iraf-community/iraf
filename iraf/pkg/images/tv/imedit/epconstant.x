include	"epix.h"
 
# EP_CONSTANT -- Replace aperture by constant value.
# The aperture is first centered.
 
procedure ep_constant (ep, ap, xa, ya, xb, yb)
 
pointer	ep			# EPIX pointer
int	ap			# Aperture type
int	xa, ya, xb, yb		# Aperture coordinates
 
int	i, x1, x2, y1, y2
pointer	mask
 
begin
	i = max (5., abs (EP_SEARCH(ep)) + 1)
	x1 = min (xa, xb) - i
	x2 = max (xa, xb) + i
	y1 = min (ya, yb) - i
	y2 = max (ya, yb) + i
	call ep_gdata (ep, x1, x2, y1, y2)
	if (EP_OUTDATA(ep) != NULL) {
	    call malloc (mask, EP_NPTS(ep), TY_INT)
 
	    call ep_search (ep, Memr[EP_OUTDATA(ep)], EP_NX(ep),
		    EP_NY(ep), ap, xa, ya, xb, yb)
	    call ep_mask (ep, mask, ap, xa, ya, xb, yb)
	    call ep_constant1 (Memr[EP_OUTDATA(ep)], Memi[mask], EP_NPTS(ep),
		    EP_VALUE(ep))
 
	    call mfree (mask, TY_INT)
	}
end
 
 
# EP_CONSTANT1 -- Replace aperture by constant value.
 
procedure ep_constant1 (data, mask, npts, value)
 
real	data[npts]		# Data subraster
int	mask[npts]		# Mask subraster
int	npts			# Number of points
real	value			# Substitution value
 
int	i
 
begin
	do i = 1, npts
	    if (mask[i] == 1)
		data[i] = value
end
