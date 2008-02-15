include	"epix.h"
 
# EP_INPUT -- Replace aperture by data from original input image.
# The aperture is first centered.
 
procedure ep_input (ep, ap, xa, ya, xb, yb)
 
pointer	ep			# EPIX pointer
int	ap			# Aperture type
int	xa, ya, xb, yb		# Aperture coordinates
 
int	i, x1, x2, y1, y2
pointer	mask, indata, im, immap(), imgs2r()
 
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
 
	    im = immap (EP_INPUT(ep), READ_ONLY, 0)
	    indata = imgs2r (im, EP_X1(ep), EP_X2(ep), EP_Y1(ep), EP_Y2(ep))
	    call ep_input1 (Memr[indata], Memi[mask], Memr[EP_OUTDATA(ep)],
		EP_NPTS(ep))
	    call imunmap (im)
 
	    call mfree (mask, TY_INT)
	}
end
 
 
# EP_INPUT1 -- Replace aperture by input data.
 
procedure ep_input1 (indata, mask, outdata, npts)
 
real	indata[npts]		# Data subraster
int	mask[npts]		# Mask subraster
real	outdata[npts]		# Input buffer data
int	npts			# Number of points
 
int	i
 
begin
	do i = 1, npts
	    if (mask[i] == 1)
		outdata[i] = indata[i]
end
