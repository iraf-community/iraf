include	<imhdr.h>
include	"epix.h"
 
# EP_GDATA -- Get input and output image subrasters with boundary checking.
# Null pointer are returned if entirely out of bounds.
 
procedure ep_gdata (ep, x1, x2, y1, y2)
 
pointer	ep			# EPIX pointer
int	x1, x2, y1, y2		# Subraster limits
 
int	nc, nl
pointer	im, imgs2r(), imps2r()
 
begin
	im = EP_IM(ep)
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
 
	if (x2 < 1 || x1 >= nc || y2 < 1 || y1 >= nl) {
	    call eprintf ("Pixel out of bounds\n")
	    EP_INDATA(ep) = NULL
	    EP_OUTDATA(ep) = NULL
	    return
	}
	
	EP_X1(ep) = max (1, x1)
	EP_X2(ep) = min (nc, x2)
	EP_Y1(ep) = max (1, y1)
	EP_Y2(ep) = min (nl, y2)
	EP_NX(ep) = EP_X2(ep) - EP_X1(ep) + 1
	EP_NY(ep) = EP_Y2(ep) - EP_Y1(ep) + 1
	EP_NPTS(ep) = EP_NX(ep) * EP_NY(ep)
	EP_OUTDATA(ep) = imps2r (im, EP_X1(ep), EP_X2(ep), EP_Y1(ep), EP_Y2(ep))
	EP_INDATA(ep) = imgs2r (im, EP_X1(ep), EP_X2(ep), EP_Y1(ep), EP_Y2(ep))
	call amovr (Memr[EP_INDATA(ep)], Memr[EP_OUTDATA(ep)], EP_NPTS(ep))
end
 
 
# EP_GINDATA -- Get input image data only with boundary checking.
# A null pointer is returned if entirely out of bounds.
 
procedure ep_gindata (ep, x1, x2, y1, y2)
 
pointer	ep			# EPIX pointer
int	x1, x2, y1, y2		# Subraster limits
 
int	nc, nl
pointer	im, imgs2r()
 
begin
	im = EP_IM(ep)
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
 
	if (x2 < 1 || x1 >= nc || y2 < 1 || y1 >= nl) {
	    call eprintf ("Pixel out of bounds\n")
	    EP_INDATA(ep) = NULL
	    return
	}
	
	EP_X1(ep) = max (1, x1)
	EP_X2(ep) = min (nc, x2)
	EP_Y1(ep) = max (1, y1)
	EP_Y2(ep) = min (nl, y2)
	EP_NX(ep) = EP_X2(ep) - EP_X1(ep) + 1
	EP_NY(ep) = EP_Y2(ep) - EP_Y1(ep) + 1
	EP_NPTS(ep) = EP_NX(ep) * EP_NY(ep)
	EP_INDATA(ep) = imgs2r (im, EP_X1(ep), EP_X2(ep), EP_Y1(ep), EP_Y2(ep))
end
