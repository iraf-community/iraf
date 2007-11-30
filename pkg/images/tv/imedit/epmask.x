include	<mach.h>
include	"epix.h"
 
# EP_MASK -- Make a mask array with 1=aperture and 2=background annulus.
#
# Exclude values outside a specified range.
 
procedure ep_mask (ep, mask, ap, xa, ya, xb, yb)
 
pointer	ep			# EPIX pointer
pointer	mask			# Mask pointer
int	ap			# Aperture type
int	xa, ya, xb, yb		# Aperture
 
int	xc, yc, i, j
real	rad, r, a, b, c, d, minv, maxv
int	x1a, x1b, x1c, x2a, x2b, x2c, y1a, y1b, y1c, y2a, y2b, y2c
pointer	sp, line, ptr1, ptr2
 
begin
	rad = max (0.5, EP_RADIUS(ep))

	switch (ap) {
	case APCIRCULAR:
	    xc = nint ((xa + xb) / 2.)
	    yc = nint ((ya + yb) / 2.)
 
	    a = rad ** 2
	    b = (rad + EP_BUFFER(ep)) ** 2
	    c = (rad + EP_BUFFER(ep) + EP_WIDTH(ep)) ** 2
 
	    ptr1 = mask
	    do j = EP_Y1(ep), EP_Y2(ep) {
	        d = (j - yc) ** 2
	        do i = EP_X1(ep), EP_X2(ep) {
		    r = d + (i - xc) ** 2
		    if (r <= a)
		        Memi[ptr1] = 1
		    else if (r >= b && r <= c)
		        Memi[ptr1] = 2
		    else
		        Memi[ptr1] = 0
		    ptr1 = ptr1 + 1
	        }
	    }
	case APCDIAG:
	    a = rad
	    b = rad + EP_BUFFER(ep)
	    c = rad + EP_BUFFER(ep) + EP_WIDTH(ep)
 
	    if (yb - ya != 0)
	        d = real (xb - xa) / (yb - ya)
	    else
		d = 1.
 
	    ptr1 = mask
	    do j = EP_Y1(ep), EP_Y2(ep) {
		xc = xa + d * (j - ya)
		do i = EP_X1(ep), EP_X2(ep) {
		    r = abs (i - xc)
		    if (r <= a)
			Memi[ptr1] = 1
		    else if (r >= b && r <= c)
			Memi[ptr1] = 2
		    else
			Memi[ptr1] = 0
		    ptr1 = ptr1 + 1
		}
	    }
	case APLDIAG:
	    a = rad
	    b = rad + EP_BUFFER(ep)
	    c = rad + EP_BUFFER(ep) + EP_WIDTH(ep)
 
	    if (xb - xa != 0)
	        d = real (yb - ya) / (xb - xa)
	    else
		d = 1.
 
	    ptr1 = mask
	    do j = EP_Y1(ep), EP_Y2(ep) {
		do i = EP_X1(ep), EP_X2(ep) {
		    yc = ya + d * (i - xa)
		    r = abs (j - yc)
		    if (r <= a)
			Memi[ptr1] = 1
		    else if (r >= b && r <= c)
			Memi[ptr1] = 2
		    else
			Memi[ptr1] = 0
		    ptr1 = ptr1 + 1
		}
	    }
	default:
	    call smark (sp)
	    call salloc (line, EP_NX(ep), TY_INT)
 
	    x1a = max (EP_X1(ep), min (xa, xb))
	    x1b = max (EP_X1(ep), int (x1a - EP_BUFFER(ep)))
	    x1c = max (EP_X1(ep), int (x1a - EP_BUFFER(ep) - EP_WIDTH(ep)))
	    x2a = min (EP_X2(ep), max (xa, xb))
	    x2b = min (EP_X2(ep), int (x2a + EP_BUFFER(ep)))
	    x2c = min (EP_X2(ep), int (x2a + EP_BUFFER(ep) + EP_WIDTH(ep)))
 
	    y1a = max (EP_Y1(ep), min (ya, yb))
	    y1b = max (EP_Y1(ep), int (y1a - EP_BUFFER(ep)))
	    y1c = max (EP_Y1(ep), int (y1a - EP_BUFFER(ep) - EP_WIDTH(ep)))
	    y2a = min (EP_Y2(ep), max (ya, yb))
	    y2b = min (EP_Y2(ep), int (y2a + EP_BUFFER(ep)))
	    y2c = min (EP_Y2(ep), int (y2a + EP_BUFFER(ep) + EP_WIDTH(ep)))
 
	    ptr1 = line - EP_X1(ep)
	    ptr2 = mask - EP_Y1(ep) * EP_NX(ep)
 
	    for (i=EP_X1(ep); i<x1c; i=i+1)
	        Memi[ptr1+i] = 0
	    for (; i<x1b; i=i+1)
	        Memi[ptr1+i] = 2
	    for (; i<x1a; i=i+1)
	        Memi[ptr1+i] = 0
	    for (; i<=x2a; i=i+1)
	        Memi[ptr1+i] = 1
	    for (; i<=x2b; i=i+1)
	        Memi[ptr1+i] = 0
	    for (; i<=x2c; i=i+1)
	        Memi[ptr1+i] = 2
	    for (; i<=EP_X2(ep); i=i+1)
	        Memi[ptr1+i] = 0
	    do i = y1a, y2a
	        call amovi (Memi[line], Memi[ptr2+i*EP_NX(ep)], EP_NX(ep))
 
	    for (i=x1a; i<=x2a; i=i+1)
	        Memi[ptr1+i] = 0
	    for (i=y1b; i<y1a; i=i+1)
	        call amovi (Memi[line], Memi[ptr2+i*EP_NX(ep)], EP_NX(ep))
	    for (i=y2a+1; i<=y2b; i=i+1)
	        call amovi (Memi[line], Memi[ptr2+i*EP_NX(ep)], EP_NX(ep))
 
	    for (i=x1b; i<=x2b; i=i+1)
	        Memi[ptr1+i] = 2
	    for (i=y1c; i<y1b; i=i+1)
	        call amovi (Memi[line], Memi[ptr2+i*EP_NX(ep)], EP_NX(ep))
	    for (i=y2b+1; i<=y2c; i=i+1)
	        call amovi (Memi[line], Memi[ptr2+i*EP_NX(ep)], EP_NX(ep))
 
	    for (i=EP_Y1(ep); i<y1c; i=i+1)
	        call aclri (Memi[ptr2+i*EP_NX(ep)], EP_NX(ep))
	    for (i=y2c+1; i<=EP_Y2(ep); i=i+1)
	        call aclri (Memi[ptr2+i*EP_NX(ep)], EP_NX(ep))
 
	    call sfree (sp)
	}

	# Exclude data values.
	ptr2 = EP_OUTDATA(ep)
	if (ptr2 == NULL ||
	    (IS_INDEFR(EP_MINVALUE(ep)) && IS_INDEFR(EP_MAXVALUE(ep))))
	    return

	minv = EP_MINVALUE(ep)
	maxv = EP_MAXVALUE(ep)
	if (IS_INDEFR(minv))
	    minv = -MAX_REAL
	if (IS_INDEFR(maxv))
	    maxv = MAX_REAL
	ptr1 = mask
	do j = EP_Y1(ep), EP_Y2(ep) {
	    do i = EP_X1(ep), EP_X2(ep) {
	        if (Memi[ptr1] != 0) {
		    if (Memr[ptr2] < minv || Memr[ptr2] > maxv)
		        Memi[ptr1] = 0
		}
		ptr1 = ptr1 + 1
		ptr2 = ptr2 + 1
	    }
	}
end
