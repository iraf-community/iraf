include	<mach.h>
include	<imhdr.h>
include	"epix.h"


# EP_REPLACE -- Replace all pixels that are ==, <=, or >= to the value at the
# reference pixel.  Since this allocates and gets sections this may result in
# the entire image being put into memory with potential memory problems.  It
# is intended for use with masks that have regions of constant values.
#
# Note that this version assumes the pixel values may be ACE object masks.


procedure ep_replacei (ep, x, y, key)

pointer	ep			#I EPIX pointer
int	x, y			#I Reference pixel
int	key			#I Key

int	i, j, nc, nl, x1, x2, y1, y2
real	minv, maxv
int	val, ival, oval
pointer	im, buf

int	andi()
pointer	imgs2i(), imps2i()
errchk	imgs2i, imps2i

begin
	im = EP_IM(ep)
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)

	EP_INDATA(ep) = NULL
	EP_OUTDATA(ep) = NULL
	if (x < 1 || x > nc || y < 1 || y > nl) {
	    call eprintf ("Pixel out of bounds\n")
	    return
	}

	# Get reference pixel value and replacement value.
	buf = imgs2i (im, x, x, y, y)
	ival = andi (Memi[buf], 0777777B)
	oval = EP_VALUE(ep)
	minv = EP_MINVALUE(ep)
	maxv = EP_MAXVALUE(ep)
	if (IS_INDEFR(minv))
	    minv = -MAX_REAL
	if (IS_INDEFR(maxv))
	    minv = MAX_REAL

	# This requires two passes to fit into the subraster model.
	# First pass finds the limits of the change and the second
	# makes the change.

	x1 = x+1; x2 = x-1; y1 = y+1; y2 = y-1
	do j = 1, nl {
	    buf = imgs2i (im, 1, nc, j, j)
	    switch (key) {
	    case '=':
		do i = 1, nc {
		    val = andi (Memi[buf+i-1], 0777777B)
		    if (val != ival || val == oval || val < minv || val > maxv)
		        next
		    x1 = min (x1, i)
		    x2 = max (x2, i)
		    y1 = min (y1, j)
		    y2 = max (y2, j)
		}
	    case '<':
		do i = 1, nc {
		    val = andi (Memi[buf+i-1], 0777777B)
		    if (val > ival || val == oval || val < minv || val > maxv)
		        next
		    x1 = min (x1, i)
		    x2 = max (x2, i)
		    y1 = min (y1, j)
		    y2 = max (y2, j)
		}
	    case '>':
		do i = 1, nc {
		    val = andi (Memi[buf+i-1], 0777777B)
		    if (val < ival || val == oval || val < minv || val > maxv)
		        next
		    x1 = min (x1, i)
		    x2 = max (x2, i)
		    y1 = min (y1, j)
		    y2 = max (y2, j)
		}
	    }
	}

	# No pixels to change.
	if (x2 < x1 || y2 < y1)
	    return

	# Set the rasters and change the pixels.
	EP_X1(ep) = x1
	EP_X2(ep) = x2
	EP_Y1(ep) = y1
	EP_Y2(ep) = y2
	EP_NX(ep) = EP_X2(ep) - EP_X1(ep) + 1
	EP_NY(ep) = EP_Y2(ep) - EP_Y1(ep) + 1
	EP_NPTS(ep) = EP_NX(ep) * EP_NY(ep)

	EP_OUTDATA(ep) = imps2i (im, EP_X1(ep), EP_X2(ep), EP_Y1(ep),
	    EP_Y2(ep))
	EP_INDATA(ep) = imgs2i (im, EP_X1(ep), EP_X2(ep), EP_Y1(ep),
	    EP_Y2(ep))

	buf = EP_OUTDATA(ep)
	call amovi (Memi[EP_INDATA(ep)], Memi[buf], EP_NPTS(ep))
	switch (key) {
	case '=':
	    do i = 1, EP_NPTS(ep) {
		val = andi (Memi[buf], 0777777B)
		if (val == ival && val >= minv && val <= maxv)
		    Memi[buf] = oval
		buf = buf + 1
	    }
	case '<':
	    do i = 1, EP_NPTS(ep) {
		val = andi (Memi[buf], 0777777B)
		if (val <= ival && val >= minv && val <= maxv)
		    Memi[buf] = oval
		buf = buf + 1
	    }
	case '>':
	    do i = 1, EP_NPTS(ep) {
		val = andi (Memi[buf], 0777777B)
		if (val >= ival && val >= minv && val <= maxv)
		    Memi[buf] = oval
		buf = buf + 1
	    }
	}
end

procedure ep_replacer (ep, x, y, key)

pointer	ep			#I EPIX pointer
int	x, y			#I Reference pixel
int	key			#I Key

int	i, j, nc, nl, x1, x2, y1, y2
real	minv, maxv
real	val, ival, oval
pointer	im, buf

pointer	imgs2r(), imps2r()
errchk	imgs2r, imps2r

begin
	im = EP_IM(ep)
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)

	EP_INDATA(ep) = NULL
	EP_OUTDATA(ep) = NULL
	if (x < 1 || x > nc || y < 1 || y > nl) {
	    call eprintf ("Pixel out of bounds\n")
	    return
	}

	# Get reference pixel value and replacement value.
	buf = imgs2r (im, x, x, y, y)
	ival = Memr[buf]
	oval = EP_VALUE(ep)
	minv = EP_MINVALUE(ep)
	maxv = EP_MAXVALUE(ep)
	if (IS_INDEFR(minv))
	    minv = -MAX_REAL
	if (IS_INDEFR(maxv))
	    minv = MAX_REAL

	# This requires two passes to fit into the subraster model.
	# First pass finds the limits of the change and the second
	# makes the change.

	x1 = x+1; x2 = x-1; y1 = y+1; y2 = y-1
	do j = 1, nl {
	    buf = imgs2r (im, 1, nc, j, j)
	    switch (key) {
	    case '=':
		do i = 1, nc {
		    val = Memr[buf+i-1]
		    if (val != ival || val == oval || val < minv || val > maxv)
		        next
		    x1 = min (x1, i)
		    x2 = max (x2, i)
		    y1 = min (y1, j)
		    y2 = max (y2, j)
		}
	    case '<':
		do i = 1, nc {
		    val = Memr[buf+i-1]
		    if (val > ival || val == oval || val < minv || val > maxv)
		        next
		    x1 = min (x1, i)
		    x2 = max (x2, i)
		    y1 = min (y1, j)
		    y2 = max (y2, j)
		}
	    case '>':
		do i = 1, nc {
		    val = Memr[buf+i-1]
		    if (val < ival || val == oval || val < minv || val > maxv)
		        next
		    x1 = min (x1, i)
		    x2 = max (x2, i)
		    y1 = min (y1, j)
		    y2 = max (y2, j)
		}
	    }
	}

	# No pixels to change.
	if (x2 < x1 || y2 < y1)
	    return

	# Set the rasters and change the pixels.
	EP_X1(ep) = x1
	EP_X2(ep) = x2
	EP_Y1(ep) = y1
	EP_Y2(ep) = y2
	EP_NX(ep) = EP_X2(ep) - EP_X1(ep) + 1
	EP_NY(ep) = EP_Y2(ep) - EP_Y1(ep) + 1
	EP_NPTS(ep) = EP_NX(ep) * EP_NY(ep)

	EP_OUTDATA(ep) = imps2r (im, EP_X1(ep), EP_X2(ep), EP_Y1(ep),
	    EP_Y2(ep))
	EP_INDATA(ep) = imgs2r (im, EP_X1(ep), EP_X2(ep), EP_Y1(ep),
	    EP_Y2(ep))

	buf = EP_OUTDATA(ep)
	call amovr (Memr[EP_INDATA(ep)], Memr[buf], EP_NPTS(ep))
	switch (key) {
	case '=':
	    do i = 1, EP_NPTS(ep) {
		val = Memr[buf]
		if (val == ival && val >= minv && val <= maxv)
		    Memr[buf] = oval
		buf = buf + 1
	    }
	case '<':
	    do i = 1, EP_NPTS(ep) {
		val = Memr[buf]
		if (val <= ival && val >= minv && val <= maxv)
		    Memr[buf] = oval
		buf = buf + 1
	    }
	case '>':
	    do i = 1, EP_NPTS(ep) {
		val = Memr[buf]
		if (val >= ival && val >= minv && val <= maxv)
		    Memr[buf] = oval
		buf = buf + 1
	    }
	}
end

