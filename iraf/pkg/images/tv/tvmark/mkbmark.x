include <imhdr.h>
include "tvmark.h"

# MK_BMARK -- Procedure to mark symbols in the frame buffer given a coordinate
# list and a mark type.

procedure mk_bmark (mk, im, iw, cl, ltid, fnt)

pointer	mk		# pointer to the mark structure
pointer	im		# frame image descriptor
pointer iw		# pointer to the wcs structure
int	cl		# coordinate file descriptor
int	ltid		# current number in the list
int	fnt		# font file descriptor

int	ncols, nlines, nr, nc, x1, x2, y1, y2
pointer	sp, str, lengths, radii, label
real	x, y, fx, fy, ofx, ofy, xmag, ymag, lmax, lratio, rmax, ratio
int	fscan(), nscan(), mk_stati(), itoc()
int	mk_plimits(), mk_llimits(), mk_rlimits(), mk_climits()
pointer	mk_statp()
real	mk_statr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (label, SZ_LINE, TY_CHAR)

	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	# Get the magnification factors.
	call mk_mag (im, iw, xmag, ymag)

	# Define the rectangles in terms of device coordinates.
	if (mk_stati (mk, MKTYPE) == MK_RECTANGLE) {
	    nr = mk_stati (mk, NRECTANGLES)
	    call salloc (lengths, nr, TY_REAL) 
	    if (xmag <= 0.0) {
	        lmax = 0.0
	        call amovkr (0.0, Memr[lengths], nr)
	    } else {
	        call adivkr (Memr[mk_statp(mk,RLENGTHS)], xmag, Memr[lengths],
		    nr)
	        lmax = Memr[lengths+nr-1] 
	    }
	    if (ymag <= 0.)
	        lratio = 0.0
	    else
	        lratio = mk_statr (mk, RATIO) * xmag / ymag
	}

	# Define the circles in terms of device coordinates.
	if (mk_stati (mk, MKTYPE) == MK_CIRCLE) {
	    nc = mk_stati (mk, NCIRCLES)
	    call salloc (radii, nc, TY_REAL)
	    if (xmag <= 0) {
	        rmax = 0.0
	        call amovkr (0.0, Memr[radii], nc)
	    } else {
	        call adivkr (Memr[mk_statp(mk,RADII)], xmag, Memr[radii], nc)
	        rmax = Memr[radii+nc-1]
	    }
	    if (ymag <= 0.0)
	        ratio = 0.0
	    else
	        ratio = xmag / ymag
	}

	# Run through the coordinate list sequentially plotting the
	# points, circles or rectangles. Speed it up later by reading
	# all the points in first, sorting and accessing the frame
	# buffer sequentially instead of randomly.

	ofx = INDEFR
	ofy = INDEFR
	while (fscan (cl) != EOF) {

	    # Get the x and y coords (possibly add an id number later).
	    call gargr (x)
	    call gargr (y)
	    if (nscan() < 2)
		next
	    if (IS_INDEFR(x) || IS_INDEFR(y))
		next
	    call gargwrd (Memc[label], SZ_LINE)
	    call iw_im2fb (iw, x, y, fx, fy)

	    switch (mk_stati (mk, MKTYPE)) {

	    case MK_POINT:
		if (mk_plimits (fx, fy, mk_stati (mk, SZPOINT),
		    ncols, nlines, x1, x2, y1, y2) == YES)
		    call mk_drawpt (im, x1, x2, y1, y2, mk_stati (mk,
		        GRAYLEVEL))

	    case MK_LINE:
		if (! IS_INDEFR(ofx) && ! IS_INDEFR(ofy)) {
		    if (mk_llimits (ofx, ofy, fx, fy, ncols, nlines, x1, x2,
			y1, y2) == YES)
		        call mk_drawline (im, ofx, ofy, fx, fy, x1, x2, y1, y2,
		            mk_stati (mk, GRAYLEVEL))
		}

	    case MK_RECTANGLE:
		if (mk_rlimits (fx, fy, lmax, lratio, ncols, nlines, x1, x2,
		    y1, y2) == YES) {
		    call mk_drawbox (im, fx, fy, x1, x2, y1, y2, Memr[lengths],
		        lratio, nr, mk_stati (mk, GRAYLEVEL))
		}

	    case MK_CIRCLE:
		if (mk_climits (fx, fy, rmax, ratio, ncols, nlines, x1, x2,
		    y1, y2) == YES) {
		    call mk_drawcircles  (im, fx, fy, x1, x2, y1, y2,
		        Memr[radii], ratio, nc, mk_stati (mk,
			GRAYLEVEL))
	            call imflush (im)
		}

	    case MK_PLUS:
		call mk_textim (im, "+", nint (fx), nint (fy), mk_stati (mk,
		    SIZE), mk_stati (mk, SIZE), mk_stati (mk, GRAYLEVEL), YES)
	        call imflush (im)

	    case MK_CROSS:
		call mk_textim (im, "x", nint (fx), nint (fy), mk_stati (mk,
		    SIZE), mk_stati (mk, SIZE), mk_stati (mk, GRAYLEVEL), YES)
	        call imflush (im)

	    default:
	    }

	    # Number the text file.
	    ltid = ltid + 1
	    if (mk_stati (mk, LABEL) == YES) {
		if (Memc[label] != EOS) {
		    call mk_textim (im, Memc[label], nint (fx) +
		        mk_stati(mk, NXOFFSET), nint (fy) + mk_stati (mk,
			NYOFFSET), mk_stati (mk, SIZE), mk_stati (mk, SIZE),
			mk_stati (mk, GRAYLEVEL), NO)
		    call imflush (im)
		}
	    } else if (mk_stati (mk, NUMBER) == YES) {
		if (itoc (ltid, Memc[str], SZ_FNAME) > 0) {
		    call mk_textim (im, Memc[str], nint (fx) +
		        mk_stati(mk, NXOFFSET), nint (fy) + mk_stati (mk,
			NYOFFSET), mk_stati (mk, SIZE), mk_stati (mk, SIZE),
			mk_stati (mk, GRAYLEVEL), NO)
		    call imflush (im)
		}
	    }

	    ofx = fx
	    ofy = fy
	}

	call imflush (im)
	call sfree (sp)
end


# MK_DRAWPT -- Procedure to draw a  point into the frame buffer.

procedure mk_drawpt (im, x1, x2, y1, y2, graylevel)

pointer	im		# pointer to the frame image
int	x1, x2		# column limits
int	y1, y2		# line limits
int	graylevel	# color of dot to be marked

int	i, npix
pointer	vp
pointer	imps2s()

begin
	npix = (x2 - x1 + 1) * (y2 - y1 + 1)
	vp = imps2s (im, x1, x2, y1, y2)
	do i = 1, npix
	    Mems[vp+i-1] = graylevel
end


# MK_PLIMITS -- Compute the extent of a dot.

int procedure mk_plimits (fx, fy, szdot, ncols, nlines, x1, x2, y1, y2)

real	fx, fy		# frame buffer coordinates of point
int	szdot		# size of a dot
int	ncols, nlines	# dimensions of the frame buffer
int	x1, x2		# column limits
int	y1, y2		# line limits

begin
	x1 = nint (fx) - szdot
	x2 = x1 + 2 * szdot
	if (x1 > ncols || x2 < 1)
	    return (NO)
	x1 = max (1, min (ncols, x1))
	x2 = min (ncols, max (1, x2))

	y1 = nint (fy) - szdot
	y2 = y1 + 2 * szdot 
	if (y1 > nlines || y2 < 1)
	    return (NO)
	y1 = max (1, min (nlines, y1))
	y2 = min (nlines, max (1, y2))

	return (YES)
end


# MK_DRAWLINE -- Procedure to draw lines.

procedure mk_drawline (im, ofx, ofy, fx, fy, x1, x2, y1, y2, graylevel)

pointer	im		# pointer to the frame buffer image
real	ofx, ofy	# previous coordinates
real	fx, fy		# current coordinates
int	x1, x2		# column limits
int	y1, y2		# line limits
int	graylevel	# picture gray level

int	i, j, ix1, ix2, npix, itemp
pointer	vp
real	m, b
pointer	imps2s()

begin
	# Compute the slope and intercept.
	if (x2 == x1) {
	    vp = imps2s (im, x1, x2, y1, y2)
	    npix = y2 - y1 + 1
	    do i = 1, npix
		Mems[vp+i-1] = graylevel
	} else if (y2 == y1) {
	    vp = imps2s (im, x1, x2, y1, y2)
	    npix = x2 - x1 + 1
	    do i = 1, npix
		Mems[vp+i-1] = graylevel
	} else {
	    m = (fy - ofy ) / (fx - ofx)
	    b = ofy - m * ofx
	    #if (m > 0.0)
		#b = y1 - m * x1
	    #else
		#b = y2 - m * x1
	    do i = y1, y2 {
		if (i == y1) {
		    ix1 = nint ((i - b) / m)
		    ix2 = nint ((i + 0.5 - b) / m)
		} else if (i == y2) {
		    ix1 = nint ((i - 0.5 - b) / m)
		    ix2 = nint ((i - b) / m)
		} else {
		    ix1 = nint ((i - 0.5 - b) / m)
		    ix2 = nint ((i + 0.5 - b) / m)
		}
		itemp = min (ix1, ix2)
		ix2 = max (ix1, ix2)
		ix1 = itemp
		if (ix1 < x1 || ix2 > x2)
		    next
	        vp = imps2s (im, ix1, ix2, i, i)
		npix = ix2 - ix1 + 1
		do j = 1, npix
		    Mems[vp+j-1] = graylevel
	    }
	}
end


# MK_LLIMITS -- Compute the limits of a line segment.

int procedure mk_llimits (ofx, ofy, fx, fy, ncols, nlines, x1, x2, y1, y2)

real	ofx, ofy	# previous coordinates
real	fx, fy		# current coordinates
int	ncols, nlines	# number of lines
int	x1, x2		# column limits
int	y1, y2		# line limits

begin
	x1 = nint (min (ofx, fx))
	x2 = nint (max (ofx, fx))
	if (x2 < 1 || x1 > ncols)
	    return (NO)
	x1 = max (1, min (ncols, x1))
	x2 = min (ncols, max (1, x2))

	y1 = nint (min (ofy, fy))
	y2 = nint (max (ofy, fy))
	if (y2 < 1 || y1 > nlines)
	    return (NO)
	y1 = max (1, min (nlines, y1))
	y2 = min (nlines, max (1, y2))

	return (YES)
end


# MK_DRAWCIRCLES -- Draw concentric circles around a point.

procedure mk_drawcircles (im, fx, fy, x1, x2, y1, y2, cradii, ratio, ncircles,
	graylevel)

pointer	im			# pointer to frame buffer image
real	fx, fy			# center of circle
int	x1, x2			# column limits
int	y1, y2			# line limits
real	cradii[ARB]		# sorted list of radii
real	ratio			# ratio of the magnifications
int	ncircles		# number of circles
int	graylevel		# gray level for marking

int	i, j, k, ix1, ix2, npix
pointer	ovp
real	dy2, dym, dyp, r2, dx1, dx2
pointer	imps2s()

begin
	if (ratio <= 0)
	    return

	npix = x2 - x1 + 1

	do i = y1, y2 {

	    dy2 = (i - fy) ** 2
	    if (i >= fy) {
	        dym = ((i - .5 - fy) / ratio) ** 2
	        dyp = ((i + .5 - fy) / ratio) ** 2
	    } else {
	        dyp = ((i - .5 - fy) / ratio) ** 2
	        dym = ((i + .5 - fy) / ratio) ** 2
	    }

	    do j = 1, ncircles {

		r2 = cradii[j] ** 2
		if (r2 < dym )
		    next

		dx1 = r2 - dym
		if (dx1 >= 0.0)
		    dx1 = sqrt (dx1)
		else
		    dx1 = 0.0
		dx2 = r2 - dyp
		if (dx2 >= 0.0)
		    dx2 = sqrt (dx2)
		else
		    dx2 = 0.0

		ix1 = nint (fx - dx1)
		ix2 = nint (fx - dx2)
		if (ix1 <= IM_LEN(im,1) && ix2 >= 1) {
		    ix1 = max (1, ix1)
		    ix2 = min (ix2, IM_LEN(im,1))
		    ovp = imps2s (im, ix1, ix2, i, i)
		    do k = 1, ix2 - ix1 + 1
		        Mems[ovp+k-1] = graylevel
		}

		ix1 = nint (fx + dx1)
		ix2 = nint (fx + dx2)
		if (ix2 <= IM_LEN(im,1) && ix1 >= 1) {
		    ix2 = max (1, ix2)
		    ix1 = min (ix1, IM_LEN(im,1))
		    ovp = imps2s (im, ix2, ix1, i, i)
		    do k = 1, ix2 - ix1 + 1
		        Mems[ovp+k-1] = graylevel
		}
	    }
	}

end


# MK_CLIMITS -- Compute the extent of a circle.

int procedure mk_climits (fx, fy, rmax, ratio, ncols, nlines, x1, x2, y1, y2)

real	fx, fy			# center of rectangle
real	rmax			# maximum half length of box
real	ratio			# ratio of the magnifications
int	ncols, nlines		# dimension of the image
int	x1, x2			# column limits
int	y1, y2			# line limits

begin
	x1 = nint (fx - rmax)
	x2 = nint (fx + rmax)
	if (x1 > ncols || x2 < 1)
	    return (NO)
	x1 = max (1, min (ncols, x1))
	x2 = min (ncols, max (1, x2))

	y1 = nint (fy - rmax * ratio)
	y2 = nint (fy + rmax * ratio)
	if (y1 > nlines || y2 < 1)
	    return (NO)
	y1 = max (1, min (nlines, y1))
	y2 = min (nlines, max (1, y2))

	return (YES)
end


# MK_DRAWBOX -- Procedure to draw a box into the frame buffer.

procedure mk_drawbox (im, fx, fy, x1, x2, y1, y2, length, ratio, nbox,
	graylevel)

pointer	im			# pointer to frame buffer image
real	fx, fy			# center of rectangle
int	x1, x2			# column limits
int	y1, y2			# line limits
real	length[ARB]		# list of rectangle lengths
real	ratio			# ratio of width/length
int	nbox			# number of boxes
int	graylevel		# value of graylevel

int	i, j, k, npix, ydist, bdist, ix1, ix2
pointer	ovp
real	hlength
pointer	imps2s()

begin
	if (x1 == x2) {
	    ovp = imps2s (im, x1, x2, y1, y2)
	    npix = y2 - y1 + 1
	    do i = 1, npix
		Mems[ovp+i-1] = graylevel
	} else if (y1 == y2) {
	    ovp = imps2s (im, x1, x2, y1, y2)
	    npix = x2 - x1 + 1
	    do i = 1, npix
		Mems[ovp+i-1] = graylevel
	} else {
	    npix = x2 - x1 + 1
	    do i = y1, y2 {
		ydist = nint (abs (i - fy))
		do j = 1, nbox {
		    hlength = length[j] / 2.0
		    bdist = nint (hlength * ratio)
		    if (ydist > bdist)
			next
		    ix1 = max (x1, nint (fx - hlength))
		    ix2 = min (x2, nint (fx + hlength))
		    if (ix1 < 1 || ix1 > IM_LEN(im,1) || ix2 < 1 ||
		        ix2 > IM_LEN(im,1))
			next
		    if (ydist == bdist) {
			ovp = imps2s (im, ix1, ix2, i, i)
			do k = 1, ix2 - ix1 + 1
			    Mems[ovp+k-1] = graylevel
		    } else {
			ovp = imps2s (im, ix1, ix1, i, i)
			Mems[ovp] = graylevel
			ovp = imps2s (im, ix2, ix2, i, i)
			Mems[ovp] = graylevel
		    }
		}
	    }
	}
end


# MK_RLIMITS -- Compute the extent of a rectangle.

int procedure mk_rlimits (fx, fy, lmax, lratio, ncols, nlines, x1, x2, y1, y2)

real	fx, fy			# center of rectangle
real	lmax			# maximum half length of box
real	lratio			# ratio of width to length
int	ncols, nlines		# dimension of the image
int	x1, x2			# column limits
int	y1, y2			# line limits

real	hlmax, wmax

begin
	hlmax = lmax / 2.0
	wmax = lmax * lratio

	x1 = nint (fx - hlmax)
	x2 = nint (fx + hlmax)
	if (x1 > ncols || x2 < 1)
	    return (NO)
	x1 = max (1, min (ncols, x1))
	x2 = min (ncols, max (1, x2))

	y1 = fy - wmax
	y2 = fy + wmax
	if (y1 > nlines || y2 < 1)
	    return (NO)
	y1 = max (1, min (nlines, y1))
	y2 = min (nlines, max (1, y2))

	return (YES)
end


# MK_PBOX -- Plot a box

procedure mk_pbox (im, x1, x2, y1, y2, graylevel)

pointer	im			# pointer to the image
int	x1, x2			# column limits
int	y1, y2			# line limits
int	graylevel		# line value

int	i, j, npix
pointer	ovp
pointer	imps2s()

begin
	do i = y1, y2 {
	    if (i == y1) {
		npix = x2 - x1 + 1
		ovp = imps2s (im, x1, x2, i, i)
		do j = 1, npix
		    Mems[ovp+j-1] = graylevel
	    } else if (i == y2) {
		npix = x2 - x1 + 1
		ovp = imps2s (im, x1, x2, i, i)
		do j = 1, npix
		    Mems[ovp+j-1] = graylevel
	    } else {
		ovp = imps2s (im, x1, x1, i, i)
		Mems[ovp] = graylevel
		ovp = imps2s (im, x2, x2, i, i)
		Mems[ovp] = graylevel
	    }
	}
end


# MK_BLIMITS -- Procedure to compute the boundary limits for drawing
# a box.

procedure mk_blimits (ofx, ofy, fx, fy, ncols, nlines, x1, x2, y1, y2)

real	ofx, ofy		# first point
real	fx, fy			# second point
int	ncols, nlines		# dimensions of the image
int	x1, x2			# column limits
int	y1, y2			# line limits

begin
    	x1 = nint (min (ofx, fx))
	x1 = max (1, min (x1, ncols))
    	x2 = nint (max (ofx, fx))
	x2 = min (ncols, max (x2, 1))

    	y1 = nint (min (ofy, fy))
	y1 = max (1, min (y1, nlines))
    	y2 = nint (max (ofy, fy))
	y2 = min (nlines, max (y2, 1))
end
