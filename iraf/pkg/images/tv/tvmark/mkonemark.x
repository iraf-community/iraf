include <imhdr.h>
include "tvmark.h"

# MK_ONEMARK -- Procedure to mark symbols in the frame buffer given a
# coordinate list and a mark type.

procedure mk_onemark (mk, im, iw, wx, wy, owx, owy, label, ltid)

pointer	mk		# pointer to the mark structure
pointer	im		# frame image descriptor
pointer	iw		# pointer to the wcs structure
real	wx, wy		# coordinates of current list object
real	owx, owy	# coordinates of previous list member
char	label[ARB]	# current label
int	ltid		# list sequence number

int	ncols, nlines, nr, nc, x1, x2, y1, y2
pointer	sp, str, lengths, radii
real	fx, fy, ofx, ofy, xmag, ymag, lmax, lratio, rmax, ratio
int	mk_stati(), itoc()
int	mk_plimits(), mk_llimits(), mk_rlimits(), mk_climits()
pointer	mk_statp()
real	mk_statr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (lengths, MAX_NMARKS, TY_REAL)
	    call salloc (radii, MAX_NMARKS, TY_REAL)

	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	# Convert from image to frame buffer coordinates.
	if (IS_INDEFR(owx) || IS_INDEFR(owy)) {
	    owx = INDEFR
	    owy = INDEFR
	} else
	    call iw_im2fb (iw, owx, owy, ofx, ofy)
	call iw_im2fb (iw, wx, wy, fx, fy)
	call mk_mag (im, iw, xmag, ymag)

	switch (mk_stati (mk, MKTYPE)) {

	case MK_POINT:
	    if (mk_plimits (fx, fy, mk_stati (mk, SZPOINT),
	        ncols, nlines, x1, x2, y1, y2) == YES) {
	        call mk_drawpt (im, x1, x2, y1, y2, mk_stati (mk, GRAYLEVEL))
		call imflush (im)
	    }

	case MK_LINE:
	    if (! IS_INDEFR(ofx) && ! IS_INDEFR(ofy)) {
		 if (mk_llimits (ofx, ofy, fx, fy, ncols, nlines, x1, x2,
		     y1, y2) == YES) {
		     call mk_drawline (im, ofx, ofy, fx, fy, x1, x2, y1, y2,
		        mk_stati (mk, GRAYLEVEL))
		    call imflush (im)
		}
	    }

	case MK_RECTANGLE:
	    nr = mk_stati (mk, NRECTANGLES)
	    if (xmag <= 0.0) {
		lmax = 0.0
		call amovkr (0.0, Memr[lengths], nr)
	    } else {
		call adivkr (Memr[mk_statp(mk,RLENGTHS)], xmag, Memr[lengths],
		    nr)
	        lmax = Memr[lengths+nr-1]
	    }
	    if (ymag <= 0.0)
		lratio = 0.0
	    else
	        lratio = mk_statr (mk, RATIO) * xmag / ymag
	    if (mk_rlimits (fx, fy, lmax, lratio, ncols, nlines, x1, x2,
		y1, y2) == YES) {
	        call mk_drawbox (im, fx, fy, x1, x2, y1, y2, Memr[lengths],
		    lratio, nr, mk_stati (mk, GRAYLEVEL))
		call imflush (im)
	    }

	case MK_CIRCLE:
	    nc = mk_stati (mk, NCIRCLES)
	    if (xmag <= 0.0) {
		rmax = 0.0
		call amovkr (0.0, Memr[radii], nc)
	    } else {
		call adivkr (Memr[mk_statp(mk, RADII)], xmag, Memr[radii], nc)
	        rmax = Memr[radii+nc-1]
	    }
	    if (ymag <= 0.0)
		ratio = 0.0
	    else
	        ratio = xmag / ymag
	    if (mk_climits (fx, fy, rmax, ratio, ncols, nlines, x1, x2,
	        y1, y2) == YES) {
	        call mk_drawcircles  (im, fx, fy, x1, x2, y1, y2,
		    Memr[radii], ratio, nc, mk_stati (mk, GRAYLEVEL))
		call imflush (im)
	    }

	case MK_PLUS:
	    call mk_textim (im, "+", nint (fx), nint (fy), mk_stati (mk, SIZE),
	        mk_stati (mk, SIZE), mk_stati (mk, GRAYLEVEL), YES)
	    call imflush (im)

	case MK_CROSS:
	    call mk_textim (im, "*", nint (fx), nint (fy), mk_stati (mk, SIZE),
	        mk_stati (mk, SIZE), mk_stati (mk, GRAYLEVEL), YES)
	    call imflush (im)

	default:
	    # Do nothing gracefully
	}

	# Number the text file.
	if (mk_stati (mk, LABEL) == YES) {
	    if (label[1] != EOS) {
		call mk_textim (im, label, nint (fx) + mk_stati (mk,
		    NXOFFSET), nint (fy) + mk_stati (mk, NYOFFSET),
		    mk_stati (mk, SIZE), mk_stati (mk, SIZE), mk_stati (mk,
		    GRAYLEVEL), NO)
		call imflush (im)
	    }
	} else if (mk_stati (mk, NUMBER) == YES) {
	    if (itoc (ltid, Memc[str], SZ_FNAME) > 0) {
		call mk_textim (im, Memc[str], nint (fx) + mk_stati (mk,
		    NXOFFSET), nint (fy) + mk_stati (mk, NYOFFSET),
		    mk_stati (mk, SIZE), mk_stati (mk, SIZE), mk_stati (mk,
		    GRAYLEVEL), NO)
		call imflush (im)
	    }
	}

	call sfree (sp)
end


# MK_DMARK -- Mark a dot.

procedure mk_dmark (mk, im, fx, fy)

pointer	mk		# pointer to the mark structure
pointer	im		# pointer to the  frame buffer
real	fx, fy		# real coordinates

int	ncols, nlines, x1, y1, x2, y2
int	mk_stati(), mk_plimits()

begin
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	if (mk_plimits (fx, fy, mk_stati (mk, SZPOINT), ncols, nlines,
	    x1, x2, y1, y2) == YES) {
	    call mk_drawpt (im, x1, x2, y1, y2, mk_stati (mk, GRAYLEVEL))
	    call imflush (im)
	}

	#call mk_seti (mk, X1, x1)
	#call mk_seti (mk, Y1, y1)
	#call mk_seti (mk, X2, x2)
	#call mk_seti (mk, Y2, x2)
end


# MK_CMARK -- Mark concentric circle(s).

procedure mk_cmark (mk, im, iw, fx, fy)

pointer	mk		# pointer to the mark structure
pointer	im		# pointer to the frame buffer image
pointer	iw		# pointer to the wcs structure
real	fx, fy		# center of circle

int	nc, ncols, nlines, x1, x2, y1, y2
pointer	sp, radii
real	xmag, ymag, rmax, ratio
int	mk_stati(), mk_climits()
pointer	mk_statp()

begin
	nc = mk_stati (mk, NCIRCLES)
	if (nc <= 0)
	    return

	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	call mk_mag (im, iw, xmag, ymag)

	call smark (sp)
	call salloc (radii, nc, TY_REAL)

	if (xmag <= 0.0) {
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

	if (mk_climits (fx, fy, rmax, ratio, ncols, nlines, x1, x2,
	    y1, y2) == YES) {
	    call mk_drawcircles (im, fx, fy, x1, x2, y1, y2, Memr[radii],
	        ratio, nc, mk_stati (mk, GRAYLEVEL))
	    call imflush (im)
	}

	#call mk_seti (mk, X1, x1)
	#call mk_seti (mk, Y1, y1)
	#call mk_seti (mk, X2, x2)
	#call mk_seti (mk, Y2, y2)

	call sfree (sp)
end


# MK_OCMARK -- Mark one circle.

procedure mk_ocmark (mk, im, iw, fx, fy, rmax)

pointer	mk		# pointer to the mark structure
pointer	im		# pointer to the frame buffer image
pointer	iw		# pointer to the wcs structure
real	fx, fy		# center of circle
real	rmax		# maximum radius

int	ncols, nlines, x1, x2, y1, y2
int	mk_climits(), mk_stati()

begin
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	if (mk_climits (fx, fy, rmax, 1.0, ncols, nlines, x1, x2,
	    y1, y2) == YES) {
	    call mk_drawcircles (im, fx, fy, x1, x2, y1, y2, rmax,
	        1.0, 1, mk_stati (mk, GRAYLEVEL))
	    call imflush (im)
	}

	#call mk_seti (mk, X1, x1)
	#call mk_seti (mk, Y1, y1)
	#call mk_seti (mk, X2, x2)
	#call mk_seti (mk, Y2, y2)
end


# MK_LMARK -- Mark s line segment

procedure mk_lmark (mk, im, ofx, ofy, fx, fy)

pointer	mk		# pointer to the mark structure
pointer	im		# pointer to the frame buffer
real	ofx, ofy	# coords of first point
real	fx, fy		# coords of second point

int	ncols, nlines, x1, y1, x2, y2
int	mk_stati(), mk_llimits()

begin
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	if (mk_llimits (ofx, ofy, fx, fy, ncols, nlines, x1, x2,
	    y1, y2) == YES) {
	    call mk_drawline (im, ofx, ofy, fx, fy, x1, x2, y1, y2,
	         mk_stati (mk, GRAYLEVEL))
	    call imflush (im)
	}

	#call mk_seti (mk, X1, x1)
	#call mk_seti (mk, Y1, y1)
	#call mk_seti (mk, X2, x2)
	#call mk_seti (mk, Y2, y2)
end


# MK_TMARK -- Mark a text string

procedure mk_tmark (mk, im, str, fx, fy, center)

pointer	mk		# pointer to the mark structure
pointer	im		# pointer to the frame buffer image
char	str[ARB]	# character string to be drawn
real	fx, fy		# lower left coords of string
int	center		# center the string

int	ncols, nlines
#int	x1, x2, y1, y2
int	mk_stati()

begin
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	call mk_textim (im, str, nint (fx), nint (fy), mk_stati (mk, SIZE),
	    mk_stati(mk, SIZE), mk_stati (mk, GRAYLEVEL), center)
	call imflush (im)

	#call mk_seti (mk, X1, x1)
	#call mk_seti (mk, Y1, y1)
	#call mk_seti (mk, X2, x1)
	#call mk_seti (mk, Y2, y2)
end


# MK_RMARK -- Mark concentric rectangles.

procedure mk_rmark (mk, im, iw, fx, fy)

pointer	mk		# pointer to the mark structure
pointer	im		# pointer to the frame buffer
pointer	iw		# pointer to the wcs structure
real	fx, fy		# x and y center coordinates

int	nr, ncols, nlines, x1, y1, x2, y2
pointer	sp, lengths
real	xmag, ymag, lmax, lratio
int	mk_stati(), mk_rlimits()
pointer	mk_statp()
real	mk_statr()

begin
	nr = mk_stati (mk, NRECTANGLES)
	if (nr <= 0)
	    return

	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	call mk_mag (im, iw, xmag, ymag)

	call smark (sp)
	call salloc (lengths, nr, TY_REAL)

	if (xmag <= 0.0) {
	    lmax = 0.0
	    call amovkr (0.0, Memr[lengths], nr)
	} else {
	    lmax = Memr[mk_statp(mk, RLENGTHS)+nr-1] / xmag
	    call adivkr (Memr[mk_statp(mk,RLENGTHS)], xmag, Memr[lengths], nr)
	}
	if (ymag <= 0.0)
	    lratio = 0.0
	else
	    lratio = mk_statr (mk, RATIO) * xmag / ymag

	if (mk_rlimits (fx, fy, lmax, lratio, ncols, nlines, x1, x2,
		y1, y2) == YES) {
	    call mk_drawbox (im, fx, fy, x1, x2, y1, y2, Memr[lengths],
		lratio, nr, mk_stati (mk, GRAYLEVEL))
	    call imflush (im)
	}

	#call mk_seti (mk, X1, x1)
	#call mk_seti (mk, Y1, y1)
	#call mk_seti (mk, X2, x2)
	#call mk_seti (mk, Y2, y2)

	call sfree (sp)
end


# MK_XMARK --  Procedure to mark a box.

procedure mk_xmark (mk, im, ofx, ofy, fx, fy)

pointer	mk		# pointer to the mark structure
pointer	im		# pointer to the frame buffer image
real	ofx, ofy	# first corner coordinates
real	fx, fy		# second corner coordinates

int	ncols, nlines, x1, x2, y1, y2
int	mk_stati()

begin
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	call mk_blimits (ofx, ofy, fx, fy, ncols, nlines, x1, x2, y1, y2)
	call mk_pbox (im, x1, x2, y1, y2, mk_stati (mk, GRAYLEVEL))
	call imflush (im)

	#call mk_seti (mk, X1, x1)
	#call mk_seti (mk, Y1, y1)
	#call mk_seti (mk, X2, x2)
	#call mk_seti (mk, Y2, y2)
end
