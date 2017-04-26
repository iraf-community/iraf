# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>

# FMD_BUF -- Procedure to maintain a buffer of image lines. A new buffer
# is created when the buffer pointer is null or if the number of lines
# requested is changed. The minimum number of image reads is used.

procedure fmd_buf (im, col1, col2, line1, line2, buf, map, a1, a2, b1, b2)

pointer	im		#I pointer to image
int	col1, col2	#I column limits in the image
int	line1, line2	#I line limits in the image
pointer	buf		#U buffer pointer
int	map		#I perform mapping on image lines
real	a1, a2		#I limits of input image line
real	b1, b2		#I limits of output image line


int	i
int	ncols, nlines, llast1, llast2, nllast, nclast
pointer	bufr, bufi, buf1, buf2
bool	fp_equalr()
pointer	imgs2r(), imgs2i()
errchk	imgs2r, imgs2i

begin
	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	# If the buffer pointer is undefined then allocate memory for the
	# buffer. If the number of lines or columns changes then reallocate
	# the buffer. Initialize the last line values to force a full
	# buffer image read.

	if (buf == NULL) {
	    call malloc (buf, ncols * nlines, TY_INT)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	} else if ((nlines != nllast) || (ncols != nclast)) {
	    call realloc (buf, ncols * nlines, TY_INT)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	}

	# Read in only image lines which are different from the last buffer.
	if (line1 < llast1) {
	    do i = line2, line1, -1 {
		buf2 = buf + (i - line1) * ncols
		if (i >= llast1) {
		    buf1 = buf + (i - llast1) * ncols
		    call amovi (Memi[buf1], Memi[buf2], ncols)
		} else {
		    if (map == YES) {
		        bufr = imgs2r (im, col1, col2, i, i)
			if (fp_equalr (a1, a2))
			    call amovkr (b1, Memr[bufr], ncols)
			else
		            call amapr (Memr[bufr], Memr[bufr], ncols, a1, a2,
			        b1, b2)
		        call achtri (Memr[bufr], Memi[buf2], ncols)
		    } else {
			switch (IM_PIXTYPE(im)) {
			case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
			    bufi = imgs2i (im, col1, col2, i, i)
			    call amaxki (Memi[bufi], nint(a1), Memi[buf2],
			        ncols)
			    call aminki (Memi[buf2], nint(a2), Memi[buf2],
			        ncols)
			default:
		            bufr = imgs2r (im, col1, col2, i, i)
			    if (fp_equalr (a1, a2))
			        call amovkr (b1, Memr[bufr], ncols)
			    else
		                call amapr (Memr[bufr], Memr[bufr], ncols, a1,
				    a2, b1, b2)
		            call achtri (Memr[bufr], Memi[buf2], ncols)
			}
		    }
		}
	    }
	} else if (line2 > llast2) {
	    do i = line1, line2 {
		buf2 = buf + (i - line1) * ncols
		if (i <= llast2) {
		    buf1 = buf + (i - llast1) * ncols
		    call amovi (Memi[buf1], Memi[buf2], ncols)
		} else {
		    if (map == YES) {
		        bufr = imgs2r (im, col1, col2, i, i)
			if (fp_equalr (a1, a2))
			    call amovkr (b1, Memr[bufr], ncols)
			else
		            call amapr (Memr[bufr], Memr[bufr], ncols, a1, a2,
			        b1, b2)
		        call achtri (Memr[bufr], Memi[buf2], ncols)
		    } else {
			switch (IM_PIXTYPE(im)) {
			case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
			    bufi = imgs2i (im, col1, col2, i, i)
			    call amaxki (Memi[bufi], nint(a1), Memi[buf2],
			        ncols)
			    call aminki (Memi[buf2], nint(a2), Memi[buf2],
			        ncols)
			default:
		            bufr = imgs2r (im, col1, col2, i, i)
			    if (fp_equalr (a1, a2))
			        call amovkr (b1, Memr[bufr], ncols)
			    else
		                call amapr (Memr[bufr], Memr[bufr], ncols, a1,
				    a2, b1, b2)
		            call achtri (Memr[bufr], Memi[buf2], ncols)
			}
		    }
		}
	    }
	}

	# Save buffer parameters.
	llast1 = line1
	llast2 = line2
	nclast = ncols
	nllast = nlines
end
