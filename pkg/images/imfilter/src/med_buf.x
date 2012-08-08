# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MED_BUF -- Procedure to maintain a buffer of image lines. A new buffer
# is created when the buffer pointer is null or if the number of lines
# requested is changed. The minimum number of image reads is used.

procedure med_buf (im, col1, col2, line1, line2, buf)

pointer	im		#I pointer to image
int	col1, col2	#I column limits in the image
int	line1, line2	#I line limits in the image
pointer	buf		#U buffer pointer

int	i
int	ncols, nlines, llast1, llast2, nllast, nclast
pointer	buf1, buf2
pointer	imgs2r()
errchk	imgs2r

begin
	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	# If the buffer pointer is undefined then allocate memory for the
	# buffer. If the number of lines or columns changes then reallocate
	# the buffer. Initialize the last line values to force a full
	# buffer image read.

	if (buf == NULL) {
	    call malloc (buf, ncols * nlines, TY_REAL)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	} else if ((nlines != nllast) || (ncols != nclast)) {
	    call realloc (buf, ncols * nlines, TY_REAL)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	}

	# Read in only image lines which are different from the last buffer.
	if (line1 < llast1) {
	    do i = line2, line1, -1 {
		if (i >= llast1)
		    buf1 = buf + (i - llast1) * ncols
		else
		    buf1 = imgs2r (im, col1, col2, i, i)
		buf2 = buf + (i - line1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	} else if (line2 > llast2) {
	    do i = line1, line2 {
		if (i <= llast2)
		    buf1 = buf + (i - llast1) * ncols
		else
		    buf1 = imgs2r (im, col1, col2, i, i)
		buf2 = buf + (i - line1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	}

	# Save buffer parameters
	llast1 = line1
	llast2 = line2
	nclast = ncols
	nllast = nlines
end
