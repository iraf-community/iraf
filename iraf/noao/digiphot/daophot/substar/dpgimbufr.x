include	<imhdr.h>

# DP_GIMBUFR -- Maintain buffer of image lines.  A new buffer is created when
# the buffer pointer is null. No changing of buffer size is allowed, although
# this should be added. The minimum number of image reads is used.

define	flush_	91

procedure dp_gimbufr (inim, outim, line1, line2, buf, flush)

pointer	inim		# input image pointer
pointer	outim		# output image pointer
int	line1		# first image line of buffer
int	line2		# last image line of buffer
pointer	buf		# buffer
bool	flush		# flush the current contents of the buffer

int	i, ncols, nlines, llast1, llast2, nllast, lp, lout
pointer	buf1, buf2
pointer	imgl2r(), impl2r()

begin
	nlines = line2 - line1 + 1
	ncols = IM_LEN (inim, 1)
	lp = 0

	if (flush)
	    goto flush_

	# If the buffer pointer is undefined then allocate memory for the
	# buffer.  If the number of columns or lines requested changes
	# reallocate the buffer.  Initialize the last line values to force
	# a full buffer image read.

	if (buf == NULL) {
	    call malloc (buf, ncols * nlines, TY_REAL)
	    #llast1 = line1 - nlines
	    #llast2 = line2 - nlines
	    llast1 = 0
	    llast2 = 0
	} else if ((nlines > nllast)) {
	    call eprintf ("Buffer requested is larger than previous one\n")
		return
	}

	#call printf ("line1=%d line2=%d llast1=%d llast2=%d\n")
	    #call pargi (line1)
	    #call pargi (line2)
	    #call pargi (llast1)
	    #call pargi (llast2)

	# Write out the lines that are not needed any more.
	if (line1 > llast1 && llast1 > 0) {
	    buf2 = buf
	    lout = min (llast2, line1 - 1)
	    do i = llast1, lout{
		buf1 = impl2r (outim, i)
		call amovr (Memr[buf2], Memr[buf1], ncols)
		#call printf ("Writing line: %d\n")
		    #call pargi (i)
		buf2 = buf2 + ncols
	    }
	}

	# Write out any skipped image lines.
	if (line1 > llast2) {
	    do i = llast2 + 1, line1 - 1 {
		buf2 = imgl2r (inim, i)
		buf1 = impl2r (outim, i)
		call amovr (Memr[buf2], Memr[buf1], ncols)
		#call printf ("Copying line: %d\n")
		    #call pargi (i)
	    }
	}

	# Now move the remaining lines to the begining of the buffer.
	if (llast2 >= line1 ) {
	    buf2 = buf + ncols * (line1 - llast1)
	    buf1 = buf
	    do i = line1, llast2 {
		lp = lp + 1
	        call amovr (Memr[buf2], Memr[buf1], ncols)
		#call printf ("Moving line: %d\n")
		    #call pargi (i)
	        buf2 = buf2 + ncols
	        buf1 = buf1 + ncols
	    }
	}

	# Read only the image lines with are different from the last buffer.
	buf1 = buf + ncols * lp
	lout = max (line1, llast2 + 1)
	do i = lout, line2 {
	    #call printf ("Reading line: %d\n")
	        #call pargi (i)
	    buf2 = imgl2r (inim, i)
	    call amovr (Memr[buf2], Memr[buf1], ncols)
	    buf1 = buf1 + ncols
	}

	# Save the buffer parameters.
	llast1 = line1
	llast2 = line2
	nllast = nlines

	# Quit
	return
	
flush_
	# If requested to flush the current contents of the buffer we
	# write out lines llast1 to llast2 and then set buf == NULL.

	# Flush the data buffer.
	if (buf != NULL) {
	    buf2 = buf
	    do i = llast1, llast2 {
		buf1 = impl2r (outim, i)
		call amovr (Memr[buf2], Memr[buf1], ncols)
		#call printf ("Writing line: %d\n")
		    #call pargi (i)
		buf2 = buf2 + ncols
	    }
	}

	# Copy any remaining image lines.
	do i = llast2 + 1, IM_LEN(inim,2) {
	    buf2 = imgl2r (inim, i)
	    buf1 = impl2r (outim, i)
	    call amovr (Memr[buf2], Memr[buf1], ncols)
	    #call printf ("Copying line: %d\n")
		#call pargi (i)
	}

	call mfree (buf, TY_REAL)
	buf = NULL
	nllast = 0
end
