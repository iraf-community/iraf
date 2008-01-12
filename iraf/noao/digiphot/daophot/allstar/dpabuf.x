include	<imhdr.h>
include "../lib/daophotdef.h"
include "../lib/allstardef.h"

# DP_GWT -- Return a pointer to the desired weights pixels. The weights buffer
# pointer is assumed to be NULL on the initial all. If the new line limits
# are totally contained within the old line limits no action is taken and
# the old line limits are returned. If the weights pixels are stored in a 
# scratch image the pixels must be accessed  sequentially, otherwise an
# error is returned.

pointer procedure dp_gwt (dao, im, line1, line2, mode, flush)

pointer	dao			# pointer to the daophot strucuture
pointer	im			# pointer to the input image
int	line1			# the lower line limit
int	line2			# the upper line limit
int	mode			# input / output mode
int	flush 			# flush the output

int	nx, ny, xoff, yoff, llast1, llast2
pointer	allstar

begin
	allstar = DP_ALLSTAR(dao)
	if (DP_WBUF(allstar) == NULL) {
	    llast1 = 0
	    llast2 = 0
	} else if (flush == YES) {
	    line1 = llast1
	    line2 = llast2
	} else if  ((line1 >= llast1) && (line2 <= llast2)) {
	    line1 = llast1
	    line2 = llast2
	    return (DP_WBUF(allstar))
	} else if (line1 < llast1)
	    call error (0,
	        "ERROR: Attempting to access the weights pixels randomly")

	# All the data is cached.
	if (DP_CACHE (allstar, A_WEIGHT) == YES) {

	    nx = IM_LEN(im,1)
	    ny = IM_LEN(im,2)
	    xoff = 1
	    yoff = 1
	    DP_WBUF(allstar) = DP_WEIGHTS(allstar)

	# Read in some new data.
	} else if (mode == READ_ONLY) {

            call dp_albufl2r (DP_WEIGHTS(allstar), DP_WBUF(allstar),
	        llast1, llast2, line1, line2, flush)

	    if (flush == NO) {
		nx = IM_LEN(im,1)
	        ny = line2 - line1 + 1
		xoff = 1
	        yoff = line1
	    } else {
		nx = 0
		ny = 0
		xoff = 0
		yoff = 0
	    }

	# Write out the old data and read in some new data.
	} else if (mode == READ_WRITE) {

            call dp_albufl2rw (DP_WEIGHTS(allstar), DP_WEIGHTS(allstar),
	        DP_WBUF(allstar), llast1, llast2, line1, line2, flush)

	    if (flush == NO) {
		nx = IM_LEN(im,1)
	        ny = line2 - line1 + 1
		xoff = 1
	        yoff = line1
	    } else {
		nx = 0
		ny = 0
		xoff = 0
		yoff = 0
	    }
	}

	# Update the required buffer parameters.
	DP_WNX(allstar) = nx
	DP_WNY(allstar) = ny
	DP_WXOFF(allstar) = xoff
	DP_WYOFF(allstar) = yoff

	# Update the buffer definition which is currently not used.
	DP_WLX(allstar) = xoff
	DP_WMX(allstar) = max (xoff + nx - 1, 0)
	DP_WLY(allstar) = yoff
	DP_WMY(allstar) = max (yoff + ny - 1, 0)

	return (DP_WBUF(allstar))
end


# DP_GST -- Return a pointer to the scratch image pixels. The scratch image
# pointer is assumed to be NULL on the initial all. If the new line limits
# are totally contained within the old line limits no action is taken and
# the old line limits are returned. If the scratch image pixels are stored in a 
# scratch image the pixels must be accessed  sequentially, otherwise an
# error is returned.

pointer procedure dp_gst (dao, im, line1, line2, mode, flush)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
int	line1			# the lower line limit
int	line2			# the upper line limit
int	mode			# input / output mode
int	flush			# flush the buffer

int	nx, ny, xoff, yoff, llast1, llast2
pointer	allstar

begin
	allstar = DP_ALLSTAR(dao)
	if (DP_SBUF(allstar) == NULL) {
	    llast1 = 0
	    llast2 = 0
	} else if (flush == YES) {
	    line1 = llast1
	    line2 = llast2
	} else if ((line1 >= llast1) && (line2 <= llast2)) {
	    line1 = llast1
	    line2 = llast2
	    return (DP_SBUF(allstar))
	} else if (line1 < llast1)
	    call error (0,
		"ERROR: Attempting to access scratch image pixels randomly")

	# All the data is cached.
	if (DP_CACHE (allstar, A_SUBT) == YES) {

	    nx = IM_LEN(im,1)
	    ny = IM_LEN(im,2)
	    xoff = 1
	    yoff = 1
	    DP_SBUF(allstar) = DP_SUBT(allstar)

	# Read in some new data.
	} else if (mode == READ_ONLY) {

            call dp_albufl2r (DP_SUBT(allstar), DP_SBUF(allstar),
	        llast1, llast2, line1, line2, flush)

	    if (flush == NO) {
		nx = IM_LEN(im,1)
	        ny = line2 - line1 + 1
		xoff = 1
	        yoff = line1
	    } else {
		nx = 0
		ny = 0
		xoff = 0
		yoff = 0
	    }

	# Write out the old data and read in some new data.
	} else if (mode == READ_WRITE) {

            call dp_albufl2rw (DP_SUBT(allstar), DP_SUBT(allstar),
	        DP_SBUF(allstar), llast1, llast2, line1, line2, flush)

	    if (flush == NO) {
		nx = IM_LEN(im,1)
	        ny = line2 - line1 + 1
		xoff = 1
	        yoff = line1
	    } else {
		nx = 0
		ny = 0
		xoff = 0
		yoff = 0
	    }
	}

	# Update the required buffer parameters.
	DP_SNX(allstar) = nx
	DP_SNY(allstar) = ny
	DP_SXOFF(allstar) = xoff
	DP_SYOFF(allstar) = yoff

	# Update the buffer description which is not currently used.
	DP_SLX(allstar) = xoff
	DP_SMX(allstar) = max (xoff + nx - 1, 0)
	DP_SLY(allstar) = yoff
	DP_SMY(allstar) = max (yoff + ny - 1, 0)

	return (DP_SBUF(allstar))
end


# DP_GDC -- Return a pointer to the subtracted image pixels. The subtracted
# image pixels pointer is assumed to be NULL on the initial all. If the new
# line limits are totally contained within the old line limits no action is
# taken and the old line limits are returned. If the subtracted image pixels are
# stored in a scratch image the pixels must be accessed  sequentially,
# otherwise an error is returned.

pointer procedure dp_gdc (dao, im, line1, line2, mode, flush)

pointer	dao			# pointer to the daophot strucuture
pointer	im			# pointer to the input image
int	line1, line2		# the upper and lower line limits
int	mode			# input / output mode
int	flush			# flush the input data

int	nx, ny, xoff, yoff, llast1, llast2
pointer	allstar

begin
	allstar = DP_ALLSTAR(dao)
	if (DP_DBUF(allstar) == NULL) {
	    llast1 = 0
	    llast2 = 0
	} else if (flush == YES) {
	    line1 = llast1
	    line2 = llast2
	} else if ((line1 >= llast1) && (line2 <= llast2)) {
	    line1 = llast1
	    line2 = llast2
	    return (DP_DBUF(allstar))
	} else if (line1 < llast1)
	    call error (0,
		"ERROR: Attempting to access subtracted image pixels randomly")

	# All the data is cached.
	if (DP_CACHE (allstar, A_DCOPY) == YES) {

	    nx = IM_LEN(im,1)
	    ny = IM_LEN(im,2)
	    xoff = 1
	    yoff = 1
	    DP_DBUF(allstar) = DP_DATA(allstar)

	# Read in some new data.
	} else if (mode == READ_ONLY) {

            call dp_albufl2r (DP_DATA(allstar), DP_DBUF(allstar),
	        llast1, llast2, line1, line2, flush)

	    if (flush == NO) {
		nx = IM_LEN(im,1)
	        ny = line2 - line1 + 1
		xoff = 1
	        yoff = line1
	    } else {
		nx = 0
		ny = 0
		xoff = 0
		yoff = 0
	    }

	# Write out the old data and read in some new data.
	} else if (mode == READ_WRITE) {

            call dp_albufl2rw (DP_DATA(allstar), DP_DATA(allstar),
	        DP_DBUF(allstar), llast1, llast2, line1, line2, flush)

	    if (flush == NO) {
		nx = IM_LEN(im,1)
	        ny = line2 - line1 + 1
		xoff = 1
	        yoff = line1
	    } else { 
		nx = 0
		ny = 0
		xoff = 0
		yoff = 0
	    }
	}

	# Update the required buffer parameters.
	DP_DNX(allstar) = nx
	DP_DNY(allstar) = ny
	DP_DXOFF(allstar) = xoff
	DP_DYOFF(allstar) = yoff

	# Update the buffer definition which is currently not used.
	DP_DLX(allstar) = xoff
	DP_DMX(allstar) = max (xoff + nx - 1, 0)
	DP_DLY(allstar) = yoff
	DP_DMY(allstar) = max (yoff + ny - 1, 0)

	return (DP_DBUF(allstar))
end


# DP_ALBUFL2RW -- Maintain a buffer of image lines and which can be read from
# an input image and written to an output image. The input and output images
# may be the same but must have the same dimensions. A new buffer is created
# when the buffer pointer is null and reallocated when the buffer changes size.

procedure dp_albufl2rw (inim, outim, buf, llast1, llast2, line1, line2, flush)

pointer	inim		# the input image pointer
pointer	outim		# the output image pointer
pointer	buf		# pointer to the internal buffer
int	llast1, llast2	# the line limits of the previous buffer
int	line1, line2	# the line limits of the requested buffer
int	flush		# flush the output buffer

int	i, ncols, nlines, nllast, lout, nmove
pointer	buf1, buf2
pointer	imgl2r(), impl2r()

define	flush_ 11

begin
	# Write the data in the buffer to the output image and free
	# the buffer.

	if (flush == YES)
	    go to flush_

	# Define the size of the current buffer.
	ncols = IM_LEN(inim,1)
	nlines = line2 - line1 + 1

	# If the buffer pointer is undefined then allocate memory for the
	# buffer.

	if (buf == NULL) {
	    call malloc (buf, ncols * nlines, TY_REAL)
	    llast1 = 0
	    llast2 = 0
	    nllast = 0
	} else
	    nllast = llast2 - llast1 + 1

	# Write out the lines that are not needed any more.

	if ((line1 > llast1) && (llast1 > 0)) {
	    buf2 = buf
	    lout = min (llast2, line1 - 1)
	    do i = llast1, lout {
		buf1 = impl2r (outim, i)
		call amovr (Memr[buf2], Memr[buf1], ncols)
		buf2 = buf2 + ncols
	    }
	}

	# Now move the remaining lines to the beginning of the buffer.

	nmove = 0
	if (llast2 >= line1) {
	    buf1 = buf
	    buf2 = buf + ncols * (line1 - llast1)
	    do i = line1, llast2 {
		if (buf1 != buf2)
	            call amovr (Memr[buf2], Memr[buf1], ncols)
		nmove = nmove + 1
	        buf2 = buf2 + ncols
	        buf1 = buf1 + ncols
	    }
	}

	# Resize the buffer if necessary.

	if (nlines > nllast)
	    call realloc (buf, ncols * nlines, TY_REAL)

	# Read only the image lines with are different from the last buffer.

	if (line2 > llast2) {
	    buf1 = buf + ncols * nmove
	    lout = max (line1, llast2 + 1)
	    do i = lout, line2 {
	        buf2 = imgl2r (inim, i)
	        call amovr (Memr[buf2], Memr[buf1], ncols)
	        buf1 = buf1 + ncols
	    }
	}

	# Save the buffer parameters.
	llast1 = line1
	llast2 = line2

	if (flush == NO)
	    return
flush_
	# Flush the remaining portion of the buffer to the output image
	# and free the buffer space.

	if (buf != NULL) {

	    buf2 = buf
	    do i = llast1, llast2 {
	        buf1 = impl2r (outim, i)
	        call amovr (Memr[buf2], Memr[buf1], ncols)
	        buf2 = buf2 + ncols
	    }
	    call imflush (outim)

	    call mfree (buf, TY_REAL)
	}

	buf = NULL
end


# DP_ALBUFL2R -- Maintain a buffer of image lines which can be read
# sequentially from an input image. The buffer pointer must be initialized
# to NULL. A new buffer is created when the buffer pointer is null and
# reallocated when the buffer increases in size.

procedure dp_albufl2r (inim, buf, llast1, llast2, line1, line2, flush)

pointer	inim		# pointer to the input image
pointer	buf		# pointer to the internal buffer
int	llast1, llast2	# the line limits of the previous buffer
int	line1, line2	# the line limits of the requested buffer
int	flush		# flush the output buffer

int	i, ncols, nlines, nllast, lout, nmove
pointer	buf1, buf2
pointer	imgl2r()

define	flush_ 11

begin

	# Write the data in the buffer to the output image and free
	# the buffer.

	if (flush == YES)
	    go to flush_

	# Define the size parameters for the new buffer.

	ncols = IM_LEN(inim,1)
	nlines = line2 - line1 + 1

	# If the buffer pointer is undefined then allocate memory for the
	# buffer.

	if (buf == NULL) {
	    call malloc (buf, ncols * nlines, TY_REAL)
	    llast1 = 0
	    llast2 = 0
	    nllast = 0
	} else
	    nllast = llast2 - llast1 + 1
	
	# Now move the remaining lines to the beginning of the buffer.

	nmove = 0
	if (llast2 >= line1)  {
	    buf1 = buf
	    buf2 = buf + ncols * (line1 - llast1)
	    do i = line1, llast2 {
		if (buf1 != buf2)
	            call amovr (Memr[buf2], Memr[buf1], ncols)
		nmove = nmove + 1
	        buf2 = buf2 + ncols
	        buf1 = buf1 + ncols
	    }
	}

	# Resize the buffer if necessary.

	if (nlines >  nllast)
	    call realloc (buf, ncols * nlines, TY_REAL)

	# Read only the image lines with are different from the last buffer.

	if (line2 > llast2) {
	    buf1 = buf + ncols * nmove
	    lout = max (line1, llast2 + 1)
	    do i = lout, line2 {
	        buf2 = imgl2r (inim, i)
	        call amovr (Memr[buf2], Memr[buf1], ncols)
	        buf1 = buf1 + ncols
	    }
	}

	# Save the buffer parameters.
	llast1 = line1
	llast2 = line2

	if (flush == NO)
	    return
flush_

	# Free the buffer space.
	if (buf != NULL)
	    call mfree (buf, TY_REAL)
	buf = NULL
end
