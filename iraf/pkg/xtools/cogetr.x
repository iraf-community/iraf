# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"cogetr.h"

# COGETR -- Get a real column vector from a 2D image.
#
# This procedure is designed to be efficient when:
#	1.  The columns are accessed sequentially.
#	2.  The number of lines does not change.
#	3.  The first and last lines change slowly with column.
# One such case is when the entire column of an image is required.  Then
# the first and last lines do not change at all.  Another type of use
# occurs when dealing with features which are aligned nearly
# with the image lines.  For example objects in a long slit spectrum or
# Echelle orders.
#
# As the columns are accessed sequentially new lines are added to a
# scrolled buffer only when the first and last lines fall outside the
# buffer.  If the buffer size is insufficient to hold the all the columns
# then the buffer is set to contain a block of columns.  When the desired
# column is outside the block of columns then a new block is read.
# The buffer is created and initialized when the buffer pointer
# is null or when the number of lines requested is changed.  Both the
# buffer and the column data pointer are allocated in this procedure.
# The user must free the buffers with the procedure COUNMAP.

pointer procedure cogetr (co, col, line1, line2)

pointer	co		# COIO pointer
long	col		# Column
long	line1		# First image line of column vector
long	line2		# Last image line of column vector

size_t	ncols, nlines, nc
long	l_val
long	lastc1, lastl1, lastl2
long	i, imlen1, imlen2, col1
pointer	im, coldata, buffer, buf, data

long	modl()
pointer	imgl2r()

begin
	# Dereference the structure elements to improve the readability of
	# the code and reduce the Mem index arithmetic.

	im = CO_IM(co)
	coldata = CO_DATA(co)
	buffer = CO_BUF(co)
	ncols = CO_NCOLS(co)
	nlines = CO_NLINES(co)
	lastc1 = CO_COL1(co)
	lastl1 = CO_LINE1(co)
	lastl2 = CO_LINE2(co)
	imlen1 = IM_LEN (im, 1)
	imlen2 = IM_LEN (im, 2)

	# If memory has not been allocated then allocate it.
	# If the number of lines changes reallocate the buffer and
	# initialize lastc1 to zero to force a full buffer read.

	i = min (imlen2, (line2 - line1 + 1) + 2 * EXTRA)

	if ((buffer == NULL) || (nlines != i)) {
	    nlines = i
	    ncols = min (imlen1, CO_MAXBUF(co) / nlines)
	    lastc1 = 0

	    call mfree (coldata, TY_REAL)
	    call mfree (buffer, TY_REAL)
	    call malloc (coldata, line2 - line1 + 1, TY_REAL)
	    call malloc (buffer, ncols * nlines, TY_REAL)

	    CO_DATA(co) = coldata
	    CO_BUF(co) = buffer
	    CO_NCOLS(co) = ncols
	    CO_NLINES(co) = nlines
	}

	# Determine the starting column and the number of columns per line.

	col1 = ((col - 1) / ncols) * ncols + 1
	nc = min (ncols, imlen1 - col1 + 1)

	# If there is no overlap with the last buffer then read all the
	# requested lines.  Otherwise read only the image lines with are
	# different from the last buffer.

	if ((col1 != lastc1) || (line1 > lastl2) || (line2 < lastl1)) {
	    lastc1 = col1
	    lastl1 = max (1, line1 - EXTRA)
	    lastl2 = min (imlen2, line2 + EXTRA)
	    do i = lastl1, lastl2 {
		l_val = nlines
	        buf = buffer + modl(i, l_val) * ncols
		call amovr (Memr[imgl2r(im, i)+col1-1], Memr[buf], nc)
	    }
	    CO_COL1(co) = lastc1
	    CO_LINE1(co) = lastl1
	    CO_LINE2(co) = lastl2

	} else if (line1 < lastl1) {
	    do i = max (1, line1 - EXTRA), min (imlen2, lastl1 - 1) {
		l_val = nlines
	        buf = buffer + modl(i, l_val) * ncols
		call amovr (Memr[imgl2r(im, i)+col1-1], Memr[buf], nc)
	    }
	    lastl1 = max (1, line1 - EXTRA)
	    lastl2 = min (imlen2, line2 + EXTRA)
	    CO_LINE1(co) = lastl1
	    CO_LINE2(co) = lastl2

	} else if (line2 > lastl2) {
	    do i = max (1, lastl2 + 1), min (imlen2, line2 + EXTRA) {
		l_val = nlines
	        buf = buffer + modl(i, l_val) * ncols
		call amovr (Memr[imgl2r(im, i)+col1-1], Memr[buf], nc)
	    }
	    lastl1 = max (1, line1 - EXTRA)
	    lastl2 = min (imlen2, line2 + EXTRA)
	    CO_LINE1(co) = lastl1
	    CO_LINE2(co) = lastl2
	}

	# Set the column data vector.

	data = coldata
	do i = line1, line2 {
	    l_val = nlines
	    buf = buffer + modl(i, l_val) * ncols
	    Memr[data] = Memr[buf+col-col1]
	    data = data + 1
	}

	return (coldata)
end


# COMAP -- Map the column access

pointer procedure comap (im, maxbuf)

pointer	im		# IMIO pointer
size_t	maxbuf		# Maximum buffer size
pointer	co		# Returned pointer

size_t	sz_val

begin
	sz_val = LEN_CO
	call malloc (co, sz_val, TY_STRUCT)
	CO_IM(co) = im
	CO_MAXBUF(co) = maxbuf
	CO_DATA(co) = NULL
	CO_BUF(co) = NULL

	return (co)
end


# COUMAP -- Unmap the column access

procedure counmap (co)

pointer	co			# Pointer to buffer structure

begin
	call mfree (CO_DATA(co), TY_REAL)
	call mfree (CO_BUF(co), TY_REAL)
	call mfree (co, TY_STRUCT)
end
