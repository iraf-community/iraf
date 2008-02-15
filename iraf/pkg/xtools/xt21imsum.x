# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# XT_21IMSUM -- Sum 2D image columns or lines to 1D.

procedure xt_21imsum (im, axis, col1, col2, line1, line2, x, y, npts)

pointer	im			# IMIO pointer
int	axis			# Axis of vector
int	col1, col2		# Range of columns
int	line1, line2		# Range of lines
pointer	x			# Vector ordinates
pointer	y			# Vector abscissa
int	npts			# Number of points in vector

int	i, line, ncols, nlines

real	asumr()
pointer	imgs2r()

begin
	# If the pointers are defined first free them.

	call mfree (x, TY_REAL)
	call mfree (y, TY_REAL)

	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	switch (axis) {
	case 1:
	    npts = ncols
	    call malloc (x, ncols, TY_REAL)
	    call calloc (y, ncols, TY_REAL)

	    do i = 1, ncols
	        Memr[x+i-1] = col1 + i - 1

	    do i = 1, nlines {
		line = line1 + i - 1
	        call aaddr (Memr[imgs2r (im, col1, col2, line, line)], Memr[y],
		    Memr[y], ncols)
	    }
	case 2:
	    npts = nlines
	    call malloc (x, nlines, TY_REAL)
	    call malloc (y, nlines, TY_REAL)

	    do i = 1, nlines {
		line = line1 + i - 1
	        Memr[x+i-1] = line
	        Memr[y+i-1] = asumr (Memr[imgs2r (im, col1, col2, line, line)],
		    ncols)
	    }
	}
end

# XT_21IMMED -- Median 2D image columns or lines to 1D.

define	MAXPIX	10000	# Maximum number of pixels to read at one time.

procedure xt_21immed (im, axis, col1, col2, line1, line2, x, y, npts)

pointer	im			# IMIO pointer
int	axis			# Axis of vector
int	col1, col2		# Range of columns
int	line1, line2		# Range of lines
pointer	x			# Vector ordinates
pointer	y			# Vector abscissa
int	npts			# Number of points in vector

int	i, j, k, n, line, ncols, nlines, maxncols
pointer	buf1, buf2

real	amedr()
pointer	imgs2r()

begin
	# If the pointers are defined first free them.

	call mfree (x, TY_REAL)
	call mfree (y, TY_REAL)

	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	switch (axis) {
	case 1:
	    npts = ncols
	    call malloc (x, ncols, TY_REAL)
	    call calloc (y, ncols, TY_REAL)
	    call malloc (buf1, nlines, TY_REAL)

	    maxncols = MAXPIX / nlines
	    j = 0
	    do i = 1, ncols {
		if (i > j) {
	            n = min (ncols - j, maxncols)
		    buf2 = imgs2r (im, col1+j, col1+j+n-1, line1, line2)
		    j = j + n
		}
		do k = 1, nlines
		    Memr[buf1+k-1] = Memr[buf2+(k-1)*n+i-1]
		Memr[y+i-1] = amedr (Memr[buf1], nlines)
	    }

	    call mfree (buf1, TY_REAL)

	    do i = 1, ncols
	        Memr[x+i-1] = col1 + i - 1
	case 2:
	    npts = nlines
	    call malloc (x, nlines, TY_REAL)
	    call malloc (y, nlines, TY_REAL)

	    do i = 1, nlines {
		line = line1 + i - 1
	        Memr[x+i-1] = line
	        Memr[y+i-1] = amedr (Memr[imgs2r (im, col1, col2, line, line)],
		    ncols)
	    }
	}
end


# XT_21IMAVG -- Average 2D image columns or lines to 1D.

procedure xt_21imavg (im, axis, col1, col2, line1, line2, x, y, npts)

pointer	im			# IMIO pointer
int	axis			# Axis of vector
int	col1, col2		# Range of columns
int	line1, line2		# Range of lines
pointer	x			# Vector ordinates
pointer	y			# Vector abscissa
int	npts			# Number of points in vector

begin
	call xt_21imsum (im, axis, col1, col2, line1, line2, x, y, npts)

	switch (axis) {
	case 1:
	    call adivkr (Memr[y], real (line2-line1+1), Memr[y], npts)
	case 2:
	    call adivkr (Memr[y], real (col2-col1+1), Memr[y], npts)
	}
end
