# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gset.h>
include	<mach.h>
include <math.h>
include	<imhdr.h>
include <imset.h>
include <math/iminterp.h>
include	"imexam.h"

define	BTYPES		"|constant|nearest|reflect|wrap|project|"
define	SZ_BTYPE	8	# Length of boundary type string
define	NLINES		16	# Number of image  lines in the buffer


# IE_VIMEXAM -- Plot the vector of image data between two pixels.
# There are two types of plot selected by the key argument.  The
# second cursor position is passed in the IMEXAM data structure.
# The first position is either the middle of the vector or the starting
# point.

procedure ie_vimexam (gp, mode, ie, x, y, key)

pointer	gp		# GIO pointer
int	mode		# Graph mode
pointer	ie		# IMEXAM pointer
real	x, y		# Starting or center coordinate
int	key		# 'u' centered vector, 'v' two endpoint vector

int	btype, nxvals, nyvals, nzvals, width
pointer	sp, title, boundary, im, x_vec, y_vec, pp
real	x1, y1, x2, y2, zmin, zmax, bconstant

bool	fp_equalr()
int	clgpseti(), clgwrd(), clopset()
real	clgpsetr()
pointer	ie_gimage()
errchk	malloc

begin
	iferr (im = ie_gimage (ie, NO)) {
	    call erract (EA_WARN)
	    return
	}

	call smark (sp)
	call salloc (title, IE_SZTITLE, TY_CHAR)
	call salloc (boundary, SZ_BTYPE, TY_CHAR)

	# Get boundary extension parameters.
	if (IE_PP(ie) != NULL)
	    call clcpset (IE_PP(ie))
	IE_PP(ie) = clopset ("vimexam")
	pp = IE_PP(ie)
	btype  = clgwrd ("vimexam.boundary", Memc[boundary], SZ_BTYPE, BTYPES)
	bconstant = clgpsetr (pp, "constant")

	nxvals = IM_LEN(im,1)
	nyvals = IM_LEN(im,2)

	if (!IS_INDEF (x))
	    IE_X1(ie) = x
	if (!IS_INDEF(y))
	    IE_Y1(ie) = y

	x1 = IE_X1(ie)
	x2 = IE_X2(ie)
	y1 = IE_Y1(ie)
	y2 = IE_Y2(ie)
	width = clgpseti (pp, "naverage")

	# Check the boundary and compute the length of the output vector.
	x1 = max (1.0, min (x1, real (nxvals)))
	x2 = min (real(nxvals), max (1.0, x2))
	y1 = max (1.0, min (y1, real (nyvals)))
	y2 = min (real(nyvals), max (1.0, y2))
	nzvals = int (sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))) + 1

	# Check for cases which should be handled by pcols or prows.
	call malloc (x_vec, nzvals, TY_REAL)
	call malloc (y_vec, nzvals, TY_REAL)
	if (fp_equalr (x1, x2))
	    call ie_get_col (im, x1, y1, x2, y2, nzvals, width, btype,
	        bconstant, Memr[x_vec], Memr[y_vec], zmin, zmax)
	else if (fp_equalr (y1, y2))
	    call ie_get_row (im, x1, y1, x2, y2, nzvals, width, btype,
	        bconstant, Memr[x_vec], Memr[y_vec], zmin, zmax)
	else
	    call ie_get_vector (im, x1, y1, x2, y2, nzvals, width, btype,
	        bconstant, Memr[x_vec], Memr[y_vec], zmin, zmax)

	# Convert endpoint plot coordinates to centered coordinates.
	if (key == 'u') {
	    zmin = (IE_X1(ie) + IE_X2(ie)) / 2
	    zmax = (IE_Y1(ie) + IE_Y2(ie)) / 2
	    zmin = sqrt ((zmin-x1)**2 + (zmax-y1)**2)
	    call asubkr (Memr[x_vec], zmin, Memr[x_vec], nzvals)
	}

	call sprintf (Memc[title], IE_SZTITLE, 
	    "%s: Vector %.1f,%.1f to %.1f,%.1f naverage: %d\n%s")
	    call pargstr (IE_IMNAME(ie))
	    call pargr (x1)
	    call pargr (y1)
	    call pargr (x2)
	    call pargr (y2)
	    call pargi (width)
	    call pargstr (IM_TITLE(im))

	call ie_graph (gp, mode, pp, Memc[title], Memr[x_vec], Memr[y_vec],
	    nzvals, "", "")
       
        # Finish up
	call mfree (x_vec, TY_REAL)
	call mfree (y_vec, TY_REAL)
	call sfree (sp)
end


# IE_GET_VECTOR -- Average a strip perpendicular to a given vector and return
# vectors of point number and average pixel value. Also returned is the min
# and max value in the data vector.

procedure ie_get_vector (im, x1, y1, x2, y2, nvals, width, btype,
        bconstant, x_vector, y_vector, zmin, zmax)

pointer im		# pointer to image header
real	x1, y1		# starting pixel of vector
real	x2, y2		# ending pixel of pixel
real	bconstant	# Boundary extension constant
int	btype		# Boundary extension type
int	nvals		# number of samples along the vector
int	width		# width of strip to average over
real	x_vector[ARB]	# Pixel numbers
real	y_vector[ARB]	# Average pixel values (returned)
real	zmin, zmax 	# min, max of data vector

double	dx, dy, dpx, dpy, ratio, xoff, yoff, noff, xv, yv
int	i, j, k, nedge, col1, col2, line1, line2
int	colb, colc, line, linea, lineb, linec
pointer sp, oxs, oys, xs, ys, yvals, msi, buf
real	sum , lim1, lim2, lim3, lim4
pointer	imgs2r()
errchk	msiinit

begin
	call smark (sp)
	call salloc (oxs, width, TY_REAL)
	call salloc (oys, width, TY_REAL)
	call salloc (xs, width, TY_REAL)
	call salloc (ys, width, TY_REAL)
	call salloc (yvals, width, TY_REAL)

	# Determine sampling perpendicular to vector.
	dx = (x2 - x1) / (nvals - 1)
	dy = (y2 - y1) / (nvals - 1)
	if (x1 < x2) {
	    dpx = -dy
	    dpy =  dx
	} else {
	    dpx =  dy
	    dpy = -dx
	}

	# Compute offset from the nominal vector to the first sample point.
	ratio = dx / dy
	nedge  = width + 1
	noff = (real (width) - 1.0) / 2.0
	xoff = noff * dpx
	yoff = noff * dpy

	# Initialize the interpolator and the image data buffer.
	call msiinit (msi, II_BILINEAR)
	buf = NULL

	# Set the boundary.
	col1 = int (min (x1, x2)) - nedge
	col2 = nint (max (x1, x2)) + nedge
	line1 = int (min (y1, y2)) - nedge
	line2 = nint (max (y2, y1)) + nedge
	call ie_setboundary (im, col1, col2, line1, line2, btype, bconstant)

	# Initialize.
	xv = x1 - xoff
	yv = y1 - yoff
	do j = 1, width { 
	    Memr[oxs+j-1] = double (j - 1) * dpx
	    Memr[oys+j-1] = double (j - 1) * dpy
	} 

	# Loop over the output image lines.
	do i = 1, nvals { 
	    x_vector[i] = real (i)
	    line = yv

	    # Get the input image data and fit an interpolator to the data.
	    # The input data is buffered in a section of size NLINES + 2 *
	    # NEDGE.

	    if (dy >= 0.0 && (buf == NULL || line > (linea))) {
		linea = min (line2, line + NLINES - 1)
		lineb = max (line1, line - nedge)
		linec = min (line2, linea + nedge)
		lim1 = xv
		lim2 = lim1 + double (width - 1) * dpx
		lim3 = xv + double (linea - line + 1) * ratio
		lim4 = lim3 + double (width - 1) * dpx
		colb = max (col1, int (min (lim1, lim2, lim3, lim4)) - 1)
		colc = min (col2, nint (max (lim1, lim2, lim3, lim4)) + 1)
		buf = imgs2r (im, colb, colc, lineb, linec)
		call msifit (msi, Memr[buf], colc - colb + 1, linec - lineb +
		    1, colc - colb + 1)

	    } else if (dy < 0.0 && (buf == NULL || line < linea)) {
		linea = max (line1, line - NLINES + 1)
		lineb = max (line1, linea - nedge)
		linec = min (line2, line + nedge)
		lim1 = xv
		lim2 = lim1 + double (width - 1) * dpx
		lim3 = xv + double (linea - line - 1) * ratio
		lim4 = lim3 + double (width - 1) * dpx
		colb = max (col1, int (min (lim1, lim2, lim3, lim4)) - 1)
		colc = min (col2, nint (max (lim1, lim2, lim3, lim4)) + 1)
		buf = imgs2r (im, colb, colc, lineb, linec)
		call msifit (msi, Memr[buf], colc - colb + 1, linec - lineb +
		    1, colc - colb + 1)
	    }

	    # Evaluate the interpolant.
	    call aaddkr (Memr[oxs], real (xv - colb + 1), Memr[xs], width)
	    call aaddkr (Memr[oys], real (yv - lineb + 1), Memr[ys], width)
	    call msivector (msi, Memr[xs], Memr[ys], Memr[yvals], width)

	    if (width == 1)
		y_vector[i] = Memr[yvals]
	    else {
		sum = 0.0
		do k = 1, width
		    sum = sum + Memr[yvals+k-1]
		y_vector[i] = sum / width
	    }

	    xv = xv + dx 
	    yv = yv + dy
	}	

	# Compute min and max values.
	call alimr (y_vector, nvals, zmin, zmax)
	 
	# Free memory .
	call msifree (msi)
	call sfree (sp)
end


# IE_GET_COL -- Average a strip perpendicular to a column vector and return
# vectors of point number and average pixel value. Also returned is the min
# and max value in the data vector.

procedure ie_get_col (im, x1, y1, x2, y2, nvals, width, btype,
        bconstant, x_vector, y_vector, zmin, zmax)

pointer im		# pointer to image header
real	x1, y1		# starting pixel of vector
real	x2, y2		# ending pixel of pixel
int	nvals		# number of samples along the vector
int	width		# width of strip to average over
int	btype		# Boundary extension type
real	bconstant	# Boundary extension constant
real	x_vector[ARB]	# Pixel numbers
real	y_vector[ARB]	# Average pixel values (returned)
real	zmin, zmax 	# min, max of data vector

real	sum
int	line, linea, lineb, linec
pointer sp, xs, ys, msi, yvals, buf
double	dx, dy, xoff, noff, xv, yv
int	i, j, k, nedge, col1, col2, line1, line2
pointer	imgs2r()
errchk	msiinit

begin
	call smark (sp)
	call salloc (xs, width, TY_REAL)
	call salloc (ys, width, TY_REAL)
	call salloc (yvals, width, TY_REAL)

	# Initialize the interpolator and the image data buffer.
	call msiinit (msi, II_BILINEAR)
	buf = NULL

	# Set the boundary.
	nedge  = max (2, width / 2 + 1)
	col1 = int (x1) - nedge
	col2 = nint (x1) + nedge
	line1  = int (min (y1, y2)) - nedge
	line2 =  nint (max (y1, y2)) + nedge
	call ie_setboundary (im, col1, col2, line1, line2, btype, bconstant)

	# Determine sampling perpendicular to vector.
	dx = 1.0d0
	if (nvals == 1)
	    dy = 0.0d0
	else
	    dy = (y2 - y1) / (nvals - 1)

	# Compute offset from the nominal vector to the first sample point.
	noff = (real (width) - 1.0) / 2.0
	xoff = noff * dx
	xv = x1 - xoff
	do j = 1, width
	    Memr[xs+j-1] = xv + double (j - col1)
	yv = y1

	# Loop over the output image lines.
	do i = 1, nvals { 
	    x_vector[i] = real (i)
	    line = yv

	    # Get the input image data and fit an interpolator to the data.
	    # The input data is buffered in a section of size NLINES + 2 *
	    # NEDGE.

	    if (dy >= 0.0 && (buf == NULL || line > (linea))) {
		linea = min (line2, line + NLINES - 1)
		lineb = max (line1, line - nedge)
		linec = min (line2, linea + nedge)
		buf = imgs2r (im, col1, col2, lineb, linec)
		call msifit (msi, Memr[buf], col2 - col1 + 1, linec - lineb +
		    1, col2 - col1 + 1)
	    } else if (dy < 0.0 && (buf == NULL || line < linea)) {
		linea = max (line1, line - NLINES + 1)
		lineb = max (line1, linea - nedge)
		linec = min (line2, line + nedge)
		buf = imgs2r (im, col1, col2, lineb, linec)
		call msifit (msi, Memr[buf], col2 - col1 + 1, linec - lineb +
		    1, col2 - col1 + 1)
	    }

	    # Evaluate the interpolant.
	    call amovkr (real (yv - lineb + 1), Memr[ys], width)
	    call msivector (msi, Memr[xs], Memr[ys], Memr[yvals], width)

	    if (width == 1)
		y_vector[i] = Memr[yvals]
	    else {
		sum = 0.0
		do k = 1, width
		    sum = sum + Memr[yvals+k-1]
		y_vector[i] = sum / width
	    }

	    yv = yv + dy
	}	

	# Compute min and max values.
	call alimr (y_vector, nvals, zmin, zmax)
	 
	# Free memory .
	call msifree (msi)
	call sfree (sp)
end


# IE_GET_ROW -- Average a strip parallel to a row vector and return
# vectors of point number and average pixel value. Also returned is the min
# and max value in the data vector.

procedure ie_get_row (im, x1, y1, x2, y2, nvals, width, btype, bconstant,
	x_vector, y_vector, zmin, zmax)

pointer im		# pointer to image header
real	x1, y1		# starting pixel of vector
real	x2, y2		# ending pixel of pixel
int	nvals		# number of samples along the vector
int	width		# width of strip to average over
int	btype		# Boundary extension type
real	bconstant	# Boundary extension constant
real	x_vector[ARB]	# Pixel numbers
real	y_vector[ARB]	# Average pixel values (returned)
real	zmin, zmax 	# min, max of data vector

double	dx, dy, yoff, noff, xv, yv
int	i, j, nedge, col1, col2, line1, line2
int	line, linea, lineb, linec
pointer sp, oys, xs, ys, yvals, msi, buf
errchk	imgs2r, msifit, msiinit
pointer	imgs2r()

begin
	call smark (sp)
	call salloc (oys, width, TY_REAL)
	call salloc (xs, nvals, TY_REAL)
	call salloc (ys, nvals, TY_REAL)
	call salloc (yvals, nvals, TY_REAL)

	# Initialize the interpolator and the image data buffer.
	call msiinit (msi, II_BILINEAR)
	buf = NULL

	# Set the boundary.
	nedge  = max (2, width / 2 + 1)
	col1 = int (min (x1, x2)) - nedge
	col2 = nint (max (x1, x2)) + nedge
	line1 = int (y1) - nedge
	line2 = nint (y1) + nedge
	call ie_setboundary (im, col1, col2, line1, line2, btype, bconstant)

	# Determine sampling perpendicular to vector.
	if (nvals == 1)
	    dx = 0.0d0
	else
	    dx = (x2 - x1) / (nvals - 1)
	dy = 1.0

	# Compute offset from the nominal vector to the first sample point.
	noff = (real (width) - 1.0) / 2.0
	xv = x1 - col1 + 1
	do i = 1, nvals {
	    Memr[xs+i-1] = xv
	    xv = xv + dx
	}
	yoff = noff * dy
	yv = y1 - yoff
	do j = 1, width
	    Memr[oys+j-1] = yv + double (j - 1)

	# Clear the accululator.
	call aclrr (y_vector, nvals)

	# Loop over the output image lines.
	do i = 1, width { 
	    line = yv

	    # Get the input image data and fit an interpolator to the data.
	    # The input data is buffered in a section of size NLINES + 2 *
	    # NEDGE.

	    if (dy >= 0.0 && (buf == NULL || line > (linea))) {
		linea = min (line2, line + NLINES - 1)
		lineb = max (line1, line - nedge)
		linec = min (line2, linea + nedge)
		buf = imgs2r (im, col1, col2, lineb, linec)
		if (buf == NULL)
		    call error (0, "Error reading input image.")
		call msifit (msi, Memr[buf], col2 - col1 + 1, linec - lineb +
		    1, col2 - col1 + 1)
	    } else if (dy < 0.0 && (buf == NULL || line < linea)) {
		linea = max (line1, line - NLINES + 1)
		lineb = max (line1, linea - nedge)
		linec = min (line2, line + nedge)
		buf = imgs2r (im, col1, col2, lineb, linec)
		if (buf == NULL)
		    call error (0, "Error reading input image.")
		call msifit (msi, Memr[buf], col2 - col1 + 1, linec - lineb +
		    1, col2 - col1 + 1)
	    }

	    # Evaluate the interpolant.
	    call amovkr (real (Memr[oys+i-1] - lineb + 1), Memr[ys], nvals)
	    call msivector (msi, Memr[xs], Memr[ys], Memr[yvals], nvals)

	    if (width == 1)
		call amovr (Memr[yvals], y_vector, nvals)
	    else 
		call aaddr (Memr[yvals], y_vector, y_vector, nvals)

	    yv = yv + dy
	}	

	# Compute the x and y vectors.
	do i = 1, nvals
	    x_vector[i] = real (i)
	if (width > 1)
	    call adivkr (y_vector, real (width), y_vector, nvals)

	# Compute min and max values.
	call alimr (y_vector, nvals, zmin, zmax)
	 
	# Free memory .
	call msifree (msi)
	call sfree (sp)
end


# IE_SETBOUNDARY -- Set boundary extension.

procedure ie_setboundary (im, col1, col2, line1, line2, btype, bconstant)

pointer	im			# IMIO pointer
int	col1, col2		# Range of columns
int	line1, line2		# Range of lines
int	btype			# Boundary extension type
real	bconstant		# Constant for constant boundary extension

int	btypes[5]
int	nbndrypix
data	btypes /BT_CONSTANT, BT_NEAREST, BT_REFLECT, BT_WRAP, BT_PROJECT/

begin
	nbndrypix = 0
	nbndrypix = max (nbndrypix, 1 - col1)
	nbndrypix = max (nbndrypix, col2 - IM_LEN(im, 1))
	nbndrypix = max (nbndrypix, 1 - line1)
	nbndrypix = max (nbndrypix, line2 - IM_LEN(im, 2))

	call imseti (im, IM_TYBNDRY, btypes[btype])
	call imseti (im, IM_NBNDRYPIX, nbndrypix + 1)
	if (btypes[btype] == BT_CONSTANT)
	    call imsetr (im, IM_BNDRYPIXVAL, bconstant)
end


# IE_BUFL2R -- Maintain buffer of image lines.  A new buffer is created when
# the buffer pointer is null or if the number of lines requested is changed.
# The minimum number of image reads is used.

procedure ie_bufl2r (im, col1, col2, line1, line2, buf)

pointer	im		# Image pointer
int	col1		# First image column of buffer
int	col2		# Last image column of buffer
int	line1		# First image line of buffer
int	line2		# Last image line of buffer
pointer	buf		# Buffer

pointer	buf1, buf2
int	i, ncols, nlines, nclast, llast1, llast2, nllast
errchk	malloc, realloc, imgs2r
pointer	imgs2r()

begin
	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	# If the buffer pointer is undefined then allocate memory for the
	# buffer.  If the number of columns or lines requested changes
	# reallocate the buffer.  Initialize the last line values to force
	# a full buffer image read.

	if (buf == NULL) {
	    call malloc (buf, ncols * nlines, TY_REAL)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	} else if ((nlines != nllast) || (ncols != nclast)) {
	    call realloc (buf, ncols * nlines, TY_REAL)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	}

	# Read only the image lines with are different from the last buffer.

	if (line1 < llast1) {
	    do i = line2, line1, -1 {
		if (i > llast1)
		    buf1 = buf + (i - llast1) * ncols
		else
		    buf1 = imgs2r (im, col1, col2, i, i)
		    
		buf2 = buf + (i - line1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	} else if (line2 > llast2) {
	    do i = line1, line2 {
		if (i < llast2)
		    buf1 = buf + (i - llast1) * ncols
		else
		    buf1 = imgs2r (im, col1, col2, i, i)
		    
		buf2 = buf + (i - line1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	}

	# Save the buffer parameters.

	llast1 = line1
	llast2 = line2
	nclast = ncols
	nllast = nlines
end
