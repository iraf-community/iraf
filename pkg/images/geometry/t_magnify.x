# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<math/iminterp.h>

# T_MAGNIFY -- Change coordinate origin and pixel interval in 2D images.
#
# The input and output images are given by image template lists.  The
# number of output images must match the number of input images.  Image
# sections are allowed in the input images and ignored in the output
# images.  If the input and output image names are the same then the
# magnification is performed to a temporary file which then replaces
# the input image.

# Interpolation types and boundary extension types.

define	ITYPES	"|nearest|linear|poly3|poly5|spline3|"
define	SZ_ITYPE	7
define	BTYPES	"|constant|nearest|reflect|wrap|project|"
define	SZ_BTYPE	8

procedure t_magnify ()

pointer	input				# Pointer to input image list
pointer	output				# Pointer to output image list
pointer	interp				# Pointer to image interpolation type
pointer	boundary			# Pointer to boundary extension type
real	bconst				# Boundary extension pixel value
real	xmag, ymag			# Image magnifications
real	dx, dy				# Step size
real	x1, y1				# Starting coordinates
real	x2, y2				# Ending coordinates
int	flux				# Flux conserve

int	list1, list2, itype, btype, logfd
pointer	sp, in, out, image1, image2, image3, time, mw
real	a, b, c, d, shifts[2], scale[2]

bool	clgetb(), envgetb(), fp_equalr()
int	clgwrd(), imtopen(), imtgetim(), imtlen(), open(), btoi(), immap()
long	clktime()
pointer	mw_openim()
real	clgetr()
errchk	open()

begin
	call smark (sp)
	call salloc (input, SZ_LINE, TY_CHAR)
	call salloc (output, SZ_LINE, TY_CHAR)
	call salloc (interp, SZ_ITYPE, TY_CHAR)
	call salloc (boundary, SZ_BTYPE, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (image2, SZ_FNAME, TY_CHAR)
	call salloc (image3, SZ_FNAME, TY_CHAR)
	call salloc (time, SZ_FNAME, TY_CHAR)

	# Get task parameters.
	call clgstr ("input", Memc[input], SZ_LINE)
	call clgstr ("output", Memc[output], SZ_LINE)
	itype = clgwrd ("interpolation", Memc[interp], SZ_ITYPE, ITYPES)
	btype = clgwrd ("boundary", Memc[boundary], SZ_BTYPE, BTYPES)
	bconst = clgetr ("constant")
	a = clgetr ("x1")
	b = clgetr ("x2")
	dx = clgetr ("dx")
	c = clgetr ("y1")
	d = clgetr ("y2")
	dy = clgetr ("dy")
	flux = btoi (clgetb ("fluxconserve")) 

	# If the pixel interval INDEF then use the a magnification factor
	# to determine the pixel interval.

	if (IS_INDEF (dx)) {
	    xmag = clgetr ("xmag")
	    if (xmag < 0.0)
		dx = -xmag
	    else if (xmag > 0.0)
		dx = 1.0 / xmag
	    else
		dx = 0.0
	}

	if (IS_INDEF (dy)) {
	    ymag = clgetr ("ymag")
	    if (ymag < 0.0)
		dy = -ymag
	    else if (ymag > 0.0)
		dy = 1.0 / ymag
	    else
		dy = 0.0
	}
	if (fp_equalr (dx, 0.0) || fp_equalr (dy, 0.0)) {
	    call error (0, "Illegal magnification")
	} else {
	    xmag = 1.0 / dx
	    ymag = 1.0 / dy
	}


	# Open the log file.
	call clgstr ("logfile", Memc[image1], SZ_FNAME)
	iferr (logfd = open (Memc[image1], APPEND, TEXT_FILE))
	    logfd = NULL

	# Expand the input and output image lists.
	list1 = imtopen (Memc[input])
	list2 = imtopen (Memc[output])
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same")
	}

	# Magnify each set of input/output images with the 2D interpolation
	# package.

	while ((imtgetim (list1, Memc[image1], SZ_FNAME) != EOF) &&
	    (imtgetim (list2, Memc[image2], SZ_FNAME) != EOF)) {

	    # Map the input and output images.
	    call xt_mkimtemp (Memc[image1], Memc[image2], Memc[image3],
	        SZ_FNAME)
	    in = immap (Memc[image1], READ_ONLY, 0)
	    out = immap (Memc[image2], NEW_COPY, in)

	    # Set the limits of the output image.
	    x1 = a
	    x2 = b
	    y1 = c
	    y2 = d


	    # Magnify the image making sure to update the wcs.
	    if (IM_NDIM(in) == 1) {
		call mg_magnify1 (in, out, itype, btype, bconst, x1, x2, dx,
		    flux)
		if (!envgetb ("nomwcs")) {
		    mw = mw_openim (in)
		    scale[1] = xmag
		    shifts[1] =  1. - xmag * x1
		    call mw_scale (mw, scale, 01B)
		    call mw_shift (mw, shifts, 01B)
		    call mw_saveim (mw, out)
		    call mw_close (mw)
		}
	    } else if (IM_NDIM(in) == 2) {
	        call mg_magnify2 (in, out, itype, btype, bconst, x1, x2, dx,
	            y1, y2, dy, flux)
		if (!envgetb ("nomwcs")) {
		    mw = mw_openim (in)
		    scale[1] = xmag
		    scale[2] = ymag
		    shifts[1] =  1. - xmag * x1
		    shifts[2] =  1. - ymag * y1
		    call mw_scale (mw, scale, 03B)
		    call mw_shift (mw, shifts, 03B)
		    call mw_saveim (mw, out)
		    call mw_close (mw)
		}
	    } else {
	        call imunmap (in)
	        call imunmap (out)
	        call xt_delimtemp (Memc[image2], Memc[image3])
		if (logfd != NULL) {
		    call cnvtime (clktime(0), Memc[time], SZ_FNAME)
		    call fprintf (logfd, "\n%s\n")
		        call pargstr (Memc[image2])
		    call fprintf (logfd,
		        "  Cannot magnify image %s to image %s.\n")
		        call pargstr (Memc[image1])
		        call pargstr (Memc[image3])
		    call fprintf (logfd, "  Dimensions are greater than 2.\n")
		}
		next
	    }

	    if (logfd != NULL) {
		call cnvtime (clktime (0), Memc[time], SZ_FNAME)
		call fprintf (logfd, "\n%s\n")
		    call pargstr (Memc[image2])
		call fprintf (logfd, "  Magnify image %s to image %s.\n")
		    call pargstr (Memc[image1])
		    call pargstr (Memc[image3])
		call fprintf (logfd, "  Interpolation is %s.\n")
		    call pargstr (Memc[interp])
		call fprintf (logfd, "  Boundary extension is %s.\n")
		    call pargstr (Memc[boundary])
		if (btype == 1) {
		    call fprintf (logfd, "  Boundary pixel constant is %g.\n")
			call pargr (bconst)
		}
		call fprintf (logfd,
		    "  Output coordinates in terms of input coordinates:\n")
		call fprintf (logfd,
		    "    x1 = %10.4g, x2 = %10.4g, dx = %10.6g\n")
		    call pargr (x1)
		    call pargr (x2)
		    call pargr (dx)
		if (IM_NDIM(in) == 2) {
		    call fprintf (logfd,
		        "    y1 = %10.4g, y2 = %10.4g, dy = %10.6g\n")
		        call pargr (y1)
		        call pargr (y2)
		        call pargr (dy)
		}
	    }

	    call imunmap (in)
	    call imunmap (out)
	    call xt_delimtemp (Memc[image2], Memc[image3])

	}

	call imtclose (list1)
	call imtclose (list2)
	call close (logfd)
	call sfree (sp)
end


define	NLINES		16	# Number of input lines to use for interpolation
define	NMARGIN		3	# Number of edge lines to add for interpolation
define	NMARGIN_SPLINE3	16	# Number of edge lines to add for interpolation


# MG_MAGNIFY1 -- Magnify the input input image to create the output image.

procedure mg_magnify1 (in, out, itype, btype, bconst, x1, x2, dx, flux)

pointer	in			# pointer to the input image
pointer	out			# pointer to the output image
int	itype			# Interpolation type
int	btype			# Boundary extension type
real	bconst			# Boundary extension constant
real	x1, x2			# Starting and ending points of output image
real	dx			# Pixel interval
int	flux			# Conserve flux?

int	i, nxin, nxout, nxymargin
int	col1, col2
int	itypes[II_NTYPES2D]
pointer	sp, x, z, buf, asi

pointer	imgs1r(), impl1r()

data	itypes /II_NEAREST, II_LINEAR, II_POLY3, II_POLY5, II_SPLINE3/

begin
	# Set the default values for the output image limits if they are INDEF
	# and calculate the number of output pixels.

	if (IS_INDEF (x1))
	    x1 = 1.
	if (IS_INDEF (x2))
	    x2 = IM_LEN (in, 1)

	# Set the number of output pixels in the image header.

	nxout = (x2 - x1) / dx + 1
	IM_LEN(out, 1) = nxout

	# Round the coordinate limits to include the output image coordinate
	# limits and the set boundary.

	col1 = x1
	col2 = nint (x2)
	if (itype == II_SPLINE3)
	    nxymargin = NMARGIN_SPLINE3
	else
	    nxymargin = NMARGIN
	call mg_setboundary1 (in, col1, col2, btype, bconst, nxymargin)

	# Initialize the interpolator.
	call asiinit (asi, itypes[itype])

	# Allocate memory for the interpolation coordinates.
	# Also initialize the image data buffer.

	call smark (sp)
	call salloc (x, nxout, TY_REAL)

	# Set the x interpolation coordinates.  The coordinates are relative
	# to the boundary extended input image.

	nxin = col2 - col1 + 1
	do i = 1, nxout
	    Memr[x+i-1] = x1 + (i - 1) * dx - col1 + 1

	# Fit the output image.
	buf = imgs1r (in, col1, col2)
	call asifit (asi, Memr[buf], nxin)

	# Evaluate the output image pixel values.
	z = impl1r (out)
	call asivector (asi, Memr[x], Memr[z], nxout)
	if (flux == YES)
	    call amulkr (Memr[z], dx, Memr[z], nxout)

	# Free memory and unmap the images.
	call asifree (asi)
	call sfree (sp)
end


# MG_MAGNIFY2 -- Magnify the input input image to create the output image.

procedure mg_magnify2 (in, out, itype, btype, bconst, x1, x2, dx, y1, y2,
        dy, flux)

pointer	in			# pointer to the input image
pointer	out			# pointer to the output image
int	itype			# Interpolation type
int	btype			# Boundary extension type
real	bconst			# Boundary extension constant
real	x1, y1			# Starting point of output image
real	x2, y2			# Ending point of output image
real	dx, dy			# Pixel interval
int	flux			# Conserve flux?

int	i, nxin, nxout, nyout, nxymargin
int	line, linea, lineb, linec
int	col1, col2, line1, line2
int	itypes[II_NTYPES2D]
real	yout
pointer	msi
pointer	sp, x, y, z, buf

pointer	impl2r()

data	itypes /II_BINEAREST, II_BILINEAR, II_BIPOLY3, II_BIPOLY5, II_BISPLINE3/

begin
	# Set the default values for the output image limits if they are INDEF
	# and calculate the number of output pixels.

	if (IS_INDEF (x1))
	    x1 = 1.
	if (IS_INDEF (x2))
	    x2 = IM_LEN (in, 1)
	if (IS_INDEF (y1))
	    y1 = 1.
	if (IS_INDEF (y2))
	    y2 = IM_LEN (in, 2)

	# Set the number of output pixels in the image header.

	if (itype == II_SPLINE3)
	    nxymargin = NMARGIN_SPLINE3
	else
	    nxymargin = NMARGIN
	nxout = (x2 - x1) / dx + 1
	nyout = (y2 - y1) / dy + 1
	IM_LEN(out, 1) = nxout
	IM_LEN(out, 2) = nyout

	# Round the coordinate limits to include the output image coordinate
	# limits and the set boundary.

	col1 = x1
	col2 = nint (x2)
	line1 = y1
	line2 = nint (y2)
	call mg_setboundary2 (in, col1, col2, line1, line2, btype, bconst,
	    nxymargin)

	# Initialize the interpolator.
	call msiinit (msi, itypes[itype])

	# Allocate memory for the interpolation coordinates.
	# Also initialize the image data buffer.

	call smark (sp)
	call salloc (x, nxout, TY_REAL)
	call salloc (y, nxout, TY_REAL)
	buf = NULL

	# Set the x interpolation coordinates which do not change from
	# line to line.  The coordinates are relative to the boundary
	# extended input image.

	nxin = col2 - col1 + 1
	do i = 1, nxout
	    Memr[x+i-1] = x1 + (i - 1) * dx - col1 + 1

	# Do each line of the output image.

	do i = 1, nyout {

	    yout = y1 + (i - 1) * dy
	    line = yout

	    # Get the input image data and fit an interpolator to the data.
	    # The input image data is buffered in a section of size NLINES
	    # + 2 * NMARGIN (except at the edge).

	    if ((buf == NULL) || (line > linea)) {
		linea = min (line2, line + NLINES - 1)
		lineb = max (line1 - nxymargin, line - nxymargin)
		linec = min (line2 + nxymargin, linea + nxymargin)
	        call mg_bufl2r (in, col1, col2, lineb, linec, buf)
	        call msifit (msi, Memr[buf], nxin, linec - lineb + 1, nxin)
	    }

	    # Evaluate the output image pixel values.  The input coordinates
	    # must be offset to the interpolation data grid.

	    call amovkr (yout - lineb + 1, Memr[y], nxout)
	    z = impl2r (out,i)
	    call msivector (msi, Memr[x], Memr[y], Memr[z], nxout)
	    if (flux == YES)
	        call amulkr (Memr[z], dx * dy, Memr[z], nxout)
	}

	# Free memory and unmap the images.
	call msifree (msi)
	call mfree (buf, TY_REAL)
	call sfree (sp)
end


# MG_BUFL2R -- Maintain buffer of image lines.  A new buffer is created when
# the buffer pointer is null or if the number of lines requested is changed.
# The minimum number of image reads is used.

procedure mg_bufl2r (im, col1, col2, line1, line2, buf)

pointer	im		# Image pointer
int	col1		# First image column of buffer
int	col2		# Last image column of buffer
int	line1		# First image line of buffer
int	line2		# Last image line of buffer
pointer	buf		# Buffer

int	i, ncols, nlines, nclast, llast1, llast2, nllast
pointer	buf1, buf2

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


# MG_SETBOUNDARY1 -- Set boundary extension for a 1D image.

procedure mg_setboundary1 (im, col1, col2, btype, bconst, nxymargin)

pointer	im			# IMIO pointer
int	col1, col2		# Range of columns
int	btype			# Boundary extension type
real	bconst			# Constant for constant boundary extension
int	nxymargin		# Number of margin pixels

int	btypes[5]
int	nbndrypix

data	btypes /BT_CONSTANT, BT_NEAREST, BT_REFLECT, BT_WRAP, BT_PROJECT/

begin
	nbndrypix = 0
	nbndrypix = max (nbndrypix, 1 - col1)
	nbndrypix = max (nbndrypix, col2 - IM_LEN(im, 1))

	call imseti (im, IM_TYBNDRY, btypes[btype])
	call imseti (im, IM_NBNDRYPIX, nbndrypix + nxymargin + 1)
	if (btypes[btype] == BT_CONSTANT)
	    call imsetr (im, IM_BNDRYPIXVAL, bconst)
end


# MG_SETBOUNDARY2 -- Set boundary extension for a 2D image.

procedure mg_setboundary2 (im, col1, col2, line1, line2, btype, bconst,
	nxymargin)

pointer	im			# IMIO pointer
int	col1, col2		# Range of columns
int	line1, line2		# Range of lines
int	btype			# Boundary extension type
real	bconst			# Constant for constant boundary extension
int	nxymargin		# Number of margin pixels to allow

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
	call imseti (im, IM_NBNDRYPIX, nbndrypix + nxymargin + 1)
	if (btypes[btype] == BT_CONSTANT)
	    call imsetr (im, IM_BNDRYPIXVAL, bconst)
end
