# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <imset.h>
include <math/iminterp.h>

# T_IMSHIFT -- Shift a 2-D image in x and y

procedure t_imshift()

pointer	imtlist1		# Input image list
pointer	imtlist2		# Output image list

pointer	image1			# Input image
pointer	image2			# Output image
pointer imtemp			# Temporary file
pointer	sfile			# Text file containing list of shifts
pointer	database		# Name of geomap database
pointer	records			# Name of geomap transforms
pointer	recname			# Transform name

int	list1, list2, interp_type, boundary_type, ixshift, iyshift, nshifts
pointer	sp, str, xs, ys, im1, im2, sf
real	txshift, tyshift, xshift, yshift, constant

bool	fp_equalr()
int	imtopen(), imtgetim(), imtlen(), clgwrd(), open(), ish_rshifts()
real	clgetr()
pointer	immap()
errchk	ish_ishiftxy, ish_gshiftxy

begin
	call smark (sp)
	call salloc (imtlist1, SZ_LINE, TY_CHAR)
	call salloc (imtlist2, SZ_LINE, TY_CHAR)
	call salloc (image1, SZ_LINE, TY_CHAR)
	call salloc (image2, SZ_LINE, TY_CHAR)
	call salloc (imtemp, SZ_LINE, TY_CHAR)
	call salloc (sfile, SZ_FNAME, TY_CHAR)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (records, SZ_FNAME, TY_CHAR)
	call salloc (recname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get task parameters.
	call clgstr ("input", Memc[imtlist1], SZ_FNAME)
	call clgstr ("output", Memc[imtlist2], SZ_FNAME)
	call clgstr ("shifts_file", Memc[sfile], SZ_FNAME)

	# Get the 2-D interpolation parameters.
	interp_type = clgwrd ("interp_type", Memc[str], SZ_LINE,
	    ",nearest,linear,poly3,poly5,spline3,")
	boundary_type = clgwrd ("boundary_type", Memc[str], SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	if (boundary_type == BT_CONSTANT)
	    constant = clgetr ("constant")

	list1 = imtopen (Memc[imtlist1])
	list2 = imtopen (Memc[imtlist2])
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same.")
	}

	# Determine the source of the shifts.
	if (Memc[sfile] != EOS) {
	    sf = open (Memc[sfile], READ_ONLY, TEXT_FILE)
	    call salloc (xs, imtlen (list1), TY_REAL)
	    call salloc (ys, imtlen (list1), TY_REAL)
	    nshifts = ish_rshifts (sf, Memr[xs], Memr[ys], imtlen (list1))
	    if (nshifts != imtlen (list1))
		call error (0,
		    "The number of input images and shifts are not the same.")
	} else {
	    txshift = clgetr ("xshift")
	    tyshift = clgetr ("yshift")
	}


	# Do each set of input and output images.
	nshifts = 0
	while ((imtgetim (list1, Memc[image1], SZ_FNAME) != EOF) &&
	      (imtgetim (list2, Memc[image2], SZ_FNAME) != EOF)) {
	    
	    call xt_mkimtemp (Memc[image1], Memc[image2], Memc[imtemp],
	        SZ_FNAME)

	    im1 = immap (Memc[image1], READ_ONLY, 0)
	    im2 = immap (Memc[image2], NEW_COPY, im1)

	    if (sf != NULL) {
		xshift = Memr[xs+nshifts]
		yshift = Memr[ys+nshifts]
	    } else {
		xshift = txshift
		yshift = tyshift
	    }

	    ixshift = int (xshift)
	    iyshift = int (yshift)

	    iferr {
		if (interp_type == II_NEAREST)
		    call ish_ishiftxy (im1, im2, nint (xshift), nint (yshift),
		        interp_type, boundary_type, constant)
		else if (fp_equalr (xshift, real (ixshift)) &&
		    fp_equalr (yshift, real (iyshift)))
		    call ish_ishiftxy (im1, im2, ixshift, iyshift, interp_type,
			boundary_type, constant)
		else
		    call ish_gshiftxy (im1, im2, xshift, yshift, interp_type,
			boundary_type, constant)
	    } then {

		call eprintf ("Error shifting image: %s\n")
		    call pargstr (Memc[image1])
		call erract (EA_WARN)
	        call imunmap (im1)
	        call imunmap (im2)
		call imdelete (Memc[image2])

	    } else {

	        # finish up
	        call imunmap (im1)
	        call imunmap (im2)
	        call xt_delimtemp (Memc[image2], Memc[imtemp])

	    }

	    nshifts = nshifts + 1
	}

	if (sf != NULL)
	    call close (sf)
	call imtclose (list1)
	call imtclose (list2)
	call sfree (sp)
end


# ISH_ISHIFTXY -- Procedure to shift a 2-D image by integral pixels in x and y.

procedure ish_ishiftxy (im1, im2, ixshift, iyshift, interp_type, boundary_type,
    constant)

pointer	im1		# pointer to the input image
pointer	im2		# pointer to the output image
int	ixshift		# shift in x and y
int	iyshift		#
int	interp_type	# type of interpolant
int	boundary_type	# type of boundary extension
real	constant	# constant for boundary extension

int	i, x1col, x2col, yline
int	ncols, nlines, nbpix
long	v[IM_MAXDIM]
pointer	buf1, buf2

int	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()
pointer	imgs2s(), imgs2i(), imgs2l(), imgs2r(), imgs2d(), imgs2x()

errchk	impnls, impnli, impnll, impnlr, impnld, impnlx
errchk	imgs2s, imgs2i, imgs2l, imgs2r, imgs2d, imgs2x

begin
	ncols = IM_LEN(im1,1)
	nlines = IM_LEN(im1,2)

	# Cannot shift off image.
	if (ixshift < -ncols || ixshift > ncols)
	    call error (0, "ISHIFTXY: X shift out of bounds.")
	if (iyshift < -nlines || iyshift > nlines)
	    call error (0, "ISHIFTXY: Y shift out of bounds.")

	# Calculate the shift.
	switch (boundary_type) {
	case BT_CONSTANT,BT_REFLECT,BT_NEAREST:
	    ixshift = min (ncols, max (-ncols, ixshift))
	    iyshift = min (nlines, max (-nlines, iyshift))
	case BT_WRAP:
	    ixshift = mod (ixshift, ncols)
	    iyshift = mod (iyshift, nlines)
	}

	# Set the boundary extension values.
	nbpix = max (abs (ixshift), abs (iyshift))
	call imseti (im1, IM_NBNDRYPIX, nbpix)
	call imseti (im1, IM_TYBNDRY, boundary_type)
	if (boundary_type == BT_CONSTANT)
	    call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Get column boundaries in the input image.
	x1col= max (-ncols + 1, - ixshift + 1) 
	x2col = min (2 * ncols,  ncols - ixshift)

	call amovkl (long (1), v, IM_MAXDIM)

	# Shift the image using the appropriate data type operators.
	switch (IM_PIXTYPE(im1)) {
	case TY_SHORT:
	    do i = 1, nlines {
	        if (impnls (im2, buf2, v) == EOF)
		    call error (0, "ISHIFTXY: Error writing in image.")
		yline = i - iyshift
		buf1 = imgs2s (im1, x1col, x2col, yline, yline)
		if (buf1 == EOF)
		    call error (0, "ISHIFTXY: Error writing in image.")
		call amovs (Mems[buf1], Mems[buf2], ncols)
	    }
	case TY_INT:
	    do i = 1, nlines {
	        if (impnli (im2, buf2, v) == EOF)
		    call error (0, "ISHIFTXY: Error writing in image.")
		yline = i - iyshift
		buf1 = imgs2i (im1, x1col, x2col, yline, yline)
		if (buf1 == EOF)
		    call error (0, "ISHIFTXY: Error writing in image.")
		call amovi (Memi[buf1], Memi[buf2], ncols)
	    }
	case TY_LONG:
	    do i = 1, nlines {
	        if (impnll (im2, buf2, v) == EOF)
		    call error (0, "ISHIFTXY: Error writing in image.")
		yline = i - iyshift
		buf1 = imgs2l (im1, x1col, x2col, yline, yline)
		if (buf1 == EOF)
		    call error (0, "ISHIFTXY: Error writing in image.")
		call amovl (Meml[buf1], Meml[buf2], ncols)
	    }
	case TY_REAL:
	    do i = 1, nlines {
	        if (impnlr (im2, buf2, v) == EOF)
		    call error (0, "ISHIFTXY: Error writing in image.")
		yline = i - iyshift
		buf1 = imgs2r (im1, x1col, x2col, yline, yline)
		if (buf1 == EOF)
		    call error (0, "ISHIFTXY: Error writing in image.")
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	case TY_DOUBLE:
	    do i = 1, nlines {
	        if (impnld (im2, buf2, v) == EOF)
		    call error (0, "ISHIFTXY: Error writing in image.")
		yline = i - iyshift
		buf1 = imgs2d (im1, x1col, x2col, yline, yline)
		if (buf1 == EOF)
		    call error (0, "ISHIFTXY: Error writing in image.")
		call amovd (Memd[buf1], Memd[buf2], ncols)
	    }
	case TY_COMPLEX:
	    do i = 1, nlines {
	        if (impnlx (im2, buf2, v) == EOF)
		    call error (0, "ISHIFTXY: Error writing in image.")
		yline = i - iyshift
		buf1 = imgs2x (im1, x1col, x2col, yline, yline)
		if (buf1 == EOF)
		    call error (0, "ISHIFTXY: Error writing in image.")
		call amovx (Memx[buf1], Memx[buf2], ncols)
	    }
	default:
	    call error (0, "ISHIFTXY: Unknown IRAF type.")
	}
end

define	NMARGIN		3	# number of boundary pixels required	
define	NYOUT		16	# number of lines output at once

# ISH_GSHIFTXY -- Procedure to shift an image by fractional pixels in x and y.

procedure ish_gshiftxy (im1, im2, xshift, yshift, interp_type, boundary_type,
    constant)

pointer		im1		# pointer to input image
pointer		im2		# pointer to output image
real		xshift		# shift in x direction
real		yshift		# shift in y direction
int		interp_type	# type of interpolant
int		boundary_type	# type of boundary extension
real		constant	# value of constant for boundary extension

int	i
int	ncols, nlines, nbpix, fstline, lstline
int	cin1, cin2, nxin, lin1, lin2, nyin
int	lout1, lout2, nyout
real	xshft, yshft, deltax, deltay, dx, dy, cx, ly
pointer	sp, x, y, msi, sinbuf, soutbuf

bool	fp_equalr()
pointer	imps2r()

errchk	imgs2r, imps2r
errchk	msiinit, msifree, msifit, msigrid
errchk	smark, salloc, sfree

begin
	ncols = IM_LEN(im1,1)
	nlines = IM_LEN(im1,2)

	# Check for out of bounds shift.
	if (xshift < -ncols || xshift > ncols)
	    call error (0, "GSHIFTXY: X shift out of bounds.")
	if (yshift < -nlines || yshift > nlines)
	    call error (0, "GSHIFTXY: Y shift out of bounds.")

	# Get the real shift.
	if (boundary_type == BT_WRAP) {
	    xshft = mod (xshift, real (ncols))
	    yshft = mod (yshift, real (nlines))
	} else {
	    xshft = xshift
	    yshft = yshift
	}

	# Set boundary extension parameters.
	nbpix = max (int (abs(xshft)+1.0), int (abs(yshft)+1.0)) + NMARGIN
	call imseti (im1, IM_NBNDRYPIX, nbpix)
	call imseti (im1, IM_TYBNDRY, boundary_type)
	if (boundary_type == BT_CONSTANT)
	    call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Allocate temporary space.
	call smark (sp)
	call salloc (x, ncols, TY_REAL)
	call salloc (y, nlines, TY_REAL)
	sinbuf = NULL

	# Initialize the 2-D interpolation routines.
	call msiinit (msi, interp_type)

	# Define the x interpolation coordinates.
	dx = abs (xshft - int (xshft))
	if (fp_equalr (dx, 0.0))
	    deltax = NMARGIN
	else if (xshft > 0.)
	    deltax = 1. - dx + NMARGIN
	else
	    deltax = dx + NMARGIN
	do i = 1, ncols
	    Memr[x+i-1] = i + deltax

	# Define the y interpolation coordinates.
	dy = abs (yshft - int (yshft))
	if (fp_equalr (dy, 0.0))
	    deltay = NMARGIN
	else if (yshft > 0.)
	    deltay = 1. - dy +  NMARGIN
	else
	    deltay = dy + NMARGIN
	do i = 1, NYOUT
	    Memr[y+i-1] = i + deltay

	# Define column ranges in the input image.
	cx = 1. - NMARGIN - xshft
	if ((cx <= 0.) &&  (! fp_equalr (dx, 0.0)))
	    cin1 = int (cx) - 1
	else
	    cin1 = int (cx)
	cin2 = ncols - xshft + NMARGIN + 1
	nxin = cin2 - cin1 + 1

	# Loop over output sections.
	for (lout1 = 1; lout1 <= nlines; lout1 = lout1 + NYOUT) { 

	    # Define range of output lines.
	    lout2 = min (lout1 + NYOUT - 1, nlines)
	    nyout = lout2 - lout1 + 1

	    # Define correspoding range of input lines.
	    ly = lout1 - NMARGIN - yshft
	    if ((ly <= 0) && (! fp_equalr (dy, 0.0)))
	        lin1 = int (ly) - 1
	    else
		lin1 = int (ly)
	    lin2 = lout2 - yshft + NMARGIN + 1
	    nyin = lin2 - lin1 + 1

	    # Get appropriate input section and calculate the coefficients.
	    if ((sinbuf == NULL) || (lin1 < fstline) || (lin2 > lstline)) {
		fstline = lin1
		lstline = lin2
		call ish_buf (im1, cin1, cin2, lin1, lin2, sinbuf)
	        call msifit (msi, Memr[sinbuf], nxin, nyin, nxin)
	    }

	    # Output the section.
	    soutbuf = imps2r (im2, 1, ncols, lout1, lout2)
	    if (soutbuf == EOF)
		call error (0, "GSHIFTXY: Error writing output image.")

	    # Evaluate the interpolant.
	    call msigrid (msi, Memr[x], Memr[y], Memr[soutbuf], ncols, nyout,
		ncols)
	}

	call msifree (msi)
	call sfree (sp)
end

# ISH_BUF -- Procedure to provide a buffer of image lines with minimum reads.

procedure ish_buf (im, col1, col2, line1, line2, buf)

pointer	im		# pointer to input image
int	col1, col2	# column range of input buffer
int	line1, line2	# line range of input buffer
pointer	buf		# buffer

int	i, ncols, nlines, nclast, llast1, llast2, nllast
pointer	buf1, buf2

pointer	imgs2r()

begin
	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	if (buf == NULL) {
	    call malloc (buf, ncols * nlines, TY_REAL)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	} else if ((nlines != nllast) || (ncols != nclast)) {
	    call realloc (buf, ncols * nlines, TY_REAL)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	}

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

	llast1 = line1
	llast2 = line2
	nclast = ncols
	nllast = nlines
end


# ISH_RSHIFTS -- Procedure to read shifts from a file.

int procedure ish_rshifts (fd, x, y, max_nshifts)

int	fd		# pointer to shifts file descriptor
real	x[ARB]		# x array
real	y[ARB]		# y array
int	max_nshifts	# the maximum number of shifts

int	nshifts
int	fscan(), nscan()

begin
	nshifts = 0
	while (fscan (fd) != EOF && nshifts < max_nshifts) {
	    call gargr (x[nshifts+1])
	    call gargr (y[nshifts+1])
	    if (nscan () != 2)
		next
	    nshifts = nshifts + 1
	}

	return (nshifts)
end
