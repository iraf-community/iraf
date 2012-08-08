# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include <imset.h>
include <math/iminterp.h>

define	NMARGIN		 3

# SH_LINES -- Shift image lines.
#
# If an integer shift is given then do an efficient integer shift
# without datatype I/O conversions and using array move operators.
# If the shift is non-integer then use image interpolation.

procedure sh_lines (im1, im2, shift, boundary, constant, interpstr)

pointer	im1				# Input image descriptor
pointer	im2				# Output image descriptor
real	shift				# Shift in pixels
int	boundary			# Boundary extension type
real	constant			# Constant boundary extension
char	interpstr[ARB]			# Interpolation type

int	i, nsinc, nincr, ncols, nimcols, nlines, nbpix, nmargin, interpolation
long	v1[IM_MAXDIM], v2[IM_MAXDIM], vout[IM_MAXDIM]
real	dx, deltax, cx
pointer	sp, x, asi, junk, buf1, buf2

bool	fp_equalr()
int	imggsr(), impnlr(), asigeti()

begin
	# Check for out of bounds shifts.
	ncols = IM_LEN(im1, 1)
	if (shift < -ncols || shift > ncols)
	    call error (0, "SHIFTLINES: Shift out of bounds")

	# Compute the shift.
	dx = abs (shift - int (shift))
	if (fp_equalr (dx, 0.0))
	    deltax = 0.0
	else if (shift > 0.0)
	    deltax = 1.0 - dx
	else
	    deltax = dx

	# Initialize the interpolation.
	call asitype (interpstr, interpolation, nsinc, nincr, cx)
	if (interpolation == II_LSINC || interpolation == II_SINC)
	    call asisinit (asi, II_LSINC, nsinc, 1, deltax - nint (deltax),
	        0.0)
	else
	    call asisinit (asi, interpolation, nsinc, 1, cx, 0.0)

	# Set up the image boundary conditions.
	if (interpolation == II_SINC || interpolation == II_LSINC)
	    nmargin = asigeti (asi, II_ASINSINC)
	else
	    nmargin = NMARGIN
	nbpix = int (abs (shift) + 1.0) + nmargin
	call imseti (im1, IM_TYBNDRY, boundary)
	call imseti (im1, IM_NBNDRYPIX, nbpix)
	call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Allocate space for and set up the interpolation coordinates.
	call smark (sp)
	call salloc (x, 2 * ncols, TY_REAL)
	deltax = deltax + nmargin
	if (interpolation == II_DRIZZLE) {
	    do i = 1, ncols {
	        Memr[x+2*i-2] = i + deltax - 0.5
	        Memr[x+2*i-1] = i + deltax + 0.5
	    }
	} else {
	    do i = 1, ncols
	        Memr[x+i-1] = i + deltax
	}

	# Initialize the input v vectors.
	cx = 1. - nmargin - shift
	if ((cx <= 0.0) && (! fp_equalr (dx, 0.0)))
	    v1[1] = long (cx) - 1
	else
	    v1[1] = long (cx)
	v2[1] = ncols - shift + nmargin + 1
	nimcols = v2[1] - v1[1] + 1
	do i = 2, IM_NDIM(im1) {
	    v1[i] = long (1)
	    v2[i] = long (1)
	}

	# Compute the number of output lines.
	nlines = 1
	do i = 2, IM_NDIM(im1)
	    nlines = nlines * IM_LEN(im1, i)

	# Initialize the output v vector.
	call amovkl (long(1), vout, IM_MAXDIM)

	# Shift the images.
	do i = 1, nlines {

	    # Get the input image data buffer.
	    buf1 = imggsr (im1, v1, v2, IM_NDIM(im1))
	    if (buf1 == EOF)
		call error (0, "SHIFTLINES: Error reading input image\n")

	    # Get the output image data buffer.
	    junk = impnlr (im2, buf2, vout)
	    if (junk == EOF)
		call error (0, "SHIFTLINES: Error writing output image\n")

	    # Evaluate the interpolation at the shifted points.
	    call asifit (asi, Memr[buf1], nimcols)
	    call asivector (asi, Memr[x], Memr[buf2], ncols)

	    # Increment the v vectors.
	    call sh_loop (v1, v2, IM_LEN(im1,1), IM_NDIM(im1))
	}

	call asifree (asi)
	call sfree (sp)
end


# SH_LINESI -- Integer pixel shift.
#
# Shift the pixels in an image by an integer amount.  Perform I/O without
# out type conversion and use array move operators.

procedure sh_linesi (im1, im2, shift, boundary, constant)

pointer	im1				# Input image descriptor
pointer	im2				# Output image descriptor
int	shift				# Integer shift
int	boundary			# Boundary extension type
real	constant			# Constant for boundary extension

int	i, ncols, nlines, junk
long	v1[IM_MAXDIM], v2[IM_MAXDIM], vout[IM_MAXDIM]
pointer	buf1, buf2

int	imggss(), imggsi(), imggsl(), imggsr(), imggsd(), imggsx()
int	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()

begin
	# Set the boundary extension parameters.
	call imseti (im1, IM_NBNDRYPIX, abs (shift))
	call imseti (im1, IM_TYBNDRY, boundary)
	call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Return if shift off image.
	ncols = IM_LEN(im1, 1)
	if (shift < -ncols || shift > ncols)
	    call error (0, "Shiftlinesi: shift out of bounds")

	# Setup start vector for sequential reads and writes.
	v1[1] = max (-ncols + 1, -shift + 1)
	v2[1] = min (2 * ncols, ncols - shift)
	do i = 2, IM_NDIM(im1) {
	    v1[i] = long (1)
	    v2[i] = long (1)
	}
	call amovkl (long(1), vout, IM_MAXDIM)

	# Setup line counter.
	nlines = 1
	do i = 2, IM_NDIM(im1)
	    nlines = nlines * IM_LEN(im1, i)


	# Shift the image using appropriate datatype operators.
	switch (IM_PIXTYPE(im1)) {

	case TY_SHORT:
	    do i = 1, nlines {
	        buf1 = imggss (im1, v1, v2, IM_NDIM(im1))
		if (buf1 == EOF)
		    call error (0, "Shiftlines: error reading input image")
		junk = impnls (im2, buf2, vout)
		if (junk == EOF)
		    call error (0, "Shiftlinesi: error writing out image")
		call amovs (Mems[buf1], Mems[buf2], ncols)
		call sh_loop (v1, v2, IM_LEN(im1,1), IM_NDIM(im1))
	    }

	case TY_INT:
	    do i = 1, nlines {
	        buf1 = imggsi (im1, v1, v2, IM_NDIM(im1))
		if (buf1 == EOF)
		    call error (0, "Shiftlines: error reading input image")
		junk = impnli (im2, buf2, vout)
		if (junk == EOF)
		    call error (0, "Shiftlinesi: error writing out image")
		call amovi (Memi[buf1], Memi[buf2], ncols)
		call sh_loop (v1, v2, IM_LEN(im1,1), IM_NDIM(im1))
	    }

	case TY_LONG:
	    do i = 1, nlines {
	        buf1 = imggsl (im1, v1, v2, IM_NDIM(im1))
		if (buf1 == EOF)
		    call error (0, "Shiftlines: error reading input image")
		junk = impnll (im2, buf2, vout)
		if (junk == EOF)
		    call error (0, "Shiftlinesi: error writing out image")
		call amovl (Meml[buf1], Meml[buf2], ncols)
		call sh_loop (v1, v2, IM_LEN(im1,1), IM_NDIM(im1))
	    }

	case TY_REAL:
	    do i = 1, nlines {
	        buf1 = imggsr (im1, v1, v2, IM_NDIM(im1))
		if (buf1 == EOF)
		    call error (0, "Shiftlines: error reading input image")
		junk = impnlr (im2, buf2, vout)
		if (junk == EOF)
		    call error (0, "Shiftlinesi: error writing out image")
		call amovr (Memr[buf1], Memr[buf2], ncols)
		call sh_loop (v1, v2, IM_LEN(im1,1), IM_NDIM(im1))
	    }

	case TY_DOUBLE:
	    do i = 1, nlines {
	        buf1 = imggsd (im1, v1, v2, IM_NDIM(im1))
		if (buf1 == EOF)
		    call error (0, "Shiftlines: error reading input image")
		junk = impnld (im2, buf2, vout)
		if (junk == EOF)
		    call error (0, "Shiftlinesi: error writing out image")
		call amovd (Memd[buf1], Memd[buf2], ncols)
		call sh_loop (v1, v2, IM_LEN(im1,1), IM_NDIM(im1))
	    }

	case TY_COMPLEX:
	    do i = 1, nlines {
	        buf1 = imggsx (im1, v1, v2, IM_NDIM(im1))
		if (buf1 == EOF)
		    call error (0, "Shiftlines: error reading input image")
		junk = impnlx (im2, buf2, vout)
		if (junk == EOF)
		    call error (0, "Shiftlinesi: error writing out image")
		call amovx (Memx[buf1], Memx[buf2], ncols)
		call sh_loop (v1, v2, IM_LEN(im1,1), IM_NDIM(im1))
	    }

	default:
	    call error (0, "Unknown pixel datatype")
	}
end


# SH_LOOP -- Increment the vector V from VS to VE (nested do loops cannot
# be used because of the variable number of dimensions).  Return LOOP_DONE
# when V exceeds VE.

procedure sh_loop (vs, ve, szdims, ndim)

long	vs[ndim]			# the start vector
long	ve[ndim]			# the end vector
long	szdims[ndim]			# the dimensions vector
int	ndim				# the number of dimensions

int	dim

begin
	for (dim=2;  dim <= ndim;  dim=dim+1) {
	    vs[dim] = vs[dim] + 1
	    ve[dim] = vs[dim]
	    if (vs[dim] - szdims[dim] == 1) {
		if (dim < ndim) {
		    vs[dim] = 1
		    ve[dim] = 1
		} else
		    break
	    } else
		break
	}
end
