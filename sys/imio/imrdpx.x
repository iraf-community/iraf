# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<syserr.h>
include	<plset.h>
include	<imhdr.h>
include	<imio.h>

# IMRDPX -- Read NPIX * STEP pixels, stored contiguously in the pixel storage
# file, starting with the pixel whose coordinates are given by the vector V,
# into the buffer BUF.  If the step size is not unity, accumulate pixels 1,
# 1 + STEP, and so on for a total of NPIX pixels, at the start of the buffer.
# If VINC is negative, flip the array of NPIX pixels end for end.

procedure imrdpx (im, obuf, npix, v, xstep)

pointer	im			# image descriptor
char	obuf[ARB]		# output buffer
int	npix			# number of pixels to extract
long	v[IM_MAXDIM]		# physical coords of first pixel
int	xstep			# step between pixels in X (neg for a flip)

pointer	pl
long	offset
int	sz_pixel, nbytes, fd, op, step, nchars, n

char	zbuf[1024]

int	read()
long	imnote()
errchk	imerr, seek, read, pl_glpi, pl_glri
include	<szpixtype.inc>

begin
	step = abs (xstep)
	if (v[1] < 1 || ((npix-1) * step) + v[1] > IM_SVLEN(im,1))
	    call imerr (IM_NAME(im), SYS_IMREFOOB)

	pl = IM_PL(im)
	fd = IM_PFD(im)
	offset = imnote (im, v)
	sz_pixel = pix_size[IM_PIXTYPE(im)]

	# If the step size is small, read in all the data at once and
	# resample.  Requires a buffer STEP times larger than necessary,
	# but is most efficient for small step sizes.  If the step size
	# is very large, read each pixel with a separate READ call (buffer
	# size no larger than necessary).  Most efficient technique for very
	# large step sizes.

	if (pl != NULL) {
	    # Read from a pixel list.  Range list i/o is permitted at this
	    # level only if no pixel conversions are required, i.e., only if
	    # "fast" i/o is enabled.  Otherwise, we must return pixels here
	    # and then convert back to a range list after the conversions.

	    n = ((npix-1) * step + 1)
	    if (and (IM_PLFLAGS(im), PL_FAST+PL_RLIO) == PL_FAST+PL_RLIO)
		call pl_glri (pl, v, obuf, 0, n, PIX_SRC)
	    else {
		call pl_glpi (pl, v, obuf, 0, n, PIX_SRC)
		if (step > 1)
		    call imsamp (obuf, obuf, npix, sz_pixel, step)
	    }

	} else if (step <= IM_MAXSTEP) {
	    # Seek to the point V in the pixel storage file.  Compute size
	    # of transfer.  Read in the data, resample.

	    call seek (fd, offset)
	    nchars = ((npix-1) * step + 1) * sz_pixel

	    if (read (fd, obuf, nchars) != nchars)
		call imerr (IM_NAME(im), SYS_IMNOPIX)
	    if (step > 1)
		call imsamp (obuf, obuf, npix, sz_pixel, step)

	} else {
	    # Seek and read each pixel directly into the output buffer.
	    nchars = npix * sz_pixel

	    for (op=1;  op <= nchars;  op=op+sz_pixel) {
		call seek (fd, offset)
		if (read (fd, obuf[op], sz_pixel) < sz_pixel)
		    call imerr (IM_NAME(im), SYS_IMNOPIX)
		offset = offset + (sz_pixel * step)
	    }
	}

	# Flip the pixel array end for end.
	if (xstep < 0)
	    call imaflp (obuf, npix, sz_pixel)

	# Byte swap if necessary.
	nbytes = npix * sz_pixel * SZB_CHAR
	if (IM_SWAP(im) == YES) {
	    switch (sz_pixel * SZB_CHAR) {
	    case 2:
		call bswap2 (obuf, 1, obuf, 1, nbytes)
	    case 4:
		call bswap4 (obuf, 1, obuf, 1, nbytes)
	    case 8:
		call bswap8 (obuf, 1, obuf, 1, nbytes)
	    }
	}

	if (pl == NULL) {
	    if ((IM_PIXTYPE(im) == TY_INT || IM_PIXTYPE(im) == TY_LONG) &&
	        SZ_INT != SZ_INT32)
	            call iupk32 (obuf, obuf, npix)
	}
end
