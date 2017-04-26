# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IMGOBF -- Get output buffer.

pointer procedure imgobf (im, vs, ve, ndim, dtype)

pointer	im, bdes
int	ndim, dtype, i
long	vs[ndim], ve[ndim]
long	nchars, totpix, imcssz(), clktime()
int	sizeof()

errchk	imopsf, malloc, realloc, calloc

include	<szpixtype.inc>

begin
	# If first write, and if new image, create pixel storage file,
	# otherwise open pixel storage file.  Allocate and initialize
	# output buffer descriptor.

	if (IM_OBDES(im) == NULL) {
	    call imopsf (im)
	    call calloc (IM_OBDES(im), LEN_BDES, TY_STRUCT)
	    IM_MTIME(im) = clktime (long(0))
	    IM_SVMTIME(im) = IM_MTIME(im)
	}

	bdes = IM_OBDES(im)

	# Compute the size of buffer needed.  A few extra chars are added
	# to guarantee that there won't be a memory violation when
	# writing a full physical length line.

	nchars = imcssz (im, vs, ve, ndim, dtype, totpix, IM_WRITE)

	if (nchars < BD_BUFSIZE(bdes))
	    call realloc (BD_BUFPTR(bdes), nchars, TY_CHAR)
	else if (nchars > BD_BUFSIZE(bdes)) {
	    call mfree (BD_BUFPTR(bdes), TY_CHAR)
	    call malloc (BD_BUFPTR(bdes), nchars, TY_CHAR)
	}

	# Save section coordinates, datatype of pixels in buffer
	# descriptor, and return buffer pointer to calling program.

	IM_LASTBDES(im) = bdes
	BD_BUFSIZE(bdes) = nchars
	BD_DTYPE(bdes) = dtype
	BD_NPIX(bdes) = totpix
	BD_NDIM(bdes) = ndim

	do i = 1, ndim {
	    BD_VS(bdes,i) = vs[i]
	    BD_VE(bdes,i) = ve[i]
	}

	return ((BD_BUFPTR(bdes) - 1) / sizeof(dtype) + 1)
end
