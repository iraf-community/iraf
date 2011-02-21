# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IMGIBF -- Get an input buffer.

pointer procedure imgibf (im, vs, ve, ndim, dtype)

pointer	im
long	vs[ARB], ve[ARB]
int	dtype, ndim

pointer	bdes
int	i
long	nget, nchars, totpix

long	imcssz()
errchk	imopsf, calloc, realloc, mfree, malloc

begin
	# If first input transfer, allocate and initialize array of
	# input buffer descriptors.

	if (IM_IBDES(im) == NULL) {
	    call imopsf (im)
	    call calloc (IM_IBDES(im), LEN_BDES * IM_VNBUFS(im), TY_STRUCT)
	}

	# Compute pointer to the next input buffer descriptor.
	# Increment NGET, the count of the number of GETPIX calls.

	nget = IM_NGET(im)
	bdes = IM_IBDES(im) + mod (nget, IM_VNBUFS(im)) * LEN_BDES
	IM_NGET(im) = nget + 1

	# Compute the size of the buffer needed.  Check buffer
	# descriptor to see if the old buffer is the right size.
	# If so, use it, otherwise make a new one.

	nchars = imcssz (im, vs, ve, ndim, dtype, totpix, IM_READ)

	if (nchars < BD_BUFSIZE(bdes))
	    call realloc (BD_BUFPTR(bdes), nchars, TY_CHAR)
	else if (nchars > BD_BUFSIZE(bdes)) {
	    call mfree (BD_BUFPTR(bdes), TY_CHAR)
	    call malloc (BD_BUFPTR(bdes), nchars, TY_CHAR)
	}

	# Save section coordinates, datatype in buffer descriptor, and
	# return buffer pointer to calling program.

	IM_LASTBDES(im) = bdes
	BD_BUFSIZE(bdes) = nchars
	BD_DTYPE(bdes) = dtype
	BD_NPIX(bdes) = totpix
	BD_NDIM(bdes) = ndim

	do i = 1, ndim {
	    BD_VS(bdes,i) = vs[i]
	    BD_VE(bdes,i) = ve[i]
	}

	return (BD_BUFPTR(bdes))		# return ptr to CHAR
end
