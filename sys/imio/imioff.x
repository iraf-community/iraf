# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<config.h>
include	<imhdr.h>
include	<imio.h>
include	<mach.h>

# IMIOFF -- Initialize the physical dimensions of a new image.  Compute and set
# the absolute file offsets of the major components of the pixel storage file.

procedure imioff (im, pixoff, compress, devblksz)

pointer	im			# image descriptor
long	pixoff			# file offset of first pixel
int	compress		# if set, do not align image lines
int	devblksz		# FIO device block size

real	impkden, envgetr()
long	offset, temp1, temp2, imnote()
int	ndim, dim, sz_pixel, lblksize, pblksize
errchk	imerr

include	<szpixtype.inc>

begin
	sz_pixel = pix_size[IM_PIXTYPE(im)]
	pblksize = max (devblksz, SZ_VMPAGE)

	if (compress == YES)
	    lblksize = 1
	else
	    lblksize = devblksz

	# Set the offset of the pixel storage area.  Compute the physical
	# dimensions of the axes of the image.  If image compression is
	# selected, the logical and physical lengths of the axes will be
	# the same.  Otherwise, the physical length of each line of the
	# image will be increased to fill an integral number of device blocks.

	IM_PIXOFF(im) = pixoff
	call amovl (IM_LEN(im,1), IM_PHYSLEN(im,1), IM_MAXDIM)
	call amovl (IM_LEN(im,1), IM_SVLEN(im,1), IM_MAXDIM)

	ndim = IM_NDIM(im)

	# If ndim was not explicitly set, compute it by counting the number
	# of nonzero dimensions.

	if (ndim == 0) {
	    for (ndim=1;  IM_LEN(im,ndim) > 0 && ndim <= IM_MAXDIM;
	    ndim=ndim+1)
		;
	    ndim = ndim - 1
	    IM_NDIM(im) = ndim
	}
	IM_NPHYSDIM(im) = ndim

	# Make sure dimension stuff makes sense.
	if (ndim < 0 || ndim > IM_MAXDIM)
	    call imerr (IM_NAME(im), SYS_IMNDIM)

	do dim = 1, ndim
	    if (IM_LEN(im,dim) <= 0)
		call imerr (IM_NAME(im), SYS_IMDIMLEN)

	# Set the unused higher dimensions to 1.  This makes is possible to
	# access the image as if it were higher dimensional, and in a way it
	# truely is.

	do dim = ndim + 1, IM_MAXDIM
	    IM_LEN(im,dim) = 1

	if (lblksize > 1) {
	    temp1 = pixoff + IM_LEN(im,1) * sz_pixel
	    temp2 = temp1
	    call imalign (temp2, lblksize)

	    # Only block lines if the packing density is above a certain
	    # threshold.  Alignment is disabled if compress=YES since lblksize
	    # will have been set to 1.

	    iferr (impkden = envgetr ("impkden"))
		impkden = IM_PACKDENSITY

	    if (real(temp1-pixoff) / real(temp2-pixoff) >= impkden)
		IM_PHYSLEN(im,1) = (temp2 - pixoff) / sz_pixel
	}

	# Set the offsets of the histogram pixels and the bad pixel list.
	# The HGMOFF offset marks the end of the pixel segment.

	offset = imnote (im, IM_LEN(im,1))
	call imalign (offset, pblksize)
	IM_HGMOFF(im) = offset

	offset = offset + (MAX_HGMLEN * SZ_REAL)
	call imalign (offset, lblksize)
	IM_BLIST(im) = offset
end


# IMALIGN -- Advance "offset" to the next block boundary.

procedure imalign (offset, blksize)

long	offset
int	blksize, diff

begin
	diff = mod (offset-1, max (1, blksize))
	if (diff != 0)
	    offset = offset + (blksize - diff)
end
