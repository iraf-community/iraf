# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<imhdr.h>
include	"oif.h"

# IMF_INITOFFSETS -- Initialize the physical dimensions of a new image.
# Compute and set the absolute file offsets of the major components of the
# pixel storage file.

procedure imf_initoffsets (im, dev_block_size)

pointer	im
int	dev_block_size
long	offset, temp1, temp2
int	ndim, dim, sz_pixel, lblksize, pblksize, sizeof()

begin
	sz_pixel = sizeof (IM_PIXTYPE(im))
	pblksize = max (dev_block_size, SZ_VMPAGE)
	lblksize = dev_block_size

	# Allow space for the pixhdr pixel storage file header.  Advance
	# "offset" to the next device block boundary.

	offset = LEN_PIXHDR * SZ_MII_INT
	call imf_align (offset, pblksize)

	# Set the offset of the pixel storage area.  Compute the physical
	# dimensions of the axes of the image.  If image compression is
	# selected, the logical and physical lengths of the axes will be
	# the same.  Otherwise, the physical length of each line of the
	# image will be increased to fill an integral number of device blocks.

	IM_PIXOFF(im) = offset
	call amovl (IM_LEN(im,1), IM_PHYSLEN(im,1), IM_MAXDIM)
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

	# Set the unused higher dimensions to 1.  This makes is possible to
	# access the image as if it were higher dimensional, and in a way it
	# truely is.

	do dim = ndim + 1, IM_MAXDIM
	    IM_LEN(im,dim) = 1

	temp1 = offset + IM_LEN(im,1) * sz_pixel
	temp2 = temp1
	call imf_align (temp2, lblksize)

	# Only block lines if the packing density is above a certain threshold.
	if (real(temp1-offset) / real(temp2-offset) >= IM_PACKDENSITY)
	    IM_PHYSLEN(im,1) = (temp2 - offset) / sz_pixel

	# Set the offsets of the histogram pixels and the bad pixel list.
	offset = IM_PHYSLEN(im,1)
	do dim = 2, ndim
	    offset = offset * IM_LEN(im,dim)
	offset = (offset * sz_pixel) + IM_PIXOFF(im)
	call imf_align (offset, lblksize)

	IM_HGMOFF(im) = offset
	IM_BLIST(im)  = offset
end


# IMF_ALIGN -- Advance "offset" to the next block boundary.

procedure imf_align (offset, blksize)

long	offset
int	blksize, diff

begin
	diff = mod (offset-1, max (1, blksize))
	if (diff != 0)
	    offset = offset + (blksize - diff)
end
