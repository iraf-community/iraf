# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IMNOTE -- Given the coordinates of a pixel, return the character offset
# of that pixel in the pixel storage file.

long procedure imnote (im, v)

pointer	im			# image descriptor
long	v[IM_MAXDIM]		# physical coords of pixel

int	sz_pixel, i
long	pixel_index, dim_offset, char_offset0
include	<szpixtype.inc>

begin
	sz_pixel = pix_size[IM_PIXTYPE(im)]
	pixel_index = v[1]
	dim_offset = 1

	do i = 2, IM_NPHYSDIM(im) {
	    dim_offset = dim_offset * IM_PHYSLEN(im,i-1)
	    pixel_index = pixel_index + dim_offset * (v[i] - 1)
	}

	char_offset0 = (pixel_index-1) * sz_pixel
	return (IM_PIXOFF(im) + char_offset0)
end
