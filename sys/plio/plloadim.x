# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<plset.h>
include	<plio.h>

# PL_LOADIM -- Load a mask stored as a conventional image, i.e., convert an
# image to a mask.

procedure pl_loadim (pl, imname)

pointer	pl			#I mask descriptor
char	imname[ARB]		#I image name or section

pointer	im, px
int	npix, naxes, maxdim, maxval, depth, i
long	v_in[PL_MAXDIM], v_out[PL_MAXDIM], axlen[PL_MAXDIM]
int	imgnli()
pointer	immap()
errchk	immap

begin
	# Open the input image.
	im = immap (imname, READ_ONLY, 0)

	# Determine the mask depth in bits.
	maxval = IM_MAX(im)
	if (maxval <= 0)
	    maxval = I_PVMAX
	do i = 1, ARB
	    if (2**i > min (I_PVMAX, maxval)) {
		depth = i
		break
	    }

	# Initialize the mask to the size of the image section.
	npix = IM_LEN(im,1)
	naxes = IM_NDIM(im)
	maxdim = min (IM_MAXDIM, PL_MAXDIM)
	call amovl (IM_LEN(im,1), axlen, maxdim)
	call pl_ssize (pl, naxes, axlen, depth)

	# Initialize the vector loop indices.
	call amovkl (long(1), v_in, maxdim)
	call amovkl (long(1), v_out, maxdim)

	# Copy the image.
	while (imgnli (im, px, v_in) != EOF) {
	    call pl_plpi (pl, v_out, Memi[px], 0, npix, PIX_SRC)
	    call amovl (v_in, v_out, maxdim)
	}

	call pl_compress (pl)
	call imunmap (im)
end
