# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>
include	<plset.h>
include	<plio.h>

# PL_SAVEIM -- Save a mask in a conventional image, i.e., convert a mask to
# an image.

procedure pl_saveim (pl, imname, flags)

pointer	pl			#I mask descriptor
char	imname[ARB]		#I image name or section
int	flags			#I bitflags

pointer	im, px
int	npix, naxes, depth, maxdim, mode
long	v_in[PL_MAXDIM], v_out[PL_MAXDIM], axlen[PL_MAXDIM]
int	impnli(), imaccess()
long	clktime()
pointer	immap()
errchk	immap

begin
	# Open the new output image.
	mode = NEW_IMAGE
	if (and (flags, PL_UPDATE) != 0)
	    if (imaccess (imname, 0) == YES)
		mode = READ_WRITE
	im = immap (imname, mode, 0)

	# Initialize the new image to the size of the mask.
	call pl_gsize (pl, naxes, axlen, depth)

	IM_NDIM(im) = naxes
	IM_PIXTYPE(im) = TY_SHORT
	if (PL_MAXVAL(pl) > MAX_SHORT)
	    IM_PIXTYPE(im) = TY_INT

	maxdim = min (IM_MAXDIM, PL_MAXDIM)
	call amovl (axlen, IM_LEN(im,1), maxdim)
	npix = axlen[1]

	# Initialize the vector loop indices.
	call amovkl (long(1), v_in, maxdim)
	call amovkl (long(1), v_out, maxdim)

	# Copy the image.
	while (impnli (im, px, v_out) != EOF) {
	    call pl_glpi (pl, v_in, Memi[px], 0, npix, PIX_SRC)
	    call amovl (v_out, v_in, maxdim)
	}

	IM_MIN(im) = 0
	IM_MAX(im) = PL_MAXVAL(pl)
	IM_LIMTIME(im) = clktime(0)

	call imunmap (im)
end
