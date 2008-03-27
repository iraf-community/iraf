# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<plset.h>
include	<plio.h>

# PL_LOADIM -- Load a mask stored as a conventional image, i.e., convert an
# image to a mask.

procedure pl_loadim (pl, imname, title, maxch)

pointer	pl			#I mask descriptor
char	imname[ARB]		#I image name or section
char	title[ARB]		#O mask title
int	maxch			#I max chars out

size_t	sz_val
long	lval
bool	sampling
pointer	im, px, im_pl, bp, p_0
long	vs_l[PL_MAXDIM], vs_p[PL_MAXDIM]
long	ve_l[PL_MAXDIM], ve_p[PL_MAXDIM]
int	naxes, maxdim, maxval, depth, sz_buf, i
size_t	npix
long	v_in[PL_MAXDIM], v_out[PL_MAXDIM], vn[PL_MAXDIM]

pointer	immap(), imstatp()
long	imgnli()
int	im_pmsvhdr()
errchk	immap, imgnli

begin
	p_0 = 0

	# Open the input image.
	im = immap (imname, READ_ONLY, p_0)

	# Encode and output the image header.
	bp = NULL;  i = im_pmsvhdr (im, bp, sz_buf)
	call strcpy (Memc[bp], title, maxch)
	call mfree (bp, TY_CHAR)

	# Determine the mask depth in bits.
	maxval = IM_MAX(im)
	if (maxval <= 0)
	    maxval = I_PVMAX
	do i = 0, ARB
	    if (2**i > min (I_PVMAX, maxval)) {
		depth = i
		break
	    }

	# Initialize the mask to the size of the image section.
	npix = IM_LEN(im,1)
	naxes = IM_NDIM(im)
	maxdim = min (IM_MAXDIM, PL_MAXDIM)
	sz_val = maxdim
	call amovl (IM_LEN(im,1), vn, sz_val)
	call pl_ssize (pl, naxes, vn, depth)

	# If the image is already a mask internally, check whether any
	# subsampling, axis flipping, or axis mapping is in effect.
	# If so we can't use PLIO to copy the mask section.

	im_pl = imstatp (im, IM_PLDES)
	sampling = false

	if (im_pl != NULL) {
	    sz_val = maxdim
	    lval = 1
	    call amovkl (lval, vs_l, sz_val)
	    call amovl (IM_LEN(im,1), ve_l, sz_val)
	    call imaplv (im, vs_l, vs_p, maxdim)
	    call imaplv (im, ve_l, ve_p, maxdim)

	    do i = 1, maxdim {
		vn[i] = ve_l[i] - vs_l[i] + 1
		if (vn[i] != ve_p[i] - vs_p[i] + 1) {
		    sampling = true
		    break
		}
	    }
	}

	# If the source image is already a mask internally and no axis
	# geometry is in effect in the image section (if any), then we can
	# use a PLIO rasterop to efficiently copy the mask subsection.

	if (im_pl != NULL && !sampling) {
	    # Copy a mask subsection (or entire image if no section).
	    call pl_rop (im_pl, vs_p, pl, vs_l, vn, PIX_SRC)

	} else {
	    # Copy image pixels.  Initialize the vector loop indices.
	    lval = 1
	    sz_val = maxdim
	    call amovkl (lval, v_in, sz_val)
	    call amovkl (lval, v_out, sz_val)

	    # Copy the image.
	    while (imgnli (im, px, v_in) != EOF) {
		call pl_plpi (pl, v_out, Memi[px], 0, npix, PIX_SRC)
		sz_val = maxdim
		call amovl (v_in, v_out, sz_val)
	    }
	}

	call pl_compress (pl)
	call imunmap (im)
end
