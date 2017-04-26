# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<pmset.h>
include	<imhdr.h>
include	<imio.h>
include	<plio.h>

# IM_PMMAPO -- Map an open pixel list onto an image descriptor, so that the
# pixel list may be accessed as a virtual "mask image".  If a reference image
# is specified the mask image inherits any image section etc., defined for
# the reference image.

pointer procedure im_pmmapo (pl, ref_im)

pointer	pl			#I mask descriptor
pointer	ref_im			#I reference image or NULL

pointer	im
long	axlen[IM_MAXDIM]
int	naxes, depth, i
errchk	syserr, immapz, pl_gsize
pointer	immapz()

begin
	# Get the mask size.
	call pl_gsize (pl, naxes, axlen, depth)

	# Verify the size if there is a reference image.
	if (ref_im != NULL)
	    do i = 1, max (naxes, IM_NPHYSDIM(ref_im))
		if (IM_SVLEN(ref_im,i) != axlen[i])
		    call syserr (SYS_IMPLSIZE)

	# Open an image header for the mask.
	call iki_init()
	im = immapz ("dev$null", NEW_IMAGE, 0)

	# Set up the image descriptor.
	IM_NDIM(im) = naxes
	IM_NPHYSDIM(im) = IM_NPHYSDIM(ref_im)
	IM_PIXTYPE(im) = TY_INT
	call amovl (axlen, IM_LEN(im,1), IM_MAXDIM)

	IM_PL(im)	= pl
	IM_PLREFIM(im)	= ref_im
	IM_PLFLAGS(im)	= 0
	IM_MIN(im)      = 0
	IM_MAX(im)      = 2 ** depth - 1
	IM_LIMTIME(im)  = IM_MTIME(im) + 1
	IM_UPDATE(im)	= NO

	PM_REFIM(pl) = im
	if (ref_im != NULL)
	    PM_MAPXY(pl) = IM_SECTUSED(ref_im)
	else
	    PM_MAPXY(pl) = NO

	# Further setup of the image descriptor is carried out by IMOPSF
	# when the first i/o access occurs, as for a regular image.

	return (im)
end
