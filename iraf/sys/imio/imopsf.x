# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<pmset.h>
include	<plset.h>
include	<imhdr.h>
include	<imio.h>
include	<fset.h>

# IMOPSF -- Open (or create) the pixel storage file.  If the file has already
# been opened do nothing but set the buffer size.  Until the pixel storage
# file has been opened we do not know the device block size, image line length,
# or whether IM_FAST type i/o is possible.

procedure imopsf (im)

pointer	im

pointer	sp, imname, ref_im, pfd
int	sv_acmode, sv_update, ndim, depth, i
errchk	iki_opix, open
int	open()

begin
	call smark (sp)
	call salloc (imname, SZ_IMNAME, TY_CHAR)

	if (IM_PL(im) != NULL) {
	    if (IM_PFD(im) == NULL) {
		# Complete the initialization of a mask image.
		ref_im = IM_PLREFIM(im)

		sv_acmode = IM_ACMODE(im)
		sv_update = IM_UPDATE(im)
		call strcpy (IM_NAME(im), Memc[imname], SZ_IMNAME)

		if (ref_im != NULL) {
		    # Create a mask the same size as the physical size of the
		    # reference image.  Inherit any image section from the
		    # reference image.

		    IM_NDIM(im) = IM_NDIM(ref_im)
		    IM_NPHYSDIM(im) = IM_NPHYSDIM(ref_im)
		    IM_SECTUSED(im) = IM_SECTUSED(ref_im)
		    call amovl (IM_LEN(ref_im,1), IM_LEN(im,1), IM_MAXDIM)
		    call amovl (IM_PHYSLEN(ref_im,1),IM_PHYSLEN(im,1),IM_MAXDIM)
		    call amovl (IM_SVLEN(ref_im,1), IM_SVLEN(im,1), IM_MAXDIM)
		    call amovl (IM_VMAP(ref_im,1), IM_VMAP(im,1), IM_MAXDIM)
		    call amovl (IM_VOFF(ref_im,1), IM_VOFF(im,1), IM_MAXDIM)
		    call amovl (IM_VSTEP(ref_im,1), IM_VSTEP(im,1), IM_MAXDIM)

		    # Tell PMIO to use this image as the reference image.
		    call pm_seti (IM_PL(im), P_REFIM, im)

		} else if (sv_acmode == NEW_IMAGE || sv_acmode == NEW_COPY) {
		    # If ndim was not explicitly set, compute it by counting
		    # the number of nonzero dimensions.

		    ndim = IM_NDIM(im)
		    if (ndim == 0) {
			ndim = 1
			while (IM_LEN(im,ndim) > 0 && ndim <= IM_MAXDIM)
			    ndim = ndim + 1
			ndim = ndim - 1
			IM_NDIM(im) = ndim
		    }

		    # Make sure dimension stuff makes sense.
		    if (ndim < 0 || ndim > IM_MAXDIM)
			call imerr (IM_NAME(im), SYS_IMNDIM)

		    do i = 1, ndim
			if (IM_LEN(im,i) <= 0)
			    call imerr (IM_NAME(im), SYS_IMDIMLEN)

		    # Set the unused higher dimensions to 1.  This makes it
		    # possible to access the image as if it were higher
		    # dimensional, and in a way it truely is.

		    do i = ndim + 1, IM_MAXDIM
			IM_LEN(im,i) = 1

		    IM_NPHYSDIM(im) = ndim
		    call amovl (IM_LEN(im,1), IM_PHYSLEN(im,1), IM_MAXDIM)
		    call amovl (IM_LEN(im,1), IM_SVLEN(im,1), IM_MAXDIM)

		    # Initialize the empty mask to the newly determined size.
		    depth = PL_MAXDEPTH
		    if (and (IM_PLFLAGS(im), PL_BOOL) != 0)
			depth = 1
		    call pl_ssize (IM_PL(im), IM_NDIM(im), IM_LEN(im,1), depth)
		}

		call strcpy (Memc[imname], IM_NAME(im), SZ_IMNAME)
		IM_ACMODE(im) = sv_acmode
		IM_UPDATE(im) = sv_update
		IM_PIXOFF(im) = 1	
		IM_HGMOFF(im) = NULL
		IM_BLIST(im) = NULL
		IM_HFD(im) = NULL

		# Do the following in two statements so that IM_PFD does
		# not get set if the OPEN fails and does an error exit.

		pfd = open ("dev$null", READ_WRITE, BINARY_FILE)
		IM_PFD(im) = pfd
	    }

	    # Execute this even if pixel file has already been opened.
	    call imsetbuf (IM_PFD(im), im)

	    # "Fast i/o" in the conventional sense no IMIO buffering)
	    # is not permitted for mask images, since IMIO must buffer
	    # the pixels, which are generated at run time.

	    if (IM_FAST(im) == YES) {
		IM_PLFLAGS(im) = or (IM_PLFLAGS(im), PL_FAST)
		IM_FAST(im) = NO
	    }

	} else {
	    # Open the pixel file for a regular image.
	    if (IM_PFD(im) == NULL)
		call iki_opix (im)

	    # Execute this even if pixel file has already been opened.
	    call imsetbuf (IM_PFD(im), im)

	    # If F_CLOSEFD is set on the pixel file, the host channel to the
	    # file will be physically closed off except when an i/o operation
	    # is in progress (used to conserve host file descriptors) in
	    # applications which must open a large number of images all at
	    # once).

	    if (IM_VCLOSEFD(im) == YES)
		call fseti (IM_PFD(im), F_CLOSEFD, YES)
	}

	call sfree (sp)
end
