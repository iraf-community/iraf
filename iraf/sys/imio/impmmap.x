# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<pmset.h>
include	<imhdr.h>
include	<imio.h>

# IM_PMMAP -- Map a pixel list as a virtual mask image.  If the mask name
# given is "BPM" (upper case) the bad pixel list for the reference image is
# opened, if the mask name is "EMPTY" an empty mask is opened, otherwise the
# mask name is taken to be the name of the file in which the mask is stored.
# If there is no bad pixel list for the image an empty mask is opened.
# If a more specialized mask is needed it should be opened or generated via
# explicit calls to the PMIO package, and then mapped onto an image descriptor
# with IM_PMMAPO.

pointer procedure im_pmmap (mask, mode, ref_im)

char	mask[ARB]		#I mask file name or "BPM"
int	mode			#I mode and flag bits
pointer	ref_im			#I reference image

pointer	sp, cluster, section, pl, im, hp
int	acmode, flags, sz_svhdr, ip
pointer	im_pmmapo(), im_pmopen()
int	btoi(), ctoi(), envfind()
errchk	im_pmopen, im_pmopen

begin
	call smark (sp)
	call salloc (cluster, SZ_PATHNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)

	acmode = PL_ACMODE(mode)
	flags  = PL_FLAGS(mode)

	# If opening an existing mask, get a buffer for the saved mask image
	# header.

	if (acmode != NEW_IMAGE && acmode != NEW_COPY) {
	    ip = 1
	    if (envfind ("min_lenuserarea", Memc[section], SZ_FNAME) > 0) {
		if (ctoi (Memc[section], ip, sz_svhdr) <= 0)
		    sz_svhdr = MIN_LENUSERAREA
	    } else
		sz_svhdr = MIN_LENUSERAREA
	    call salloc (hp, sz_svhdr, TY_CHAR)
	}

	# Parse the full image specification into a root name and an image
	# section.
	call imgimage (mask, Memc[cluster], SZ_PATHNAME)
	call imgsection (mask, Memc[section], SZ_FNAME)

	# Open the mask.
	pl = im_pmopen (Memc[cluster], mode, Memc[hp], sz_svhdr, ref_im)

	# Map the mask onto an image descriptor.
	iferr (im = im_pmmapo (pl, ref_im)) {
	    call pl_close (pl)
	    call erract (EA_ERROR)
	} else {
	    call strcpy (mask, IM_NAME(im), SZ_IMNAME)
	    if (acmode != NEW_IMAGE && acmode != NEW_COPY)
		call im_pmldhdr (im, hp)
	}

	# Set flag to close PL descriptor at IMUNMAP time.
	IM_PLFLAGS(im) = or (IM_PLFLAGS(im), PL_CLOSEPL)

	# If we are creating a new mask of type boolean, set bool flag so
	# that imopsf will make a boolean mask.

	if (acmode == NEW_IMAGE || acmode == NEW_COPY)
	    if (and (flags, BOOLEAN_MASK) != 0)
		IM_PLFLAGS(im) = or (IM_PLFLAGS(im), PL_BOOL)

	# Set access mode for mask, and mask update at unmap flag.
	IM_ACMODE(im) = acmode
	IM_UPDATE(im) = btoi (acmode != READ_ONLY)

	IM_NPHYSDIM(im) = IM_NDIM(im)
	call amovl (IM_LEN(im,1), IM_PHYSLEN(im,1), IM_MAXDIM)
	call amovl (IM_LEN(im,1), IM_SVLEN(im,1),   IM_MAXDIM)

	# Set up section transformation.
	if (ref_im == NULL && Memc[section] != EOS)
	    call imisec (im, Memc[section])

	call sfree (sp)
	return (im)
end
