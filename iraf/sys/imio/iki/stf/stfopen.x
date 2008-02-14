# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	"stf.h"

# STF_OPEN -- Open/create an STF group format image.

procedure stf_open (kernel, im, o_im,
	root, extn, ksection, gr_arg, gc_arg, acmode, status)

int	kernel			#I IKI kernel
pointer	im			#I image descriptor
pointer	o_im			#I other descriptor for NEW_COPY image
char	root[ARB]		#I root image name
char	extn[ARB]		#I extension, if any
char	ksection[ARB]		#I NOT USED
int	gr_arg			#I index of group to be accessed
int	gc_arg			#I number of groups in STF image
int	acmode			#I access mode
int	status			#O return value

bool	subimage
pointer	sp, fname, stf, stf_extn, ua, o_stf
int	group, gcount, newimage, gpb, hdr, o_stflen

bool	fnullfile(), envgetb()
int	open(), stropen(), access()
errchk	stf_initwcs, fmkcopy, calloc, realloc, syserrs
define	err_ 91

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (stf_extn, MAX_LENEXTN, TY_CHAR)

	ua = IM_USERAREA(im)

	# Allocate internal STF image descriptor.
	call calloc (stf, LEN_STFDES, TY_STRUCT)
	IM_KDES(im) = stf

	group  = max (1, gr_arg)
	gcount = max (group, gc_arg)

	STF_GRARG(stf)	= max (0, gr_arg)
	STF_GROUP(stf)  = group
	STF_GCOUNT(stf) = gcount
	STF_ACMODE(stf) = acmode
	STF_PFD(stf)    = NULL

	# If a nonzero gcount is specified when a new-image or new-copy image
	# is opened (e.g., [1/10] we assume that an entire new group format
	# image is to be created with the given group count.  If neither the
	# group or gcount values are specified we assume that a new image is
	# to be created.  If the gcount field is zero (e.g., [1/0] or just [1])
	# then we assume that the image already exists and that we are being
	# asked to rewrite the indexed image.

	newimage = NO
	if (acmode == NEW_IMAGE || acmode == NEW_COPY)
	    if (gc_arg > 0 || (gr_arg <= 0 && gc_arg <= 0))
		newimage = YES
	STF_NEWIMAGE(stf) = newimage

	# Generate full header file name.
	if (extn[1] == EOS) {
	    call stf_gethdrextn (im, o_im, acmode, Memc[stf_extn], MAX_LENEXTN)
	    call iki_mkfname (root, Memc[stf_extn], Memc[fname], SZ_PATHNAME)
	    call strcpy (Memc[stf_extn], extn, MAX_LENEXTN)
	} else
	    call iki_mkfname (root, extn, Memc[fname], SZ_PATHNAME)

	call strcpy (Memc[fname], IM_HDRFILE(im), SZ_IMHDRFILE)

	# Generate full pixel file name.
	call stf_mkpixfname (root, extn, Memc[fname], SZ_PATHNAME)
	call strcpy (Memc[fname], IM_PIXFILE(im), SZ_IMPIXFILE)

	# Create and open the image header file if create a new physical
	# image.  If opening an existing image we do not open the header file
	# here since the header may already be in the STF header cache.
	# Since STF header files have a weird file type on some systems (VMS)
	# we must create a new header file with FMKCOPY rather than OPEN.

	if (STF_NEWIMAGE(stf) == YES && !fnullfile (IM_HDRFILE(im))) {
	    if (access (IM_HDRFILE(im), 0,0) == YES) {
		subimage = (gr_arg > 0 && gr_arg <= gc_arg)
		if (subimage || envgetb ("imclobber")) {
		    iferr (call delete (IM_PIXFILE(im)))
			goto err_
		    iferr (call delete (IM_HDRFILE(im)))
			goto err_
		} else {
		    call mfree (stf, TY_STRUCT)
		    call syserrs (SYS_IKICLOB, IM_HDRFILE(im))
		}
	    }
	    iferr (call fmkcopy (HDR_TEMPLATE, IM_HDRFILE(im)))
		goto err_
	    iferr (IM_HFD(im) = open (IM_HDRFILE(im), READ_WRITE, TEXT_FILE))
		goto err_
	}

	# If opening an existing image, read the image header into the STF
	# image descriptor.

	switch (acmode) {
	case NEW_IMAGE:
	    # For group formatted images, open NEW_IMAGE can mean either
	    # creating a new group format image, or opening a new group
	    # within an existing group format image.  The latter case is
	    # indicated by a group index greater than 1.  If we are creating
	    # a new group format image, wait until the user has set up the
	    # dimension parameters before doing anything further (in stfopix).

	    if (STF_NEWIMAGE(stf) == NO)
		iferr (call stf_rdheader (im, group, acmode))
		    goto err_

	case NEW_COPY:
	    # Make sure the FITS encoded user area we inherited is blocked.

	    ### For now, always reblock the old header as the blocked flag
	    ### does not seem to be reliable and a header with variable length
	    ### lines can cause the header update to fail.  This should be
	    ### fixed as a reblock of the full header is expensive.

	    ### if (IM_UABLOCKED(o_im) != YES)
		call stf_reblock (im)

	    if (STF_NEWIMAGE(stf) == NO) {
		# Open new group within existing GF image.  The FITS header and
		# GPB structure of the image being opened must be used, but the
		# default data values for the GPB parameters are inherited from
		# the image being copied.

		# Filter the copied user area to retain only the GPB cards.
		# Opening the user area on two string file descriptors is a
		# bit tricky, but will work since fixed size cards are copied,
		# and the EOS isn't written until close time.

		if (IM_KDES(o_im) != NULL && IM_KERNEL(o_im) == IM_KERNEL(im)) {
		    hdr = stropen (Memc[ua], ARB, READ_ONLY)
		    gpb = stropen (Memc[ua], ARB, NEW_FILE)
		    call stf_copyfits (IM_KDES(o_im), hdr, gpb, NULL)
		    call close (gpb)
		    call close (hdr)
		}

		# Read in the FITS header of the new image after the inherited
		# GPB data cards, and set up the STF descriptor for the new GPB
		# as defined in the new FITS header.

		iferr (call stf_rdheader (im, group, acmode))
		    goto err_

		# Initialize the WCS description if this is not done by the
		# inherited user header.

		call stf_initwcs (im)

	    } else {
		# Completely new copy of an existing image, which may or may
		# not be an STF format image.  IMIO has already copied the
		# size parameters of the old image as well as the cards in the
		# user area of the old image (but without leaving space for
		# the GPB cards if not an STF image).  Copy old STF descriptor
		# if the old image is also an STF format image, to inherit
		# GPB structure.  Wait until opix time to init the rest of the
		# descriptor.  

		if (IM_KDES(o_im) != NULL && IM_KERNEL(o_im) == IM_KERNEL(im)) {
		    o_stf = IM_KDES(o_im)
		    o_stflen = LEN_STFBASE + STF_PCOUNT(o_stf) * LEN_PDES
		    call amovi (Memi[o_stf], Memi[stf], o_stflen)
		    STF_ACMODE(stf)   = acmode
		    STF_GROUP(stf)    = group
		    STF_GCOUNT(stf)   = gcount
		    STF_NEWIMAGE(stf) = newimage
		    STF_PFD(stf)      = NULL
		    if (gcount > 1)
			STF_GROUPS(stf) = YES
		} else
		    STF_GROUPS(stf) = YES

		# Inherit datatype of input template image if specified,
		# otherwise default datatype to real.

		if (IM_PIXTYPE(o_im) != NULL)
		    IM_PIXTYPE(im) = IM_PIXTYPE(o_im)
		else
		    IM_PIXTYPE(im) = TY_REAL
	    }

	default:
	    # Open an existing group within an existing image.
	    iferr (call stf_rdheader (im, group, acmode))
		goto err_
	}

	# Set group number and count for the external world if this is a group
	# format image.

	if (STF_GROUPS(stf) == YES) {
	    IM_CLINDEX(im) = STF_GROUP(stf)
	    IM_CLSIZE(im)  = STF_GCOUNT(stf)
	}

	# Free any unneeded space in the STF descriptor.
	if (STF_PCOUNT(stf) > 0)
	    call realloc (stf,
		LEN_STFBASE + STF_PCOUNT(stf)*LEN_PDES, TY_STRUCT)
	IM_KDES(im) = stf
	status = OK

	call sfree (sp)
	return
err_
	status = ERR
	call mfree (stf, TY_STRUCT)
	call sfree (sp)
end
