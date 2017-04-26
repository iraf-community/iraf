# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<protect.h>
include	<imhdr.h>
include	"imfort.h"

# IMDELX -- Delete an image (both the header file and the pixel file).
# It is not an error if there is no pixel file.

procedure imdelx (image, ier)

char	image[ARB]		#I image to be deleted
int	ier			#O receives error status

int	status
pointer	im, sp, hdrfile, pixfile, ip
int	stridxs()
define	quit_ 91

begin
	call smark (sp)
	call salloc (hdrfile, SZ_PATHNAME, TY_CHAR)
	call salloc (pixfile, SZ_PATHNAME, TY_CHAR)

	# Get the OS pathnames of the header and pixel files.

	call imopnx (image, RO, im, ier)
	if (ier != OK) {
	    ier = IE_IMDELNEXIM
	    goto quit_
	} else {
	    call strcpy (IM_HDRFILE(im), Memc[hdrfile], SZ_PATHNAME)
	    call imf_gpixfname (IM_PIXFILE(im), IM_HDRFILE(im), Memc[pixfile],
		SZ_PATHNAME)
	    ip = pixfile + stridxs ("!", Memc[pixfile])
	    call strcpy (Memc[ip], Memc[pixfile], SZ_PATHNAME)
	    call imclos (im, ier)
	    if (ier != OK)
		goto quit_
	}

	call strpak (Memc[hdrfile], Memc[hdrfile], SZ_FNAME)
	call strpak (Memc[pixfile], Memc[pixfile], SZ_PATHNAME)

	# Verify that the header file exists.
	call zfacss (Memc[hdrfile], 0, 0, status)
	if (status == NO) {
	    ier = IE_IMDELNEXIM
	    goto quit_
	}

	# Remove any file delete protection from the image header file.
	# Do not complain if the header is not protected, or if there is
	# no pixel file to be deleted.

	call zfprot (Memc[hdrfile], REMOVE_PROTECTION, status)
	call zfdele (Memc[hdrfile], status)

	if (status == ERR)
	    ier = IE_IMDELETE
	else {
	    call zfacss (Memc[pixfile], 0, 0, status)
	    if (status == NO)
		ier = OK
	    else {
		call zfdele (Memc[pixfile], status)
		if (status == ERR)
		    ier = IE_IMDELETE
	    }
	}

quit_
	if (ier != OK)
	    call im_seterrop (ier, image)
	call sfree (sp)
end
