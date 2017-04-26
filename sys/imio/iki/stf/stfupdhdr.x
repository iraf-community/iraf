# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	<imio.h>
include	"stf.h"

# STF_UPDHDR -- Update the STF/GEIS format image header.

procedure stf_updhdr (im, status)

pointer	im			# image descriptor
int	status			# return status

pointer	stf
int	acmode
real	datamin, datamax
errchk	imerr, imputr, stf_wgpb

begin
	acmode = IM_ACMODE(im)
	status = OK
	stf    = IM_KDES(im)

	if (acmode == READ_ONLY)
	    call imerr (IM_NAME(im), SYS_IMUPIMHDR)

	# Compute the values of DATAMIN and DATAMAX.
	if (IM_LIMTIME(im) == 0 || IM_LIMTIME(im) < IM_MTIME(im)) {
	    datamin = 0.
	    datamax = 0.
	} else {
	    datamin = IM_MIN(im)
	    datamax = IM_MAX(im)
	}

	# Update the group parameter block.
	call stf_wgpb (im, STF_GROUP(stf), datamin, datamax)

#	# Update the FITS header file, unless we are writing to a new group
#	# in an existing group format image, in which case only the GPB is
#	# updated.
#
#	if (acmode != NEW_IMAGE && acmode != NEW_COPY)
#	    call stf_wfitshdr (im)
#	else if (STF_NEWIMAGE(stf) == YES)
#	    call stf_wfitshdr (im)

	# The new strategy for FITS header updates is to always update, unless
	# we are explicitly updating an existing group of a multigroup image.
	# Hence, the FITS header is always updated for an STF image with only
	# one group, or when writing the first group of a new STF imagefile.
	# The FITS header of an existing STF multigroup image can still be
	# updated, but only if the image is not opened to any particular group,
	# e.g., as "pix" rather than "pix[n]", N > 0.  NEW_[IMAGE|COPY] or
	# READ_WRITE access to "pix[n]" will update only the GPB header.

	if (STF_NEWIMAGE(stf)==YES || STF_GCOUNT(stf)<=1 || STF_GRARG(stf)==0)
	    call stf_wfitshdr (im)
end
