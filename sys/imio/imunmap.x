# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	<imio.h>

# IMUNMAP -- Unmap a image.  Flush the output buffer, append the bad pixel
# list, update the image header.  Close all files and return buffer space.

procedure imunmap (im)

pointer	im

int	acmode
errchk	imflush, close, imerr, iki_updhdr

begin
	acmode = IM_ACMODE(im)

	# Note that if no pixel i/o occurred, the pixel storage file will
	# never have been opened or created.

	if (IM_PFD(im) != NULL)
	    call imflush (im)

	# Update the image header, if necessary (count of bad pixels,
	# minimum and maximum pixel values, etc.).

	if (IM_UPDATE(im) == YES) {
	    if (acmode == READ_ONLY)
		call imerr (IM_NAME(im), SYS_IMUPIMHDR)

	    # Restore those fields of the image header that may have been
	    # modified to map a section (if accessing an existing image).

	    switch (acmode) {
	    case NEW_COPY, NEW_IMAGE:
		;			# Cannot access section of new image
	    default:
		IM_NDIM(im) = IM_NPHYSDIM(im)
		IM_MTIME(im) = IM_SVMTIME(im)
		call amovl (IM_SVLEN(im,1), IM_LEN(im,1), IM_NDIM(im))
	    }

	    # Update the image header or mask storage file.
	    call iki_updhdr (im)
	}

	# Physically close the image.
	call iki_close (im)

	# If the image is a mask image and the PL_CLOSEPL flag is set, close
	# the associated mask.

	if (IM_PL(im) != NULL && and(IM_PLFLAGS(im),PL_CLOSEPL) != 0)
	    call pl_close (IM_PL(im))

	# Free all buffer space allocated by IMIO.
	call imrmbufs (im)
	call mfree (im, TY_STRUCT)
end
