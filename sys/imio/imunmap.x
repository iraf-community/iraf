# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<plset.h>
include	<imhdr.h>
include	<imio.h>

# IMUNMAP -- Unmap a image.  Flush the output buffer, append the bad pixel
# list, update the image header.  Close all files and return buffer space.

procedure imunmap (im)

pointer	im

pointer	pl
int	acmode, flags
errchk	imflush, close, imerr, iki_updhdr, pl_savef

begin
	pl = IM_PL(im)
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
	    if (pl == NULL)
		call iki_updhdr (im)
	    else {
		flags = 0
		if (acmode == READ_WRITE)
		    flags = PL_UPDATE
		call pl_savef (IM_PL(im), IM_NAME(im), IM_TITLE(im), flags)
	    }
	}

	# Physically close the image.
	if (IM_PL(im) != NULL) {
	    if (IM_PFD(im) != NULL)
		call close (IM_PFD(im))
	    if (and (IM_PLFLAGS(im), PL_CLOSEPL) != 0)
		call pl_close (IM_PL(im))
	} else
	    call iki_close (im)

	# Free all buffer space allocated by IMIO.
	call imrmbufs (im)
	call mfree (im, TY_STRUCT)
end
