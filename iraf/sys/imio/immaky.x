# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<imhdr.h>
include	<imio.h>


# IM_MAKE_NEWCOPY -- Copy the header of an existing, mapped image to
# initialize the header of a new image.  Clear all fields that describe
# the pixels (a NEW_COPY image does not inherit any pixels).

procedure im_make_newcopy (im, o_im)

pointer	im				# new copy image
pointer	o_im				# image being copied

pointer	mw
int	strlen()
long	clktime()
pointer	mw_open()
bool	strne(), envgetb()
errchk	imerr, realloc, mw_open, mw_loadim, mw_saveim, mw_close

begin
	if (strne (IM_MAGIC(o_im), "imhdr"))
	    call imerr (IM_NAME(im), SYS_IMMAGNCPY)

	# Copy the old image header (all fields, including user fields).
	# Note that the incore version of the old header may be shorter than
	# the actual header, in which case the user fields are currently
	# not copied (would require reopening old header file).  This is
	# unlikely, however, since a very large in memory user area is
	# allocated.

	# Update the value of HDRLEN for the input image in case the
	# header has grown since the image was opened.

	IM_HDRLEN(o_im) = LEN_IMHDR +
	    (strlen(Memc[IM_USERAREA(o_im)])+1 + SZ_STRUCT-1) / SZ_STRUCT

	# Copy the header.
	if (IM_LENHDRMEM(im) < IM_HDRLEN(o_im)) {
	    IM_LENHDRMEM(im) = IM_HDRLEN(o_im) + (SZ_UAPAD / SZ_STRUCT)
	    call realloc (im, IM_LENHDRMEM(im) + LEN_IMDES, TY_STRUCT)
	}
	call amovi (IM_MAGIC(o_im), IM_MAGIC(im), IM_HDRLEN(o_im) + 1)

	# If the old image was opened with an image section, modify the
	# WCS of the new image accordingly.  The section is applied to the
	# MWCS Lterm automatically when the WCS is loaded from an image,
	# so all we have to do is load the WCS of the old image section,
	# and store it in the new image.

	if (IM_SECTUSED(o_im) == YES)
	    if (!envgetb ("nomwcs")) {
		iferr (mw = mw_open (NULL, IM_NPHYSDIM(o_im)))
		    call erract (EA_WARN)
		else {
		    call mw_loadim (mw, o_im)
		    call mw_saveim (mw, im)
		    call mw_close (mw)
		}
	    }

	# If the pixels of the old image were stored in byte stream mode,
	# make the new image that way too.  Otherwise, the physical line
	# length must be recomputed, as the new image may reside on a
	# device with a different block size.

	if (IM_LEN(im,1) == IM_PHYSLEN(im,1))
	    IM_VCOMPRESS(im) = YES

	IM_PIXOFF(im) = NULL
	IM_HGMOFF(im) = NULL
	IM_BLIST(im) = NULL
	IM_SZBLIST(im) = 0
	IM_NBPIX(im) = 0
	IM_LIMTIME(im) = 0
	IM_OHDR(im) = o_im
	IM_PIXFILE(im) = EOS

	IM_CTIME(im) = clktime (long(0))
	IM_MTIME(im) = IM_CTIME(im)

	# Add a line to the history file (inherited from old image).
	call strcat ("New copy of ", IM_HISTORY(im), SZ_IMHIST)
	call strcat (IM_NAME(o_im), IM_HISTORY(im), SZ_IMHIST)
	call strcat ("\n", IM_HISTORY(im), SZ_IMHIST)
end
