include <ctype.h>
include	<imhdr.h>

#---------------------------------------------------------------------------
.help sp_wcsparams 3Aug95 source
.ih
NAME
sp_wcsparams -- Read the WCS descriptor from the parameters.
.ih
DESCRIPTION
This procedure returns the WCS descriptor created from task parameters
and the logical space that will be graphed.
.ih
BUGS
This only deals with two axes.
.endhelp
#---------------------------------------------------------------------------
procedure sp_wcsparams( mw, log_x1, log_x2, log_y1, log_y2 )

pointer mw              # O: The MWCS descriptor.
real    log_x1, log_x2,
        log_y1, log_y2  # O: The extent of the logical space to graph.

# Declarations.
pointer	b		# Buffer pointer.
double	clgetd()	# Get double-valued parameter.
real	clgetr()	# Get real-valued parameter.
pointer	im		# Temporary image descriptor.
pointer	immap()		# Open an image.
pointer	impl2s()	# Put line in 2d image.
pointer	imw		# Temporary MWCS descriptor.
pointer	mw_newcopy()	# Copy MWCS descriptor.
pointer	mw_openim()	# Get MWCS descriptor from image.
char	s[SZ_LINE]	# Generic string.

string	tmpimage ".SPWCSS"

begin
	# Since no one knows how mwcs really works, we cheat.
	# Create an image and set the header keywords to what
	# the parameters are.  Then use the image load to get the
	# mwcs instead of trying to create it from scratch.

	# Create an image.
	iferr (call imdelete (tmpimage))
	    ;
	im = immap (tmpimage, NEW_IMAGE, 20000)
	IM_NDIM(im) = 2
	IM_LEN(im,1) = 1
	IM_LEN(im,2) = 1
	IM_PIXTYPE(im) = TY_SHORT

	# Now populate the WCS-relevant keywords.
	call clgstr ("ctype1", s, SZ_LINE)
	call imastr (im, "ctype1", s)
	call clgstr ("ctype2", s, SZ_LINE)
	call imastr (im, "ctype2", s)
	call imaddd (im, "crpix1", clgetd ("crpix1"))
	call imaddd (im, "crpix2", clgetd ("crpix2"))
	call imaddd (im, "crval1", clgetd ("crval1"))
	call imaddd (im, "crval2", clgetd ("crval2"))
	call imaddd (im, "cd1_1", clgetd ("cd1_1"))
	call imaddd (im, "cd1_2", clgetd ("cd1_2"))
	call imaddd (im, "cd2_1", clgetd ("cd2_1"))
	call imaddd (im, "cd2_2", clgetd ("cd2_2"))

	# Write a pixel, close and reopen the image.
	b = impl2s (im, 1)
	call imunmap (im)
	im = immap (tmpimage, READ_ONLY, 0)

	# Retrieve the MWCS descriptor.  Make a copy so we can close the
	# temporary image.
	imw = mw_openim (im)
	mw = mw_newcopy (imw)

	# Get the logical workspace.
	log_x1 = clgetr ("log_x1")
	log_x2 = clgetr ("log_x2")
	log_y1 = clgetr ("log_y1")
	log_y2 = clgetr ("log_y2")

	# That's all folks.
	call mw_close (imw)
	call imunmap (im)
	call imdelete (tmpimage)
end
#---------------------------------------------------------------------------
# End of sp_wcsparams
#---------------------------------------------------------------------------
