include	<imhdr.h>

#* HISTORY *
#* B.Simon	30-Sep-98	Original code

# GF_HASGROUP -- Return YES if image contains the specified group or extension
#
# This code assumes that all extensions are images and will return NO if
# asked to open a table extension.

int procedure gf_hasgroup (im, gn)

pointer	im		# i: image descriptor
int	gn		# i: group number
#--
int	group
pointer	sp, image, gr

pointer	immap()

begin
	call smark (sp)
	call salloc (image, SZ_PATHNAME, TY_CHAR)

	call sprintf (Memc[image], SZ_PATHNAME, "%s[%d]")
	call pargstr (IM_HDRFILE(im))
	call pargi (gn)

	# Try opening first as an image

	iferr {
	    gr = immap (Memc[image], READ_ONLY, NULL)
	} then {
	    group = NO
	} else if (gr == NULL) {
	    group = NO
	} else {
	    call imunmap (gr)
	    group = YES
	}

	call sfree (sp)
	return (group)
end
