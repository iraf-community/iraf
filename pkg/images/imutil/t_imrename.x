# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMRENAME -- Rename an image or list of images, or move a image or images
# to a new directory.  Pixel files are moved to the current IMDIR.  Moving
# an image to the same directory will move the pixel file if IMDIR has been
# changed since the image was created.

procedure t_imrename()

pointer	sp, old_list, new_list
pointer	old_name, new_name, old_dir, new_dir
bool	verbose

int	list1, list2, root_len
int	imtopen(), imtgetim(), imtlen()
int	fnldir(), isdirectory()
bool	clgetb()

begin
	call smark (sp)
	call salloc (old_list, SZ_LINE, TY_CHAR)
	call salloc (new_list, SZ_LINE, TY_CHAR)
	call salloc (old_name, SZ_PATHNAME, TY_CHAR)
	call salloc (new_name, SZ_PATHNAME, TY_CHAR)
	call salloc (new_dir,  SZ_PATHNAME, TY_CHAR)
	call salloc (old_dir,  SZ_PATHNAME, TY_CHAR)

	# Get input and output image template lists.
	call clgstr ("oldnames", Memc[old_list], SZ_LINE)
	call clgstr ("newnames", Memc[new_list], SZ_LINE)
	verbose = clgetb ("verbose")

	# Check if the output string is a directory.

	if (isdirectory (Memc[new_list], Memc[new_dir], SZ_PATHNAME) > 0) {
	    list1 = imtopen (Memc[old_list])
	    while (imtgetim (list1, Memc[old_name], SZ_PATHNAME) != EOF) {

		# Strip the image section first because fnldir recognizes it
		# as part of a directory.  Place the input image name
		# without a directory or image section in string Memc[old_dir].

		call get_root (Memc[old_name], Memc[new_name], SZ_PATHNAME)
		root_len = fnldir (Memc[new_name], Memc[old_dir], SZ_PATHNAME)
		call strcpy (Memc[new_name+root_len], Memc[old_dir],SZ_PATHNAME)

		call strcpy (Memc[new_dir], Memc[new_name], SZ_PATHNAME)
		call strcat (Memc[old_dir], Memc[new_name], SZ_PATHNAME)
		call img_rename (Memc[old_name], Memc[new_name], verbose)
	    }
	    call imtclose (list1)

	} else {
	    # Expand the input and output image lists.
	    list1 = imtopen (Memc[old_list])
	    list2 = imtopen (Memc[new_list])

	    if (imtlen (list1) != imtlen (list2)) {
	        call imtclose (list1)
	        call imtclose (list2)
	        call error (1, "Different number of old and new image names")
	    }

	    # Do each set of input/output images.
	    while ((imtgetim (list1, Memc[old_name], SZ_PATHNAME) != EOF) &&
		(imtgetim (list2, Memc[new_name], SZ_PATHNAME) != EOF)) {

		call img_rename (Memc[old_name], Memc[new_name], verbose)
	    }

	    call imtclose (list1)
	    call imtclose (list2)
	}

	call sfree (sp)
end


# IMG_RENAME -- Rename an image, optionally printing a message to the STDOUT.

procedure img_rename (old_name, new_name, verbose)

char	old_name[ARB]			#I old image name
char	new_name[ARB]			#I new image name
bool	verbose				#I print message?

begin
	iferr (call imrename (old_name, new_name)) {
	    call eprintf ("Warning: cannot rename `%s' -> `%s'\n")
		call pargstr (old_name)
		call pargstr (new_name)
	} else if (verbose) {
	    call printf ("`%s' -> `%s'\n")
		call pargstr (old_name)
		call pargstr (new_name)
	    call flush (STDOUT)
	}
end
