# CCDDELETE -- Delete an image by renaming it to a backup image.
#
#   1.	Get the backup prefix which may be a path name.
#   2.  If no prefix is specified then delete the image without a backup.
#   3.  If there is a prefix then make a backup image name.
#       Rename the image to the backup image name.
#
#   The backup image name is formed by prepending the backup prefix to the
#   image name.  If a previous backup exist append integers to the backup
#   prefix until a nonexistant image name is created.

procedure ccddelete (image)

char	image[ARB]		# Image to delete (backup)

int	i, imaccess()
pointer	sp, prefix, backup
errchk	imdelete, imrename

begin
	call smark (sp)
	call salloc (prefix, SZ_FNAME, TY_CHAR)
	call salloc (backup, SZ_FNAME, TY_CHAR)

	# Get the backup prefix.
	call clgstr ("backup", Memc[prefix], SZ_FNAME)
	call xt_stripwhite (Memc[prefix])

	# If there is no prefix then simply delete the image.
	if (Memc[prefix] == EOS)
	    call imdelete (image)

	# Otherwise create a backup image name which does not exist and
	# rename the image to the backup image.

	else {
	    i = 0
	    repeat {
		if (i == 0) {
	    	    call sprintf (Memc[backup], SZ_FNAME, "%s%s")
		        call pargstr (Memc[prefix])
		        call pargstr (image)
		} else {
	            call sprintf (Memc[backup], SZ_FNAME, "%s%d%s")
			call pargstr (Memc[prefix])
			call pargi (i)
			call pargstr (image)
		}
	        i = i + 1
	    } until (imaccess (Memc[backup], READ_ONLY) == NO)
	    call imrename (image, Memc[backup])
	}

	call sfree (sp)
end
