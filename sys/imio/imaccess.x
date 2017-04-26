# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>


# IMACCESS -- Test if an image exists and is accessible with the given access
# mode.  If the access mode given is NEW_IMAGE, test if the image name given
# is legal (has a legal extension, i.e., type).  YES is returned if the named
# image exists, NO if no image exists with the given name, and ERR if the
# image name is ambiguous (multiple images, e.g. of different types, exist
# with the same name).

int procedure imaccess (image, acmode)

char	image[ARB]		# image name
int	acmode			# access mode

int	exists, cl_index, cl_size, mode, status
pointer	sp, cluster, ksection, section, root, extn, im
int	iki_access()
errchk	syserrs
pointer	immap()

begin
	call smark (sp)
	call salloc (cluster, SZ_PATHNAME, TY_CHAR)
	call salloc (ksection, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_PATHNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	call iki_init()

	call imparse (image,
	    Memc[cluster], SZ_PATHNAME,
	    Memc[ksection], SZ_FNAME,
	    Memc[section], SZ_FNAME, cl_index, cl_size)

	# If an image section, kernel section, or cluster index was specified
	# we must actually attempt to open the image to determine if the
	# object specified by the full notation exists, otherwise we can just
	# call the IKI access function to determine if the cluster exists.

	if (Memc[section] != EOS || Memc[ksection] != EOS || cl_index >= 0) {
	    mode = acmode
	    if (acmode == 0)
		mode = READ_ONLY
	    iferr (im = immap (image, mode, 0))
		exists = NO
	    else {
		exists = YES
		call imunmap (im)
	    }
	} else {
	    status = iki_access (image, Memc[root], Memc[extn], acmode)
	    if (status > 0)
		exists = YES
	    else if (status == 0)
		exists = NO
	    else
		call syserrs (SYS_IKIAMBIG, image)
	}

	call sfree (sp)
	return (exists)
end
