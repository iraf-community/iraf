# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMGIMAGE -- Get the name of an individual image within a cluster of images,
# i.e., the image name minus any image section.

procedure imgimage (imspec, image, maxch)

char	imspec[ARB]		# full image specification
char	image[ARB]		# receives image name
int	maxch

int	cl_index, cl_size
pointer	sp, cluster, ksection, section

begin
	call smark (sp)
	call salloc (cluster, SZ_PATHNAME, TY_CHAR)
	call salloc (ksection, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)

	call imparse (imspec,
	    Memc[cluster], SZ_PATHNAME,
	    Memc[ksection], SZ_FNAME,
	    Memc[section], SZ_FNAME, cl_index, cl_size)

	if (cl_index >= 0 && cl_size == -1) {
	    call sprintf (image, maxch, "%s[%d]")
		call pargstr (Memc[cluster])
		call pargi (cl_index)
	} else if (cl_index >= 0 && cl_size > 0) {
	    call sprintf (image, maxch, "%s[%d/%d]")
		call pargstr (Memc[cluster])
		call pargi (cl_index)
		call pargi (cl_size)
	} else
	    call strcpy (Memc[cluster], image, maxch)

	call strcat (Memc[ksection], image, maxch)
	call sfree (sp)
end
